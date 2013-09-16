(use-package :cl-custom-hash-table)

(defstruct multitree-type
  ;; Takes the multitree, a key, and a value. Destructively add the value or change the value stored below the key in the multitree. Returns the modified multitree and a boolean indicating if the key was changed (and not added).
  (set-elt nil :type (function (t t t) (values t (member t nil))))
  ;; Takes the multitree and the element whose subtree is to be returned. Returns the subtree (a multitree) and a value indicating if the element was present.
  (select-elt nil :type (function (t t) (values t (member t nil)))))

(defun multitree-select-path (multitree-type multitree path)
  "Walk through the MULTITREE (which must be of type MULTITREE-TYPE) recursively by selecting the car of PATH.
Returns as values the subtree found and a value indicating if all elements of PATH were found in MULTITREE."
  (if (null path)
      multitree
      (let* ((e (car path))
	     (select (multitree-type-select-elt multitree-type)))
	(multiple-value-bind (subtree present-p)
	    (funcall select multitree e)
	  (if present-p
	      (multitree-select-path multitree-type subtree (cdr path))
	      (values nil nil))))))

(defun multitree-set-elt-hash-table (multitree key value)
  (multiple-value-bind (old-value present)
      (gethash key multitree)
    (declare (ignore old-value))
    (setf (gethash key multitree) value)
    (values multitree present)))

(let* ((smt0 (make-hash-table))
       (smt1 (multitree-set-elt-hash-table smt0 'a 1))
       (smt2 (multitree-set-elt-hash-table smt1 'b 2))
       (mt0 (make-hash-table))
       (mt1 (multitree-set-elt-hash-table mt0 'a smt2))
       (mt2 (multitree-set-elt-hash-table mt1 'b 3))
       (mt-type (make-multitree-type :set-elt #'multitree-set-elt-hash-table
				     :select-elt (lambda (mt e) (gethash e mt)))))
  (assert (eq 3 (multitree-select-path mt-type mt2 '(b))))
  (assert (eq 2 (multitree-select-path mt-type mt2 '(a b))))
  (assert (eq 1 (multitree-select-path mt-type mt2 '(a a)))))

;;;; Implementation of a better sxhash for lists
;; this is copied from SBCL
;; originally this was "(ftype (sfunction". what's an sfunction? 
(declaim (ftype (function ((and fixnum unsigned-byte)
			   (and fixnum unsigned-byte))
			  (and fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun mix (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (and fixnum unsigned-byte) x y))
  (let* ((xy (+ (* x 3) y)))
    (logand most-positive-fixnum
            (logxor 441516657
                    xy
                    (ash xy -5)))))

(declaim (inline lsxhash))
(defun lsxhash (x)
  "Return a hash value for value X.
X, (car X), and (cdr X) may be a list, a symbol, or a number."
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  (declare (values (and fixnum unsigned-byte))) ;inferred automatically (see describe 'lsxhash)
  (etypecase x
    (number (sxhash x))
    (symbol (sxhash x))
    ;; here, X can't be nil since (symbolp nil) == T.
    (list (mix (lsxhash (car x)) (lsxhash (cdr x))))))

(defun make-bit-cache (size)
  (make-array size :element-type 'fixnum :initial-element 0))

;;;; A doubly linked list

;;(defclass dllist ()
;;  ((obj :accessor dllist-obj :type t :initarg nil)
;;   (bdr :accessor dllist-bdr :initarg nil :type (or dllist nil) :documentation "The ancestor list.")
;;   (cdr :accessor dllist-cdr :initarg nil :type (or dllist nil) :documentation "The successor list."))
;;  (:documentation "A doubly linked list"))

(defstruct dllist
  "An element of a doubly linked list."
  (:print-function #'print-dllist)
  (obj nil :type t)
  (bdr nil :type (or null dllist))
  (cdr nil :type (or null dllist)))

(defun dllist-p (o)
  ;; The empty doubly linked list is defined to be NIL.
  (or (null o) (typep o 'dllist)))

(defun print-dllist (dll stream depth)
  "Print a doubly linked list.
Doesn't yet print whether dll has any circularities in it (but detects them already)."
  (declare (ignore depth))
  ;; FIXME: rewrite this function to print a reader-readable format (or define a reader function or whatever is supposed work).
  (print-unreadable-object (dll stream :type nil :identity nil)
    (format stream "DLLIST")
    (let ((visited nil))
      ;; FIXME: only detect loops if *PRINT-CIRCLE* is true.
      ;; FIXME: print where the loop occurs (using "#1=OBJ ...more-objs... #1#" syntax).
      (format stream " O:~A" (dllist-obj dll))
      (push dll visited)
      (do ((cur (dllist-cdr dll) (dllist-cdr cur))) ((or (null cur) (find cur visited)))
	(push cur visited)
	(format stream " ~A" (dllist-obj cur)))
      (do ((cur (dllist-bdr dll) (dllist-bdr cur))) ((or (null cur) (find cur visited)))
	(push cur visited)
	(format stream " B:~A" (dllist-obj cur))))))

(defmethod print-object ((l dllist) stream)
  (print-dllist l stream 0))

(defun dllist (&rest args)
  (if (null args)
      nil
      (let* ((dll (make-dllist :obj (car args)))
	     (ret dll))
	(loop for a in (cdr args) do
	     (let ((adll (make-dllist :obj a :bdr dll)))
	       (setf (dllist-cdr dll) adll)
	       (setf dll adll)))
	ret)))

(defun dllist-first (dll)
  "Returns the first doubly linked element of doubly linked list DLL.
DLL must not be a circular doubly linked list."
  (if (null dll)
      nil
      (do ((cur dll (dllist-bdr cur))) ((null (dllist-bdr cur)) cur))))

(defun dllist-last (dll)
  "Returns the last doubly linked element of doubly linked list DLL.
DLL must not be a circular doubly linked list."
  (if (null dll)
      nil
      (do ((cur dll (dllist-cdr cur))) ((null (dllist-cdr cur)) cur))))

(defun dllist-circular (arg0 &rest args)
  "Return a circular doubly linked list containing ARG0 as first element and the objects in list ARGS as elements in the circle."
  (let* ((dll (apply #'dllist arg0 args))
	 (dll-last (dllist-last dll)))
    (setf (dllist-bdr dll) dll-last)
    (setf (dllist-cdr dll-last) dll)
    dll))

;; Add (defun dllist-circular-p (dll)) which returns T if dll is one circle (i.e. the bdr of the first points to the last and the cdr of the last points to the first).
;; Add (defun dllist-circularities-p (dll)) which returns T if dll contains any circularities (i.e. dll is the first in a chain whose last element points to some element in the chain).
;; Add (defun dllist-broken-p (dll)) which returns T if the bdr of the cdr of element A is not A for any element reachable by DLL.

(defun dllist-delete (dll &key (return-cdr t))
  "Modify the dllist by removing the element DLL.
DLL may be circular.
Returns the former cdr of DLL if RETURN-CDR is T, the bdr otherwise, and NIL if DLL contained only one element."
  (let ((bdr (dllist-bdr dll))
	(cdr (dllist-cdr dll)))
    (if (eq bdr dll)
	nil
	(progn
	  (setf (dllist-cdr bdr) cdr)
	  (setf (dllist-bdr cdr) bdr)
	  (if return-cdr cdr bdr)))))

(defun dllist-insert (dll obj &key (after t) (new-circular nil))
  "Modify the dllist DLL by inserting OBJ after the element DLL (or before DLL if after is NIL).
Returns the dllist pointing to the newly inserted element, or a newly constructed (circular if NEW-CIRCULAR is T) dllist."
  (if (null dll)
      (if new-circular (dllist-circular obj) (dllist obj))
      (progn
	(when (not after)
	  (setf dll (dllist-bdr dll)))
	(let* ((cdr (dllist-cdr dll))
	       (new-dll (make-dllist :obj obj :bdr dll :cdr cdr)))
	  (setf (dllist-cdr dll) new-dll)
	  (setf (dllist-bdr cdr) new-dll)
	  new-dll))))
