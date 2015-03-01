(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-custom-hash-table)
(use-package :cl-custom-hash-table)
(ql:quickload :dlist2)
(ql:quickload :alexandria)
(use-package :alexandria)

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

;;(declaim (inline lsxhash))
(defun lsxhash (x)
  "Return a hash value for value X.
X, (car X), and (cdr X) may be a list, a symbol, or a number."
  ;; FIXME: handle circular lists
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  (declare (values (and fixnum unsigned-byte))) ;inferred automatically (see describe 'lsxhash)
  ;; in SBCL, etypecase takes 80% of the time of defmethod-ing on the different types of X: (let ((h (make-hash-table))) (timediff (lsxhash h) (mlsxhash h) :showtimes t))
  (etypecase x
    (single-float (sxhash x))
    (double-float (sxhash x))
    (ratio (sxhash x))
    (fixnum (sxhash x)) ;;in SBCL, close numbers seem to have close hashes.
    (string (sxhash x)) ;;in SBCL, (sxhash "ABC") == (sxhash 'abc).
    ;;(number (sxhash x))
    (symbol (sxhash x))
    ;; here, X can't be nil since (symbolp nil) == T.
    (list (mix (lsxhash (car x)) (lsxhash (cdr x))))
    (hash-table (let ((ret 448291823))
		  (declare (type (and fixnum unsigned-byte) ret))
		  (setf ret (mix (sxhash (hash-table-count x))
				 (mix ret (sxhash (hash-table-test x)))))
		  ;; use logxor for speed and so that the order of key/value pairs does not matter
		  (maphash (lambda (k v) (setf ret (logxor ret (mix (lsxhash k) (lsxhash v)))))
			   x)
		  ret))
    (simple-array (let* ((size (array-total-size x))
			 (dim (array-dimensions x))
			 (type (array-element-type x))
			 (ret 518591303))
		    (declare (type (and fixnum unsigned-byte) ret))
		    (setf ret (mix (mix ret (sxhash type))
				   (lsxhash dim)))
		    (ecase type
		      ((fixnum)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (declare (type fixnum e))
			      (setf ret (mix ret (sxhash e))))))
		      ((t)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (setf ret (mix ret (lsxhash e))))))
		      )
		    ret))))

(define-custom-hash-table-constructor make-lsxhash-equal-hash-table
    ;; equalp required when hashing hash tables
    :test equal :hash-function lsxhash)

;;;; A deterministic function cacher (mru: most recently used)

(defclass mru-cache ()
  ((slots :initarg :slots :type (and fixnum unsigned-byte))
   (ht :initarg :ht :type hash-table)
   (mru :initarg :mru :type dlist2))
  (:documentation "A most-recently-used-cache class."))

(defun make-mru-cache (slots)
  (declare (type (integer 1 #.most-positive-fixnum) slots))
  (let* ((ht (make-lsxhash-equal-hash-table))
	 (mru (dlist2:dlist))
	 (c (make-instance 'mru-cache
			   :slots slots
			   :ht ht
			   :mru mru)))
    c))

(declaim (inline mru-cache-key-present-p))
(defun mru-cache-key-present-p (mru key)
  "Return whether the key KEY is present in the mru-cache MRU."
  (declare (type mru-cache mru))
  (with-custom-hash-table
    (multiple-value-bind (mru-dcons p) (gethash key (slot-value mru 'ht))
      (declare (ignore mru-dcons))
      p)))

(declaim (inline mru-dlist-hit-mru-dcons))
(defun mru-dlist-hit-mru-dcons (mru mru-dcons)
  (declare (type dlist2:dlist mru)
	   (type dlist2:dcons mru-dcons))
  "Bring the dcons MRU-DCONS to the front of the dlist MRU."
  ;;(print "mru hit") (describe-object mru t)
  ;; slice out mru-dcons from its current position.
  (dlist2:dcons-delete mru-dcons)
  ;; bring mru-dcons to the beginning of the dlist.
  (let ((first (dlist2:dlist-first mru)))
    ;; need to re-use existing dcons mru-dcons, because the hash-table points to it.
    (dlist2:dcons-existing-insert-between first mru-dcons (dlist2:next first)))
  ;; updating the hash-table is not necessary, because its key still points to MRU-DCONS.
  )

(declaim (inline mru-cache-get-value))
(defun mru-cache-get-value (mru key &optional (default nil))
  "If the key KEY is present in mru-cache MRU, return the values for key KEY and T as secondary value and bring KEY to the front of MRU.
If key is not present return the value DEFAULT and as secondary value NIL."
  (let ((ht (slot-value mru 'ht))
	(mru (slot-value mru 'mru)))
    (with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    ;; the function call is known, therefore bring it to front in mru.
	    (destructuring-bind (key . value) (dlist2:data mru-dcons)
	      (declare (ignore key))
	      (mru-dlist-hit-mru-dcons mru mru-dcons)
	      ;;(describe-object mru t)
	      (values value t))
	    (values default nil))))))

(declaim (inline mru-cache-set))
(defun mru-cache-set (mru key value)
  "Set the value VALUE for the key KEY in mru-cache MRU, if KEY was not present yet.
Bring the key-value-pair to the front of the mru-cache."
  (declare (type mru-cache mru))
  (let* ((ht (slot-value mru 'ht))
	 (slots (slot-value mru 'slots))
	 (mru (slot-value mru 'mru)))
    (with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (mru-dlist-hit-mru-dcons mru mru-dcons)
	    ;; push new cache entry to front of mru.
	    (let (front-dcons)
	      (if (>= (hash-table-count ht) slots)
		  (progn
		    ;; the cache is already full. Remove the last element, and re-use its dcons for the new front element.
		    (destructuring-bind (old-key . old-value) (dlist2:data (dlist2:prev (dlist2:dlist-last mru)))
		      (declare (ignorable old-value))
		      ;;(print "mru full") (describe-object mru t)
		      (setf front-dcons (dlist2:dlist-pop-dcons mru :from-end t))
		      (remhash old-key ht))
		    (setf (dlist2:data front-dcons) (cons key value))
		    (let ((first (dlist2:dlist-first mru)))
		      ;; need to re-use existing dcons mru-dcons, because the hash-table points to it.
		      (dlist2:dcons-existing-insert-between first front-dcons (dlist2:next first)))
		    ;;(describe-object mru t)
		    )
		  (setf front-dcons (dlist2:dlist-push-return-new-dcons (cons key value) mru)))
	      ;; insert front-dcons into ht
	      (setf (gethash key ht) front-dcons)
	      ;;(print (list "hash-table-count" (hash-table-count ht) slots))
	      value))))))

;; TODO: implement a new function like mru-function-cacher that allows saving and recalling multiple values.

(defun mru-function-cacher (fun slots)
  "Most-recently-used cache for determinitic and side-effect-free functions."
  (let* ((mru (make-mru-cache slots)))
    (lambda (&rest rest)
      ;;(print (list "mru-function-cacher ht" (let (l) (maphash (lambda (key value) (push (list key value) l)) (slot-value mru 'ht)) l) "mru" (slot-value mru 'mru)))
      (if (mru-cache-key-present-p mru rest)
	  (nth-value 0 (mru-cache-get-value mru rest))
	  ;; the function call is unkown
	  (let* ((value (apply fun rest)))
	    (mru-cache-set mru rest value)
	    value)))))

(let ((last nil))
  (flet ((add (a b)
	   ;;(print (list "add" a b))
	   (setf last (list a b))
	   (+ a b)))
    (let ((cadd (mru-function-cacher #'add 2)))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last '(1 2)))
      (setf last nil)
      (assert (= (funcall cadd 3 4) 7))
      (assert (equal last '(3 4)))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last nil))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last nil))
      ;; now the mru has items '((1 2) (3 4)). test pushing the last mru-cached item, i.e. '(3 4), out.
      (setf last nil)
      (assert (= (funcall cadd 5 6) 11))
      (assert (equal last '(5 6)))
      ;; check that '(3 4) was not cached in the mru anymore.
      (setf last nil)
      (assert (= (funcall cadd 3 4) 7))
      (assert (equal last '(3 4)))
      (setf last nil)
      (assert (= (funcall cadd 1 2) 3))
      (assert (equal last '(1 2))))))

#|
(ql:quickload :utils)
(flet ((add (a b)
	 (+ a b)))
  (let ((cadd (mru-function-cacher #'add 2)))
    (defun time-mru ()
      (time
       (utils:timesec (lambda ()
			(funcall cadd 1 2)
			(funcall cadd 3 4)
			(funcall cadd 1 2)
			(funcall cadd 1 2)
			(funcall cadd 5 6)
			(funcall cadd 3 4)
			(funcall cadd 1 2)))))))
;; with mru implementation using dlist: CL-USER> (time-mru) = 19/1638400 1.15966796875d-5 8192
;; with mru implementation using dlist2: CL-USER> (time-mru) = 7/1024000 6.8359375d-6 8192
|#

;;;; Property lists, i.e. a list of alternating SYMBOL and VALUE.
;;;; Example '(a 1 b 2 c (a list))

(defun plist-cdr (symbol plist)
  "Return the cdr of plist PLIST whose car is SYMBOL and whose cadr is the value of SYMBOL.
Return values NIL if SYMBOL is not found in plist PLIST, or if PLIST has an odd length."
  (labels ((rec (plist)
	     (if (null plist)
		 nil
		 (if (eq symbol (car plist))
		     plist
		     (rec (cddr plist))))))
    (rec plist)))

(defun plist-get (symbol plist)
  "Return the value associated with symbol SYMBOL in plist PLIST, or NIL if SYMBOL is not in PLIST.
Returns NIL if plist has an odd length."
  (cadr (plist-cdr symbol plist)))

(defun plist-has (symbol plist)
  "Return whether plist PLIST has symbol SYMBOL."
  (not (null (plist-cdr symbol plist))))

(defun plist-set (symbol val plist)
  "Return the plist PLIST that results from setting the symbol SYMBOL in PLIST to value VAL (or extending PLIST with the symbol SYMBOL and value VAL if SYMBOL didn't exist in PLIST before)."
  (let ((old-cdr (plist-cdr symbol plist)))
    (if (null old-cdr)
	(cons symbol (cons val plist))	
	(progn
	  (setf (cadr old-cdr) val)
	  plist))))

(defun alist-to-plist (alist)
  "Return the plist that is equivalent to alist ALIST."
  ;; NOTE: could return an plist that has multiple entries with the same symbol. (maybe FIXME: raise an error in that case.)
  (let ((plist nil))
    (loop for (a . b) in alist do
	 (setf plist (cons b (cons a plist))))
    (nreverse plist)))

(defun plist-to-alist (plist)
  "Return the alist that is equivalent to plist PLIST."
  (let ((alist nil))
    (loop for a on plist by #'cddr do
	 (setf alist (cons (cons (car a) (cadr a)) alist)))
    (nreverse alist)))

