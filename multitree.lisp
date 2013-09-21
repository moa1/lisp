;;(use-package :cl-custom-hash-table)

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
  (etypecase x
    (single-float (sxhash x))
    (double-float (sxhash x))
    (ratio (sxhash x))
    (fixnum (sxhash x))
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
		  ret))))

(defun make-bit-cache (size)
  (make-array size :element-type 'fixnum :initial-element 0))

;;;; A doubly linked list

;;(deftype dllist ()
;;  (or null dllist))

(defstruct dllist
  "An element of a doubly linked list.
To clarify the nomenclature of 'element' and 'object' for DLLIST, the object of this element is OBJ."
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
  (assert (eq after t))
  (if (null dll)
      (if new-circular (dllist-circular obj) (dllist obj))
      (progn
	(let* ((cdr (dllist-cdr dll))
	       (new-dll (make-dllist :obj obj :bdr dll :cdr cdr)))
	  (setf (dllist-cdr dll) new-dll)
	  (unless (null cdr)
	    (setf (dllist-bdr cdr) new-dll))
	  new-dll))))

(defmacro with-gensyms (symbols &body body)
  ;;(declare (type unique-list symbols))
  `(let ,(loop for symbol in symbols collect `(,symbol (gensym)))
     ,@body))

(defmacro specializing-if (test then &optional else)
  "If TEST is T, only insert THEN, if TEST is NIL, only ELSE, otherwise the if-statement (if ,test ,then ,else)."
  (case test
    ((t) then)
    ((nil) else)
    (t `(if ,test ,then ,else))))

(defmacro do-dllist ((cur dll &key circular) &body body)
  "Iterate over the elements of DLL and assign CUR to each element (get the object using (DLLIST-OBJ CUR)).
If CIRCULAR is true, DLL is assumed to be a circular list and the iteration starts at DLL and ends at (DLLIST-BDR DLL).
If CIRCULAR is false, DLL is non-circular and iteration starts at (DLLIST-FIRST DLL) and ends at (DLLIST-LAST DLL).
Returns NIL."
  ;; FIXME: add an option :dir with values :bdr or :cdr, which determines the direction. (i.e. a non-circular list will only be traversed in one direction.)
  ;; FIXME: get rid of warning "undefined variable #:GO" in (do-dllist (cur (dllist 1 2 3)) (print (dllist-obj cur)))
  (with-gensyms (dll-evaluated first)
    `(let ((,dll-evaluated ,dll))
       (when ,dll-evaluated
	 (specializing-if ,circular
	     (let* ((,first ,dll-evaluated)
		    (,cur ,first))
	       ,@body
	       (do ((,cur (dllist-cdr ,dll-evaluated) (dllist-cdr ,cur))) ((eq ,cur ,first))
		 ,@body))
	     (do ((,cur (dllist-first ,dll-evaluated) (dllist-cdr ,cur))) ((null ,cur))
	       ,@body))))))

(defun dllist-insert-list (dll dll2 &key (after t) (dll2-is-circular nil))
  "Modify the dllist DLL by inserting all elements of dllist DLL2 (in order) after the element DLL.
If DLL2-IS-CIRCULAR is true, the circle of DLL2 is broken before the element DLL2 before inserting.
If DLL2 is circular, and DLL2-IS-CIRCULAR is false, the result is undefined.
If DLL is empty and DLL2 is circular, the result is the circular DLL2.
DLL may or may not be circular.
Return the modified DLL at the position of the original DLL."
  (if (null dll)
      dll2
      (let ((orig-dll dll))
	(do-dllist (cur dll2 :circular dll2-is-circular)
	  (setf dll (dllist-insert dll (dllist-obj cur) :after after)))
	orig-dll)))
;; write tests for dllist-insert-list

(defun dllist-to-list (dll &key dll-is-circular obj)
  "Return a freshly consed list of the doubly linked list DLL with the same order as followed by DO-DLLIST.
If DLL-IS-CIRCULAR is true, DLL may be a circular dllist.
If OBJ is true, the list is made of the DLL elements, and of the objects of DLL otherwise."
  ;; FIXME: add option :dir which determines the dir of do-dllist.
  (let ((l nil))
    ;; FIXME: when do-dllist has option :dir, avoid nreverse by inverting dir for circular lists.
    ;; FIXME avoid nreverse for non-circular lists by first going into :dir and then iterating until the original DLL is reached (and consing every position. this gives a list of correct order).
    (if obj
	(do-dllist (cur dll :circular dll-is-circular)
	  (setf l (cons (dllist-obj cur) l)))
	(do-dllist (cur dll :circular dll-is-circular)
	  (setf l (cons cur l))))
    (nreverse l)))

;;;; implement a priority queue using a fibonacci heap to find the element with lowest/highest priority.
;;;; See Wikipedia article "Fibonacci heap".

(deftype dltree ()
  `dllist)

(defstruct node
  (priority 0 :type number)
  (marked nil :type (or t null))
  ;; The number of children (total number of leaves) of this node.
  (degree 0 :type (and unsigned-byte fixnum))
  ;; The doubly linked list of children nodes. (Or NIL if no children).
  (children nil :type (or null dltree)))

(defclass fibheap ()
  ((forest :accessor fibheap-forest :type (or null dllist) :initarg nil
	   :documentation "A doubly linked list of trees. Each object (accessed by dllist-obj) of the list is a NODE."
	   :initarg :forest)
   (root :accessor fibheap-root :type (or null dltree)
	 :documentation "A pointer to the minimum of FOREST."
	 :initarg :root)
   (size :accessor fibheap-size :type (and fixnum unsigned-byte)
	 :documentation "The total number of nodes stored in the heap."
	 :initarg :size))
  (:documentation "A fibonacci heap."))

(defmethod print-object ((fh fibheap) stream)
  (print-unreadable-object (fh stream)
    (format stream "FIBHEAP :FOREST ~A :ROOT ~A :SIZE ~A" (fibheap-forest fh) (fibheap-root fh) (fibheap-size fh))))
(defun list-fibheap-forest (fh-forest)
  (labels ((collect (dll)
	     (if (null dll)
		 nil
		 (let ((ret nil))
		   (do-dllist (cur dll :circular t)
		     (setf ret (append ret (list :P (node-priority (dllist-obj cur))
						 :C (collect (node-children (dllist-obj cur)))))))
		   ret))))
    (collect fh-forest)))

(defgeneric fibheap-empty (fh))
(defmethod fibheap-empty ((fh fibheap))
  (null (fibheap-root fh)))

(defgeneric fibheap-min (fh))
(defmethod fibheap-min ((fh fibheap))
  "Return the minimum of the fibonacci heap FH, or NIL if FH is empty."
  (if (fibheap-empty fh)
      nil
      (node-priority (dllist-obj (fibheap-root fh)))))

(defgeneric fibheap-merge (fh1 fh2)
  (:documentation "Destructively merge fibonacci heaps FH1 and FH2 and return the resulting fibonacci heap (which shares memory with FH1 and FH2)."))
(defmethod fibheap-merge ((fh1 fibheap) (fh2 fibheap))
  ;; FIXME: Change this function so that it always changes FH1 and doesn't choose returning FH1 or FH2 (this makes it possible to avoid a (setf fh (fibheap-merge fh1 fh2)).
  (if (fibheap-empty fh1)
      (if (fibheap-empty fh2)
	  nil
	  fh2)
      (if (fibheap-empty fh2)
	  fh1
	  (if (< (fibheap-min fh1) (fibheap-min fh2))
	      (progn
		(setf (fibheap-forest fh1)
		      (dllist-insert-list (fibheap-forest fh1) (fibheap-forest fh2) :dll2-is-circular t))
		;; (fibheap-root fh1) points to the correct element
		(incf (fibheap-size fh1) (fibheap-size fh2))
		fh1)
	      (progn
		(setf (fibheap-forest fh2)
		      (dllist-insert-list (fibheap-forest fh2) (fibheap-forest fh1) :dll2-is-circular t))
		;; (fibheap-root fh2) points to the correct element
		(incf (fibheap-size fh2) (fibheap-size fh1))
		fh2)))))

(defun fibheap-new (&optional priority)
  "Create a new fibheap.
If PRIORITY is specified, a node with this priority is created."
  (if priority
      (let* ((new-node (make-node :priority priority))
	     (new-forest (dllist-circular new-node))
	     (new-fibheap (make-instance 'fibheap
					 :forest new-forest
					 :root new-forest
					 :size 1)))
	new-fibheap)
      (make-instance 'fibheap :forest nil :root nil :size 0)))

(defgeneric fibheap-insert (fh priority)
  (:documentation "Destructively insert a new element with priority PRIORITY into the fibonaccy heap FH and return the new element."))
(defmethod fibheap-insert ((fh fibheap) priority)
  (fibheap-merge fh (fibheap-new priority)))

(defmacro swap (var1 var2)
  "Swap the values of places VAR1 and VAR2.
Returns VAR1."
  (with-gensyms (temp)
    `(progn
       (let ((,temp ,var1))
	 (setf ,var1 ,var2)
	 (setf ,var2 ,temp)))))

(defgeneric fibheap-pop (fh)
  (:documentation "Destructively remove the minimum of fibheap FH and return the resulting tree.
Get the minimum value by fibheap-min first."))
(defmethod fibheap-pop ((fh fibheap))
  (declare (optimize (debug 3) (safety 3)))
  (labels ((remove-root ()
	     "In the first phase, remove the root node; its children become part of the forest."
	     (let* ((children (node-children (dllist-obj (fibheap-root fh)))))
	       (setf (fibheap-forest fh) (dllist-delete (fibheap-root fh)))
	       (setf (fibheap-root fh) nil) ;; Just so that we remember that the root is invalid for now.
	       ;;(format t "AAAREMOVE-ROOT A (fibheap-forest fh):~A children:~A~%" (list-fibheap-forest (fibheap-forest fh)) (list-fibheap-forest children))
	       (setf (fibheap-forest fh) (dllist-insert-list (fibheap-forest fh) children :dll2-is-circular t))
	       ;;(format t "AAAREMOVE-ROOT B (fibheap-forest fh):~A~%" (list-fibheap-forest (fibheap-forest fh)))
	       (decf (fibheap-size fh))))
	   (merge-trees-from-forest (root1 root2)
	     "TREE1 and TREE2 are elements of (FIBHEAP-FOREST FH). Merge them and return the remaining tree."
	     (let ((node1 (dllist-obj root1))
		   (node2 (dllist-obj root2)))
	       (when (> (node-priority node1) (node-priority node2))
		 (swap root1 root2)
		 (swap node1 node2))
	       ;; make root2 a child of root1
	       ;;(format t "children-root1:~A root2-obj:~A~%" (node-children (dllist-obj root1)) (dllist-obj root2))
	       (setf (node-children node1)
		     (dllist-insert (node-children node1) node2 :new-circular t))
	       (incf (node-degree node1) (node-degree node2))
	       ;; delete root2 from fibheap-forest
	       (setf (fibheap-forest fh) (dllist-delete root2))
	       ;; root1 remains
	       root1))
	   (merge-trees ()
	     "In the second phase, merge trees of the forest which have equal degree.
To find trees of the same degree efficiently we use an array of length O(log n) in which we keep a pointer to one root of each degree. When a second root is found of the same degree, the two are linked and the array is updated."
	     ;;(format t "AAAMERGE-TREES~%")
	     (when (> (fibheap-size fh) 1)
	       (let* ((logsize (ceiling (log (fibheap-size fh) 2)))
		      (degree-array (make-array logsize :element-type '(or dltree null) :initial-element nil))
		      ;; copying the forest is neccessary because it is changed in merge-trees-from-forest
		      (forest (dllist-to-list (fibheap-forest fh) :dll-is-circular t :obj nil)))
		 ;;(format t "fibheap-size:~A logsize:~A~%" (fibheap-size fh) logsize)
		 (dolist (cur forest)
		   (let* ((node (dllist-obj cur))
			  (degree (node-degree node)))
		     ;;(format t "degree:~A~%" degree)
		     (if (aref degree-array degree)
			 ;; merge trees
			 (setf (aref degree-array degree)
			       (merge-trees-from-forest (aref degree-array degree) cur))
			 (setf (aref degree-array degree) cur))))
		 ;; fibheap-forest was already updated by merge-trees-from-forest
		 )))
	   (find-new-root ()
	     "In the third phase we check each of the remaining roots and find the minimum.
Update root and decrease size by 1."
	     (let* ((min (fibheap-forest fh))
		    (min-priority (node-priority (dllist-obj min))))
	       ;; FIXME: the first element is checked twice. Omit checking it against itself.
	       (do-dllist (cur (fibheap-forest fh) :circular t)
		 (let ((cur-priority (node-priority (dllist-obj cur))))
		   ;;(format t "cur:~A cur-prio:~A min:~A min-prio:~A~%" cur cur-priority min min-priority)
		   (when (< cur-priority min-priority)
		     (setf min cur)
		     (setf min-priority cur-priority))))
	       (setf (fibheap-root fh) min))))
    (when (fibheap-empty fh)
      (error "Cannot pop an element off of an empty fibheap."))
    (remove-root)
    (if (null (fibheap-forest fh))
	fh ;; root was set to nil already by remove-root, therefore fh is consistent like this.
	(progn
	  (merge-trees)
	  (find-new-root)
	  fh))))

;;(let ((fh (fibheap-new 4))
;;	       ;(ins (loop for i below 10 collect (random 10)))
;;	       (ins '(1 8 4 8 8 3 2 3 5 3)))
;;	   (print ins)
;;	   (loop for i in ins do (setf fh (fibheap-insert fh i)))
;;	   (loop for i below 10 do
;;		(print (list "fibheap-min" (fibheap-min fh)))
;;		(fibheap-pop fh))
;;	   fh)
