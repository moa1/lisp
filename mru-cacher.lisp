(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-custom-hash-table)
(ql:quickload :dlist2)

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

(cl-custom-hash-table:define-custom-hash-table-constructor make-lsxhash-equal-hash-table
    ;; equalp required when hashing hash tables
    :test equal :hash-function lsxhash)

(cl-custom-hash-table:define-custom-hash-table-constructor make-sxhash-equal-hash-table
    ;; equalp required when hashing hash tables
    :test equal :hash-function sxhash)

;;;; A deterministic function cacher (mru: most recently used)

(defclass mru-cache ()
  ((slots :initarg :slots :accessor mru-cache-slots :type (and fixnum unsigned-byte))
   (ht :initarg :ht :accessor mru-cache-ht :type hash-table)
   (mru :initarg :mru :accessor mru-cache-mru :type dlist2))
  (:documentation "A most-recently-used-cache class."))

(defun make-mru-cache (slots &key (make-hash-table-fn #'make-lsxhash-equal-hash-table))
  (declare (type (integer 1 #.most-positive-fixnum) slots))
  (let* ((ht (funcall make-hash-table-fn))
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
  (cl-custom-hash-table:with-custom-hash-table
    (multiple-value-bind (mru-dcons p) (gethash key (mru-cache-ht mru))
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
  (let ((ht (mru-cache-ht mru))
	(mru (mru-cache-mru mru)))
    (cl-custom-hash-table:with-custom-hash-table
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
  (let* ((ht (mru-cache-ht mru))
	 (slots (mru-cache-slots mru))
	 (mru (mru-cache-mru mru)))
    (cl-custom-hash-table:with-custom-hash-table
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

(defun mru-function-cacher (fun slots &key (make-hash-table-fn #'make-lsxhash-equal-hash-table))
  "Most-recently-used cache for determinitic and side-effect-free functions."
  (let* ((mru (make-mru-cache slots :make-hash-table-fn make-hash-table-fn)))
    (lambda (&rest rest)
      ;;(print (list "mru-function-cacher ht" (let (l) (maphash (lambda (key value) (push (list key value) l)) (mru-cache-ht mru)) l) "mru" (mru-cache-mru mru)))
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
