(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-custom-hash-table)
(ql:quickload :dlist2)

;;;; Implementation of a better sxhash for lists, which regards all elements of the list, not just the first couple of elements.
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

;; cannot make LSXHASH inline, since it is recursive.
(defun lsxhash (x)
  "Return a hash value for value X.
X, (car X), and (cdr X) may be a list, a symbol, or a number."
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  (declare (values (and fixnum unsigned-byte))) ;inferred automatically (see (DESCRIBE 'LSXHASH))
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
    ;; FIXME: handle circular lists.
    (list (mix (lsxhash (car x)) (lsxhash (cdr x))))
    ;; FIXME: handle circular hash-tables.
    (hash-table (let ((ret 448291823))
		  (declare (type (and fixnum unsigned-byte) ret))
		  (setf ret (mix (sxhash (hash-table-count x))
				 (mix ret (sxhash (hash-table-test x)))))
		  ;; use logxor for speed and so that the order of key/value pairs does not matter
		  (loop for k being the hash-key of x using (hash-value v) do
		       (setf ret (logxor ret (mix (lsxhash k) (lsxhash v)))))
		  ret))
    ;; FIXME: handle circular arrays.
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

;; for caching mutable objects (including hash-tables) supported by LSXHASH.
(cl-custom-hash-table:define-custom-hash-table-constructor make-lsxhash-equalp-hash-table
    :test equalp :hash-function lsxhash)

;; for caching mutable objects supported by LSXHASH.
(cl-custom-hash-table:define-custom-hash-table-constructor make-lsxhash-equal-hash-table
    :test equal :hash-function lsxhash)

(cl-custom-hash-table:define-custom-hash-table-constructor make-sxhash-equal-hash-table
    :test equal :hash-function sxhash)

;; for quickly caching immutable objects.
(cl-custom-hash-table:define-custom-hash-table-constructor make-sxhash-eq-hash-table
    :test eq :hash-function sxhash)

;;;; A MRU (most recently used) cache.

(defclass mru-cache ()
  ((slots :initarg :slots :accessor mru-cache-slots :type (and fixnum unsigned-byte))
   (ht :initarg :ht :accessor mru-cache-ht :type hash-table)
   (dlist :initarg :dlist :accessor mru-cache-dlist :type dlist2))
  (:documentation "A most-recently-used-cache class."))

(defun make-mru-cache (slots &key (make-hash-table-fn #'make-lsxhash-equal-hash-table))
  "Make a most-recently-used cache, being able to cache up to SLOTS items."
  (declare (type unsigned-byte slots))
  (let* ((ht (funcall make-hash-table-fn :size slots))
	 (dlist (dlist2:dlist))
	 (c (make-instance 'mru-cache
			   :slots slots
			   :ht ht
			   :dlist dlist)))
    c))

(defun mru-cache-used (mru)
  "Return the number of key-value pairs stored in most-recently-used cache MRU."
  (hash-table-count (mru-cache-ht mru)))

(defun mru-cache-key-present-p (mru key)
  "Return whether the key KEY is present in the most-recently-used-cache MRU.
This does not modify the order of the key-value-pairs stored in MRU."
  (declare (type mru-cache mru))
  (cl-custom-hash-table:with-custom-hash-table
    (nth-value 1 (gethash key (mru-cache-ht mru)))))

(declaim (inline mru-dlist-hit-mru-dcons))
(defun mru-dlist-hit-mru-dcons (dlist mru-dcons)
  (declare (type dlist2:dlist dlist)
	   (type dlist2:dcons mru-dcons))
  "Bring the dcons MRU-DCONS to the front of the dlist DLIST."
  ;;(print "mru hit") (describe-object dlist t)
  ;; slice out mru-dcons from its current position.
  (dlist2:dcons-delete mru-dcons)
  ;; bring mru-dcons to the beginning of the dlist.
  (let ((first (dlist2:dlist-first dlist)))
    ;; need to re-use existing dcons mru-dcons, because the hash-table points to it.
    (dlist2:dcons-existing-insert-between first mru-dcons (dlist2:next first)))
  ;; updating the hash-table is not necessary, because its key still points to MRU-DCONS.
  )

(defun mru-cache-get (mru key &optional (default nil))
  "If the key KEY is present in mru-cache MRU, return the values for key KEY and as secondary value T, and bring KEY to the front of MRU.
If KEY is not present return the value DEFAULT and NIL as secondary value."
  (let ((ht (mru-cache-ht mru))
	(dlist (mru-cache-dlist mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (progn
	      ;; an entry is present, therefore bring it to front in dlist.
	      (mru-dlist-hit-mru-dcons dlist mru-dcons)
	      ;;(describe-object dlist t)
	      (values (cdr (dlist2:data mru-dcons)) t))
	    (values default nil))))))

(defun mru-cache-get-nohit (mru key &optional (default nil))
  "If the key KEY is present in mru-cache MRU, return the values for key KEY and as secondary value T, but do not bring KEY to the front of MRU.
If KEY is not present return the value DEFAULT and NIL as secondary value.
This function is like #'MRU-CACHE-GET, but does not bring KEY to the front of the list."
  (let ((ht (mru-cache-ht mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (values (cdr (dlist2:data mru-dcons)) t)
	    (values default nil))))))

(defun mru-cache-set (mru key value)
  "Set the value VALUE for the key KEY in mru-cache MRU, if KEY was not present yet.
Bring the KEY-VALUE-pair to the front of the mru-cache, possibly removing the least-recently-used pair."
  (declare (type mru-cache mru))
  (let* ((ht (mru-cache-ht mru))
	 (slots (mru-cache-slots mru))
	 (dlist (mru-cache-dlist mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(if p
	    (mru-dlist-hit-mru-dcons dlist mru-dcons)
	    ;; push new cache entry to front of dlist.
	    (let (front-dcons)
	      (if (>= (hash-table-count ht) slots)
		  (progn
		    ;; the cache is already full. Remove the last element, and re-use its dcons for the new front element.
		    (destructuring-bind (old-key . old-value) (dlist2:data (dlist2:prev (dlist2:dlist-last dlist)))
		      (declare (ignorable old-value))
		      ;;(print "mru full") (describe-object dlist t)
		      (setf front-dcons (dlist2:dlist-pop-dcons dlist :from-end t))
		      (remhash old-key ht))
		    (setf (dlist2:data front-dcons) (cons key value))
		    (let ((first (dlist2:dlist-first dlist)))
		      ;; need to re-use existing dcons mru-dcons, because the hash-table points to it.
		      (dlist2:dcons-existing-insert-between first front-dcons (dlist2:next first)))
		    ;;(describe-object dlist t)
		    )
		  (setf front-dcons (dlist2:dlist-push-return-new-dcons (cons key value) dlist)))
	      ;; insert front-dcons into ht
	      (setf (gethash key ht) front-dcons)
	      ;;(print (list "hash-table-count" (hash-table-count ht) slots))
	      value))))))

(defun (setf mru-cache-get) (value mru key)
  "Set the value VALUE for the key KEY in mru-cache MRU, if KEY was not present yet.
Bring the key-value-pair to the front of the mru-cache, possibly removing the least-recently-used pair."
  (mru-cache-set mru key value))

(define-condition key-not-found (error)
  ((mru-cache :initarg :mru-cache :reader key-not-found-mru-cache :documentation "The mru-cache that the key was not found in")
   (key :initarg :key :reader key-not-found-key :documentation "The key that was not found"))
  (:report (lambda (c stream) (format stream "Key ~A not found in most-recently-used cache ~A." (key-not-found-key c) (key-not-found-mru-cache c))))
  (:documentation "Signals that KEY was not found in MRU"))

(defun mru-cache-del (mru key)
  "Remove KEY from mru-cache MRU.
Signal a KEY-NOT-FOUND error if KEY is not present."
  (declare (type mru-cache mru))
  (let* ((ht (mru-cache-ht mru)))
    (cl-custom-hash-table:with-custom-hash-table
      (multiple-value-bind (mru-dcons p) (gethash key ht)
	(unless p
	  (error (make-condition 'key-not-found :mru-cache mru :key key)))
	(dlist2:dcons-delete mru-dcons)
	(remhash key ht)))))

(defun mru-cache-iterate (mru fun &key from-end)
  "Iterate over the key-value pairs stored in mru cache MRU, without changing their order.
Calls function FUN with two values: the KEY and the VALUE. MRU may be modified in FUN, but the items prepended to the cache will not be iterated over if FROM-END=NIL."
  (let ((dlist (mru-cache-dlist mru)))
    (dlist2:dodcons-between (pair (dlist2:dlist-first dlist) (dlist2:dlist-last dlist) :from-end from-end)
      (let ((data (dlist2:data pair)))
	(funcall fun (car data) (cdr data))))))

;; test
(let ((mru (make-mru-cache 3)))
  (flet ((to-list (mru)
	   (let ((l nil))
	     (mru-cache-iterate mru (lambda (k v) (push (cons k v) l)) :from-end t)
	     l)))
    (setf (mru-cache-get mru 1) 'one)
    (setf (mru-cache-get mru 2) 'two)
    (setf (mru-cache-get mru 3) 'three)
    (assert (equal (to-list mru) '((3 . three) (2 . two) (1 . one))))
    (assert (and (eq (mru-cache-get-nohit mru 1) 'one) (eq (mru-cache-get-nohit mru 2) 'two) (eq (mru-cache-get-nohit mru 3) 'three)))
    (setf (mru-cache-get mru 4) 'four)
    (assert (eq (mru-cache-get mru 4) 'four))
    (assert (not (mru-cache-key-present-p mru 1)))
    (assert (equal (to-list mru) '((4 . four) (3 . three) (2 . two))))
    (mru-cache-del mru 3)
    (assert (equal (to-list mru) '((4 . four) (2 . two))))
    (assert (= (mru-cache-used mru) 2))
    (mru-cache-del mru 4)
    (mru-cache-del mru 2)
    (assert (= (mru-cache-used mru) 0))
    (assert (equal (to-list mru) nil))))

;;;; Function cacher

(defun make-mru-function-cacher (fun slots &key (make-hash-table-fn #'make-lsxhash-equal-hash-table))
  "Make and return a function that has the same arguments as function FUN, but caches the SLOTS most-recently passed arguments and values.
The cached version returns values from the cache if FUN was already evaluated with those arguments. If FUN returns multiple values, the cached function version will do so as well.
FUN must be determinitic and side-effect-free."
  (let* ((mru (make-mru-cache slots :make-hash-table-fn make-hash-table-fn)))
    (lambda (&rest rest)
      ;;(print (list "make-mru-function-cacher ht" (let (l) (maphash (lambda (key value) (push (list key value) l)) (mru-cache-ht mru)) l) "mru" (mru-cache-dlist mru)))
      (if (mru-cache-key-present-p mru rest)
	  (apply #'values (mru-cache-get mru rest))
	  ;; the function call is unkown
	  (let* ((values (multiple-value-list (apply fun rest))))
	    (mru-cache-set mru rest values)
	    (apply #'values values))))))

(let ((last nil))
  (flet ((add (a b)
	   ;;(print (list "add" a b))
	   (setf last (list a b))
	   (+ a b)))
    (let ((cadd (make-mru-function-cacher #'add 2)))
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
  (let ((cadd (make-mru-function-cacher #'add 2)))
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
