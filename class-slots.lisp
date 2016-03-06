;; I want to list the slots of an existing class, so that I can find out the memory usage of an instance of the class.
;;(sb-pcl:class-slots (find-class 'nest[]))

(load "~/quicklisp/setup.lisp")
(ql:quickload :moptilities)

(defun depth-first-walk (object function-new function-old)
  "Recursively walk all objects reachable from object OBJECT.
For each object, call FUNCTION-NEW with the object if the object was not yet seen, and FUNCTION-OLD with the object if then object was already seen.
The recursive walking is done in a depth-first manner."
  (let ((visited (make-hash-table :test #'eq)))
    (labels ((recurse (x)
	       (multiple-value-bind (val present) (gethash x visited)
		 (declare (ignore val))
		 ;;(print (list "walk" x))
		 (if present
		     (funcall function-old x)
		     (progn
		       (setf (gethash x visited) t)
		       (funcall function-new x)
		       (etypecase x
			 (number nil)
			 (symbol nil)
			 (cons
			  (recurse (car x))
			  (recurse (cdr x)))
			 (vector
			  (loop for e across x do
			       (recurse e)))
			 (structure-object
			  (loop for slot-name in (moptilities:slot-names (class-of x)) do
			       (recurse (slot-value x slot-name))))
			 (standard-object
			  (loop for slot-name in (moptilities:slot-names (class-of x)) do
			       (when (slot-boundp x slot-name)
				 (recurse (slot-value x slot-name)))))
			 ))))))
      (recurse object)
      nil)))

(defun reachable-object-counts (object)
  (let ((n-pointer 0)
	(n-number 0)
	(n-symbol 0)
	(n-cons 0)
	(n-vector 0)
	(n-structure-object 0)
	(n-standard-object 0))
    (labels ((function-new (x)
	       (etypecase x
		 (number (incf n-number))
		 (symbol (incf n-symbol))
		 (cons (incf n-cons))
		 (vector
		  (incf n-vector)
		  (incf n-pointer (array-dimension x 0)))
		 (structure-object
		  (incf n-structure-object)
		  (incf n-pointer (length (moptilities:slot-names (class-of x)))))
		 (standard-object
		  (incf n-standard-object)
		  (incf n-pointer (length (moptilities:slot-names (class-of x))))))))
      (depth-first-walk object #'function-new (constantly nil))
      (list :n-pointer n-pointer
	    :n-number n-number
	    :n-symbol n-symbol
	    :n-cons n-cons
	    :n-vector n-vector
	    :n-structure-object n-structure-object
	    :n-standard-object n-standard-object))))


(defstruct s
  a b c)

(defun s-slot-names ()
  (moptilities:slot-names 's))

(let ((a (list 1 2 3))
      (b (make-s :a 1 :b 2 :c 3))
      (c (make-array 3 :initial-element (list 1 2 3))))
  (defparameter *ex* (list (make-s :a a :b b :c c) 'x 'y 'z)))

(defclass c ()
  ((a :initarg :a)
   (b :initarg :b)
   c))

(let ((a (list 1 2 3))
      (b (make-s :a 1 :b 2 :c 3))
      (c (make-array 3 :initial-element (list 1 2 3))))
  (defparameter *ex2* (list (make-s :a a :b b :c c) (make-instance 'c :a a :b b) 'y 'z)))

(defun test-walk ()
  (let ((*print-circle* t)
	(l1 (let ((c1 (cons 1 nil))) (rplacd c1 c1))))
    (print "walking l1")
    (depth-first-walk l1 (lambda (x) (print (list "new" x))) (lambda (x) (print (list "old" x))))
    (print "walking *ex*")
    (depth-first-walk *ex* (lambda (x) (print (list "new" x))) (lambda (x) (print (list "old" x))))
    (print "walking *ex2*")
    (depth-first-walk *ex2* (lambda (x) (print (list "new" x))) (lambda (x) (print (list "old" x))))))
