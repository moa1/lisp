;; https://stackoverflow.com/questions/19485248/what-do-optional-and-rest-mean-in-a-values-type-specifier: CLHS states "The &optional and &rest markers can appear in the value-type list; they indicate the parameter list of a function that, when given to multiple-value-call along with the values, would correctly receive those values."

(defun declare-special-1 ()
  (labels ((test (n)
	     (if (= n 0)
		 a
		 (let ((a :special))
		   (declare (special a))
		   (test 0)))))
    (test 1)))

(defun declare-special-error-1 ()
  (labels ((test (n)
	     (if (= n 0)
		 a
		 (let ((a :special))
		   ;; here, A is not declared special, which means that A in the recursion is unbound.
		   (test 0)))))
    (test 1)))
;; TODO: add a check that (DECLARE-SPECIAL-ERROR-1) fails with an error,

(defun declare-unknown-1 ()
  (let ((a 1))
    (declare (foo 1)) ;produces a warning both in SBCL and CLISP.
    a))

;;;; This tests how type information of functions can be represented in lisp.

;; for example types could be represented as a list of OR-concatenated types, like in
(declaim (ftype (or (function ((eql t)) integer) (function (null) single-float)) declare-ftype))
(defun declare-ftype (a)
  (if a 1 1.0))

;; The following has the disadvantage that the declared type is more general than intended, and the correspondence of input types and output types is not clearly readable, is: (DECLAIM (FTYPE (FUNCTION ((OR FLOAT INTEGER) (OR FLOAT INTEGER)) (OR (VECTOR INTEGER) (VECTOR FLOAT))) F1))
(declaim (ftype (function ((or (eql t) null)) (or integer single-float)) declare-ftype-too-general))
(defun declare-ftype-too-general (a)
  (if a 1 1.0))

#|
;; declaring function type as concatenated declare specifications doesn't work, since they are interpreted as AND-concatenated types, leading to conflicts.
(defun test2 ()
  (flet ((f1 (a b)
	   (make-array 2 :initial-contents (list a b))))
    (declare (ftype (function (integer integer) (vector integer)) f1)
	     (ftype (function (float float) (vector float)) f1))

    (f1 2 3)
    (f1 2.0 3.0)))
|#

#|
(defun f1 (a b)
  (make-array 2 :initial-contents (list a b)))
|#
