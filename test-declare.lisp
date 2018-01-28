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
