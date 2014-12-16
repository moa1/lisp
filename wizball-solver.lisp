

#+() (defun def-my-eval (max-steps exit)
  (let ((check (lambda ()
		 (setf max-steps (1- max-steps))
		 (if (not (plusp max-steps))
		     (funcall exit))))
	(my+ (lambda (&rest args)
	       (check)
	       (apply #'+ args)))
	(my- (lambda (&rest args)
	       (check)
	       (apply #'- args)))
	(my= (lambda (&rest args)
	       (check)
	       (apply #'= args)))
	(myif (lambda (testfn thenfn elsefn)
		(check)
		(if (funcall testfn)
		    (funcall thenfn)
		    (funcall elsefn)))))))


(defmacro mylambda
(lambda 


#-()



(defstruct 