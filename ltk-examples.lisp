(load "~/quicklisp/setup.lisp")
(ql:quickload 'ltk)
(use-package 'ltk)

(defun scribble ()
  (with-ltk ()
    (let* ((canvas (make-instance 'canvas))
	   (down nil))
      (pack canvas)
      (bind canvas "<ButtonPress-1>"
	    (lambda (evt)
	      (setf down t)
	      (create-oval canvas
			   (- (event-x evt) 10) (- (event-y evt) 10)
			   (+ (event-x evt) 10) (+ (event-y evt) 10))))
      (bind canvas "<ButtonRelease-1>" (lambda (evt)
					 (declare (ignore evt))
					 (setf down nil)))
      (bind canvas "<Motion>"
	    (lambda (evt)
	      (when down
		(create-oval canvas
			     (- (event-x evt) 10) (- (event-y evt) 10)
			     (+ (event-x evt) 10) (+ (event-y evt) 10))))))))
