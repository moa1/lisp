(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :utils)

(defun sigmoid (x)
  (declare (optimize (speed 3))
	   (type single-float x))
  (cond
    ((<= -88.7 x)
     (/ 1 (+ 1 (exp (- x)))))
    (t
     0.0)))

(defun make-sigmoid-interpolated (&optional (precision 0.5) (cutoff -6.0))
  (declare (type single-float precision cutoff))
  (let* ((npoints (ceiling (1+ (* 2 (/ (- cutoff) precision)))))
	 (array (make-array npoints))
	 (1/precision (/ 1.0 precision)))
    (declare (type fixnum npoints))
    (loop for p below npoints do
	 (setf (aref array p)
	       (let ((x (+ (* p precision) cutoff)))
		 (sigmoid x))))
    (lambda (x)
      (declare (optimize (speed 3))
	       (type single-float x))
      (let ((p (* (- x cutoff) 1/precision)))
	(multiple-value-bind (i f) (floor p)
	  (declare (type fixnum i))
	  ;;(format t "I:~S F:~S~%" i f)
	  (cond
	    ((and (<= 0 i) (< i (1- npoints)))
	     (let ((y0 (aref array i))
		   (y1 (aref array (1+ i))))
	       (declare (type single-float y0 y1))
	       (+ (* (- y1 y0) f) y0)))
	    (t
	     (sigmoid x))))))))

(defun sigmoid-comparison ()
  (let ((sigmoidf (make-sigmoid-interpolated)))
    (flet ((sigmoid1 ()
	     (loop for x from -100.0 upto 100.0 by 0.1 sum
		  (sigmoid x)))
	   (sigmoid2 ()
	     (loop for x from -100.0 upto 100.0 by 0.1 sum
		  (funcall sigmoidf x))))
      (utils:timediff (sigmoid1) (sigmoid2) :showtimes t))))

(defun sigmoid-speed ()
  (utils:timesec (lambda ()
		   (loop for x from -100.0 upto 100.0 by 0.1 sum
			(sigmoid x)))))
