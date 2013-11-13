(load "~/quicklisp/setup.lisp")
(ql:quickload 'lhstats)
(asdf:oos 'asdf:load-op 'utils)

(defun plist-vs-hashtable (putlength)
  (let* ((pl nil)
	 (ht (make-hash-table))
	 (putit (loop repeat putlength collect (random 400000000)))
	 (getit (loop for i below 1000 collect i)))
    (dolist (i putit) (setf (getf pl i) i))
    (dolist (i putit) (setf (gethash i ht) i))
    (timediff (loop for i in getit collect (aif (getf pl i) it 0))
	      (loop for i in getit collect (aif (gethash i ht) it 0))
	      :showtimes t
	      :maxtime 2)))

(defun alist-vs-hashtable (putlength)
  (let* ((ht (make-hash-table))
	 (putit (loop repeat putlength collect (random 400000000)))
	 (al (pairlis putit putit))
	 (getit (loop for i below 1000 collect i)))
    (dolist (i putit) (setf (gethash i ht) i))
    (timediff (loop for i in getit collect (aif (assoc i al) it 0))
	      (loop for i in getit collect (aif (gethash i ht) it 0))
	      :showtimes t
	      :maxtime 2)))

(defun recurse-normal (depth)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (if (= 0 depth)
      0
      (1+ (recurse-normal (1- depth)))))

(defun recurse-restart (depth)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (restart-case
      (if (= 0 depth)
	  0
	  (1+ (recurse-restart (1- depth))))
    (restart-name ())))

(defun restart-vs-no-restart (depth)
  (timediff (recurse-normal depth)
	    (recurse-restart depth)
	    :showtimes t
	    :maxtime 2))

(defun joy-eval-interrupt-speed (count times)
  "Call joy-eval with a never-terminating program, and interrupt evaluation after every COUNT ticks.
Repeat this TIMES times.
Is used to measure the performance of interrupting joy-eval."
  (handler-case
      (let ((i 1))
	(multiple-value-bind (counter counter-delta) (make-counter count)
	  (handler-bind
	      ((joy-overrun-error (lambda (c)
				    (declare (ignorable c))
				    (when (< i times)
				      (incf i)
				      (funcall counter-delta count)
				      (invoke-restart 'proceed)))))
	    (joy-eval '() '(0 (true) (succ) while) :c counter))))
    (joy-time-error (c)
      (joy-time-error-stk c))))
;;(timediff (joy-eval-interrupt-speed 100000 1) (joy-eval-interrupt-speed 1 100000) :showtimes t)
;;(timediff (joy-eval-interrupt-speed 100000 1) (joy-eval-interrupt-speed 50 2000) :showtimes t) says that evaluating 50 tick counts before signalling a joy-overrun-error makes joy-eval about 2 times slower.
