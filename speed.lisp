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

(defun thrower (tag)
  (throw tag 1))
(defun throw-catch (tag)
  (catch 'a
    (1+ (catch 'b
	  (thrower tag)))))

(defun a-handler-bind ()
  (catch 'a
    (handler-bind ((error (lambda (c)
			    (declare (ignore c))
			    (throw 'a 2))))
      (error "bla"))))

(defun throw-catch-vs-handler-bind ()
  (timediff (throw-catch 'a) (a-handler-bind) :maxtime 2 :showtimes t))

(defun a-handler-case ()
  (handler-case (progn (error "bla") 1)
    (error () 2)))

(defun handler-bind-vs-handler-case ()
  (timediff (a-handler-bind) (a-handler-case) :maxtime 2 :showtimes t))

(defun make-counter (c)
  (let ((i c))
    (lambda ()
      (decf i)
      (= i 0))))

(defun plus-return (a b &key (c (make-counter 0)))
  (if (funcall c)
      (return-from plus-return 'overrun))
  (if (= 0 a)
      b
      (plus-return (1- a) (1+ b) :c c)))

(defun plus-catch (a b &key (c (make-counter 0)))
  (if (funcall c)
      (throw 'overrun 'overrun))
  (if (= 0 a)
      b
      (plus-catch (1- a) (1+ b) :c c)))

(defun timediff-return-vs-catch (&optional (d 1))
  (let ((v most-positive-fixnum))
    (timediff (eq (plus-return v 0 :c (make-counter d)) 'overrun)
	      (catch 'overrun (plus-catch v 0 :c (make-counter d)))
	      :showtimes t
	      :maxtime 2)))

(defun plus-return-inline (a b &key (c 0))
  (if (progn
	(decf c)
	(= c 0))
      (return-from plus-return-inline 'overrun))
  (if (= 0 a)
      b
      (plus-return-inline (1- a) (1+ b) :c c)))

(defun timediff-funcall-vs-inline (&optional (d 1))
  (let ((v most-positive-fixnum))
    (timediff (plus-return v 0 :c (make-counter d))
	      (plus-return-inline v 0 :c d)
	      :showtimes t
	      :maxtime 2)))

(defun return-list (d)
  (labels ((f1 (d a1 a2 a3 a4 a5 a6 a7 a8)
	     (list (1- d) a1 a2 a3 a4 a5 a6 a7 a8))
	   (f2 (d)
	     (let ((l (list d 1 2 3 4 5 6 7 8)))
	       (loop until (<= (car l) 0) do
		    (setf l (apply #'f1 l))))))
    (f2 d)))

(defun return-values-list (d)
  (labels ((f1 (d a1 a2 a3 a4 a5 a6 a7 a8)
	     (values (1- d) a1 a2 a3 a4 a5 a6 a7 a8))
	   (f2 (d)
	     (let ((l (list d 1 2 3 4 5 6 7 8)))
	       (loop until (<= (car l) 0) do
		    (setf l (multiple-value-list (apply #'f1 l)))))))
    (f2 d)))

(defun return-values-bind (d)
  (labels ((f1 (d a1 a2 a3 a4 a5 a6 a7 a8)
	     (values (1- d) a1 a2 a3 a4 a5 a6 a7 a8))
	   (f2 (d)
	     (let ((a1 1) (a2 2) (a3 3) (a4 4) (a5 5) (a6 6) (a7 7) (a8 8))
	       (loop until (<= d 0) do
		    (multiple-value-bind (r b1 b2 b3 b4 b5 b6 b7 b8)
			(f1 d a1 a2 a3 a4 a5 a6 a7 a8)
		      (setf d r a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7 a8 b8)
		      )))))
    (f2 d)))

(defun timediff-return-list-vs-return-values-list (&optional (d 1))
  (timediff (return-list d)
	    (return-values-list d)
	    :showtimes t
	    :maxtime 2))

(defun timediff-return-values-list-vs-return-values-bind (&optional (d 1))
  (timediff (return-values-list d)
	    (return-values-bind d)
	    :showtimes t
	    :maxtime 2))
