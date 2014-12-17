(defmethod shuffle* ((l list))
  "Shuffle list L in-place."
  (labels ((rec (rest n)
	     (when (> n 0)
	       (let* ((r (nthcdr (random n) rest))
		      (a (car rest)))
		 (setf (car rest) (car r))
		 (setf (car r) a))
	       (rec (cdr rest) (1- n)))))
    (rec l (length l)))
  l)

(defmethod shuffle* ((v vector))
  "Shuffle simple-vector V in-place."
  (let ((n (array-dimension v 0)))
    (labels ((rec (i)
	       (when (> i 0)
		 (let ((r (random i))
		       (a (aref v i)))
		   (setf (aref v i) (aref v r))
		   (setf (aref v r) a))
		 (rec (1- i)))))
      (rec (1- n))))
  v)

;;(let ((a (make-array 10 :initial-contents (loop for i below 10 collect i))))
;;  (utils:timecps (lambda () nil)))

(defun time-shuffle* (&key (min-out-of 18) (measurable-seconds 0.05) (measurable-repeats 5))
  (loop for i below 20 do
       (let* ((n (ceiling (expt 1.5 i)))
	      (l (loop for i below n collect i))
	      (a (make-array n :initial-contents l)))
	 (format t "~A ~G ~G~%"
		 n
		 (utils:timesec (lambda () (shuffle* l))
				:min-out-of min-out-of
				:measurable-seconds measurable-seconds
				:measurable-repeats measurable-repeats)
		 (utils:timesec (lambda () (shuffle* a))
				:min-out-of min-out-of
				:measurable-seconds measurable-seconds
				:measurable-repeats measurable-repeats)))))
