
(load "numeric.lisp")

(defun sigmoid (x)
  (cond
    ((< x -88) 0.0)
    (t (/ 1 (1+ (exp (- x)))))))

(defun h-from-v (v w)
;;  (format t "h-from-v~%")
  (let ((h-size (cdr (matrix-dim w)))
	(v-size (length v)))
    (loop for j below h-size collect
	 (sigmoid (loop for i below v-size for wi in w for vi in v sum
		       (progn
;;			 (format t "i:~A j:~A wij:~A~%" i j (elt wi j))
			 (* vi (elt wi j))))))))

(defun v-from-h (w h)
;;  (format t "v-from-h~%")
  (let ((h-size (cdr (matrix-dim w)))
	(v-size (car (matrix-dim w))))
    (loop for i below v-size for wi in w collect
	 (sigmoid (loop for j below h-size for hj in h sum
		       (progn 
;;			 (format t "i:~A j:~A wij:~A~%" i j (elt wi j))
			 (* hj (elt wi j))))))))

(defun recall (v w &optional (verbose nil))
  (let* ((h (h-from-v v w)))
    (when verbose
      (format t "h:~A~%" h))
    (v-from-h w h)))

(defun new-genome (num-v num-hidden)
  (let ((w (loop for i below num-v collect (loop for j below num-hidden collect (1- (random 2.0))))))
    w))

(defun mutate-genome (w amplitude)
  (loop for wi in w collect (loop for wij in wi collect (+ wij (- (/ amplitude 2)) (random amplitude)))))

(defun fit-genome (w)
  (- (loop for i below 20 sum
	  (let* ((a (random .5))
		 (v (list a 0 1))
		 (vr (recall v w))
		 (vg (list a (+ .1 a) 1)))
	    ;;	 (format t "~%i:~A v:~A~%" i v)
	    ;;	 (format t "v-recall:~A~%" vr)
	    (reduce #'+ (mapcar (lambda (x) (abs (- (elt vr x) (elt vg x))))
				(loop for i below (length v) collect i)))))))

(defun best-ga (num-hidden generations)  
  (let* ((num 10)
	 (seq (loop for i below num collect i))
	 (population (mapcar (lambda (x) (declare (ignore x)) (new-genome 3 num-hidden)) seq))
	 (fit (mapcar #'fit-genome population)))
    (loop for i below generations do
	 (let* ((i1 (elt seq (random num)))
		(i2 (elt seq (random num)))
		(fit1 (elt fit i1))
		(fit2 (elt fit i2))
		(i-fit (if (> fit1 fit2) i1 i2))
		(i-unfit (if (> fit1 fit2) i2 i1))
		(fit-unfit (if (> fit1 fit2) fit2 fit1))
		(genome-new (mutate-genome (elt population i-fit) .5))
		(fit-new (fit-genome genome-new)))
	   (format t "fit-unfit:~A fit-new:~A~%" fit-unfit fit-new)
	   (when (> fit-new fit-unfit)
	     (setf (elt population i-unfit) genome-new)
	     (setf (elt fit i-unfit) fit-new)))
	 (format t "i:~A maxfit:~A~%" i (reduce #'max fit)))
;;    (format t "best:~A~%" (elt population (position (reduce #'max fit) fit)))
    (elt population (position (reduce #'max fit) fit))))

(defparameter best (best-ga 50 1000))

(loop for i below 10 do
     (let* ((w best)
	    (a (random .5))
	    (v (list a 0 1))
	    (vr (recall v w)))
       (format t "v:~A vr:~A diff:~A~%" v vr (- (cadr vr) (car v)))))
