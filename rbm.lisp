;; Restricted Boltzmann Machine

(load "numeric.lisp")

(defun random-gaussian-2 ()
  "Return two with mean 0 and standard deviation 1 normally distributed random variables."
  (flet ((xinit ()
	   (- (* 2.0 (random 1.0)) 1)))
    (do* ((x1 (xinit) (xinit))
	  (x2 (xinit) (xinit))
	  (w (+ (* x1 x1) (* x2 x2)) (+ (* x1 x1) (* x2 x2))))
 	 ((< w 1.0)
	  (let ((v (sqrt (/ (* -2.0 (log w)) w))))
	    (values (* x1 v) (* x2 v)))))))

(defun random-gaussians (n &key (mean 0) (sd 1))
  "Return a list of N with mean MEAN and standard deviation SD normally distributed random variables."
  (labels ((scale (x)
	     (+ mean (* x sd)))
	   (rec-scale (n l)
	     (multiple-value-bind (r1 r2) (random-gaussian-2)
	       (cond
		 ((>= n 2) (rec-scale (- n 2) (cons (scale r1) (cons (scale r2) l))))
		 ((= n 1) (cons (scale r1) l))
		 (t l))))
	   (rec-noscale (n l)
	     (multiple-value-bind (r1 r2) (random-gaussian-2)
	       (cond
		 ((>= n 2) (rec-noscale (- n 2) (cons r1 (cons r2 l))))
		 ((= n 1) (cons r1 l))
		 (t l)))))
    (if (and (= mean 0) (= sd 1))
	(rec-noscale n nil) ;is not really faster: 2.776 vs 2.732 seconds
	(rec-scale n nil))))

(let ((next-random-gaussian nil))
  (defun random-gaussian ()
    "Return a with mean MEAN and standard deviation SD normally distributed random variable."
    (if (null next-random-gaussian)
	(multiple-value-bind (r1 r2) (random-gaussian-2)
	  (setf next-random-gaussian r2)
	  r1)
	(let ((r1 next-random-gaussian))
	  (setf next-random-gaussian nil)
	  r1))))

(defun sigmoid (x)
  (cond
    ((>= x -88) (/ 1 (1+ (exp (- x)))))
    (t 0.0)))

(defun array-transpose (a r)
  "Put the transpose of matrix A into matrix R."
  (let ((arows (array-dimension a 0))
	(acols (array-dimension a 1))
	(rrows (array-dimension r 0))
	(rcols (array-dimension r 1)))
    (assert (and (= arows rcols) (= acols rrows)))
    (loop for i below rrows do
	 (loop for j below rcols do
	      (setf (aref r i j) (aref a j i))))))

(defun array-array-mul (a b r &key (a-t nil) (b-t nil))
  "Multiply matrix A with matrix B and put the result in matrix R.
Matrices A and/or B are transposed before multiplication if A-T and/or B-t is T, respectively."
  (let ((arows (array-dimension a (if a-t 1 0)))
	(acols (array-dimension a (if a-t 0 1)))
	(brows (array-dimension b (if b-t 1 0)))
	(bcols (array-dimension b (if b-t 0 1)))
	(rrows (array-dimension r 0))
	(rcols (array-dimension r 1)))
    (assert (and (= arows rrows) (= bcols rcols) (= acols brows)))
    (loop for i below rrows do
	 (loop for j below rcols do
	      (setf (aref r i j)
		    (if a-t
			(if b-t
			    (loop for k below acols sum
				 (* (aref a k i) (aref b j k)))
			    (loop for k below acols sum
				 (* (aref a k i) (aref b k j))))
			(if b-t
			    (loop for k below acols sum
				 (* (aref a i k) (aref b j k)))
			    (loop for k below acols sum
				 (* (aref a i k) (aref b k j))))))))))
;; test array-array-mul
(let ((a #2A((1 2 3) (4 5 6)))
      (b #2A((0 1) (2 3) (4 5)))
      (r (make-array '(2 2)))
      (ta (make-array '(3 2)))
      (tb (make-array '(2 3))))
  (array-transpose a ta)
  (array-transpose b tb)
  (print (list a ta b tb))
  (array-array-mul a b r)
  (assert (equalp r #2A((16 22) (34 49))))
  (array-array-mul ta b r :a-t t)
  (assert (equalp r #2A((16 22) (34 49))))
  (array-array-mul a tb r :b-t t)
  (assert (equalp r #2A((16 22) (34 49))))
  (array-array-mul ta tb r :a-t t :b-t t)
  (assert (equalp r #2A((16 22) (34 49)))))

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
;;	(v-size (length v)))
    (loop for i below v-size for wi in w collect
	 (sigmoid (loop for j below h-size for hj in h sum
		       (progn 
;;				   (format t "i:~A j:~A wij:~A~%" i j (elt wi j))
			 (* hj (elt wi j))))))))

(defun activate (n)
  (loop for p in n collect
       (if (<= (random 1.0) p) 1 0)))

(defun bias (n)
  "set last element to 1 (bias node)"
  (let ((m (loop for i in n collect i)))
    (setf (elt m (1- (length m))) 1)
    m))

(defun learn (v w rate)
  ;; v are the input layer values, w is the connection matrix
  (let* ((h (h-from-v v w))
	 (ha (activate h))
	 (pos (outer v h))
	 (v1 (v-from-h w ha))
	 (v1b (bias v1))
	 (h1 (h-from-v v1b w))
	 (neg (outer v1b h1))
	 (update (num* rate (num- pos neg))))
    update))

(defun recall (v w &optional (verbose nil))
  (let* ((h (h-from-v v w))
	 (ha (activate h)))
    (when verbose
      (format t "h:~A~%" h))
    (v-from-h w ha)))

;; learn a simple relation between a and a+.5
(defun test1 (num-hidden)
  (let ((w (loop for i below 3 collect (loop for j below num-hidden collect (1- (random 2.0))))))
    (loop for i below 1000 do
	 (let* ((a (random .5))
		(v (list a (+ .5 a) 1)))
	   (format t "~%i:~A v:~A~%" i v)
	   (setf w (num+ w (learn v w .05)))
	   (format t "new w:~A~%h:~A~%" w (h-from-v v w))
	   (format t "v-recall:~A~%" (recall v w))))))
  
(defun test2 (num-hidden)
  (let ((w (loop for i below 10 collect (loop for j below num-hidden collect (- (random 0.2) 0.1)))))
    (loop for i below 1000 do
	 (let* ((a (random 3))
		(v (ecase a
		     (0 '(1 1 1 0 0 0 0 0 0 1))
		     (1 '(0 0 0 1 1 1 0 0 0 1))
		     (2 '(0 0 0 0 0 0 1 1 1 1)))))
	   (format t "~%i:~A v:~A~%" i v)
	   (setf w (num+ w (learn v w .05)))
	   (format t "new w:~A~%h:~A~%" w (h-from-v v w))
	   (format t "v-recall:~A~%" (recall v w))))
    w))

(defun hidden-test2 (w)
  (loop for v in '((1 1 1 0 0 0 0 0 0 1)
		   (0 0 0 1 1 1 0 0 0 1)
		   (0 0 0 0 0 0 1 1 1 1)) collect
       (h-from-v v w)))

(defun recall-test2 (w)
  (loop for v in '((1 1 1 0 0 0 0 0 0 1)
		   (0 0 0 1 1 1 0 0 0 1)
		   (0 0 0 0 0 0 1 1 1 1)) collect
       (recall v w)))
