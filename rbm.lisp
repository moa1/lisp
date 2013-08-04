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
  "Put the transpose of 2-dimensional array A into array R."
  (let ((arows (array-dimension a 0))
	(acols (array-dimension a 1))
	(rrows (array-dimension r 0))
	(rcols (array-dimension r 1)))
    (assert (and (= arows rcols) (= acols rrows)))
    (loop for i below rrows do
	 (loop for j below rcols do
	      (setf (aref r i j) (aref a j i)))))
  (values))

(defun array-array-mul (a b r &key (a-t nil) (b-t nil))
  "Multiply array A with array B and put the result in array R.
Arrays A and/or B are transposed before multiplication if A-T and/or B-t is T, respectively."
  ;;TODO: speed this function up by specializing on A-T and B-T.
  ;;TODO: generalize for dimensions>2 (i.e. perform multiplication on dimensions 0 and 1, and repeat array multiplication for other dimensions (like matlab *))
  (let ((arows (array-dimension a (if a-t 1 0)))
	(acols (array-dimension a (if a-t 0 1)))
	(brows (array-dimension b (if b-t 1 0)))
	(bcols (array-dimension b (if b-t 0 1)))
	(rrows (array-dimension r 0))
	(rcols (array-dimension r 1)))
    (assert (= arows rrows))
    (assert (= bcols rcols))
    (assert (= acols brows))
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
				 (* (aref a i k) (aref b k j)))))))))
  (values))

(defun array-array-fun (a b f r &key (a-t nil) (b-t nil))
  "Return the result array R of applying each element in arrays A and B to the function F.
If A-T and/or B-T are T, the arrays A and/or B are transposed before the operation."
  ;;TODO: speed this function up by specializing on A-T and B-T.
  ;;TODO: generalize for dimensions>2, i.e. call F on all elements of A and B, and generalize A-T and B-T to be able to permute the axes before multiplying.
  ;;permutation example: dimensions==(5 3 2) permutation=(2 0 1)
  ;;index 0 == (0 0 0) == p(0 0 0) == perm 0
  ;;index 1 == (0 0 1) == p(1 0 0) == perm 1*5*3 + 0*3 + 0 = perm 15
  ;;index 2 == (0 1 0) == p(0 0 1) == perm 0*5*3 + 0*3 + 1 = perm 1
  ;;index 3 == (0 1 1) == p(1 0 1) == perm 1*5*3 + 0*3 + 1 = perm 16
  (let ((arows (array-dimension a (if a-t 1 0)))
	(acols (array-dimension a (if a-t 0 1)))
	(brows (array-dimension b (if b-t 1 0)))
	(bcols (array-dimension b (if b-t 0 1)))
	(rrows (array-dimension r 0))
	(rcols (array-dimension r 1)))
    (assert (= arows brows rrows))
    (assert (= acols bcols rcols))
    (loop for i below rrows do
	 (loop for j below rcols do
	      (setf (aref r i j)
		    (funcall f
			     (if a-t (aref a j i) (aref a i j))
			     (if b-t (aref b j i) (aref b i j)))))))
  (values))

;; test array-array-mul and array-array-fun
(let ((a #2A((1 2 3) (4 5 6)))
      (b #2A((0 1) (2 3) (4 5)))
      (r (make-array '(2 2)))
      (ta (make-array '(3 2)))
      (tb (make-array '(2 3)))
      (ra (make-array '(2 3))))
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
  (assert (equalp r #2A((16 22) (34 49))))
  (array-array-fun a a #'* ra)
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun ta a #'* ra :a-t t)
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun a ta #'* ra :b-t t)
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun ta ta #'* ra :a-t t :b-t t)
  (assert (equalp ra #2A((1 4 9) (16 25 36)))))

(defun array-fun (a f r &key (a-t nil))
  (declare (ignore a-t))
  "Fill the array R by calling function F on each element of A."
  ;;TODO: implement a-t
  (assert (eq a-t nil))
  (let ((x (reduce #'* (array-dimensions a))))
    (loop for i below x do
	 (setf (row-major-aref r i) (funcall f (row-major-aref a i)))))
  (values))

(defun array-repeat (a new-before-dimensions new-after-dimensions)
  (let* ((a-dimensions (array-dimensions a))
	 (r-dimensions (append new-before-dimensions a-dimensions new-after-dimensions))
	 (r (make-array r-dimensions :element-type (array-element-type a)))
	 (x (apply #'* new-before-dimensions))
	 (y (apply #'* a-dimensions))
	 (z (apply #'* new-after-dimensions))
	 (index 0))
    (dotimes (i x)
      (dotimes (j y)
	(dotimes (k z)
	  (setf (row-major-aref r index) (row-major-aref a j))
	  (setf index (1+ index)))))
    r))

(defun array-project (a f &optional (dim 0))
  "Project the array A along axis DIM.
This produces a new array, where DIM is omitted.
For all elements of the new array, call function F with pairwise items (like reduce) along the DIM axis and store the result in the element.
Return the new array."
  (let* ((a-dimensions (array-dimensions a))
	 (before-dimensions (subseq a-dimensions 0 dim))
	 (after-dimensions (subseq a-dimensions (1+ dim)))
	 (r-dimensions (append before-dimensions after-dimensions))
	 (r (make-array r-dimensions :element-type (array-element-type a)))
	 (x (apply #'* before-dimensions))
	 (y (elt a-dimensions dim))
	 (z (apply #'* after-dimensions))
	 (index-r 0))
    (dotimes (i x)
      (dotimes (k z)
	(let* ((index (+ (* i y z) k))
	       (res (row-major-aref a index)))
	  (loop for j from 1 below y do
	       (setf index (+ index z))
	       (setf res (funcall f res (row-major-aref a index))))
	  (setf (row-major-aref r index-r) res))
	(setf index-r (1+ index-r))))
    r))

(defun new-rbm (n-visible n-hidden)
  "Return a new randomly initialized restricted boltzmann machine."
  (let ((w (make-array (list n-visible n-hidden) :element-type 'float))
	(visbiases (make-array n-visible :element-type 'float))
	(hidbiases (make-array n-hidden :element-type 'float)))
    (loop for i below n-visible do
	 (loop for j below n-hidden do
	      (setf (aref w i j) (* (random-gaussian) .01))))
    (loop for i below n-visible do (setf (aref visbiases i) 0))
    (loop for j below n-hidden do (setf (aref hidbiases j) 0))
    (list w visbiases hidbiases)))

(defun rbm-learn-cd1 (data rbm learn-rate weight-cost)
  ;;TODO: possibility to specify neuron types
  "Do one contrastive-divergence-1 step on a restricted boltzmann machine.
DATA is a two-dimensional array of input values: the first dimension represents the cases of the mini-batch, the second dimension represents visible neurons.
RBM is a restricted boltzmann machine as returned by new-rbm or rbm-learn-cd1."
  (error "this function doesn't yet work")
  (let* ((w (car rbm))
	 (v-biases (cadr rbm))
	 (h-biases (caddr rbm))
	 (n-v (length v-biases))
	 (n-h (length h-biases))
	 (n-cases (array-dimension data 0))
	 (pos-h-probs (make-array (list n-cases n-h) :element-type 'float))
	 (pos-prods (make-array (list n-v n-h) :element-type 'float))
	 (pos-h-act nil)
	 (pos-v-act nil)
	 (pos-h-states (make-array (list n-cases n-h) :element-type 'float))
	 (neg-data (make-array (list n-cases n-v) :element-type 'float))
	 (neg-h-probs (make-array (list n-cases n-h) :element-type 'float))
	 (neg-prods (make-array (list n-v n-h) :element-type 'float))
	 (neg-h-act nil)
	 (neg-v-act nil)
	 (w-inc (make-array (list n-v n-h) :element-type 'float))
	 (v-biases-inc (make-array (list n-v) :initial-element 0.0 :element-type 'float))
	 (h-biases-inc (make-array (list n-h) :initial-element 0.0 :element-type 'float)))
    (array-array-mul data w pos-h-probs)
    (array-array-fun pos-h-probs (array-repeat h-biases (list n-cases) nil) #'+ pos-h-probs)
    (array-fun pos-h-probs (lambda (x) (sigmoid (- x))) pos-h-probs)
    (print (list "pos-h-probs" pos-h-probs))
    (array-array-mul data pos-h-probs pos-prods :a-t t)
    (setf pos-h-act (array-project pos-h-probs #'+ 0))
    (setf pos-v-act (array-project data #'+ 0))
    (print (list "pos-prods" pos-prods "pos-h-act" pos-h-act "pos-v-act" pos-v-act))
    (array-fun pos-h-probs (lambda (x) (if (> x (random 1.0)) 1 0)) pos-h-states)
    (print (list "pos-h-states" pos-h-states))
    ;; negative phase
    (array-array-mul pos-h-states w neg-data :b-t t)
    (array-array-fun neg-data (array-repeat v-biases (list n-cases) nil) #'+ neg-data)
    (array-fun neg-data (lambda (x) (sigmoid (- x))) neg-data)
    (print (list "neg-data" neg-data))
    (array-array-mul neg-data w neg-h-probs)
    (array-array-fun neg-h-probs (array-repeat h-biases (list n-cases) nil) #'+ neg-h-probs)
    (array-fun neg-h-probs (lambda (x) (sigmoid (- x))) neg-h-probs)
    (print (list "neg-h-probs" neg-h-probs))
    (array-array-mul neg-data neg-h-probs neg-prods :a-t t)
    (setf neg-h-act (array-project neg-h-probs #'+ 0))
    (setf neg-v-act (array-project neg-data #'+ 0))
    (print (list "neg-prods" neg-prods "neg-h-act" neg-h-act "neg-v-act" neg-v-act))
    ;; learning
    (let ((err-array (make-array (list n-cases n-v) :element-type 'float))
	  (err nil))
      (array-array-fun data neg-data (lambda (a b) (expt (- a b) 2)) err-array)
      (setf err (aref (array-project (array-project err-array #'+) #'+)))
      (print (list "err" err)))
    (array-array-fun pos-prods neg-prods #'- w-inc)
    (array-fun w-inc (lambda (x) (/ x n-cases))  w-inc)
    (print "A")
    (array-array-fun w-inc w (lambda (a b) (- a (* weight-cost b))) w-inc)
    (print "B")
    (array-array-fun pos-v-act neg-v-act (lambda (a b) (/ (- a b) n-cases)) v-biases-inc)
    (array-array-fun pos-h-act neg-h-act (lambda (a b) (/ (- a b) n-cases)) h-biases-inc)
    (print (list "w-inc" w-inc "v-biases-inc" v-biases-inc "h-biases-inc" h-biases-inc))
    ))

(defparameter *data* #2A((1 1 0 0 0 0) (0 0 1 1 0 0) (0 0 0 0 1 1)))	     

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
