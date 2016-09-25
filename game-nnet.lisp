;;;; MISCELLANEOUS

(defun copy-array-to-array (input output &optional (input-start-index 0) (output-start-index 0) (size (length input)))
  (loop for i below size do
       (setf (aref output (+ i output-start-index)) (aref input (+ i input-start-index)))))

(defun maximal-index (sequence)
  "Return two values: the index with the highest value in SEQUENCE and the highest value, or NIL if SEQUENCE is empty."
  (when (= 0 (length sequence))
    (return-from maximal-index nil))
  (let ((max-value (elt sequence 0))
	(max-index 0)
	(len (length sequence)))
    (loop
       for i from 1 below len do
	 (let ((e (elt sequence i)))
	   (when (> e max-value)
	     (setf max-value e max-index i))))
    (values max-index max-value)))

;;;; NNET

(declaim (inline sigmoid))
(defun sigmoid (x)
  (declare (optimize (speed 3))
	   (type single-float x))
  (cond
    ((<= -88.7 x)
     (/ 1 (+ 1 (exp (- x)))))
    (t
     0.0)))

(defun sample1 (&rest elements)
  (let ((l (length elements)))
    (elt elements (random l))))

(defstruct nnet
  (weights-list nil :type list)) ;lists of (SIMPLE-ARRAY SINGLE-FLOAT 2)

(defun eval-nnet (nnet input)
  "Inputting INPUT into the NNET, propagate forward until the last layer is reached. Return the last layer.
Note that INPUT should have one array element that is 1.0, so that it can be used as bias unit."
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
	   (type (simple-array single-float 1) input)
	   (inline sigmoid))
  (let* ((weights-list (nnet-weights-list nnet)))
    (dolist (weights weights-list)
      (declare (type (simple-array single-float 2) weights))
      (assert (= (length input) (array-dimension weights 0)))
      (let* ((input-size (array-dimension weights 0))
	     (output-size (array-dimension weights 1))
	     (output (make-array output-size :element-type 'single-float)))
	(declare (type (simple-array single-float 1) output))
	(loop for j fixnum from 0 below output-size do
	     (setf (aref output j)
		   (sigmoid (do ((i 0 (1+ i)) (sum 0.0)) ((>= i input-size) sum)
			      (declare (type single-float sum) (type fixnum i))
			      (incf sum (* (aref input i) (aref weights i j)))))))
	(setf input output))))
  input)

(defun nnet-get-layer-sizes (nnet)
  (let* ((wl (nnet-weights-list nnet)))
    (cons (array-dimension (car wl) 0)
	  (mapcar (lambda (w) (array-dimension w 1)) wl))))

(defun make-nnet-offspring (nnet1 nnet2)
  (flet ((weights-offspring (weights1 weights2 in on)
	   "IN and ON are the number of inputs and outputs, respectively."
	   (declare (type (simple-array single-float 2) weights1 weights2))
	   (let* ((i1 (array-dimension weights1 0)) (o1 (array-dimension weights1 1))
		  (i2 (array-dimension weights2 0)) (o2 (array-dimension weights2 1))
		  (weightsn (make-array (list in on) :element-type 'single-float)))
	     (declare (type (simple-array single-float 2) weightsn))
	     (loop for i below in do
		  (loop for o below on do
		       (setf (aref weightsn i o)
			     (+ (random .1) -0.05
				(apply #'sample1 (let ((poss (append (when (and (< i i1) (< o o1)) (list (aref weights1 i o)))
								     (when (and (< i i2) (< o o2)) (list (aref weights2 i o))))))
						   (if (null poss) '(0) poss)))))))
	     weightsn)))
    (let* ((wl1 (nnet-weights-list nnet1)) (l1 (length wl1))
	   (wl2 (nnet-weights-list nnet2)) (l2 (length wl2))
	   (wln nil))
      (assert (= l1 l2)) ;maybe TODO: write an algorithm that tries to match layers between nnets with different number of layers.
      ;; TODO: the following code sucks.
      (let* ((layer-sizes1 (nnet-get-layer-sizes nnet1))
	     (layer-sizes2 (nnet-get-layer-sizes nnet2))
	     (layer-sizes (append (list (car layer-sizes1))
				  (mapcar (lambda (ls1 ls2) (max 1 (+ (sample1 ls1 ls2) (if (= 0 (random 8)) (- (random 3) 1) 0))))
					  (subseq layer-sizes1 1 (1- (length layer-sizes1)))
					  (subseq layer-sizes2 1 (1- (length layer-sizes2))))
				  (last layer-sizes1))))
	(mapcar (lambda (w1 w2 in on) (push (weights-offspring w1 w2 in on) wln))
		wl1 wl2
		layer-sizes (cdr layer-sizes)))
      (make-nnet :weights-list (nreverse wln)))))

(defun make-new-nnet (layer-sizes)
  (let ((wl (mapcar (lambda (in on) (make-array (list in on) :element-type 'single-float :initial-element 0.0))
		    layer-sizes
		    (cdr layer-sizes))))
    (make-nnet :weights-list wl)))

