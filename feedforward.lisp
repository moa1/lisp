;; Feed forward neural network
;; right now, it needs joy-arch.lisp for the test-cases

(defun sigmoid (x)
  (cond
    ((>= x -88) (/ 1 (1+ (exp (- x)))))
    (t 0.0)))

(defun new-weights (layers-sizes &key (init-fn (lambda (l1 i1 i2) (declare (ignore l1 i1 i2)) (- (random 1.0) 0.5))))
  "Return a sequence of arrays representing the weights between adjacent layers of sizes LAYERS-SIZES.
Init each weight with a call to INIT-FN, which receives the number of the first layer and the indices of the weight matrix, and should return a number representing the weight."
  (let ((n-layers (length layers-sizes))
	(w nil))
    (loop
       for li1 below (1- n-layers)
       for li2 from 1 below n-layers
       do (let* ((l1-size (elt layers-sizes li1))
		 (l2-size (elt layers-sizes li2))
		 (w-l1-l2 (make-array (list l1-size l2-size) :element-type 'float)))
	    (loop for i1 below l1-size do
		 (loop for i2 below l2-size do
		      (setf (aref w-l1-l2 i1 i2) (funcall init-fn li1 i1 i2))))
	    (setf w (cons w-l1-l2 w))))
    (nreverse w)))

(defun new-layers (layers-sizes)
  (let ((l (length layers-sizes)))
    (loop for l1 below l for l1-size in layers-sizes collect
	 (make-array l1-size :element-type 'float))))

(defun feed-forward (l w)
  "Return the values of the layers when feeding the signals in L through weights W to the respective next layer.
The return values of the first layer are a copy of the first layer of L.
The dimensions of W must match to the sizes of L."
  (let* ((layers-sizes (mapcar #'length l))
	 (n-layers (length layers-sizes))
	 (new-l (new-layers layers-sizes)))
    (loop for i below (car layers-sizes) do (setf (aref (car new-l) i) (aref (car l) i))) ;copy first layer
    (loop for li1 below (1- n-layers) for li2 from 1 below n-layers do
	 (let* ((l1 (elt l li1))
		(l2 (elt new-l li2))
		(l1-size (length l1))
		(l2-size (length l2))
		(w12 (elt w li1)))
	   (loop for i2 below l2-size do
		(let ((i2-value (loop for i1 below l1-size sum
				     (* (aref l1 i1) (aref w12 i1 i2)))))
		  (setf (aref l2 i2) (sigmoid i2-value))))))
    new-l))

(defun copy-array (a)
  "Return the copy of an array A, preserving the element-type, the adjustable value, and the fill-pointer."
  (let* ((d (array-dimensions a))
	 (na (make-array d :element-type (array-element-type a) :adjustable (adjustable-array-p a) :fill-pointer (if (array-has-fill-pointer-p a) (fill-pointer a) nil))))
    (loop for i below (array-total-size a) do
	 (setf (row-major-aref na i) (row-major-aref a i)))
    na))

(defun copy-weights (w)
  (loop for w1 in w collect (copy-array w1)))

(defun mutate-weights (w amplitude)
  (loop for w1 in w do
       (loop for i below (array-total-size w1) do
	    (setf (row-major-aref w1 i) 
		  (+ (row-major-aref w1 i) (- (/ amplitude 2)) (random amplitude)))))
  nil)

(defun feedforward-ga (genomes cycles fitness fitness-fn mutate-fn)
  (let* ((size (length genomes))
	 (fit (make-array size :initial-element 0)))
    (let ((test-goal-values (fitness-generate-test-goal-values fitness)))
      (dotimes (s size) (setf (aref fit s) (funcall fitness-fn test-goal-values (aref genomes s)))))
    (loop for i below cycles do
	 (let* ((test-goal-values (fitness-generate-test-goal-values fitness))
		(i1 (random size))
		(i2 (random size))
		(fit1 (elt fit i1))
		(fit2 (elt fit i2))
		(i-fit (if (> fit1 fit2) i1 i2))
		(i-unfit (if (> fit1 fit2) i2 i1))
		(fit-unfit (if (> fit1 fit2) fit2 fit1))
		(genome-new (funcall mutate-fn (elt genomes i-fit) (/ i cycles)))
		(fit-new (funcall fitness-fn test-goal-values genome-new)))
	   (format t "fit-unfit:~A fit-new:~A~%" fit-unfit fit-new)
	   (when (> fit-new fit-unfit)
	     (setf (elt genomes i-unfit) genome-new)
	     (setf (elt fit i-unfit) fit-new)))
	 (format t "i:~A maxfit:~A~%" i (reduce #'max fit)))
;;    (format t "best:~A~%" (elt population (position (reduce #'max fit) fit)))
    (elt genomes (position (reduce #'max fit) fit))))

(defun fitness-feedforward-multi-layer (test-goal-values genome &optional (verbose nil))
  "Test a genome on the problem cases given by TEST-GOAL-VALUES and return its fitness.
In our case, GENOME is a list of feedforward neural network weight matrices."
  (let* ((w genome)
	 (n-layers (1+ (length w)))
	 (layers-sizes (append (mapcar (lambda (x) (array-dimension x 0)) w)
			       (list (array-dimension (car (last w)) 1))))
	 (test-values-list (mapcar #'car test-goal-values))
	 (goal-value-list (mapcar #'cadr test-goal-values)))
    (loop for test-values in test-values-list
       for goal-value in goal-value-list
       sum
	 (let ((l (new-layers layers-sizes)))
	   (test-value-floats-to-layer test-values 3 (car l)) ;3 is the number of neurons for one test-cases value
	   (loop for i below (1- n-layers) do
		(setf l (feed-forward l w)))
	   (when verbose
	     (print (list "l" l)))
	   (let* ((res (car (last l)))
		  (f (layer-bit-vector-to-float res)))
	     (when verbose
	       (print (list "test-values" test-values "f" f "goal-value" goal-value)))
	     (- (absdiff goal-value f)))))))

(defun test-value-floats-to-layer (float-test-values bits-per-float layer)
  (loop for f in float-test-values
     for bit-start from 0 by bits-per-float
     do
       (let ((displaced-layer (make-array bits-per-float :displaced-to layer :displaced-index-offset bit-start)))
	 (float-to-layer-bit-vector f displaced-layer)))
  nil)

(defun float-to-layer-bit-vector (f bit-layer)
  "Convert a float to a bit vector.
The element of BIT-LAYER numbered 1 to (1- (length BIT-LAYER)) store the bits 0 to (- (length BIT-LAYER) 2) of the floor-ed value of F.
The element numbered 0 stores the remainder of the floor-ed F."
  (multiple-value-bind (int rem) (floor f)
    (assert (and (>= int 0) (<= int (1- (expt 2 (1- (length bit-layer))))))) ;TODO: remove (>= f 0) limitation with sign bit (and also adapt layer-bit-vector-to-float)
    (setf (aref bit-layer 0) rem)
    (do ((i 1 (1+ i)) (ipow 1 (* 2 ipow))) ((>= i (length bit-layer)))
      (setf (aref bit-layer i) (if (= 0 (logand int ipow)) 0 1))))
  nil)

(defun layer-bit-vector-to-float (bit-layer)
  "Assume that the BIT-LAYER array has a format like the one required for input of float-to-layer-bit-vector.
Return the float that is represented by BIT-LAYER."
  (+ (aref bit-layer 0)
     (do ((i 1 (1+ i)) (ipow 1 (* 2 ipow)) (sum 0)) ((>= i (length bit-layer)) sum)
       (setf sum (+ sum (* ipow (if (< (aref bit-layer i) 0.5) 0 1)))))))
       ;;(setf sum (+ sum (* ipow (aref bit-layer i)))))))

(defun test1 ()
  (let* ((test-cases *test-cases-identity4*)
	 (genomes (apply #'vector (loop for i below 200 collect (new-weights '(6 1 3)))))
	 (best (feedforward-ga genomes 10000 test-cases #'fitness-feedforward-multi-layer
			       (lambda (g j) (let ((new (copy-weights g))) (mutate-weights new (float (* 40 (- 1 j)))) new))))
	 (test-goal-values (fitness-generate-test-goal-values test-cases)))
    (fitness-feedforward-multi-layer test-goal-values best t)
    (defparameter *best* best)))