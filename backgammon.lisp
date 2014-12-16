(asdf:oos 'asdf:load-op 'utils)

(defstruct ff-network
  (layernodes nil)
  (weightmatrix nil))

(defun make-ff-layers (layernodes &key initfn)
  (let* ((wm (mappair (lambda (n m)
			(let ((contents (loop for i below n collect
					     (loop for j below m
						  collect (funcall initfn)))))
			  (make-array (list n m)
				      :initial-contents contents)))
		      layernodes)))
    (make-ff-network :layernodes layernodes :weightmatrix wm)))

(defun step-ff-layer (input weights activation-fn)
  "calculate output for one layer with input and weights"
  (let ((n-out (array-dimension weights 1))
	(n-in (array-dimension weights 0)))
    (assert (eq n-in (length input)))
    (loop for j below n-out collect
	 (funcall activation-fn
		  (loop for i below n-in sum
		       (* (aref weights i j) (nth i input)))))))

(defun calc-ff-network (input network)
  (let ((n-layers (length (ff-network-layernodes network)))
	(wm (ff-network-weightmatrix network)))
    (labels ((rec (input layer)
	       (prind input layer)
	       (if (>= layer (1- n-layers))
		   input
		   (rec (step-ff-layer input (nth layer wm) #'sigmoid)
			(1+ layer)))))
      (rec input 0))))

(defun crossover-ff-networks (network1 network2)
  "Calculate a mix of network1 and network2, which must have same layernodes."
  (let ((layernodes1 ((ff-network-layernodes network1)))	
	(layernodes2 ((ff-network-layernodes network2))))
    (assert (eq layernodes1 layernodes2))
    (let ((wm 