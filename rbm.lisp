;; Restricted Boltzmann Machine

(load "numeric.lisp")

(defun h-from-v (v w)
  (let ((h-size (cdr (matrix-dim w)))
	(v-size (length v)))
    (loop for j below h-size collect
	 (loop for i below v-size for wi in w for vi in v sum
	      (* vi (elt wi j))))))

(defun v-from-h (w h)
  (let ((h-size (cdr (matrix-dim w)))
	(v-size (length v)))
    (loop for i below v-size for wi in w collect
	 (loop for j below h-size for hj in h sum
	      (* hj (elt wi j))))))

(defun learn (v w rate)
  ;; v are the input layer values, w is the connection matrix
  (let* ((h (h-from-v v w))
	 (pos (outer v h))
	 (v1 (v-from-h w h))
	 (h1 (h-from-v v w))
	 (neg (outer v1 h1))
	 (update (num* rate (num- pos neg))))
    update))

(defparameter v '(1 2))

(let ((w '((-1.5 -.4 .2) (-.3 .1 -2.2))))
  (loop for i below 100 do
       (setf w (num+ w (learn v w .05)))
       (format t "new w:~A h:~A~%" w (h-from-v v w))))
