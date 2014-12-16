(defun factorial (n)
  (cond ((<= n 0) 1)
	(t (* n (factorial (- n 1))))))

(defparameter *ht* nil)
(defparameter *ht2* nil)

(defun random-permutation (max)
  "Return a random permutation of the numbers 0 to (1- max)."
  (let ((s (make-array max :element-type 'integer))
	(r nil))
    (loop for i below max do (setf (aref s i) i))
    (loop for i from (1- max) downto 0 do
	 (let* ((j (random (1+ i)))
		(x (aref s j)))
	   (setf (aref s j) (aref s i))
	   (setf r (cons x r))))
    r))

(defun fill-order (n)
  (setf *ht* (make-hash-table :test 'equal))
  (setf *ht2* (make-hash-table :test 'equal))
  (loop for i below n do
       (print i)
       (let ((start i) (stop (+ i 100)))
	 (let ((l (append '(1 2 3 4 5) (loop for j from start upto stop collect j))))
	   (setf (gethash l *ht*) 1))))
  (loop for i in (random-permutation n) do
       (print i)
       (let ((start i) (stop (+ i 100)))
	 (let ((l (append '(1 2 3 4 5) (loop for j from start upto stop collect j))))
	   (setf (gethash l *ht2*) 1)))))

(defun order (ht)
  (let ((r nil))
    (maphash (lambda (k v) (setf r (cons (cons k v) r))) ht)
    r))

(defun golden-ratio (n &optional (initial 1))
  (if (= 0 n)
      initial
      (golden-ratio (1- n) (1+ (/ 1 initial)))))
