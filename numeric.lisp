;; implements numeric operations like in R

(defun all (&rest bools)
  (dolist (b bools)
    (if (not b)
	(return-from all nil)))
  t)

(defun any (&rest bools)
  (dolist (b bools)
    (if b
	(return-from any t)))
  nil)

(defun num-type (a)
  "Return the type of a num."
  (cond
    ((null a) (values 0 nil))
    ((listp a) (if (listp (car a))
		   (values (length a) (length (car a)))
		   (values (length a) nil)))
    (t (values t nil))))

(defun num-is-number (a)
  (multiple-value-bind (x y) (num-type a)
    (and (null y) (eq x t))))

(defun num-is-vector (a)
  (multiple-value-bind (x y) (num-type a)
    (and (null y) (numberp x))))

(defun num-is-matrix (a)
  "Return wether a is a matrix and all second dimensions are equal."
  (and (multiple-value-bind (x y) (num-type a)
	 (declare (ignore x))
	 (not (null y)))
       (apply #'all (mapcar #'listp a))
       (apply #'= (mapcar #'length a))))

(defun num-iterate-2 (a b f)
  "Iterates over a and b and applies function to the elements."
  (cond
    ((num-is-number a)
     (cond
       ((num-is-number b) (funcall f a b))
       ((num-is-vector b) (loop for x in b collect (funcall f a x)))
       ((num-is-matrix b) (loop for row in b collect
			       (loop for x in row collect (funcall f a x))))
       (t (error "unknown num b"))))
    ((num-is-vector a)
     (cond
       ((num-is-number b) (loop for x in a collect (funcall f x b)))
       ((num-is-vector b) (loop for x in a for y in b collect
			       (funcall f x y)))
       ((num-is-matrix b) (loop for row in b collect
			       (loop for x in row for y in a collect
				    (funcall f y x))))
       (t (error "unkown num b"))))
    ((num-is-matrix a)
     (cond
       ((num-is-number b) (loop for row in a collect
			       (loop for x in row collect (funcall f x b))))
       ((num-is-vector b) (loop for row in a collect
			       (loop for x in row for y in b collect
				    (funcall f x y))))
       ((num-is-matrix b) (loop for rowa in a for rowb in b collect
			       (loop for x in rowa for y in rowb collect
				    (funcall f x y))))
       (t (error "unknown num b"))))
    (t (error "unknown num a"))))

(defun num+-2 (a b)
  (num-iterate-2 a b #'+))

(defun num*-2 (a b)
  (num-iterate-2 a b #'*))

(defun num--2 (a b)
  (num-iterate-2 a b #'-))

(defun num/-2 (a b)
  (num-iterate-2 a b #'/))

(defun num+ (&rest r)
  (reduce #'num+-2 r))

(defun num* (&rest r)
  (reduce #'num*-2 r))

(defun num- (&rest r)
  (reduce #'num--2 r))

(defun num/ (&rest r)
  (reduce #'num/-2 r))

(defun matrix-dim (m)
  ;; m is a matrix
  (cons (length m) (length (car m))))

(defun outer (a b &optional (f #'*))
  "a, b are vectors. computes the outer product of a * b (or, if f is specified (f a b))"
  (loop for x in a collect
       (loop for y in b collect
	    (funcall f x y))))
