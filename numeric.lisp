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

(defun num>-2 (a b)
  (num-iterate-2 a b #'>))

(defun num<-2 (a b)
  (num-iterate-2 a b #'<))

(defun num>=-2 (a b)
  (num-iterate-2 a b #'>=))

(defun num=-2 (a b)
  (num-iterate-2 a b #'=))

(defun num<=-2 (a b)
  (num-iterate-2 a b #'<=))

(defun num> (&rest r)
  (reduce #'num>-2 r))

(defun num< (&rest r)
  (reduce #'num<-2 r))

(defun num>= (&rest r)
  (reduce #'num>=-2 r))

(defun num<= (&rest r)
  (reduce #'num<=-2 r))

(defun num= (&rest r)
  (reduce #'num=-2 r))

(defun ifelse (test then else)
  "Returns a value with the same shape as test from then or else depending on
whether test is T or NIL. then and else must have the same shape, and this shape
must be smaller than test."
  (cond
    ((num-is-number test) (if test then else))
    ((num-is-vector test)
     (cond ((num-is-number then) (loop for a in test collect (if a then else)))
	   ((num-is-vector then)
	    (loop for a in test for b in then for c in else collect
		 (if a b c)))
	   ((num-is-matrix then) (error "then and else must have a shape smaller than test"))
	   (t (error "unknown num then"))))
    ((num-is-matrix test)
     (cond 
       ((num-is-number then) (loop for row in test collect
				  (loop for x in row collect (if x then else))))
       ((num-is-vector then) (loop for row in test collect
				  (loop for x in row
				     for y in then for z in else collect
				       (if x y z))))
       ((num-is-matrix then) (loop for rowa in test
				for rowb in then for rowc in else collect
				  (loop for x in rowa
				     for y in rowb for z in rowc collect
				       (if x y z))))
       (t (error "unknown num then"))))
    (t (error "unknown num test"))))
