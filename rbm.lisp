;; Restricted Boltzmann Machine

(load "numeric.lisp")

(defun join (result-type s separator)
  "Concatenate the sequences given in the sequence S, with SEPARATOR between each pair of sequences.
RESULT-TYPE is the resulting sequence type."
  (reduce (lambda (a b) (concatenate result-type a separator b)) s))

(defmacro prind (&rest rest)
  (labels ((rec (rest pairs)
	     (if (null rest)
		 (reverse pairs)
		 (if (keywordp (car rest))
		     (let ((text (format nil "~A=" (car rest)))
			   (value (cadr rest)))
		       (rec (cddr rest) (cons `((princ ,text) (princ ,value)) pairs)))
		     (let ((text (format nil "~A=" (car rest)))
			   (value (car rest)))
		       (rec (cdr rest) (cons `((princ ,text) (princ ,value)) pairs)))))))
    (let* ((progns (rec rest nil))
	   (progns-with-separator (join 'list progns '((princ " ")))))
      ;;(print progns-with-separator)
      `(progn ,@progns-with-separator (terpri)))))

(defun map-into-list (result-list function &rest lists)
  "Same as map-into, but for lists instead of sequences."
  (labels ((rec (res lists)
	     (if (null res) (return-from rec))
	     (loop for l in lists do
		  (if (null l) (return-from rec)))
	     (setf (car res)
		   (apply function (mapcar #'car lists)))
	     (rec (cdr res) (mapcar #'cdr lists))))
    (rec result-list lists)))

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
  nil)

(defun array-array-mul (a b r &key (a-t nil) (b-t nil))
  "Multiply two-dimensional arrays A and B and put the result in two-dimensional array R.
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
  nil)

(defun permute (l perm)
  "Permute list L with permutation list PERM.
PERM contains, for each element at position x, the dimension number that is mapped to dimension x.
PERM must contain all numbers between 0 and (1- (length perm)), and each number only once."
  (loop for p in perm collect
       (elt l p)))

(defun reverse-permute (l perm)
  "Reverse permute list L with permutation list PERM.
Reverse permute means, that for each element e at position x in PERM, the dimension x of L is mapped to position e in the result.
PERM must contain all numbers between 0 and (1- (length perm)), and each number only once."
  (let ((res (loop for i below (length perm) collect -1)))
    (loop for e in perm for x below (length perm) do
	 (setf (elt res e) (elt l x)))
    res))

(defun inverse-permutation (perm)
  "Return the permutation that must be applied after or before applying PERM to obtain the identity permutation."
  (reverse-permute (loop for i below (length perm) collect i) perm))

(defun permutation-product (perm-a perm-b)
  "Return the permutation that results from first applying PERM-A, then PERM-B."
  ;; (0 1 2 3 4 5) * (0 1 2 3 4 5) = (0 1 2 3 4 5)
  ;; (0 3 2 1 5 4)   (0 5 2 1 3 4)   (0 1 2 5 4 3)
  (permute perm-b perm-a))

;; test permutations
(let* (;;   0 1 2 3 4 5
       (a '(2 1 3 0 5 4))
       (a-1 (inverse-permutation a))
       ;;   0 1 2 3 4 5
       (b '(2 4 3 5 1 0))
       (b-1 (inverse-permutation b))
       (id (loop for i below 6 collect i)))
  (assert (equal (permute id a) a))
  (assert (equal (reverse-permute a a) id))
  (assert (equal (permutation-product a a-1) id))
  (assert (equal (permutation-product a-1 a) id))
  (assert (equal (permutation-product a b) '(3 4 5 2 0 1)))
  (assert (equal (permutation-product (permutation-product a b) b-1) a)))

(defun row-major-index-to-subscripts (dims index)
  "Return the list of subscripts S so that (row-major-aref A index) == (apply #'aref A S) for all arrays A."
  (labels ((rec (d i sub)
	     (if (null d)
		 (if (= 0 i)
		     sub
		     (error "Index ~A is greater than (remainder ~A) the greatest possible index for an array of dimensions ~A." index i dims))
		 (multiple-value-bind (q r) (floor i (car d))
		   (rec (cdr d) q (cons r sub))))))
    (rec (reverse dims) index nil)))

(defun subscripts-to-row-major-index (dims subscripts)
  "Return the row-major-index I so that (row-major-aref A I) == (apply #'aref A SUBSCRIPTS) for all arrays A."
  (declare (type list dims subscripts))
  (labels ((rec (d s i)
	     (declare (type (and fixnum unsigned-byte) i))
	     (if (null d)
		 (if (null s)
		     (the (and fixnum unsigned-byte) i)
		     (error "DIMS and SUBSCRIPTS don't have the same length: ~A ~A." dims subscripts))
		 (if (null s)
		     (error "DIMS and SUBSCRIPTS don't have the same length: ~A ~A." dims subscripts)
		     (let ((dim (car d))
			   (sub (car s)))
		       (declare (type (and fixnum unsigned-byte) sub dim))
		       (if (or (< sub 0) (>= sub dim))
			   (error "Subscript (~A) must be non-negative and smaller than dimension (~A)." sub dim)
			   (rec (cdr d) (cdr s) (+ (the (and fixnum unsigned-byte) (* dim i)) sub)))))))) ;doesn't optimize in sbcl
    (rec dims subscripts 0)))

(defun array-permute (array perm)
  "Return a new array that has the same contents as array ARRAY but with its dimensions permuted by permutation list PERM.
This function is slow, you should use array-walk or arrays-walk instead."
  (let* ((dim (array-dimensions array))
	 (r-dim (permute dim perm))
	 (r (make-array r-dim :element-type (array-element-type array)))
	 (size (reduce #'* dim)))
    (loop for index below size do
	 (let* ((subscripts (row-major-index-to-subscripts dim index))
		(r-subscripts (permute subscripts perm))
		(r-index (subscripts-to-row-major-index r-dim r-subscripts)))
	   (setf (row-major-aref r r-index) (row-major-aref array index))))
    r))

(defun valid-array-slice (slice array-dimensions)
  "Check if the list SLICE is a valid array slice of array ARRAY.
Example: (valid-array-slice '((0 2) nil (0 0 3 4)) '(2 3 4))"
  (when (not (= (length slice) (length array-dimensions)))
    (return-from valid-array-slice (values nil 'dimensions)))
  (mapc (lambda (slice dim)
	  (let* ((slice-len (length slice)))
	    (when (oddp slice-len)
	      (return-from valid-array-slice (values nil 'oddp)))
	    (when (not (apply #'<= 0 (append slice (list dim))))
	      (return-from valid-array-slice (values nil 'order)))))
	slice array-dimensions)
  (values t t))

(defun array-walk (dim perm slice function)
  "Call function FUNCTION with row-major-indices of an array with dimensions DIMS.
PERM is the array's dimensions permutation, which is applied before iterating through all the dimensions of the array.
SLICE is the array slice which is applied before iterating but after permuting."
  (let* ((n-dims (length dim))
	 (dim-inc (loop for i below n-dims collect
		       (reduce #'* (nthcdr (1+ i) dim))))
	 (dim-perm (permute dim perm))
	 (dim-inc-perm (permute dim-inc perm)))
    (assert (valid-array-slice slice dim-perm))
    (labels ((rec (n-dim i-0 dim dim-inc slice)
	       (if (< n-dim n-dims)
		   (let ((d-max (car dim))
			 (i-inc (car dim-inc))
			 (d-slice (car slice)))
		     (when (null d-slice)
		       (setf d-slice (list 0 d-max)))
		     (do* ((interval d-slice (cddr interval))
			   (start (car interval) (car interval))
			   (end (cadr interval) (cadr interval)))
			  ((null interval))
		       (loop
			  for d from start below end
			  for i from (+ i-0 (* start i-inc)) by i-inc do
			    (rec (1+ n-dim) i (cdr dim) (cdr dim-inc) (cdr slice)))))
		   (funcall function i-0))))
      (rec 0 0 dim-perm dim-inc-perm slice))))

(defun arrays-walk (arrays-dim arrays-perm slice function)
  "Call function FUNCTION with row-major-indices of arrays.
ARRAYS-DIM is the list of the arrays' array-dimensions.
ARRAYS-PERM is the list of the arrays' dimension permutations, which is applied before iterating through all the dimensions of the arrays.
The permuted array dimensions must all be equal.
ARRAYS-SLICE is the array slice which is applied before iterating but after permuting (hence there's only one array slice not one for every array)."
  (assert (= (length arrays-dim) (length arrays-perm))) ;equal number of arrays
  (mapcar (lambda (dim perm)
	    (assert (= (length dim) (length perm) (length (car arrays-dim)) (length slice))))
	  arrays-dim arrays-perm) ;equal number of dimensions
  (when (not (null arrays-dim))
    (let* ((n-arrays (length arrays-dim))
	   (n-dims (length (car arrays-dim)))
	   (dims-inc (loop for dim in arrays-dim collect
			  (loop for i below n-dims collect
			       (reduce #'* (nthcdr (1+ i) dim)))))
	   (array0-dim-perm (permute (car arrays-dim) (car arrays-perm)))
	   (dims-inc-perm (mapcar (lambda (dim-inc perm) (permute dim-inc perm))
				  dims-inc arrays-perm)))
      (assert (valid-array-slice slice array0-dim-perm))
      (loop for dim in (cdr arrays-dim) for perm in (cdr arrays-perm) do
	   (assert (equal array0-dim-perm (permute dim perm))))
      (labels ((rec (n-dim dims i0s i0s-incs slice)
		 ;; N-DIM is the number of dimensions.
		 ;; DIMS is the list of dimension sizes, which is equal for all arrays
		 ;; I0S is the list of start indices
		 ;; I0S-INCS is the list of list of i0 (index) increments
		 ;; SLICES is the list of array slices
		 ;;(prind n-dim dims i0s i0s-incs slice)
		 (if (< n-dim n-dims)
		     (let ((d-max (car dims))
			   (i0s-copy nil)
			   (i-incs (mapcar #'car i0s-incs))
			   (d-slice (car slice)))
		       (when (null d-slice)
			 (setf d-slice (list 0 d-max)))
		       (do* ((interval d-slice (cddr interval))
			     (start (car interval) (car interval))
			     (end (cadr interval) (cadr interval)))
			    ((null interval))
			 (setf i0s-copy (loop for i0 in i0s for i-inc in i-incs
					   collect (+ i0 (* start i-inc))))
			 (loop
			    for d from start below end do
			      (rec (1+ n-dim) (cdr dims) i0s-copy (mapcar #'cdr i0s-incs) (cdr slice))
			      (map-into-list i0s-copy #'+ i0s-copy i-incs))))
		     (apply function i0s))))
	(rec 0 array0-dim-perm (loop for i below n-arrays collect 0) dims-inc-perm slice)))))

;; test arrays-walk
(let* ((a #3A(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23))))
       (a-perm '(2 0 1))
       (a-dim (array-dimensions a))
       (a-slice '(nil nil nil))
       (a2-slice (permute '(nil (1 2) (0 1 3 4)) a-perm))
       (b-dim (permute a-dim a-perm))
       (b (make-array b-dim))
       (b-perm '(0 1 2))
       (r1 nil)
       (r2 nil))
  (array-walk a-dim a-perm a-slice (lambda (x) (push x r1)))
  (arrays-walk (list a-dim) (list a-perm) a-slice (lambda (x) (push x r2)))
  (assert (equal r1 r2)) ;array-walk and arrays-walk walk the same order
  (arrays-walk (list a-dim b-dim) (list a-perm b-perm) a-slice
	       (lambda (x y) (setf (row-major-aref b y) (row-major-aref a x))))
  (assert (equalp b (array-permute a a-perm))) ;permutation works the same way as array-permute does
  (array-walk a-dim a-perm a2-slice (lambda (x) (push x r1)))
  (arrays-walk (list a-dim) (list a-perm) a2-slice (lambda (x) (push x r2)))
  (assert (equal r1 r2)) ;array-walk and arrays-walk walk the same order with slice
  )

(defun array-array-fun-slice (a b f r &key a-perm b-perm slice)
  (let ((default-a-perm (loop for i below (length (array-dimensions a)) collect i))
	(default-b-perm (loop for i below (length (array-dimensions b)) collect i)))
    (when (null a-perm)
      (setf a-perm default-a-perm))
    (when (null b-perm)
      (setf b-perm default-b-perm))
    (arrays-walk (list (array-dimensions a) (array-dimensions b) (array-dimensions r))
		 (list a-perm b-perm (loop for i below (length (array-dimensions r)) collect i))
		 slice
		 (lambda (a-i b-i r-i)
		   (setf (row-major-aref r r-i)
			 (funcall f
				  (row-major-aref a a-i)
				  (row-major-aref b b-i)))))))

(defun array-array-fun-noperm (a b f r)
  (let* ((a-dims (array-dimensions a))
	 (b-dims (array-dimensions b))
	 (r-dims (array-dimensions r))
	 (size (reduce #'* a-dims)))
    (declare (type (and fixnum unsigned-byte) size))
    (assert (and (equal r-dims a-dims) (equal r-dims b-dims)))
      (loop for i below size do
	   (setf (row-major-aref r i) (funcall f (row-major-aref a i) (row-major-aref b i)))))
  nil)

(defun array-array-fun-perm (a b f r &key a-perm b-perm)
  (declare (type cons a-perm b-perm))
  "Return the result array R of applying each element in arrays A and B to the function F.
A-PERM and B-PERM are indexing permutations of matrix A and B, i.e. the indices used to address elements of A and B are permuted with the function PERMUTE before calling function F."
  (let* ((a-dims (array-dimensions a))
	 (b-dims (array-dimensions b))
	 (r-dims (array-dimensions r))
	 (r-max-dim (1- (length r-dims)))
	 (perm-a-dims (reverse-permute a-dims a-perm))
	 (perm-b-dims (reverse-permute b-dims b-perm)))
    ;;(prind r-dims perm-a-dims perm-b-dims)
    (assert (and (equal r-dims perm-a-dims) (equal r-dims perm-b-dims)))
    (let* ((lowest-r-dim (car (last r-dims)))
	   (butlowest-r-dims (butlast r-dims))
	   (butlowest-r-size (reduce #'* butlowest-r-dims))
	   (perm-a-inc (reduce #'* (nthcdr (1+ (position r-max-dim a-perm)) a-dims)))
	   (perm-b-inc (reduce #'* (nthcdr (1+ (position r-max-dim b-perm)) b-dims)))
	   (r-index 0))
      (declare (type (and fixnum unsigned-byte) lowest-r-dim butlowest-r-size perm-a-inc perm-b-inc))
      ;;(prind r-dims a-dims b-dims a-perm b-perm perm-a-inc perm-b-inc)
      (loop for i below butlowest-r-size do
	   (let* ((r-subscripts (row-major-index-to-subscripts r-dims r-index))
		  ;;(debug1 (prind r-dims r-index r-subscripts))
		  (a-subscripts (permute r-subscripts a-perm))
		  (b-subscripts (permute r-subscripts b-perm))
		  ;;(debug (progn (prind r-subscripts a-subscripts b-subscripts)))
		  (a-index (subscripts-to-row-major-index a-dims a-subscripts))	
		  (b-index (subscripts-to-row-major-index b-dims b-subscripts)))
	     (declare (type (and fixnum unsigned-byte) a-index b-index))
	     (loop for j below lowest-r-dim do
		  ;;(prind r-index a-index b-index :r-s (row-major-index-to-subscripts r-dims r-index) :a-s (row-major-index-to-subscripts a-dims a-index) :b-s (row-major-index-to-subscripts b-dims b-index))
		  (setf (row-major-aref r r-index)
			(funcall f
				 (row-major-aref a a-index)
				 (row-major-aref b b-index)))
		  (incf r-index)
		  (incf a-index perm-a-inc)
		  (incf b-index perm-b-inc))))))
  nil)

(defun array-array-fun (a b f r &key (a-perm nil) (b-perm nil))
  "Return the result array R of applying each element in arrays A and B to the function F.
A-PERM and B-PERM are indexing permutations of matrix A and B, i.e. the indices used to address elements of A and B are permuted with the function PERMUTE before calling function F."
  ;; TODO: generalize this function to allow computing with arbitrarily many arrays (f then takes arbitrarily many parameters)
  (let ((default-a-perm (loop for i below (length (array-dimensions a)) collect i))
	(default-b-perm (loop for i below (length (array-dimensions b)) collect i)))
    (when (null a-perm)
      (setf a-perm default-a-perm))
    (when (null b-perm)
      (setf b-perm default-b-perm))
    (if (and (equal a-perm default-a-perm) (equal b-perm default-b-perm))
	(array-array-fun-noperm a b f r)
	(array-array-fun-perm a b f r :a-perm a-perm :b-perm b-perm))))

;; test array-array-mul and array-array-fun
(let ((a #2A((1 2 3) (4 5 6)))
      (b #2A((0 1) (2 3) (4 5)))
      (r (make-array '(2 2)))
      (ta (make-array '(3 2)))
      (tb (make-array '(2 3)))
      (ra (make-array '(2 3))))
  (array-transpose a ta)
  (array-transpose b tb)
  ;;(print (list a ta b tb))
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
  (array-array-fun ta a #'* ra :a-perm '(1 0))
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun a ta #'* ra :b-perm '(1 0))
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun ta ta #'* ra :a-perm '(1 0) :b-perm '(1 0))
  (assert (equalp ra #2A((1 4 9) (16 25 36)))))
(let ((a (make-array '(3 4 2))))
  (array-array-fun #1=#3A(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23))) #1# #'+ a :a-perm '(2 0 1) :b-perm '(2 0 1))
  ;;(prind a)
  (assert (equalp a #3A(((0 24) (2 26) (4 28) (6 30)) ((8 32) (10 34) (12 36) (14 38)) ((16 40) (18 42) (20 44) (22 46))))))

(defun array-fun (a f r &key (a-t nil))
  "Fill the array R by calling function F on each element of A."
  ;;TODO: implement a-t
  (assert (eq a-t nil))
  (let ((x (reduce #'* (array-dimensions a))))
    (loop for i below x do
	 (setf (row-major-aref r i) (funcall f (row-major-aref a i)))))
  nil)

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

(defun array-select-dimension (a dim selector)
  "Return a new array like array A but with index dimension DIM fixed at SELECTOR."
  ;; TODO: Generalize to all dimensions, and selector a vector with possible nil values (meaning give all values of this dimension)
  ;; TODO: rename this function to asref (for "array-slice-reference")
  ;; TODO: write a setf-expander for this
  (let* ((a-dimensions (array-dimensions a))
	 (before-dimensions (subseq a-dimensions 0 dim))
	 (after-dimensions (subseq a-dimensions (1+ dim)))
	 (r-dimensions (append before-dimensions after-dimensions))
	 (r (make-array r-dimensions :element-type (array-element-type a)))
	 (x (apply #'* before-dimensions))
	 (y (elt a-dimensions dim))
	 (z (apply #'* after-dimensions)))
    (dotimes (i x)
      (dotimes (k z)
	(let* ((a-index (+ (* i y z) (* selector z) k))
	       (r-index (+ (* i z) k)))
	  (setf (row-major-aref r r-index) (row-major-aref a a-index)))))
    r))

(defstruct rbm
  (w nil :type (simple-array float (* *)))
  (v-biases nil :type (simple-array float (*)))
  (h-biases nil :type (simple-array float (*))))

(defun new-rbm (n-visible n-hidden)
  "Return a new randomly initialized restricted boltzmann machine."
  (let ((w (make-array (list n-visible n-hidden) :element-type 'float))
	(v-biases (make-array n-visible :element-type 'float))
	(h-biases (make-array n-hidden :element-type 'float)))
    (declare (type (simple-array float) w))
    (loop for i below n-visible do
	 (loop for j below n-hidden do
	      (setf (aref w i j) (* (random-gaussian) .01))))
    (loop for i below n-visible do (setf (aref v-biases i) 0))
    (loop for j below n-hidden do (setf (aref h-biases j) 0))
    (make-rbm :w w :v-biases v-biases :h-biases h-biases)))

(defun new-rbm-1 ()
  (make-rbm :w #2A((0.40 0.50 0.60)
		   (0.41 0.51 0.61)
		   (0.42 0.52 0.62)
		   (0.43 0.53 0.63)
		   (0.44 0.54 0.64)
		   (0.45 0.55 0.65))
	    :v-biases #(0 0 0 0 0 0)
	    :h-biases #(0 0 0)))

(defun rbm-n-v (rbm)
  (array-dimension (rbm-w rbm) 0))

(defun rbm-n-h (rbm)
  (array-dimension (rbm-w rbm) 1))

(defun rbm-learn-cd1 (data rbm)
  ;;TODO: possibility to specify neuron types
  "Do one contrastive-divergence-1 step on a restricted boltzmann machine.
DATA is a two-dimensional array of input values: the first dimension represents the cases of the mini-batch, the second dimension represents visible neurons.
RBM is a restricted boltzmann machine as returned by new-rbm or rbm-learn-cd1."
  (let* ((w (rbm-w rbm))
	 (v-biases (rbm-v-biases rbm))
	 (h-biases (rbm-h-biases rbm))
	 (n-v (rbm-n-v rbm))
	 (n-h (rbm-n-h rbm))
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
    ;; positive phase
    (array-array-mul data w pos-h-probs)
    (array-array-fun pos-h-probs (array-repeat h-biases (list n-cases) nil) #'+ pos-h-probs)
    ;;(print (list "prod" pos-h-probs))
    (array-fun pos-h-probs (lambda (x) (sigmoid x)) pos-h-probs)
;;    (print (list "pos-h-probs" pos-h-probs))
    (array-array-mul data pos-h-probs pos-prods :a-t t)
    (setf pos-h-act (array-project pos-h-probs #'+ 0))
    (setf pos-v-act (array-project data #'+ 0))
;;    (print (list "pos-prods" pos-prods "pos-h-act" pos-h-act "pos-v-act" pos-v-act))
    (array-fun pos-h-probs (lambda (x) (if (> x (random 1.0)) 1 0)) pos-h-states)
    ;;(array-fun pos-h-probs #'identity pos-h-states)
;;    (print (list "pos-h-states" pos-h-states))
    ;; negative phase
    (array-array-mul pos-h-states w neg-data :b-t t)
    (array-array-fun neg-data (array-repeat v-biases (list n-cases) nil) #'+ neg-data)
    (array-fun neg-data (lambda (x) (sigmoid x)) neg-data)
;;    (print (list "neg-data" neg-data))
    (array-array-mul neg-data w neg-h-probs)
    (array-array-fun neg-h-probs (array-repeat h-biases (list n-cases) nil) #'+ neg-h-probs)
    (array-fun neg-h-probs (lambda (x) (sigmoid x)) neg-h-probs)
;;    (print (list "neg-h-probs" neg-h-probs))
    (array-array-mul neg-data neg-h-probs neg-prods :a-t t)
    (setf neg-h-act (array-project neg-h-probs #'+ 0))
    (setf neg-v-act (array-project neg-data #'+ 0))
;;    (print (list "neg-prods" neg-prods "neg-h-act" neg-h-act "neg-v-act" neg-v-act))
    ;; learning
    (array-array-fun pos-prods neg-prods #'- w-inc)
    (array-fun w-inc (lambda (x) (/ x n-cases))  w-inc)
    (array-array-fun pos-v-act neg-v-act (lambda (a b) (/ (- a b) n-cases)) v-biases-inc)
    (array-array-fun pos-h-act neg-h-act (lambda (a b) (/ (- a b) n-cases)) h-biases-inc)
;;    (print (list "w-inc" w-inc "v-biases-inc" v-biases-inc "h-biases-inc" h-biases-inc))
    (let ((err-array (make-array (list n-cases n-v) :element-type 'float))
	  (err nil))
      (array-array-fun data neg-data (lambda (a b) (expt (- a b) 2)) err-array)
      (setf err (aref (array-project (array-project err-array #'+) #'+)))
      (print (list "err" err))
      )
    (values w-inc v-biases-inc h-biases-inc)
    ))

(defparameter *data-imbalanced* #2A((1 1 0 0 0 0) (0 0 1 1 0 0) (0 0 0 0 1 1) (0 0 0 0 1 1)))
(defparameter *data* #2A((1 1 0 0 0 0) (0 0 1 1 0 0) (0 0 0 0 1 1)))
(defparameter *data900* (make-array '(3 900) :initial-element 0.0 :element-type 'float :initial-contents (list (nconc (loop for i below 300 collect 1) (loop for i below 600 collect 0)) (nconc (loop for i below 300 collect 0) (loop for i below 300 collect 1) (loop for i below 300 collect 0)) (nconc (loop for i below 600 collect 0) (loop for i below 300 collect 1)))))
(defparameter *data2* #2A((0 0) (0 1) (1 0) (1 1)))

(defun rbm-update (rbm w-inc v-biases-inc h-biases-inc learn-rate)
  (let* ((w (rbm-w rbm))
	 (v-biases (rbm-v-biases rbm))
	 (h-biases (rbm-h-biases rbm))
	 (w-new (make-array (array-dimensions w) :element-type (array-element-type w)))
	 (v-biases-new (make-array (array-dimensions v-biases) :element-type (array-element-type v-biases)))
	 (h-biases-new (make-array (array-dimensions h-biases) :element-type (array-element-type h-biases))))
    (flet ((update-learn (w-value w-inc-value)
	     (+ w-value (* w-inc-value learn-rate))))
      (array-array-fun w w-inc #'update-learn w-new)
      (array-array-fun v-biases v-biases-inc #'update-learn v-biases-new)
      (array-array-fun h-biases h-biases-inc #'update-learn h-biases-new)
      (make-rbm :w w-new :v-biases v-biases-new :h-biases h-biases-new))))

(defun rbm-learn (data rbm learn-rate momentum weight-cost max-iterations)
  (assert (> max-iterations 0))
  (labels ((rec (rbm w-inc v-biases-inc h-biases-inc iteration)
	     (print (list "iteration" iteration))
	     (multiple-value-bind (w-inc-1 v-biases-inc-1 h-biases-inc-1) (rbm-learn-cd1 data rbm)
	       (array-array-fun w-inc-1 (rbm-w rbm) (lambda (a b) (- a (* weight-cost b))) w-inc-1) ;add weight-cost
	       (array-array-fun w-inc w-inc-1 (lambda (a b) (+ (* a momentum) b)) w-inc)
	       (array-array-fun v-biases-inc v-biases-inc-1 (lambda (a b) (+ (* a momentum) b)) v-biases-inc)
	       (array-array-fun h-biases-inc h-biases-inc-1 (lambda (a b) (+ (* a momentum) b)) h-biases-inc)
	       (let ((new-rbm (rbm-update rbm w-inc v-biases-inc h-biases-inc learn-rate)))
		 (if (= iteration max-iterations)
		     new-rbm
		     (rec new-rbm w-inc v-biases-inc h-biases-inc (1+ iteration)))))))
    (let ((w-inc (make-array (array-dimensions (rbm-w rbm)) :element-type 'float))
	  (v-biases-inc (make-array (array-dimensions (rbm-v-biases rbm)) :element-type 'float))
	  (h-biases-inc (make-array (array-dimensions (rbm-h-biases rbm)) :element-type 'float)))
      (rec rbm w-inc v-biases-inc h-biases-inc 1))))

(defun rbm-h-from-v (data rbm)
  (let* ((w (rbm-w rbm))
	 (v-biases (rbm-v-biases rbm))
	 (h-biases (rbm-h-biases rbm))
	 (n-v (length v-biases))
	 (n-h (length h-biases))
	 (n-cases (array-dimension data 0))
	 (pos-h-probs (make-array (list n-cases n-h) :element-type 'float))
	 (pos-prods (make-array (list n-v n-h) :element-type 'float))
	 (pos-h-act nil)
	 (pos-v-act nil)
	 (pos-h-states (make-array (list n-cases n-h) :element-type 'float)))
    ;; positive phase
    (array-array-mul data w pos-h-probs)
    (array-array-fun pos-h-probs (array-repeat h-biases (list n-cases) nil) #'+ pos-h-probs)
    (array-fun pos-h-probs (lambda (x) (sigmoid x)) pos-h-probs)
    (array-array-mul data pos-h-probs pos-prods :a-t t)
    (setf pos-h-act (array-project pos-h-probs #'+ 0))
    (setf pos-v-act (array-project data #'+ 0))
;;    (array-fun pos-h-probs (lambda (x) (if (> x (random 1.0)) 1 0)) pos-h-states)
    (array-fun pos-h-probs #'identity pos-h-states)
    pos-h-states))

(defun rbm-v-from-h (pos-h-states rbm)
  (let* ((w (rbm-w rbm))
	 (n-v (rbm-n-v rbm))
	 (v-biases (rbm-v-biases rbm))
	 (n-cases (array-dimension pos-h-states 0))
	 (neg-data (make-array (list n-cases n-v) :element-type 'float)))
    ;; negative phase
    (array-array-mul pos-h-states w neg-data :b-t t)
    (array-array-fun neg-data (array-repeat v-biases (list n-cases) nil) #'+ neg-data)
    (array-fun neg-data (lambda (x) (sigmoid x)) neg-data)
    neg-data))

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
