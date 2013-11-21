;; Restricted Boltzmann Machine

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
		       (rec (cddr rest) (cons `((princ ,text) (prin1 ,value)) pairs)))
		     (let ((text (format nil "~A=" (car rest)))
			   (value (car rest)))
		       (rec (cdr rest) (cons `((princ ,text) (prin1 ,value)) pairs)))))))
    (let* ((progns (rec rest nil))
	   (progns-with-separator (join 'list progns '((princ " ")))))
      ;;(print progns-with-separator)
      `(progn ,@progns-with-separator (terpri)))))

;;(declaim (inline map-into-list)) this causes warnings: funcall with wrong parmeter number
(defun map-into-list (result-list function &rest lists)
  "Same as map-into, but for lists instead of sequences.
This function is faster than map-into, but slower than DO (which is possible when the number of LISTS is fixed and FUNCTION is known)."
  (declare (optimize (speed 3) (debug 0) (safety 3) (space 0) (compilation-speed 0))
	   (type list result-list lists))
  ;; TODO: write a macro-expansion for this function which uses DO, if the number of lists is fixed.
  (macrolet ((labels-rec-n (name n &body body)
	       (let ((list-vars (loop for i below n collect (gensym))))
		 `(labels ((,name (res ,@list-vars)
			     (if (null res) (return-from ,name result-list))
			     ,@(mapc (lambda (x) `(if (null ,x) (return-from ,name))) list-vars)
			     (setf (car res)
				   (funcall (the function function) ,@(mapcar (lambda (x) `(car ,x)) list-vars)))
			     (,name (cdr res) ,@(mapcar (lambda (x) `(cdr ,x)) list-vars))))
		    ,@body))))
    (labels ((rec (res lists)
	       (if (null res) (return-from rec result-list))
	       (loop for l in lists do
		    (if (null l) (return-from rec)))
	       (setf (car res)
		     (apply (the function function) (mapcar #'car lists)))
	       (do ((l lists (cdr l))) ((null l)) ;same as (mapcar #'cdr lists) but faster b/c in-place
		 (setf (car l) (cdar l)))
	       (rec (cdr res) lists)))
      (labels-rec-n
       rec0 0
       (labels-rec-n
	rec1 1
	(labels-rec-n
	 rec2 2
	 (labels-rec-n
	  rec3 3
	  (labels-rec-n
	   rec4 4
	   (case (length lists)
	     (0 (rec0 result-list))
	     (1 (rec1 result-list (first lists)))
	     (2 (rec2 result-list (first lists) (second lists)))
	     (3 (rec3 result-list (first lists) (second lists) (third lists)))
	     (4 (rec4 result-list (first lists) (second lists) (third lists) (fourth lists)))
	     (t (rec result-list (copy-list lists))))))))))))

(defun copy-array (array &key (array-element-type (array-element-type array)) (adjustable (adjustable-array-p array)))
  ;; (fill-pointer array) is only available for arrays of type vector, therefore I'm omitting it.
  (let* ((array-dimensions (array-dimensions array))
	 (res (make-array array-dimensions :element-type array-element-type :adjustable adjustable))
	 (max-index (apply #'* array-dimensions)))
    ;;(prind (array-element-type res) array-element-type adjustable)
    (loop for i below max-index do
	 (setf (row-major-aref res i) (coerce (row-major-aref array i) array-element-type)))
    res))

(defun random-gaussian-2 ()
  "Return two with mean 0 and standard deviation 1 normally distributed random variables."
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
  (flet ((xinit ()
	   (the single-float (- (* 2.0 (random 1.0)) 1))))
    (do* ((x1 (xinit) (xinit))
	  (x2 (xinit) (xinit))
	  (w (+ (* x1 x1) (* x2 x2)) (+ (* x1 x1) (* x2 x2))))
 	 ((< w 1.0)
	  (let* ((wlog (if (<= w 0) -1000.0 (the single-float (log w))))
		 (v (the single-float (sqrt (/ (* -2.0 wlog) w)))))
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
    (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
    (if (null next-random-gaussian)
	(multiple-value-bind (r1 r2) (random-gaussian-2)
	  (setf next-random-gaussian r2)
	  (the single-float r1))
	(let ((r1 next-random-gaussian))
	  (setf next-random-gaussian nil)
	  (the single-float r1)))))

(defun sigmoid (x)
  (cond
    ((>= x -88) (/ 1 (1+ (exp (- x)))))
    (t 0.0)))

(defun permute (sequence permutation &optional (result-type 'list))
  (declare (type sequence sequence) (type list permutation) (type (member list vector) result-type))
  "Permute sequence SEQUENCE with permutation list PERMUTATION and return the resulting list.
PERMUTATION contains, for each element at position x, the dimension number that is mapped to dimension x.
PERMUTATION must contain all numbers between 0 and (1- (length perm)), and each number only once."
  (ecase result-type
    (list (loop for p in permutation collect
	       (elt sequence p)))
    (vector (let ((v (make-array (length sequence))))
	      (loop for p in permutation for i from 0 do
		   (setf (elt v i) (elt sequence p)))
	      v))))

(defun reverse-permute (sequence permutation &optional (result-type 'list))
  (declare (type sequence sequence) (type list permutation) (type (member list vector) result-type))
  "Reverse-permute sequence SEQUENCE with permutation list PERMUTATION and return the resulting list.
Reverse-permute means, that for each element e at position x in PERMUTATION, the dimension x of SEQUENCE is mapped to position e in the result.
PERMUTATION must contain all numbers between 0 and (1- (length PERMUTATION)), and each number only once."
  (let ((res (ecase result-type
	       (list (loop for i below (length permutation) collect -1))
	       (vector (make-array (length permutation))))))
    (loop for e in permutation for x below (length permutation) do
	 (setf (elt res e) (elt sequence x)))
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
(let* (;;          0 1 2 3 4 5
       (a        '(2 1 3 0 5 4))
       (a-vector #(2 1 3 0 5 4))
       (a-1 (inverse-permutation a))
       ;;   0 1 2 3 4 5
       (b '(2 4 3 5 1 0))
       (b-1 (inverse-permutation b))
       (id (loop for i below 6 collect i))
       (id-vector (make-array 6 :initial-contents (loop for i below 6 collect i))))
  (assert (equal (permute id a) a))
  (assert (equalp (permute id a 'vector) a-vector))
  (assert (equal (reverse-permute a a) id))
  (assert (equalp (reverse-permute a a 'vector) id-vector))
  (assert (equal (permutation-product a a-1) id))
  (assert (equal (permutation-product a-1 a) id))
  (assert (equal (permutation-product a b) '(3 4 5 2 0 1)))
  (assert (equal (permutation-product (permutation-product a b) b-1) a)))

(defun array-slice-valid (slice array-dimensions)
  "Check if the list SLICE is a valid array slice of array ARRAY.
Example: (array-slice-valid '((0 2) nil (0 0 3 4)) '(2 3 4)) is invalid because of the nil."
  (when (not (= (length slice) (length array-dimensions)))
    (return-from array-slice-valid (values nil 'dimensions)))
  (mapc (lambda (slice dim)
	  (when (null slice)
	    (return-from array-slice-valid (values nil 'null)))
	  (let* ((slice-len (length slice)))
	    (when (oddp slice-len)
	      (return-from array-slice-valid (values nil 'oddp))))
	  (loop for pair on slice by #'cddr do
	       (when (not (<= 0 (car pair) (cadr pair) dim))
		 (return-from array-slice-valid (values nil 'order)))))
	slice array-dimensions)
  (values t t))

(defun default-array-slice (dim)
  "Return the whole array slice for an array of dimensions DIM."
  (loop for d in dim collect
       (list 0 d)))

(defun array-dim-slice-count (dim-slice)
  "Count the number of elements addressed by the array dim-slice DIM-SLICE."
  ;; a dim-slice is an element of a slice, e.g. '(0 2) and (0 4) are dim-slices of ((0 2) (0 4)).
  (loop for int on dim-slice by #'cddr
     for start = (car int) for end = (cadr int)
     sum (- end start)))

(defun array-slices-compatible (a-slice b-slice &key a-dim b-dim)
  "Check whether the array-slices A-SLICE and B-SLICE refer to equal number of elements.
A-DIM and B-DIM are the dimensions along which the check is performed.
If they are nil, then all dimensions are checked."
  (assert (= (length a-dim) (length b-dim)))
  (when (/= (length a-slice) (length b-slice))
    (return-from array-slices-compatible nil))
  (labels ((test-dim (a-sl b-sl)
	     (= (array-dim-slice-count a-sl) (array-dim-slice-count b-sl))))
    (when (null a-dim)
      (setf a-dim (loop for i below (length a-slice) collect i)))
    (when (null b-dim)
      (setf b-dim (loop for i below (length b-slice) collect i)))
    (mapc (lambda (a-d b-d)
	    (when (not (test-dim (elt a-slice a-d) (elt b-slice b-d)))
	      (return-from array-slices-compatible nil)))
	  a-dim b-dim)
    t))

(assert (array-slices-compatible '((0 1) nil (0 2 3 4)) '((1 2) nil (0 3))))
(assert (not (array-slices-compatible '((0 1) nil (0 2 3 4)) '((1 2) nil (0 4)))))

(defun array-dim-slices-merge (&rest dim-slices)
  "Merge the dim-slices DIM-SLICES to one dim-slice."
  ;; TODO: use merge instead of sort
  (let* ((merged (apply #'append dim-slices))
	 (grouped (do ((m merged (cddr m)) (res nil)) ((null m) res)
		    (setf res (cons (list (car m) (cadr m)) res))))
	 (sorted (sort grouped (lambda (a b) (let ((s1 (car a)) (s2 (car b)) (e1 (cadr a)) (e2 (cadr b)))
					       (if (= s1 s2) (< e1 e2) (< s1 s2)))))))
    ;;(prind sorted)
    (do* ((l sorted) (start (caar l) (caar l)) (end (cadar l) (cadar l)) (res nil))
	 ((null l) (nreverse res))
      ;;(prind l start end)
      (let ((l2 (do* ((l2 l (cdr l2)) (start2 (caar l2) (caar l2)) (end2 (cadar l2) (cadar l2)))
		     ((> start2 end) l2)
		  ;;(prind l2 start2 end2)
		  (setf end (max end end2))
		  (when (null (cdr l2)) (return nil)))))
	(setf res (nconc (list end start) res))
	;;(prind start end l l2)
	(setf l l2)))))

(assert (equal (array-dim-slices-merge '(0 5) '(3 4 3 10 18 25) '(3 7 10 15) '(30 30 30 31))
	       '(0 15 18 25 30 31)))

(deftype index () `(integer 0 ,array-dimension-limit))

(defun array-dim-slice-iterate (dim-slice-list inc-list off-list function)
  "Iterate over all the dim-slices DIM-SLICE-LIST in parallel.
Calls FUNCTION for each iteration with the list of the slice value multiplied by the respective factor in list INC-LIST, and an offset added from the respective value in OFF-LIST.
The iteration ends when the first DIM-SLICE is fully iterated through.
Returns NIL.
Example: (array-dim-slice-iterate '((0 1 3 4) (6 8)) '(10 1) '(5 10) (lambda (i j) (print (list i j)))) prints (5 16) (35 17)"
  ;; This could also be implemented as a macro, but that would not give any speed or expressiveness advantage.
  ;;Iterate over the dim-slice-list using the variables.
  ;;Example: (array-dim-slice-iterate ((i '(0 1 3 4)) (j '(6 8))) (print (list i j)))
  ;;VAR-AND-DIM-SLICE-LIST is a list of (VAR DIM-SLICE) lists."
  (declare (optimize (speed 3) (debug 0) (safety 3) (space 0) (compilation-speed 0))
	   (type list dim-slice-list inc-list))
  (when (null dim-slice-list)
    (return-from array-dim-slice-iterate nil))
  (let* ((poss (mapcar #'car dim-slice-list)) ;positions
	 (ends (mapcar #'cadr dim-slice-list)) ;next ends
	 (ints (mapcar #'cddr dim-slice-list)) ;next intervals
	 (poss-mul (mapcar (lambda (x inc off)
			     (declare (type index x inc off))
			     (the index (+ off (the index (* x inc)))))
			   poss inc-list off-list))
	 )
    (tagbody
     start
       (let* ((lengths (mapcar (lambda (x y) ;for some reason mapcar is faster than map-into-list
				 (declare (type index x y))
				 (the index (- x y)))
			       ends poss))
	      (min-length (apply #'min lengths)))
	 (declare (type index min-length))
	 (dotimes (i min-length)
	   (declare (type index i min-length))
	   (apply (the function function) poss-mul)
	   (do ((p poss-mul (cdr p)) (i inc-list (cdr i))) ((null p))
	     (incf (the index (car p)) (the index (car i)))))
	 (map-into-list poss (lambda (p)
			       (declare (type index p))
			       (the index (+ p min-length)))
			poss)
	 ;; assign new poss/ends
	 (loop for p on poss for e on ends for i on ints for p-m on poss-mul for inc of-type index in inc-list for off of-type index in off-list
	    ;; e.g. p=(2 1) e=(5 1) i=((3 4) nil)
	    do (let ((pos (car p)) (end (car e)))
		 (declare (type index pos end))
		 (when (>= pos end)
		   ;;(prind p e i p-m inc)
		   (when (null (car i))
		     (go end))
		   (setf (car p) (caar i))
		   (setf pos (car p))
		   (setf (car e) (cadar i))
		   (setf (car i) (cddr (car i)))
		   (setf (car p-m) (the index (+ off (the index (* pos inc)))))))))
       (go start)
     end))
  nil)

(let (a)
  (array-dim-slice-iterate '((0 1 3 4) (6 8)) '(1 2) '(5 10) (lambda (x y) (push (list x y) a)))
  ;;(print a)
  (assert (equal a '((8 24) (5 22)))))

(defun array-indices-to-dim-slice (indices)
  "Convert a list of indices to a dim-slice.
Example (array-indices-to-dim-slice '(7 5 8 9)) == '(7 8 5 6 8 10)."
  (let ((dim-slice nil))
    (loop for i in indices do
	 (if (null dim-slice)
	     (progn
	       (push i dim-slice)
	       (push (1+ i) dim-slice))
	     (if (= (car dim-slice) i)
		 (setf (car dim-slice) (1+ i))
		 (progn
		   (push i dim-slice)
		   (push (1+ i) dim-slice)))))
    (nreverse dim-slice)))

(defun array-transpose (a r)
  "Put the transpose of 2-dimensional array A into array R."
  (assert (= 2 (array-rank a) (array-rank r)))
  (let ((arows (array-dimension a 0))
	(acols (array-dimension a 1))
	(rrows (array-dimension r 0))
	(rcols (array-dimension r 1)))
    (assert (and (= arows rcols) (= acols rrows)))
    (loop for i below rrows do
	 (loop for j below rcols do
	      (setf (aref r i j) (aref a j i)))))
  nil)

(defun array-array-mul (a b r &key (a-t nil) (b-t nil) a-slice b-slice)
  "Multiply two-dimensional arrays A and B and put the result in two-dimensional array R.
Arrays A and/or B are transposed before multiplication if A-T and/or B-t is T, respectively.
The array slices A-SLICE and B-SLICE are applied after transposition but before multiplication.
The array slice for R is computed from A-SLICE and B-SLICE."
  ;;TODO: speed this function up by specializing on A-T and B-T.
  ;;TODO: generalize for dimensions>2 (is there something like matrix multiplication for tensors?)
  (assert (= 2 (array-rank a) (array-rank b) (array-rank r)))
  (when (null a-slice)
    (setf a-slice (default-array-slice (permute (array-dimensions a) (if a-t '(1 0) '(0 1))))))
  (when (null b-slice)
    (setf b-slice (default-array-slice (permute (array-dimensions b) (if b-t '(1 0) '(0 1))))))
  (let ((ai-slice (elt a-slice 0)) ;slicing is after transposition, so the dimensions don't depend on a-t.
	(aj-slice (elt a-slice 1))
	(bi-slice (elt b-slice 0))
	(bj-slice (elt b-slice 1)))
    ;;(prind ai-slice aj-slice bi-slice bj-slice)
    (assert (= (array-dim-slice-count aj-slice) (array-dim-slice-count bi-slice)))
    (assert (array-slice-valid a-slice (permute (array-dimensions a) (if a-t '(1 0) '(0 1)))))
    (assert (array-slice-valid b-slice (permute (array-dimensions b) (if b-t '(1 0) '(0 1)))))
    (let ((r-slice (list (first a-slice) (second b-slice))))
      (assert (array-slice-valid r-slice (array-dimensions r))))
    (macrolet ((do-dim-slice (variable dim-slice &body body)
		 (let ((int (gensym)) (start (gensym)) (end (gensym)))
		   `(loop for ,int on ,dim-slice by #'cddr
		       for ,start = (car ,int) for ,end = (cadr ,int)
		       do (loop for ,variable from ,start below ,end
			     do ,@body)))))
      (do-dim-slice
	  i ai-slice
	  (do-dim-slice
	      j bj-slice
	      (setf (aref r i j)
		    (let ((sum 0))
		      (array-dim-slice-iterate
		       (list aj-slice bi-slice) '(1 1) '(0 0)
		       (if a-t
			   (if b-t
			       (lambda (aj bi)
				 (incf sum (* (aref a aj i) (aref b j bi))))
			       (lambda (aj bi)
				 (incf sum (* (aref a aj i) (aref b bi j)))))
			   (if b-t
			       (lambda (aj bi)
				 (incf sum (* (aref a i aj) (aref b j bi))))
			       (lambda (aj bi)
				 (incf sum (* (aref a i aj) (aref b bi j)))))))
		      sum))))))
  nil)

(defun default-array-permutation (a)
  (loop for i below (array-rank a) collect i))

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
	     (declare (type index i))
	     (if (null d)
		 (if (null s)
		     (the index i)
		     (error "DIMS and SUBSCRIPTS don't have the same length: ~A ~A." dims subscripts))
		 (if (null s)
		     (error "DIMS and SUBSCRIPTS don't have the same length: ~A ~A." dims subscripts)
		     (let ((dim (car d))
			   (sub (car s)))
		       (declare (type index sub dim))
		       (if (or (< sub 0) (>= sub dim))
			   (error "Subscript (~A) must be non-negative and smaller than dimension (~A)." sub dim)
			   (rec (cdr d) (cdr s) (+ (the index (* dim i)) sub)))))))) ;doesn't optimize in sbcl
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

(defun array-row-major-multipliers (dimensions)
  "Return, for each dimension, the multiplier in row-major indices necessary to increase this dimension by one subscript."
  (loop for i below (length dimensions) collect
       (reduce #'* (nthcdr (1+ i) dimensions))))

(defun array-walk (dim perm slice function)
  "Call function FUNCTION with a row-major-index for all elements of an array specified by DIM, PERM, and SLICE.
DIM are the array-dimensions of the array.
PERM is the array's dimensions permutation, which is applied before iterating through all the dimensions of the array.
SLICE is the array slice which is applied before iterating but after permuting."
  (let* ((n-dims (length dim))
	 (dim-mul (array-row-major-multipliers dim))
	 (dim-perm (permute dim perm))
	 (dim-mul-perm (permute dim-mul perm)))
    (assert (array-slice-valid slice dim-perm))
    (labels ((rec (n-dim i-0 dim dim-mul slice)
	       (if (< n-dim n-dims)
		   (let ((i-mul (car dim-mul))
			 (d-slice (car slice)))
		     (do* ((interval d-slice (cddr interval))
			   (start (car interval) (car interval))
			   (end (cadr interval) (cadr interval)))
			  ((null interval))
		       (loop
			  for d from start below end
			  for i from (+ i-0 (* start i-mul)) by i-mul do
			    (rec (1+ n-dim) i (cdr dim) (cdr dim-mul) (cdr slice)))))
		   (funcall function i-0))))
      (rec 0 0 dim-perm dim-mul-perm slice))))

(defun array-slices-iterate (n-dim i0s i0s-muls slices function)
  "Iterate over all the slices in parallel.
Returns NIL.
N-DIM is the current dimension number (counting down), and 1 is the last.
I0S is the list of start indices.
I0S-MULS is the list of list of i0 (index) multipliers.
SLICES is the list of array slices."
  (declare (optimize (speed 3) (debug 0) (safety 3) (space 0) (compilation-speed 0))
	   (type index n-dim)
	   (type cons i0s i0s-muls slices))
  ;;(prind n-dim i0s i0s-muls slices)
  (let ((i-muls (mapcar #'car i0s-muls))
	(dim-slices (mapcar #'car slices)))
    (if (> n-dim 0)
	(array-dim-slice-iterate
	 dim-slices i-muls i0s
	 (lambda (&rest d-indices)
	   (array-slices-iterate (1- n-dim) d-indices (mapcar #'cdr i0s-muls) (mapcar #'cdr slices) function)))
	(array-dim-slice-iterate dim-slices i-muls i0s function))))

(defun arrays-walk (arrays-dim arrays-perm arrays-slice function)
  "Call function FUNCTION with row-major-indices of arrays.
ARRAYS-DIM is the list of the arrays' array-dimensions.
ARRAYS-PERM is the list of the arrays' dimension permutations, which is applied before iterating through all the dimensions of the arrays.
The permuted array dimensions must all be equal.
ARRAYS-SLICE is the list of array slices which is applied before iterating but after permuting (one different slice for every array is possible, but they must be compatible)."
  (when (not (null arrays-dim))
    (let ((n-arrays (length arrays-dim))
	  (rank (length (car arrays-dim))))
      (assert (= n-arrays (length arrays-perm) (length arrays-slice))) ;equal number of arrays
      (mapc (lambda (dim perm slice)
	      (assert (= rank (length dim) (length perm) (length slice) (length slice)) (dim perm (car arrays-dim))))
	    arrays-dim arrays-perm arrays-slice) ;equal number of dimensions
      (let* ((dims-mul (mapcar (lambda (dim) (array-row-major-multipliers dim)) arrays-dim))
	     (array0-dim-perm (permute (car arrays-dim) (car arrays-perm)))
	     (dims-mul-perm (mapcar (lambda (dim-mul perm) (permute dim-mul perm))
				    dims-mul arrays-perm)))
	;;(prind slice (car  arrays-dim))
	(loop for dim in (cdr arrays-dim) for perm in (cdr arrays-perm) for slice in arrays-slice do
	     ;; Note that the number of dimensions doesn't have to be equal, only the sliced ranges must be compatible.
	     (assert (array-slice-valid slice array0-dim-perm)))
	(array-slices-iterate (1- rank) (loop for i below n-arrays collect 0) dims-mul-perm arrays-slice function)))))

;; test arrays-walk
(let* ((a #3A(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23))))
       (a-perm '(2 0 1))
       (a-dim (array-dimensions a))
       (a-dim-perm (permute a-dim a-perm))
       (a-slice (default-array-slice a-dim-perm))
       (a2-slice (permute '((0 2) (1 2) (0 1 3 4)) a-perm))
       (b-dim a-dim-perm)
       (b (make-array b-dim))
       (b-perm '(0 1 2))
       (r1 nil)
       (r2 nil))
  (array-walk a-dim a-perm a-slice (lambda (x) (push x r1)))
  (arrays-walk (list a-dim) (list a-perm) (list a-slice) (lambda (x) (push x r2)))
  (assert (equal r1 r2) nil "~A ~A" r1 r2) ;array-walk and arrays-walk walk in the same order
  (arrays-walk (list a-dim b-dim) (list a-perm b-perm) (list a-slice a-slice)
	       (lambda (x y) (setf (row-major-aref b y) (row-major-aref a x))))
  (assert (equalp b (array-permute a a-perm))) ;permutation works the same way as array-permute does
  (setq r1 nil r2 nil)
  (array-walk a-dim a-perm a2-slice (lambda (x) (push x r1)))
  (arrays-walk (list a-dim) (list a-perm) (list a2-slice) (lambda (x) (push x r2)))
  (assert (equal r1 r2) nil "~A ~A" r1 r2) ;array-walk and arrays-walk walk the same order with slice
  )

(defun array-array-fun-perm-slice (a b f r a-perm b-perm r-perm a-slice b-slice r-slice)
  (declare (type (simple-array single-float *) a b r)) ;important float declaration
  (arrays-walk (list (array-dimensions a) (array-dimensions b) (array-dimensions r))
	       (list a-perm b-perm r-perm)
	       (list a-slice b-slice r-slice)
	       (lambda (a-i b-i r-i)
		 (declare (type index a-i b-i r-i)
			  (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
		 (setf (row-major-aref r r-i)
		       (funcall (the function f)
				(row-major-aref a a-i)
				(row-major-aref b b-i)))))
  nil)

(defun array-array-fun-noperm (a b f r)
  (declare (type (simple-array single-float *) a b r)
	   (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
  (let* ((a-dims (array-dimensions a))
	 (b-dims (array-dimensions b))
	 (r-dims (array-dimensions r))
	 (size (reduce #'* a-dims)))
    (declare (type index size))
    (assert (and (equal r-dims a-dims) (equal r-dims b-dims)))
      (loop for i below size do
	   (setf (row-major-aref r i) (funcall (the function f)
					       (row-major-aref a i)
					       (row-major-aref b i)))))
  nil)

(defun array-array-fun-perm (a b f r a-perm b-perm)
  "Return the result array R of applying each element in arrays A and B to the function F.
A-PERM and B-PERM are indexing permutations of matrix A and B, i.e. the indices used to address elements of A and B are permuted with the function PERMUTE before calling function F."
  (declare (type cons a-perm b-perm)
	   (type (simple-array single-float *) a b r)
	   (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
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
      (declare (type index lowest-r-dim butlowest-r-size perm-a-inc perm-b-inc))
      ;;(prind r-dims a-dims b-dims a-perm b-perm perm-a-inc perm-b-inc)
      (loop for i below butlowest-r-size do
	   (let* ((r-subscripts (row-major-index-to-subscripts r-dims r-index))
		  ;;(debug1 (prind r-dims r-index r-subscripts))
		  (a-subscripts (permute r-subscripts a-perm))
		  (b-subscripts (permute r-subscripts b-perm))
		  ;;(debug (progn (prind r-subscripts a-subscripts b-subscripts)))
		  (a-index (subscripts-to-row-major-index a-dims a-subscripts))	
		  (b-index (subscripts-to-row-major-index b-dims b-subscripts)))
	     (declare (type index a-index b-index))
	     (loop for j below lowest-r-dim do
		  ;;(prind r-index a-index b-index :r-s (row-major-index-to-subscripts r-dims r-index) :a-s (row-major-index-to-subscripts a-dims a-index) :b-s (row-major-index-to-subscripts b-dims b-index))
		  (setf (row-major-aref r r-index)
			(funcall (the function f)
				 (row-major-aref a a-index)
				 (row-major-aref b b-index)))
		  (incf r-index)
		  (incf a-index perm-a-inc)
		  (incf b-index perm-b-inc))))))
  nil)

(defun array-speed-test (&optional (iterations 10))
  (let ((a (make-array '(500 10) :element-type 'single-float :initial-element 1.0))
	(r (make-array '(500 10) :element-type 'single-float :initial-element 0.0)))
    (time (loop for i below iterations do
	       (array-array-fun-noperm a a #'+ r)))
    (time (loop for i below iterations do
	       (array-array-fun-perm a a #'+ r '(0 1) '(0 1))))
    (time (loop for i below iterations do
	       (array-array-fun-perm-slice a a #'+ r '(0 1) '(0 1) '(0 1) '((0 500) (0 10)) '((0 500) (0 10)) '((0 500) (0 10)))))))

(defun array-array-fun (a b f r &key a-perm b-perm r-perm a-slice b-slice r-slice)
  "Return the result array R of applying each element in arrays A and B to the function F.
A-PERM and B-PERM are indexing permutations of matrix A and B, i.e. the indices used to address elements of A and B are permuted with the function PERMUTE before calling function F."
  ;; TODO: generalize this function to allow computing with arbitrarily many arrays (f then takes arbitrarily many parameters)
  (let* ((default-a-perm (default-array-permutation a))
	 (default-b-perm (default-array-permutation b))
	 (default-r-perm (default-array-permutation r))
	 (default-a-slice (permute (default-array-slice (array-dimensions a)) a-perm))
	 (default-b-slice (permute (default-array-slice (array-dimensions b)) b-perm))
	 (default-r-slice (permute (default-array-slice (array-dimensions r)) r-perm)))
    (when (null a-perm)
      (setf a-perm default-a-perm))
    (when (null b-perm)
      (setf b-perm default-b-perm))
    (when (null r-perm)
      (setf r-perm default-r-perm))
    (when (null a-slice)
      (setf a-slice default-a-slice))
    (when (null b-slice)
      (setf b-slice default-b-slice))
    (when (null r-slice)
      (setf r-slice default-r-slice))
    (if (and (equal a-perm default-a-perm) (equal b-perm default-b-perm))
	(if (and (equal a-slice default-a-slice) (equal b-slice default-b-slice))
	    (array-array-fun-noperm a b f r)
	    (array-array-fun-perm-slice a b f r a-perm b-perm r-perm a-slice b-slice r-slice))
	(if (and (equal a-slice default-a-slice) (equal b-slice default-b-slice))
	    (array-array-fun-perm a b f r a-perm b-perm)
	    (array-array-fun-perm-slice a b f r a-perm b-perm r-perm a-slice b-slice r-slice)))))

;; test array-array-mul and array-array-fun
(let ((a (make-array '(2 3) :element-type 'single-float :initial-contents #(#(1.0 2.0 3.0) #(4.0 5.0 6.0))))
      (b #2A((0.0 1.0) (2.0 3.0) (4.0 5.0)))
      (r (make-array '(2 2) :element-type 'single-float :initial-element 0.0))
      (ta (make-array '(3 2) :element-type 'single-float :initial-element 0.0))
      (tb (make-array '(2 3) :element-type 'single-float :initial-element 0.0))
      (ra (make-array '(2 3) :element-type 'single-float :initial-element 0.0)))
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
  (array-array-mul a b r :a-slice '((0 2) (0 1 2 3)) :b-slice '((0 1 2 3) (0 2)))
  (assert (equalp r #2A((12 16) (24 34))))
  (array-array-fun a a #'* ra)
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun ta a #'* ra :a-perm '(1 0))
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun a ta #'* ra :b-perm '(1 0))
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  (array-array-fun ta ta #'* ra :a-perm '(1 0) :b-perm '(1 0))
  (assert (equalp ra #2A((1 4 9) (16 25 36))))
  ;; some tests for :a-slice, :b-slice and :r-slice are missing here
  )
(let ((init (make-array '(2 3 4) :element-type 'single-float :initial-contents '(((0.0 1.0 2.0 3.0) (4.0 5.0 6.0 7.0) (8.0 9.0 10.0 11.0)) ((12.0 13.0 14.0 15.0) (16.0 17.0 18.0 19.0) (20.0 21.0 22.0 23.0)))))
      (a (make-array '(3 4 2) :element-type 'single-float)))
  (array-array-fun init init #'+ a :a-perm '(2 0 1) :b-perm '(2 0 1) :r-perm '(2 0 1))
  (assert (equalp a #3A(((0 24) (2 26) (4 28) (6 30)) ((8 32) (10 34) (12 36) (14 38)) ((16 40) (18 42) (20 44) (22 46)))))
  (setf a (make-array '(2 3 4) :element-type 'single-float :initial-element 0.0))
  (array-array-fun init init #'+ a :a-perm '(2 0 1) :b-perm '(2 0 1) :r-perm '(2 0 1) :a-slice #3='((0 4) (0 2) (1 2)) :b-slice #3# :r-slice #3#)
  (assert (equalp a #3A(((0 0 0 0) (8 10 12 14) (0 0 0 0)) ((0 0 0 0) (32 34 36 38) (0 0 0 0))))))

(defun array-fun-noperm (a f r)
  "Fill the array R by calling function F on each element of A."
  (declare (optimize (speed 3) (debug 0) (safety 3) (space 0) (compilation-speed 0))
	   (type (simple-array single-float) a r))
  (let ((x (reduce #'* (array-dimensions a))))
    (declare (type index x))
    (loop for i below x do
	 (setf (row-major-aref r i) (funcall (the function f) (row-major-aref a i)))))
  nil)

(defun array-fun-perm-slice (a f r a-perm r-perm a-slice r-slice)
  (declare (optimize (speed 3) (debug 0) (safety 3) (space 0) (compilation-speed 0))
	   (type (simple-array single-float *) a r)) ;important float declaration
  (arrays-walk (list (array-dimensions a) (array-dimensions r))
	       (list a-perm r-perm)
	       (list a-slice r-slice)
	       (lambda (a-i r-i)
		 (declare (type index a-i r-i)
			  (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
		 (setf (row-major-aref r r-i)
		       (funcall (the function f)
				(row-major-aref a a-i)))))
  nil)

(defun array-fun (a f r &key (a-perm (default-array-permutation a)) (r-perm (default-array-permutation r)) (a-slice (permute (default-array-slice (array-dimensions a)) a-perm)) (r-slice (permute (default-array-slice (array-dimensions r)) r-perm)))
  (let ((default-a-perm (default-array-permutation a))
	(default-r-perm (default-array-permutation r))
	(default-a-slice (permute (default-array-slice (array-dimensions a)) a-perm))
	(default-r-slice (permute (default-array-slice (array-dimensions r)) r-perm)))
    (if (and (equal a-perm default-a-perm) (equal r-perm default-r-perm))
	(if (and (equal a-slice default-a-slice) (equal r-slice default-r-slice))
	    (array-fun-noperm a f r)
	    (array-fun-perm-slice a f r a-perm r-perm a-slice r-slice))
	(if (and (equal a-slice default-a-slice) (equal r-slice default-r-slice))
	    (array-fun-perm-slice a f r a-perm r-perm a-slice r-slice)
	    (array-fun-perm-slice a f r a-perm r-perm a-slice r-slice)))))

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

(defun array-project-noperm (a f &key (dim 0))
  "Project the array A along axis DIM.
This produces a new array, where DIM is omitted.
For all elements of the new array, call function F with pairwise items (like reduce) along the DIM axis and store the result in the element.
Return the new array."
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0))
	   (type (simple-array single-float) a))
  (let* ((a-dimensions (array-dimensions a))
	 (before-dimensions (subseq a-dimensions 0 dim))
	 (after-dimensions (subseq a-dimensions (1+ dim)))
	 (r-dimensions (append before-dimensions after-dimensions))
	 (r (make-array r-dimensions :element-type (array-element-type a)))
	 (x (apply #'* before-dimensions))
	 (y (elt a-dimensions dim))
	 (z (apply #'* after-dimensions))
	 (index-r 0))
    (declare (type index x y z index-r))
    (dotimes (i x)
      (declare (type index i))
      (dotimes (k z)
	(declare (type index k))
	(let* ((index (+ (the index (* (the index (* i y)) z)) k))
	       (res (row-major-aref a index)))
	  (declare (type index index))
	  (loop for j from 1 below y do
	       (setf index (+ index z))
	       (setf res (funcall (the function f) res (row-major-aref a index))))
	  (setf (row-major-aref r index-r) res))
	(setf index-r (1+ index-r))))
    r))

(defun array-project-perm-slice (a f &key (a-perm (default-array-permutation a)) (a-slice (permute (default-array-slice (array-dimensions a)) a-perm)) (dim 0))
  "Project the array A along a dimension.
This produces a new array with one dimension less than A has.
Before producing the new array, the array dimension permutation A-PERM is applied to A and, after that, the array slice A-SLICE.
The new array is like A except that after the modifications by A-PERM dimension DIM is omitted.
For all elements of the new array, call function F with pairwise items (like reduce) along the DIM axis and store the result in the element.
Return the new array."
  ;; The idea of this function is to permute the dimensions of A so that the dimension DIM to be omitted is coming last, and when walking the permuted array to use the consecutive elements of the last dimension in calling F.
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0))
	   (type (simple-array single-float) a))
  (flet ((perm-last (perm dim)
	   "Modify PERM so that dimension DIM is mapped last."
	   (declare (type list perm))
	   ;; example: original perm=(2 0 1) and dim=1, i.e. dimension 0 is to be put last, so the result is (2 1 0).
	   (append (subseq perm 0 dim) (subseq perm (1+ dim)) (list (elt perm dim))))
	 (inc-dim-slice-by-1 (slice)
	   (let* ((start1 (car slice))
		  (end1 (cadr slice))
		  (rest (cddr slice))
		  (new-start1 (1+ start1)))
	     (declare (type index start1 end1 new-start1))
	     (if (>= new-start1 end1)
		 rest
		 (cons new-start1 (cons end1 rest))))))
    (let* ((a-rank (array-rank a))
	   (a-dim (array-dimensions a))
	   (new-perm (perm-last a-perm dim))
	   (a-dim-perm (permute a-dim new-perm))
	   (r-dim (butlast a-dim-perm))
	   (r (make-array r-dim :element-type (array-element-type a)))
	   (r-dim-mul (array-row-major-multipliers r-dim))
	   (slice-perm (permute (reverse-permute a-slice a-perm) new-perm))
	   (slice-perm-butlast (butlast slice-perm))
	   (r-slice slice-perm-butlast)
	   (last-dim-slice (car (last slice-perm)))
	   (last-dim-slice-inc-by-1 (inc-dim-slice-by-1 last-dim-slice))
	   (last-dim-slice-orig-start (car last-dim-slice))
	   (a-dim-mul (array-row-major-multipliers a-dim))
	   (a-dim-mul-perm (permute a-dim-mul new-perm))
	   (a-last-dim-mul (car (last a-dim-mul-perm))))
      (declare (type index a-last-dim-mul last-dim-slice-orig-start))
      ;;(prind r-dim r a-rank)
      ;;(prind r-dim-mul a-dim-mul-perm slice-perm)
      (when (= a-rank 1)
	(setf a-rank 2)
	(setf r-dim-mul (list 0))
	(setf slice-perm-butlast slice-perm)
	(setf r-slice '((0 1))))
      (array-slices-iterate
       (- a-rank 2) (list 0 0) (list r-dim-mul a-dim-mul-perm) (list r-slice slice-perm-butlast)
       (lambda (r-index a-index)
	 (declare (type index r-index a-index))
	 (let* ((a-last-dim-start-index (the index (+ a-index (the index (* a-last-dim-mul last-dim-slice-orig-start)))))
		(res (row-major-aref a a-last-dim-start-index)))
	   (array-dim-slice-iterate (list last-dim-slice-inc-by-1) (list a-last-dim-mul) (list a-index)
				    (lambda (a-index)
				      (setf res (funcall (the function f) res (row-major-aref a a-index)))))
	   (setf (row-major-aref r r-index) res))))
      r)))

(let* ((a (make-array '(2 3 4) :element-type 'single-float :initial-contents '(((0.0 1.0 2.0 3.0) (4.0 5.0 6.0 7.0) (8.0 9.0 10.0 11.0)) ((12.0 13.0 14.0 15.0) (16.0 17.0 18.0 19.0) (20.0 21.0 22.0 23.0)))))
       (b (make-array '(6) :element-type 'single-float :initial-contents '(0.0 1.0 2.0 3.0 4.0 5.0))))
  (assert (equalp (array-project-perm-slice b #'+) #0A15))
  (assert (equalp (array-project-perm-slice a #'+) (array-project-noperm a #'+)))
  (assert (equalp (array-project-perm-slice a #'+ :dim 1) (array-project-noperm a #'+ :dim 1)))
  (assert (equalp (array-project-perm-slice a #'+ :a-perm '(2 0 1)) (array-project-noperm (array-permute a '(2 0 1)) #'+)))
  (assert (equalp (array-project-perm-slice a #'+ :a-perm '(2 0 1) :a-slice '((2 4) (0 2) (0 3)) :dim 0) #2A((5 13 21) (29 37 45)))) ;slice doesn't affect output zeroes because they are projected away
  (assert (equalp (array-project-perm-slice a #'+ :a-perm '(2 0 1) :a-slice '((2 4) (0 2) (1 2)) :dim 1) #2A((0 0 0) (0 0 0) (0 24 0) (0 26 0)))))

(defun array-project (a f &key (a-perm (default-array-permutation a)) (a-slice (permute (default-array-slice (array-dimensions a)) a-perm)) (dim 0))
  ;; array-project-perm-slice is always faster than array-project-noperm
  (array-project-perm-slice a f :a-perm a-perm :a-slice a-slice :dim dim))

(defun array-select-dimension (a dim selector)
  "Return a new array like array A but with index dimension DIM fixed at SELECTOR."
  ;; TODO: Generalize to all dimensions, and selector a vector with possible nil values (meaning to give all values of this dimension)
  ;; TODO: use an array-slice as input instead of DIM and SELECTOR
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

(defun array-slice-dimensions (slices)
  "Return the dimensions (as returned by 'array-dimensions) of the slices SLICES.
Example: (array-slice-dimensions '((0 3 5 7) (0 1 2 3 4 5))) == '(5 3)."
  (loop for dim-slice in slices collect
       (loop for s on dim-slice by #'cddr while (not (null s)) sum
	    (- (second s) (first s)))))

(defun array-select-perm-slice (a a-perm a-slice)
  "Return a new array obtained from permuting array's A dimensions with permutation A-PERM, and slicing the resulting array using slice A-SLICE."
  (let* ((a-dim (array-dimensions a))
	 (r-dim (array-slice-dimensions a-slice))
	 (r (make-array r-dim :element-type (array-element-type a)))
	 (r-perm (default-array-permutation r))
	 (r-slice (default-array-slice r-dim))) ;don't need to permute r-dim, because r-perm is identity
    (assert (array-slice-valid a-slice a-dim))
    (arrays-walk (list a-dim r-dim) (list a-perm r-perm) (list a-slice r-slice)
		 (lambda (a-index r-index)
		   (setf (row-major-aref r r-index) (row-major-aref a a-index))))
    r))

(defun array-insert-perm-slice (a a-perm a-slice r r-perm r-slice)
  "Insert the with A-PERM permuted and A-SLICe sliced elements of array A into the elements of R specified by dimension permutation R-PERM and slice R-SLICE."
  (array-fun a #'identity r :a-perm a-perm :a-slice a-slice :r-perm r-perm :r-slice r-slice))

(let* ((a (make-array '(2 3 4) :element-type 'single-float :initial-contents '(((0.0 1.0 2.0 3.0) (4.0 5.0 6.0 7.0) (8.0 9.0 10.0 11.0)) ((12.0 13.0 14.0 15.0) (16.0 17.0 18.0 19.0) (20.0 21.0 22.0 23.0)))))
       (a-perm '(1 0 2))
       (a-slice '((0 1) (1 2) (0 1 3 4)))
       (r (array-select-perm-slice a a-perm a-slice))
       (r-perm (default-array-permutation r))
       (r-slice (default-array-slice (permute (array-dimensions r) r-perm))))
  (assert (equalp r #3A(((12 15)))))
  (array-fun r #'1+ r)
  (array-insert-perm-slice r r-perm r-slice a a-perm a-slice)
  ;;(assert (equalp a #3A(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((13 13 14 16) (16 17 18 19) (20 21 22 23))))) ;SBCL says this assertion doesn't hold, but that's wrong, so I'm commenting it out.
  )

(defun compile-map-into-array (arrays-dim arrays-perm arrays-slice arrays-type)
  "Returns a compiled function MAP-INTO-ARRAY, which is to be called with simple-arrays RES, a function F, and simple-arrays A, ..., Z.
MAP-INTO-ARRAY walks in parallel over the elements of RES, A, ..., Z specified by ARRAYS-DIM, ARRAYS-PERM and ARRAYS-SLICE, and calls F with the elements.
The value returned by F is stored in the element of RES.
ARRAYS-DIM is a list containing the array-dimensions of RES, A, ..., Z.
ARRAYS-PERM is a list of permutation lists of RES, A, ..., Z.
ARRAYS-SLICE is a list of array slices of RES, A, ..., Z.
ARRAYS-TYPE is the element-type of RES, A, ..., Z."
  (let* ((arrays-sym (loop for i in arrays-dim collect (gensym)))
	 (res-sym (car arrays-sym))
	 (rest-arrays-sym (cdr arrays-sym))
	 (f-sym (gensym))
	 (body nil))
    (arrays-walk arrays-dim arrays-perm arrays-slice
		 (lambda (&rest indices)
		   (let ((res-index (car indices))
			 (rest-indices (cdr indices)))
		     (setf body (cons `(setf (row-major-aref ,res-sym ,res-index)
					     (funcall (the function ,f-sym) ,@(loop for a in rest-arrays-sym for i in rest-indices collect `(row-major-aref ,a ,i))))
				      body)))))
    (let ((map-into-array `(lambda (,res-sym ,f-sym ,@rest-arrays-sym)
			     (declare (type (simple-array ,(car arrays-type) ,(car arrays-dim)) ,res-sym)
				      ,@(loop for dim in (cdr arrays-dim) for sym in rest-arrays-sym for type in (cdr arrays-type) collect
					     `(type (simple-array ,type ,dim) ,sym))
				      (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
			     ,@body)))
      ;;(print body)
      (compile nil map-into-array))))

(defstruct rbm
  (w nil :type (simple-array single-float (* *)))
  (v-biases nil :type (simple-array single-float (*)))
  (h-biases nil :type (simple-array single-float (*)))
  ;; the neuron descriptors are dim-slices for the hidden or visible layer
  (v-binary nil :type list)
  (v-softmax nil :type list) ;dim-slice (4 6 6 9) means two softmax units with 2 and 3 alternative states
  (v-gaussian nil :type list)
  (v-linear nil :type list)
  (h-binary nil :type list)
  (h-softmax nil :type list) ;dim-slice (4 6 6 9) means two softmax units with 2 and 3 alternative states
  (h-gaussian nil :type list)
  (h-linear nil :type list)
  (h-noisefree nil :type list))

(defun new-rbm (n-visible n-hidden &key (v-binary 0) (v-softmax nil) (v-gaussian 0) (v-linear 0) (h-binary 0) (h-softmax nil) (h-gaussian 0) (h-linear 0) (h-noisefree 0))
  "Return a new randomly initialized restricted boltzmann machine.
V-BINARY, V-GAUSSIAN, V-LINEAR must be non-negative integers adding up to N-VISIBLE.
H-BINARY, H-GAUSSIAN, H-LINEAR, H-NOISEFREE must be non-negative integers.
V-SOFTMAX and H-SOFTMAX must each be a list of integers greater than 2 each specifying the number of alternative states that each softmax unit can have.
The sum of the numbers of the H-parameters plus the sum of the list of numbers of H-SOFTMAX must add up to N-HIDDEN."
  (let ((w (make-array (list n-visible n-hidden) :element-type 'single-float))
	(v-biases (make-array n-visible :element-type 'single-float))
	(h-biases (make-array n-hidden :element-type 'single-float)))
    (declare (type (simple-array single-float) w v-biases h-biases))
    (let nil
      (flet ((dim-slice-for-softmax (start softmax-list)
	       (if (null softmax-list)
		   (list start start)
		   ;; n-states=1 means the unit is always on, n-states=2 is equivalent to a binary unit
		   (apply #'append (loop for n-states in softmax-list collect
					(let* ((end (+ start n-states))
					       (range (list start end)))
					  (setf start end)
					  range)))))
	     (last-car (list)
	       (car (last list))))
	;; check that the visible and hidden neuron type ranges fit seamlessly
	(assert (= n-visible (+ (apply #'+ (list v-binary v-gaussian v-linear))
				(apply #'+ v-softmax))))
	(setf v-binary (list 0 v-binary))
	(setf v-softmax (dim-slice-for-softmax (second v-binary) v-softmax))
	(setf v-gaussian (list (last-car v-softmax) (+ (last-car v-softmax) v-gaussian)))
	(setf v-linear (list (second v-gaussian) (+ (second v-gaussian) v-linear)))
	(assert (= n-hidden (+ (apply #'+ (list h-binary h-gaussian h-linear h-noisefree))
			       (apply #'+ h-softmax))))
	(setf h-binary (list 0 h-binary))
	(setf h-softmax (dim-slice-for-softmax (second h-binary) h-softmax))
	(setf h-gaussian (list (last-car h-softmax) (+ (last-car h-softmax) h-gaussian)))
	(setf h-linear (list (second h-gaussian) (+ (second h-gaussian) h-linear)))
	(setf h-noisefree (list (second h-linear) (+ (second h-linear) h-noisefree)))
	;; init weights and biases
	(loop for i below n-visible do
	     (loop for j below n-hidden do
		  (setf (aref w i j) (* (random-gaussian) .01))))
	(loop for i below n-visible do (setf (aref v-biases i) 0.0))
	(loop for j below n-hidden do (setf (aref h-biases j) 0.0))
	(make-rbm :w w :v-biases v-biases :h-biases h-biases
		  :v-binary v-binary :v-softmax v-softmax :v-gaussian v-gaussian :v-linear v-linear
		  :h-binary h-binary :h-softmax h-softmax :h-gaussian h-gaussian :h-linear h-linear :h-noisefree h-noisefree)))))

;;(defun new-rbm-1 ()
;;  (make-rbm :w #2A((0.40 0.50 0.60)
;;		   (0.41 0.51 0.61)
;;		   (0.42 0.52 0.62)
;;		   (0.43 0.53 0.63)
;;		   (0.44 0.54 0.64)
;;		   (0.45 0.55 0.65))
;;	    :v-biases #(0 0 0 0 0 0)
;;	    :h-biases #(0 0 0)))

(defun rbm-n-v (rbm)
  (array-dimension (rbm-w rbm) 0))

(defun rbm-n-h (rbm)
  (array-dimension (rbm-w rbm) 1))

(defun softmax-from-exps-to-probs (terms)
  "Given a sequence of TERMS=A1,A2,...,An, compute the corresponding sequence (/ (exp A1) S),(/ (exp A2) S),...,(/ (exp An) S) with S=(sum-of (exp A1) (exp A2) ... (exp An)) in a way that doesn't produce floating-point overflows."
  ;; A disadvantage of this algorithm is that it takes O(|terms|^2), because REC needs to loop over all AIS-REST.
  ;; If speed becomes critical, one could sort the Ais, detect the Ai-mid where (- Ai0 Ai-mid) is greater than, say 20 (because (/ 1 (exp 20)) is neglegibly small), run REC only on the list from Ai0 to Ai-mid, and fill the end of the by REC returned list with a list of zeroes.
  (labels ((rec (ais bi-1 bis)
	     (if (null ais)
		 (nreverse bis)
		 (let* ((ai (car ais))
			(ais-rest (cdr ais))
			(si (loop for aj in ais-rest sum (exp (- aj ai))))
			(bi (/ bi-1 (1+ si))))
		   (rec ais-rest (- bi-1 bi) (cons bi bis))))))
    (let* ((terms-and-order (map 'list (lambda (a b) (cons a b)) terms (loop for i below (length terms) collect i)))
	   (terms-and-order-sorted (sort terms-and-order #'> :key #'car))
	   (terms-sorted (mapcar #'car terms-and-order-sorted))
	   (order-sorted (mapcar #'cdr terms-and-order-sorted))
	   (probs-sorted (rec terms-sorted 1 nil))
	   (probs-orig (reverse-permute probs-sorted order-sorted 'vector)))
      probs-orig)))

(defun softmax-calc-probs (n-cases softmax probs)
  "For the softmax units specified in SOFTMAX, calculate from the input array PROBS their probabilities and put them back into PROBS."
  (loop for case below n-cases for case-dim-slice = (list case (1+ case)) do
       (loop for l on softmax by #'cddr
	  for start = (car l)
	  for end = (cadr l) do
	    ;;(prind l start end)
	    (let* ((probs-perm (default-array-permutation probs))
		   (probs-softmax-slice (list case-dim-slice (list start end)))
		   (terms (array-select-perm-slice probs probs-perm probs-softmax-slice))
		   (terms-1d (make-array (- end start) :element-type (array-element-type terms) :displaced-to terms))
		   (softmax-probs (softmax-from-exps-to-probs terms-1d))
		   (softmax-probs-2d (make-array (list 1 (- end start)) :element-type (array-element-type probs) :initial-contents (list softmax-probs))))
	      (array-insert-perm-slice softmax-probs-2d (default-array-permutation softmax-probs-2d) (default-array-slice (array-dimensions softmax-probs-2d)) probs probs-perm probs-softmax-slice))))
  nil)

(defun rbm-learn-cd1 (data rbm &key (print-err nil))
  ;;TODO: possibility to specify neuron types
  "Do one contrastive-divergence-1 step on a restricted boltzmann machine.
DATA is a two-dimensional array of input values: the first dimension represents the cases of the mini-batch, the second dimension represents visible neurons.
RBM is a restricted boltzmann machine as returned by new-rbm or rbm-learn-cd1."
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 3) (space 0)))
  (let* ((w (rbm-w rbm))
	 (v-biases (rbm-v-biases rbm))
	 (h-biases (rbm-h-biases rbm))
	 (n-v (rbm-n-v rbm))
	 (n-h (rbm-n-h rbm))
	 (n-cases (array-dimension data 0))
	 (pos-h-probs (make-array (list n-cases n-h) :element-type 'single-float))
	 (pos-prods (make-array (list n-v n-h) :element-type 'single-float))
	 (pos-h-act nil)
	 (pos-v-act nil)
	 (pos-h-states (make-array (list n-cases n-h) :element-type 'single-float))
	 (neg-data (make-array (list n-cases n-v) :element-type 'single-float))
	 (neg-h-probs (make-array (list n-cases n-h) :element-type 'single-float))
	 (neg-prods (make-array (list n-v n-h) :element-type 'single-float))
	 (neg-h-act nil)
	 (neg-v-act nil)
	 (w-inc (make-array (list n-v n-h) :element-type 'single-float))
	 (v-biases-inc (make-array (list n-v) :initial-element 0.0 :element-type 'single-float))
	 (h-biases-inc (make-array (list n-h) :initial-element 0.0 :element-type 'single-float))
	 (cases-dim-slice (list 0 n-cases))
	 (v-binary (rbm-v-binary rbm))
	 (v-softmax (rbm-v-softmax rbm))
	 ;;(v-gaussian (rbm-v-gaussian rbm))
	 (v-linear (rbm-v-linear rbm))
	 (h-binary (rbm-h-binary rbm))
	 (h-softmax (rbm-h-softmax rbm))
	 (h-gaussian (rbm-h-gaussian rbm))
	 (h-linear (rbm-h-linear rbm))
	 (h-gaussian-linear-merged (array-dim-slices-merge h-gaussian h-linear))
	 (h-noisefree (rbm-h-noisefree rbm)))
    (flet ((softmax-calc-states (softmax probs states)
	     (loop for case below n-cases for case-dim-slice = (list case (1+ case)) do
		  (loop for l on softmax by #'cddr
		     for start = (car l)
		     for end = (cadr l) do
		       (let ((r (random 1.0))
			     (sum 0.0)
			     (softmax-dim-slice (list case-dim-slice (list start end))))
			 (declare (type single-float r sum))
			 (array-fun probs (lambda (x)
					    (declare (type single-float x))
					    (let ((from sum) (to (the single-float (+ sum x))))
					      (declare (type single-float from to))
					      ;;(prind from r to)
					      (setf sum to)
					      (if (and (<= from r) (< r to)) 1.0 0.0)))
				    states :a-slice softmax-dim-slice :r-slice softmax-dim-slice)
			 ;;(prind probs r states)
			 )))))
      ;; positive phase
      (array-array-mul data w pos-h-probs)
      (array-array-fun pos-h-probs (array-repeat h-biases (list n-cases) nil) #'+ pos-h-probs)
;;      (print (list "prod" pos-h-probs "a-slice" (list cases-dim-slice h-binary)))
      (array-fun pos-h-probs (lambda (x) (sigmoid x)) pos-h-probs :a-slice (list cases-dim-slice h-binary) :r-slice (list cases-dim-slice h-binary))
      (softmax-calc-probs n-cases h-softmax pos-h-probs)
;;      (print (list "pos-h-probs" pos-h-probs))
      (array-array-mul data pos-h-probs pos-prods :a-t t)
      (setf pos-h-act (array-project pos-h-probs #'+))
      (setf pos-v-act (array-project data #'+))
;;      (print (list "pos-prods" pos-prods "pos-h-act" pos-h-act "pos-v-act" pos-v-act))
      (array-fun pos-h-probs (lambda (x) (declare (type single-float x)) (if (> x (random 1.0)) 1.0 0.0)) pos-h-states :a-slice (list cases-dim-slice h-binary) :r-slice (list cases-dim-slice h-binary))
      (softmax-calc-states h-softmax pos-h-probs pos-h-states)
      (array-fun pos-h-probs (lambda (x) (declare (type single-float x)) (+ x (the single-float (random-gaussian)))) pos-h-states :a-slice (list cases-dim-slice h-gaussian-linear-merged) :r-slice (list cases-dim-slice h-gaussian-linear-merged))
      (array-fun pos-h-states (lambda (x) (declare (type single-float x)) (max 0.0 x)) pos-h-states :a-slice (list cases-dim-slice h-linear) :r-slice (list cases-dim-slice h-linear))
      (array-fun pos-h-probs #'identity pos-h-states :a-slice (list cases-dim-slice h-noisefree) :r-slice (list cases-dim-slice h-noisefree))
;;      (print (list "pos-h-states" pos-h-states))
      ;; negative phase
      (array-array-mul pos-h-states w neg-data :b-t t)
      (array-array-fun neg-data (array-repeat v-biases (list n-cases) nil) #'+ neg-data)
      (array-fun neg-data (lambda (x) (sigmoid x)) neg-data :a-slice (list cases-dim-slice v-binary) :r-slice (list cases-dim-slice v-binary))
      (softmax-calc-probs n-cases v-softmax neg-data)
      (array-fun neg-data (lambda (x) (declare (type single-float x)) (max x 0.0)) neg-data :a-slice (list cases-dim-slice v-linear) :r-slice (list cases-dim-slice v-linear))
;;      (print (list "neg-data" neg-data))
      (array-array-mul neg-data w neg-h-probs)
      (array-array-fun neg-h-probs (array-repeat h-biases (list n-cases) nil) #'+ neg-h-probs)
      (array-fun neg-h-probs (lambda (x) (declare (type single-float x)) (sigmoid x)) neg-h-probs :a-slice (list cases-dim-slice h-binary) :r-slice (list cases-dim-slice h-binary))
      (softmax-calc-probs n-cases h-softmax neg-h-probs)
;;      (print (list "neg-h-probs" neg-h-probs))
      (array-array-mul neg-data neg-h-probs neg-prods :a-t t)
      (setf neg-h-act (array-project neg-h-probs #'+))
      (setf neg-v-act (array-project neg-data #'+))
;;      (print (list "neg-prods" neg-prods "neg-h-act" neg-h-act "neg-v-act" neg-v-act))
      ;; learning
      (array-array-fun pos-prods neg-prods (lambda (a b) (declare (type single-float a b)) (/ (- a b) n-cases)) w-inc)
      (array-array-fun pos-v-act neg-v-act (lambda (a b) (declare (type single-float a b)) (/ (- a b) n-cases)) v-biases-inc)
      (array-array-fun pos-h-act neg-h-act (lambda (a b) (declare (type single-float a b)) (/ (- a b) n-cases)) h-biases-inc)
;;      (print (list "w-inc" w-inc "v-biases-inc" v-biases-inc "h-biases-inc" h-biases-inc))
      (when print-err
	(let ((err-array (make-array (list n-cases n-v) :element-type 'single-float))
	      (err nil))
	  (array-array-fun data neg-data (lambda (a b) (declare (type single-float a b)) (expt (- a b) 2)) err-array)
	  (setf err (aref (the (simple-array single-float) (array-project (the (simple-array single-float) (array-project err-array #'+)) #'+))))
	  (print (list "err" err))))
      (values w-inc v-biases-inc h-biases-inc))))

(defun visible-free-energy-log (data rbm)
  "For given vectors of visible layer assignments DATA and a given RBM, return the log of the free energy of the vectors in DATA."
  (when (not (equal (array-element-type data) 'single-float))
    (setf data (copy-array data :array-element-type 'single-float)))
  (let* ((n-visible (rbm-n-v rbm))
	 (n-cases (array-dimension data 0))
	 (n-hidden (rbm-n-h rbm))
	 (v-biases (rbm-v-biases rbm))
	 (h-biases (rbm-h-biases rbm))
	 (w (rbm-w rbm))
	 (pos-h-probs (make-array (list n-cases n-hidden) :element-type 'single-float))
	 (t1 (make-array (list n-cases n-visible) :element-type 'single-float))
	 (t2 (make-array (list n-cases n-hidden) :element-type 'single-float)))
    (array-array-mul data w pos-h-probs)
    (array-array-fun pos-h-probs (array-repeat h-biases (list n-cases) nil) #'+ pos-h-probs)
    ;;(prind pos-h-probs)
    (array-array-fun data (array-repeat v-biases (list n-cases) nil) #'* t1)
    (flet ((safe-log1+exp (x)
	     (handler-case (log (1+ (exp x)))
	       (floating-point-overflow (c) (declare (ignore c)) x))))
      (array-fun pos-h-probs #'safe-log1+exp t2))
    ;;(prind t1)
    ;;(prind t2)
    (let ((s1 (array-project t1 #'+ :dim 1))
	  (s2 (array-project t2 #'+ :dim 1))
	  (e (make-array n-cases :element-type 'single-float)))
      ;;(prind s1)
      ;;(prind s2)
      (array-array-fun s1 s2 (lambda (a b) (+ a b)) e)
      e)))

(defparameter *data-imbalanced* #2A((1 1 0 0 0 0) (0 0 1 1 0 0) (0 0 0 0 1 1) (0 0 0 0 1 1)))
(defparameter *data* #2A((1 1 0 0 0 0) (0 0 1 1 0 0) (0 0 0 0 1 1)))
(defparameter *data900* (make-array '(3 900) :initial-contents (list (nconc (loop for i below 300 collect 1) (loop for i below 600 collect 0)) (nconc (loop for i below 300 collect 0) (loop for i below 300 collect 1) (loop for i below 300 collect 0)) (nconc (loop for i below 600 collect 0) (loop for i below 300 collect 1)))))
(defparameter *data2* #2A((0 0) (0 1) (1 0) (1 1)))
(defparameter *data3* #2A((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)))
(defparameter *data-l* #2A((2 2 0 0) (3 3 0 0) (4 4 0 0) (5 5 0 0) (0 0 2 2) (0 0 3 3) (0 0 4 4) (0 0 5 5)))
(defparameter *data*1000* #2A((1000 1000 0 0 0 0) (0 0 1000 1000 0 0) (0 0 0 0 1000 1000)))
(defparameter *data+0.5* #2A((0 0.5) (0.1 0.6) (0.2 0.7) (0.3 0.8) (0.4 0.9) (0.5 1)))
(defparameter *data+0.5-unseen* #2A((0.05 0.55) (0.15 0.65) (0.25 0.75) (0.35 0.85) (0.45 0.95)
				    (0.05 1) (0.15 0.9) (0.25 0.8) (0.35 0.7) (0.45 0.6) ;some examples not following the rule to see what the network does
				    (0 1) (0.1 1) (0.2 1) (0.3 1) (0.4 1) (0.5 1)))
(defparameter *data+0.5-extrapolate* #2A((0.5 1) (0.6 1.1) (0.7 1.2) (0.8 1.3) (0.9 1.4) (1 1.5)))
(defparameter *data+5* #2A((0 5) (1 6) (2 7) (3 8) (4 9) (5 10)))
(defparameter *data+5-extrapolate* #2A((6 11) (7 12) (8 13) (9.5 14.5) (-40 -35) (0 0)))
(defparameter *data-softmax* #2A((1 0 0 1 0 0) (0 1 0 0 1 0) (0 0 1 0 0 1))) ;v-softmax='(3 3)
;; rock-paper-scissors-lizard-spock game
;;                              -player1-  -player2-  -win- (win0=1:draw win1=1 player1_wins win2=1 player2_wins)
(defparameter *data-rpsls* #2A((1 0 0 0 0  1 0 0 0 0  1 0 0)
			       (1 0 0 0 0  0 1 0 0 0  0 0 1)
			       (1 0 0 0 0  0 0 1 0 0  0 1 0)
			       (1 0 0 0 0  0 0 0 1 0  0 1 0)
			       (1 0 0 0 0  0 0 0 0 1  0 0 1)
			       (0 1 0 0 0  1 0 0 0 0  0 1 0)
			       (0 1 0 0 0  0 1 0 0 0  1 0 0)
			       (0 1 0 0 0  0 0 1 0 0  0 0 1)
			       (0 1 0 0 0  0 0 0 1 0  0 0 1)
			       (0 1 0 0 0  0 0 0 0 1  0 1 0)
			       (0 0 1 0 0  1 0 0 0 0  0 0 1)
			       (0 0 1 0 0  0 1 0 0 0  0 1 0)
			       (0 0 1 0 0  0 0 1 0 0  1 0 0)
			       (0 0 1 0 0  0 0 0 1 0  0 1 0)
			       (0 0 1 0 0  0 0 0 0 1  0 0 1)
			       (0 0 0 1 0  1 0 0 0 0  0 0 1)
			       (0 0 0 1 0  0 1 0 0 0  0 1 0)
			       (0 0 0 1 0  0 0 1 0 0  0 0 1)
			       (0 0 0 1 0  0 0 0 1 0  1 0 0)
			       (0 0 0 1 0  0 0 0 0 1  0 1 0)
			       (0 0 0 0 1  1 0 0 0 0  0 1 0)
			       (0 0 0 0 1  0 1 0 0 0  0 0 1)
			       (0 0 0 0 1  0 0 1 0 0  0 1 0)
			       (0 0 0 0 1  0 0 0 1 0  0 0 1)
			       (0 0 0 0 1  0 0 0 0 1  1 0 0)))
(defparameter *rbmnew-rpsls* (new-rbm 13 13 :v-softmax '(5 5 3) :h-binary 13))
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* *rbmnew-rpsls* .01 0.0 .0002 10000)) yields a reconstruction-error of 20.60756.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* *rbmnew-rpsls* .01 0.9 .0002 10000)) yields a reconstruction-error of 7.000717e-5.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 25 :v-softmax '(5 5 3) :h-softmax '(25)) .01 0.9 .0002 10000)) yields a reconstruction-error of 31.975252.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 25 :v-softmax '(5 5 3) :h-softmax '(25)) .1 0.9 .0002 10000)) yields a reconstruction-error of 35.655266, 31.311026, 31.989447, 30.147305.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 25 :v-softmax '(5 5 3) :h-softmax '(25)) .1 .0 .0002 10000)) yields a reconstruction-error of 31.96534, 33.841312, 31.949482.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 25 :v-softmax '(5 5 3) :h-softmax '(25)) 4.0 .9 .0002 1000)) yields a reconstruction-error of 31.21259, 28.04349, 32.60846.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 25 :v-softmax '(5 5 3) :h-softmax '(25)) 1.0 .9 .0002 1000)) yields a reconstruction-error of 25.86813, 29.170877, 27.095364, 31.12667, 27.525604.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 50 :v-softmax '(5 5 3) :h-softmax '(50)) 1.5 .9 .0002 1000)) yields a reconstruction-error of 32.02281, 25.38587, 27.085512, 32.00493.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 50 :v-softmax '(5 5 3) :h-softmax '(25 25)) 1.5 .9 .0002 1000)) yields a reconstruction-error of 0.046805285.
;;(defparameter *rbm-rpsls* (rbm-learn-minibatch *data-rpsls* (new-rbm 13 5 :v-softmax '(5 5 3) :h-binary 5) 1.5 .9 .0002 1000)) yields a reconstruction-error of 5.8492384, 1.6673808, 4.351033, 9.602575, 3.659302.
(defun rpsls-win-doesntwork (data rbm iterations)
  (when (not (equal (array-element-type data) 'single-float))
    (setf data (copy-array data :array-element-type 'single-float)))
  (let ((v (copy-array data)))
    (loop for j below (array-dimension v 0) do
	 (loop for k from 10 below 13 do (setf (aref v j k) (float 1/3))))
    (loop for i below iterations do
	 (loop for j below (array-dimension v 0) do
	      (loop for k from 0 below 10 do (setf (aref v j k) (aref data j k))))
	 (prind v)
	 (let* ((h (rbm-h-from-v v rbm))
		(v1 (rbm-v-from-h h rbm)))
	   (setf v v1)))
    v))
(defun rpsls-win (data rbm)
  ;; TODO: rewrite this function using array-select-dim (like selecting a row or col in R) or something like that.
  (loop for i below (array-dimension data 0) do
       (let ((best nil)
	     (best-free-energy nil))
	 (loop for win-case below 3 do
	      (let ((da (make-array '(1 13) :initial-contents '((0 0 0 0 0  0 0 0 0 0  0 0 0)))))
		(loop for j below 10 do
		     (setf (aref da 0 j) (aref data i j)))
		(loop for j below 3 do
		     (setf (aref da 0 (+ 10 j)) (if (= j win-case) 1 0)))
		(let ((free-energy (aref (visible-free-energy-log da rbm) 0)))
		  (when (or (null best) (> free-energy best-free-energy))
		    (setf best da)
		    (setf best-free-energy free-energy)))))
	 (prind best))))

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
      (make-rbm :w w-new :v-biases v-biases-new :h-biases h-biases-new
		:v-binary (rbm-v-binary rbm) :v-softmax (rbm-v-softmax rbm) :v-gaussian (rbm-v-gaussian rbm) :v-linear (rbm-v-linear rbm)
		:h-binary (rbm-h-binary rbm) :h-softmax (rbm-h-softmax rbm) :h-gaussian (rbm-h-gaussian rbm) :h-linear (rbm-h-linear rbm) :h-noisefree (rbm-h-noisefree rbm)))))

(defun rbm-learn-minibatch (data rbm learn-rate momentum weight-cost max-iterations)
  (assert (> max-iterations 0))
  (when (not (equal (array-element-type data) 'single-float))
    (setf data (copy-array data :array-element-type 'single-float)))
  (labels ((rec (rbm w-inc v-biases-inc h-biases-inc iteration)
	     (print (list "iteration" iteration))
	     (multiple-value-bind (w-inc-1 v-biases-inc-1 h-biases-inc-1) (rbm-learn-cd1 data rbm :print-err t)
	       (array-array-fun w-inc-1 (rbm-w rbm) (lambda (a b) (- a (* weight-cost b))) w-inc-1) ;add weight-cost
	       (array-array-fun w-inc w-inc-1 (lambda (a b) (+ (* a momentum) b)) w-inc)
	       (array-array-fun v-biases-inc v-biases-inc-1 (lambda (a b) (+ (* a momentum) b)) v-biases-inc)
	       (array-array-fun h-biases-inc h-biases-inc-1 (lambda (a b) (+ (* a momentum) b)) h-biases-inc)
	       (let ((new-rbm (rbm-update rbm w-inc v-biases-inc h-biases-inc learn-rate)))
		 (if (>= iteration max-iterations)
		     new-rbm
		     (rec new-rbm w-inc v-biases-inc h-biases-inc (1+ iteration)))))))
    (let ((w-inc (make-array (array-dimensions (rbm-w rbm)) :element-type 'single-float))
	  (v-biases-inc (make-array (array-dimensions (rbm-v-biases rbm)) :element-type 'single-float))
	  (h-biases-inc (make-array (array-dimensions (rbm-h-biases rbm)) :element-type 'single-float)))
      (rec rbm w-inc v-biases-inc h-biases-inc 1))))

(defun rbm-learn (get-data-fn rbm learn-rate momentum weight-cost)
  "Learn a Restricted Boltzmann Machine (rbm) using different mini-batches of data and using RBM-LEARN-CD1 on the current rbm RBM to get a training signal.
Each iteration, the funtion GET-DATA-FN is called with the current iteration and rbm.
The function must decide whether to proceed with learning or to abort.
If it wants to proceed, it must return a list of input data to be learned.
If it wants to quit, it must return NIL.
The training signal obtained from RBM-LEARN-CD1 is added, after using LEARN-RATE, MOMENTUM, and WEIGHT-COST, to the current RBM using RBM-UPDATE.
The resulting rbm is returned."
  (labels ((rec (rbm w-inc v-biases-inc h-biases-inc iteration)
	     ;;(print (list "iteration" iteration))
	     (let ((data (funcall get-data-fn iteration rbm))
		   (momentum (if (< iteration 10) (/ momentum 2) momentum)))
	       (if (null data)
		   rbm
		   (multiple-value-bind (w-inc-1 v-biases-inc-1 h-biases-inc-1) (rbm-learn-cd1 data rbm)
		     (array-array-fun w-inc-1 (rbm-w rbm) (lambda (a b) (- a (* weight-cost b))) w-inc-1) ;add weight-cost
		     (array-array-fun w-inc w-inc-1 (lambda (a b) (+ (* a momentum) b)) w-inc)
		     (array-array-fun v-biases-inc v-biases-inc-1 (lambda (a b) (+ (* a momentum) b)) v-biases-inc)
		     (array-array-fun h-biases-inc h-biases-inc-1 (lambda (a b) (+ (* a momentum) b)) h-biases-inc)
		     (let ((new-rbm (rbm-update rbm w-inc v-biases-inc h-biases-inc learn-rate)))
		       (rec new-rbm w-inc v-biases-inc h-biases-inc (1+ iteration))))))))
    (let ((w-inc (make-array (array-dimensions (rbm-w rbm)) :element-type 'single-float))
	  (v-biases-inc (make-array (array-dimensions (rbm-v-biases rbm)) :element-type 'single-float))
	  (h-biases-inc (make-array (array-dimensions (rbm-h-biases rbm)) :element-type 'single-float)))
      (rec rbm w-inc v-biases-inc h-biases-inc 0))))

(defun rbm-h-from-v (data rbm)
  (when (not (equal (array-element-type data) 'single-float))
    (setf data (copy-array data :array-element-type 'single-float)))
  (let* ((w (rbm-w rbm))
	 (h-biases (rbm-h-biases rbm))
	 (n-h (rbm-n-h rbm))
	 (n-cases (array-dimension data 0))
	 (pos-h-probs (make-array (list n-cases n-h) :element-type 'single-float))
	 (cases-dim-slice (list 0 n-cases))
	 (h-binary (rbm-h-binary rbm))
	 (h-softmax (rbm-h-softmax rbm))
	 ;;(h-gaussian (rbm-h-gaussian rbm))
	 (h-linear (rbm-h-linear rbm))
	 )
    ;; positive phase
    (array-array-mul data w pos-h-probs)
    (array-array-fun pos-h-probs (array-repeat h-biases (list n-cases) nil) #'+ pos-h-probs)
    (array-fun pos-h-probs (lambda (x) (sigmoid x)) pos-h-probs :a-slice (list cases-dim-slice h-binary) :r-slice (list cases-dim-slice h-binary))
    (softmax-calc-probs n-cases h-softmax pos-h-probs)
    ;; need to clamp pos-h-probs at 0.0.
    (array-fun pos-h-probs (lambda (x) (declare (type single-float x)) (max 0.0 x)) pos-h-probs :a-slice (list cases-dim-slice h-linear) :r-slice (list cases-dim-slice h-linear))
    pos-h-probs))

(defun rbm-v-from-h (pos-h-states rbm)
  (when (not (equal (array-element-type pos-h-states) 'single-float))
    (setf pos-h-states (copy-array pos-h-states :array-element-type 'single-float)))
  (let* ((w (rbm-w rbm))
	 (n-v (rbm-n-v rbm))
	 (v-biases (rbm-v-biases rbm))
	 (n-cases (array-dimension pos-h-states 0))
	 (neg-data (make-array (list n-cases n-v) :element-type 'single-float))
	 (cases-dim-slice (list 0 n-cases))
	 (v-binary (rbm-v-binary rbm))
	 (v-softmax (rbm-v-softmax rbm))
	 (v-linear (rbm-v-linear rbm)))
    ;; negative phase
    (array-array-mul pos-h-states w neg-data :b-t t)
    (array-array-fun neg-data (array-repeat v-biases (list n-cases) nil) #'+ neg-data)
    (array-fun neg-data (lambda (x) (sigmoid x)) neg-data :a-slice (list cases-dim-slice v-binary) :r-slice (list cases-dim-slice v-binary))
    (softmax-calc-probs n-cases v-softmax neg-data)
    (array-fun neg-data (lambda (x) (max x 0)) neg-data :a-slice (list cases-dim-slice v-linear) :r-slice (list cases-dim-slice v-linear))
    neg-data))

(defun rbm-reconstruction-error (data rbm &key (neg-data (rbm-v-from-h (rbm-h-from-v data rbm) rbm)))
  (when (not (equal (array-element-type data) 'single-float))
    (setf data (copy-array data :array-element-type 'single-float)))
  (when (not (equal (array-element-type neg-data) 'single-float))
    (setf neg-data (copy-array neg-data :array-element-type 'single-float)))
  (let* ((n-cases (array-dimension data 0))
	 (n-v (rbm-n-v rbm))
	 (err-array (make-array (list n-cases n-v) :element-type 'single-float)))
    (array-array-fun data neg-data (lambda (a b) (declare (type single-float a b)) (expt (- a b) 2)) err-array)
    (aref (the (simple-array single-float) (array-project (the (simple-array single-float) (array-project err-array #'+)) #'+)))))

(defun rbm-learn-sequentially (data rbm learn-rate momentum weight-cost mini-batch-size continue-fun)
  "CONTINUE-FUN has to return NIL if it wants the learning to stop, a non-NIL value otherwise."
  (let* ((n-samples (array-dimension data 0))
	 (n-features (array-dimension data 1))
	 (indices-mini-batches (loop for i below n-samples by mini-batch-size collect (loop for j from i below (min (+ i mini-batch-size) n-samples) collect j)))
	 (slice-mini-batches (mapcar (lambda (indices)
				       (list (array-indices-to-dim-slice indices) (list 0 n-features)))
				     indices-mini-batches))
	 (data-mini-batches (mapcar (lambda (slice) (array-select-perm-slice data (default-array-permutation data) slice))
				    slice-mini-batches))
	 (n-mini-batches (length data-mini-batches)))
    (flet ((get-data (iteration rbm)
	     (if (funcall continue-fun iteration rbm)
		 (nth (mod iteration n-mini-batches) data-mini-batches)
		 nil)))
      (rbm-learn #'get-data rbm learn-rate momentum weight-cost))))

;; these give good models:
;;(defparameter *rbm* (rbm-learn-minibatch *data* (new-rbm 6 3 :v-binary 6 :h-softmax '(3)) .01 0 .002 10000))
;;(let* ((h (rbm-h-from-v *data* *rbm*))
;;       (v (rbm-v-from-h h *rbm*)))
;;  (list h v))
;;(defparameter *rbm* (rbm-learn-minibatch *data+0.5* (new-rbm 2 1 :v-gaussian 2 :h-noisefree 1) .001 .9 .0002 10000))
;;(defparameter *rbm* (rbm-learn-minibatch *data+5* (new-rbm 2 1 :v-gaussian 2 :h-gaussian 1) .001 .9 .0002 10000))
;;(defparameter *rbm* (rbm-learn-minibatch *data-softmax* (new-rbm 6 3 :v-softmax '(3 3) :h-softmax '(3)) .1 0 .0002 1000))

(let ((c '((0.0
	    1.0 0.0 0.0 0.0 0.0 0.0 0.0
	    0.0 0.0 0.0 0.0 1.0 0.0 0.0
	    0.0 0.0 0.0 0.0 0.0 0.0 1.0
	    0.0 0.0 0.0 0.0 1.0 0.0 0.0
	    1.0 0.0 0.0 0.0 0.0 0.0 0.0
	    0.0 0.0 0.0 0.0 1.0 0.0 0.0
	    5.5 0.0 0.0 0.0 6.5 0.0)
	   (0.0
	    1.0 0.0 0.0 0.0 0.0 0.0 0.0
	    0.0 0.0 0.0 0.0 1.0 0.0 0.0
	    0.0 0.0 0.0 0.0 0.0 1.0 0.0
	    0.0 0.0 0.0 0.0 1.0 0.0 0.0
	    1.0 0.0 0.0 0.0 0.0 0.0 0.0
	    0.0 0.0 0.0 0.0 1.0 0.0 0.0
	    5.5 0.0 0.0 0.0 4.5 0.0))))
  (defparameter *data-code* (make-array '(2 49) :element-type 'single-float :initial-contents c))
  (defparameter *rbm-mini*  (new-rbm 49 12 :V-BINARY 1 :V-SOFTMAX '(7 7 7 7 7 7) :V-GAUSSIAN 6 :H-SOFTMAX nil :H-BINARY 12 :H-GAUSSIAN 0 :h-noisefree 0)))
;;(defparameter *rbm-res* (rbm-learn-minibatch *data-code* *rbm-mini* .1 .9 .0002 3000))

(defun rbm-print-h-and-v (data rbm)
  (let* ((h (rbm-h-from-v data rbm))
	 (v (rbm-v-from-h h rbm)))
    (print h)
    (print v)
    nil))

(let ((c '((1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
	   (0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0)
	   (0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0)
	   (0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0)
	   (0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0)
	   (0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0)
	   (0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0)
	   (0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0))))
  (defparameter *data-code2* (make-array '(8 8) :element-type 'single-float :initial-contents c))
  (defparameter *rbm-code2*  (new-rbm 8 3 :V-BINARY 0 :V-SOFTMAX '(8) :V-GAUSSIAN 0 :H-SOFTMAX nil :H-BINARY 3 :H-GAUSSIAN 0 :h-noisefree 0))
  (defun rbm-code2-learn (learn-rate momentum weight-cost max-iterations mini-batch-size)
    (flet ((get-data (i rbm)
	     (declare (ignore rbm))
	     (if (>= i max-iterations)
		 nil
		 (let ((r (make-array (list mini-batch-size 8) :element-type 'single-float :initial-contents (loop for i below mini-batch-size collect (elt c (random 8)))))
		       ;;(r (make-array (list mini-batch-size 8) :element-type 'single-float :initial-contents (loop for i below mini-batch-size collect (elt c i))))
		       )
		   ;;(prind r)
		   r))))
      (let ((rbm (rbm-learn #'get-data *rbm-code2* learn-rate momentum weight-cost)))
	(rbm-print-h-and-v *data-code2* rbm)
	(defparameter *rbm-res* rbm)))))

;; rbm-code2-learn sometimes finds a model where all v-units are 1/8, even when learning 8 times as many iterations as rbm-learn-minibatch.
;; however, when learning all data at once, rbm-learn-minibatch finds good models:
;;(defparameter *rbm-res* (rbm-learn-minibatch *data-code2* *rbm-code2* 2 .9 .0002 3000))

(let* ((cases 4)
       (variables 8)
       (hidden 1)
       (a (make-array (list cases variables) :element-type 'single-float)))
  (loop for i below cases do (loop for j below variables do (setf (aref a i j) (+ i (random 1.0)))))
  (defparameter *data-code3* a)
  ;;(defparameter *rbm-code3* (new-rbm variables hidden :v-gaussian variables :h-gaussian hidden))
  (defparameter *rbm-code3* (new-rbm variables hidden :v-linear variables :h-linear hidden))
  )

;;(require :sb-sprof)
;;(defun do-sprof-rbm-learn-minibatch (max-iterations)
;;  (sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil :reset t)
;;    (rbm-learn-minibatch *data* (new-rbm 6 3 :h-binary 3) 1 .9 .0002 max-iterations))
