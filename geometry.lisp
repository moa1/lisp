;;(asdf:oos 'asdf:load-op 'utils)
;;(use-package 'alexandria)
;; in slime: (load ".../geometry.lisp") avoids the undefined function
;; make-vector3d, make-vector2d error of (define-constant)
;; or maybe c-c on make-vector3d, make-vector2d



;;;; vector stuff

;; (defstruct (vector3d (:constructor make-vector3d (x y z)))
;;   (x 0 :type rational)
;;   (y 0 :type rational)
;;   (z 0 :type rational))

;;(defstruct (vector2d (:constructor make-vector2d (x y)))
;;  (x 0 :type rational)
;;  (y 0 :type rational))


;; vector3d      ---ct--- 98.44
;; vector3d-     -------- 93.44
;; vector3d-p    -f------ 91.77
;; vector3d-x    -f------ 91.77
;; vector3d-y    -f------ 91.77
;; vector3d-z    -f------ 91.77
;; copy-vector3d -f------ 80.41
;; make-vector3d -f------ 80.41

(deftype vector2d ()
  `(array rational (2)))

(deftype vector3d ()
  `(array rational (3)))

(deftype vector4d ()
  `(array rational (4)))

(defun make-vector (&rest rest)
  (make-array (list (length rest)) :initial-contents rest))

(defun make-vector2d (&optional (x 0) (y 0))
  (make-array 2 :element-type 'rational :initial-contents (list x y)))

(defun make-vector3d (&optional (x 0) (y 0) (z 0))
  (make-array 3 :element-type 'rational :initial-contents (list x y z)))

(defun make-vector4d (&optional (w 0) (x 0) (y 0) (z 0))
  (make-array 4 :element-type 'rational :initial-contents (list x y z w)))

(defun vector= (a b)
  (equal-array a b))

(defun vector2d= (a b)
  (equal-array a b))

(defun vector3d= (a b)
  (equal-array a b))

(defun vector4d= (a b)
  (equal-array a b))

(defstruct (line (:constructor make-line (origin direction)))
  origin
  direction)

(defun assert-line (line)
  (let* ((dir (line-direction line))
	 (dirdim (length dir)))
    (assert (= dirdim (length (line-origin line))))
    (assert (loop for i below dirdim thereis (/= 0 (aref dir i))))))

(defun line-interpolate (line factor)
  (assert (<= 0 factor 1))
  (assert-line line)
  (vector-add (line-origin line) (vector-scale factor (line-direction line))))

(defstruct (plane (:constructor make-plane (origin normal)))
  (origin nil :type vector3d)
  (normal nil :type vector3d))

(define-constant +origin3d+ (make-vector3d 0 0 0) :test 'vector3d=)
(define-constant +unit3d-x+ (make-vector3d 1 0 0) :test 'vector3d=)
(define-constant +unit3d-y+ (make-vector3d 0 1 0) :test 'vector3d=)
(define-constant +unit3d-z+ (make-vector3d 0 0 1) :test 'vector3d=)

(define-constant +origin2d+ (make-vector2d 0 0) :test 'vector2d=)
(define-constant +unit2d-x+ (make-vector2d 1 0) :test 'vector2d=)
(define-constant +unit2d-y+ (make-vector2d 0 1) :test 'vector2d=)

(defmacro define-many-vector (dimension-names suffix parameters &body body)
  `(progn 
     ,@(loop for name in dimension-names collect
	    (let ((symbol
		   (intern (concatenate 'string
					"VECTOR"
					(string-toupper (string name))
					"-"
					(string-toupper (string suffix))))))
	      `(defun ,symbol (,@parameters)
		,@body)))))

(defmacro define-2d-and-3d (suffix parameters &body body)
  `(define-many-vector ("" "2D" "3D") ,suffix ,parameters ,@body))

(define-many-vector (2d 3d 4d) x (instance)
  (aref instance 0))

(define-many-vector (2d 3d 4d) y (instance)
  (aref instance 1))

(define-many-vector (3d 4d) z (instance)
  (aref instance 2))

(define-many-vector (4d) w (instance)
  (aref instance 3)) ;; this makes the ordering x y z w, not w x y z

(defun arrays-equal-dimensions (&rest arrays)
  (if (consp arrays)
      (let ((ad (array-dimensions (car arrays))))
	(all-if (lambda (a) (equal (array-dimensions a) ad))
		(cdr arrays)))
      t))

(defun map-array (function &rest arrays)
  (assert (apply #'arrays-equal-dimensions arrays))
  (apply #'map 'vector function (car arrays) (cdr arrays)))

(defmacro define-map-array (dimension-names suffix parameters function)
  `(define-many-vector ,dimension-names ,suffix (,@parameters
						 vector &rest more-vectors)
     (apply #'map-array ,function vector more-vectors)))

(define-map-array ("" 2d 3d 4d) add nil #'+)

(define-map-array ("" 2d 3d 4d) sub nil #'-)

(define-map-array ("" 2d 3d) invert nil #'-)

(define-map-array ("" 2d 3d 4d) scale (factor) (lambda (x) (* factor x)))

(define-2d-and-3d scalar (a b)
  (assert (arrays-equal-dimensions a b))
  (reduce #'+ (map 'list #'* a b)))

(define-many-vector ("" 2d 3d 4d) norm (v)
  (reduce #'+ (map 'list (lambda (x) (* x x)) v)))

;;(define-many-vector ("" 2d 3d 4d) length (v)
;;  damn irrational numbers
;;  (sqrt (vector-norm v)))

(define-2d-and-3d colinear-scale (a b)
  "If a and b are colinear, return the factor f so that (vector-scale f b) == a.
Return NIL otherwise."
  (assert (arrays-equal-dimensions a b))
  (let (factor)
    (loop for i below (length a)
       do
	 (let ((val-a (aref a i))
	       (val-b (aref b i)))
	     (if (= val-b 0)
		 (if (not (= val-a 0))
		     (progn
		       (setf factor nil)
		       (return nil)))
		 (if (null factor)
		     (setf factor (/ val-a val-b))
		     (if (/= factor (/ val-a val-b))
			 (progn 
			   (setf factor nil)
			   (return nil)))))))
    factor))
  
(defun vector3d-cross (a b)
  (let ((u (vector3d-x a))
	(v (vector3d-y a))
	(w (vector3d-z a))
	(x (vector3d-x b))
	(y (vector3d-y b))
	(z (vector3d-z b)))
    (make-vector3d (- (* v z) (* w y))
		   (- (* w x) (* u z))
		   (- (* u y) (* v x)))))

(defun vector2d-perpendicular (a)
  "Rotates a 90 degrees counterclockwise in a right-hand coordinate system."
  (make-vector2d (vector2d-y a)
		 (- (vector2d-x a))))

(defun vector4d-conjugate (q)
  (make-vector4d (vector4d-w q)
		 (- (vector4d-x q))
		 (- (vector4d-y q))
		 (- (vector4d-z q))))

(defun vector4d-* (p q)
  (let* ((vp (make-vector3d (vector4d-x p) (vector4d-y p) (vector4d-z p)))
	 (vq (make-vector3d (vector4d-x q) (vector4d-y q) (vector4d-z q)))
	 (wp (vector4d-w p))
	 (wq (vector4d-w q))
	 (vr (vector3d-add (vector3d-cross vp vq)
			   (vector3d-scale wp vq)
			   (vector3d-scale wq vp))))
    (make-vector4d (- (* wp wq) (vector3d-scalar vp vq))
		   (vector3d-x vr) (vector3d-y vr) (vector3d-z vr))))

(defun vector4d-inverse (q)
  (vector4d-scale (/ 1 (vector4d-norm q)) (vector4d-conjugate q)))

(defun vector4d-unit-p (q)
  (= 1 (vector4d-norm q)))

(define-constant +vector4d-*-identity+ (make-vector4d 1 0 0 0) :test 'vector4d=)
(define-constant +vector4d-+-identity+ (make-vector4d 0 0 0 0) :test 'vector4d=)

(defun vector3d-to-4d (v)
  (make-vector4d 0 (vector3d-x v) (vector3d-y v) (vector3d-z v)))

(defun vector4d-to-3d (q)
  (assert (= 0 (vector4d-w q)))
  (make-vector3d (vector4d-x q) (vector4d-y q) (vector4d-z q)))

(defun vector3d-rotate-quaternion (v q)
  (assert (vector4d-unit-p q))
  (vector4d-to-3d
   (vector4d-* (vector4d-* q (vector3d-to-4d v))
	       (vector4d-inverse q))))

(defun vector3d-rotate-axis (v axis angle)
  (assert (= 1 (vector3d-norm axis)))
  (let* ((angle2 (/ angle 2))
	 (u-axis (vector3d-scale (sin-rational angle2) axis)))
    (vector3d-rotate-quaternion v (make-vector4d (cos-rational angle2)
						 (vector3d-x u-axis)
						 (vector3d-y u-axis)
						 (vector3d-z u-axis)))))

(defun unit-circle-parametrization (pt)
  (let ((d (+ 1 (pow2 pt))))
    (make-vector2d (/ (- 1 (pow2 pt)) d)
		   (/ (* 2 pt) d))))

(defun unit-sphere-parametrization (ps pt)
  (let ((d (+ (pow2 ps) (pow2 pt) 1)))
    (make-vector3d (/ (* 2 ps) d)
		   (/ (* 2 pt) d)
		   (/ (+ (pow2 ps) (pow2 pt) -1) d))))

(defun valid-plane (plane)
  (/= 0 (vector3d-scalar (plane-normal plane) (plane-normal plane))))

(defun vector3d-intersect-line-plane (line plane)
  (assert (valid-plane plane))
  (let* ((diff (vector3d-sub (plane-origin plane) (line-origin line)))
	 (line-fraction (vector3d-scalar diff (plane-normal plane)))
	 (intersection (vector3d-add (line-origin line)
				     (vector3d-scale line-fraction
						     (line-direction line)))))
    (values intersection line-fraction)))

(defun plane-side (plane point)
  "Return which side a point is on. 0 is on the plane."
  (let ((diff (vector3d-sub point (plane-origin plane))))
    (vector3d-scalar diff (plane-normal plane))))

(defun vector3d-perpendicular-any (a)
  (if (null (vector3d-colinear-scale a +unit3d-x+))
      (vector3d-cross a +unit3d-x+)
      (vector3d-cross a +unit3d-y+)))

;;;; matrix stuff

(defun make-matrix (&rest vectors)
  (assert (and (> (length vectors) 0)
	       (all-if (lambda (x) (= (length (car vectors)) (length x)))
		       vectors)))
  (let ((rows (length vectors))
	(cols (length (car vectors))))
    (make-array (list rows cols) :initial-contents vectors)))

(defun matrix-column (matrix column)
  (let-array-dims matrix
    (loop for i below rows collect
	 (aref matrix i column))))

(defun matrix-row (matrix row)
  (let-array-dims matrix
    (loop for i below cols collect
	 (aref matrix row i))))

(defun matrix2-determinant (m)
  "Return the determinant of the top-left-most 2x2 matrix in M."
  (- (* (aref m 0 0) (aref m 1 1))
     (* (aref m 0 1) (aref m 1 0))))

(defun matrix3-determinant (m)
  "Return the determinant of the top-left-most 3x3 matrix in M."
  ;; (det M) != 0 <=> M is solvable
  (- (+ (* (aref m 0 0) (aref m 1 1) (aref m 2 2))
	(* (aref m 0 1) (aref m 1 2) (aref m 2 0))
	(* (aref m 0 2) (aref m 1 0) (aref m 2 1)))
     (* (aref m 0 0) (aref m 1 2) (aref m 2 1))
     (* (aref m 0 1) (aref m 1 0) (aref m 2 2))
     (* (aref m 0 2) (aref m 1 1) (aref m 2 0))))

(defun lineqsolve-make0 (matrix to0row to0col row)
  "set to0col to zero by adding a multiple of row to to0row"
  (let ((factor (/ (aref matrix to0row to0col) (aref matrix row to0col))))
    (let-array-dims matrix
      (loop for i from to0col upto maxcol do
	   (symbol-macrolet ((dst (aref matrix to0row i))
			     (src (aref matrix row i)))
	     (let ((x (* src factor)))
	       ;; (format t "src:~A dst:~A~&" src dst)
	       (setf dst (- dst x))
	       ;; (format t "src:~A dst:~A~&" src (aref matrix to0row i))
	     ))))))

(defun lineqsolve-backward (matrix diagonal solutions)
;;  (declare (optimize (debug 3)))
;  (format t "remaining:~A solutions:~A matrix:~A~&" remaining solutions matrix)
  (if (< diagonal 0)
      solutions
      (let-array-dims matrix
	;; set (r,c) with r=c=diagonal implicitly to 1; 0 for r<diagonal
	(symbol-macrolet ((coeff (aref matrix diagonal diagonal))
			  (value (aref matrix diagonal maxcol)))
	  (if (= 0 coeff)
	      nil
	      (let* ((solution (/ value coeff)))
		(loop for i from diagonal downto 0 do
		     (symbol-macrolet ((ivalue (aref matrix i maxcol))
				       (icoeff (aref matrix i diagonal)))
		       (setf ivalue (- ivalue (* icoeff solution)))))
		(lineqsolve-backward matrix
				     (1- diagonal)
				     (cons solution solutions))))))))

(defun array-swap-rows (matrix r0 r1)
  "Swaps the two array rows with indices r0 and r1."
  (let-array-dims matrix 
    (let ((r0-i (array-row-major-index matrix r0 0))
	  (r1-i (array-row-major-index matrix r1 0)))
      (loop for col upto maxcol
	 do (let* ((temp
		    (row-major-aref matrix (+ r0-i col))))
	      (setf (row-major-aref matrix (+ r0-i col))
		    (row-major-aref matrix (+ r1-i col)))
	      (setf (row-major-aref matrix (+ r1-i col))
		    temp))))))

(defun lineqsolve-forward (matrix &optional (diagonal 0))
  "Converts MATRIX into a upper triangular matrix (with the diagonal entries not
necessarily 1). DIAGONAL must be set to 0"
  ;; all (r,c) with r>diagonal,col=diagonal should be set to 0
  ;;(declare (optimize (debug 3)))
  (let-array-dims matrix
    (if (>= diagonal maxrow)
	(lineqsolve-backward matrix
			     maxrow
			     nil)
	(let ((not0row (loop for i from diagonal upto maxrow
			  when (/= 0 (aref matrix i diagonal))
			  return i)))
;;	  (format t "diagonal:~A not0row:~A matrix:~A~&" diagonal not0row matrix)
	  (if not0row
	      (progn
		(if (/= not0row diagonal)
		    (array-swap-rows matrix not0row diagonal))
		(loop for r from (1+ not0row) upto maxrow
		   do (lineqsolve-make0 matrix r diagonal diagonal))
		(lineqsolve-forward matrix (1+ diagonal)))
	      nil)))))

(defun lineqsolve (matrix)
  (declare (type array matrix))
  ;; solve the linear equation in matrix
  (lineqsolve-forward matrix 0))

(defun matrix-inverse (matrix)
  "Return the inverse matrix of MATRIX, or NIL if MATRIX is not invertible."
  ;;        (j k l)
  ;;        (m n o)
  ;;        (p q r)
  ;; (a b c) 1 0 0
  ;; (d e f) 0 1 0
  ;; (g h i) 0 0 1
  ;; gauss-jordan would be faster
  (let-array-dims matrix
    (assert (= rows cols))
    (let ((m-1 (make-array (list rows cols))))
      (loop for i below rows do
	   (let ((m (adjust-array (copy-array matrix :adjustable t)
				  (list rows (1+ cols))
				  :initial-element 0)))
	     (setf (aref m i cols) 1)
	     (let ((solution (lineqsolve m)))
	       (if (null solution)
		   (return-from matrix-inverse nil))
	       (loop
		  for s in solution
		  for j from 0 do
		    (setf (aref m-1 j i) s)))))
      m-1)))

(defun matrix-* (m n)
  "Return the matrix product of m * n."
  (let* ((m-dimensions (array-dimensions m))
	 (n-dimensions (array-dimensions n)))
    (assert (= (length m-dimensions) 2))
    (assert (= (length n-dimensions) 2))
    (let* ((m-rows (car m-dimensions))
	   (m-cols (cadr m-dimensions))
	   (n-rows (car n-dimensions))
	   (n-cols (cadr n-dimensions))
	   (p (make-array (list m-rows n-cols))))
      (assert (= m-cols n-rows))
      (loop for r below m-rows do
	   (loop for c below n-cols do
		(setf (aref p r c)
		      (loop for i below m-cols sum
			   (* (aref m r i) (aref n i c))))))
      p)))

(defun matrix-vector-* (m v)
  "Return the vector result of (matrix m) * (vector v)"
  (let* ((vl (car (array-dimensions v)))
	 (vm (make-array (list vl 1) :displaced-to v))
	 (rm (matrix-* m vm))
	 (rv (make-array (list vl) :displaced-to rm)))
    (apply #'make-vector (vector->list rv))))

(defun matrix-identity (dims)
  (let ((m (make-array (list dims dims) :element-type 'bit)))
    (loop for i below dims do
	 (setf (aref m i i) 1))
    m))

(defun test-lineqsolve (dims &optional fn-determinant)
  (let* ((var (loop repeat dims collect (1+ (random 1000))))
	 (contents (loop repeat dims collect
			(let ((coeff (loop repeat dims collect (random 10))))
			  (append coeff
				  (list (apply #'+
					       (map 'list
						    (lambda (x y) (* x y))
						    var coeff)))))))

	 (a (make-array `(,dims ,(1+ dims)) :initial-contents contents))
	 (d 1))
    (if fn-determinant
	(setf d (funcall fn-determinant a)))
    (format t "a:~A var:~A d:~A~%" a var d)
    (or (= d 0) (equal var (lineqsolve a)))))

(defun test-matrix-*-inverse (dims &optional fn-determinant)
  (let ((m (make-array (list dims dims)))
	(d 1))
    (loop for r below dims do
	 (loop for c below dims do
	      (setf (aref m r c) (random 1000))))
    (if fn-determinant
	(setf d (funcall fn-determinant m)))
    (if (= d 0)
	t
	(let* ((m-1 (matrix-inverse m))
	       (mp1 (matrix-* m m-1))
	       (mp2 (matrix-* m-1 m)))
	  (and (equal-array mp1 mp2)
	       (equal-array mp1 (matrix-identity dims)))))))

;;;; high-level geometry stuff

(defun projector-into (&rest vectors)
  "Return a function that accepts a vector and returns a vector. The
function projects the vector into the coordinate system given by the VECTORS."
  (assert (and (> (length vectors) 0)
	       (= (length vectors) (length (car vectors)))))
  (let* ((dims (length vectors))
	 (m (make-array (list dims dims)
			:initial-contents (mapcar #'vector->list vectors)))
	 (m-1 (matrix-inverse m)))
    ;;(format t "~A~%" m)
    (if (null m-1)
	nil
	(lambda (v)
	  (declare (type vector v))
	  (matrix-vector-* m-1 v)))))

(defun project-into (vector &rest vectors)
  (let ((projector (apply #'projector-into vectors)))
    (funcall projector vector)))

(defun test-project-into ()
  (vector3d= #(1/2 1/2 1/2) (project-into (make-vector3d 1 1 1)
					  (vector3d-scale 2 +unit3d-x+)
					  (vector3d-scale 2 +unit3d-y+)
					  (vector3d-scale 2 +unit3d-z+))))



;;;; polygon stuff

;;(defstruct (polygon (:constructor make-polygon (points lines)))
;;  points
;;  lines)

;;(defun make-polygon-circular (points)
;;  (make-polygon points (mappair* #'list points)))

(defun make-polygon (points)
  points)

(defun eql-polygon (a b)
  "Return T if polygon A and B have (equal vector=) points in the same order."
  (mapc (lambda (x y) (if (not (vector= x y)) (return-from eql-polygon nil)))
	a b)
  t)

;;(defun polygon-p (p)
;;  ;; no 3 consecutive points colinear

(defun equal-polygon (a b)
  "Return T if polygon A is only rotated with respect to polygon B."
  (or (eq a b) ;; for a,b = nil
      (let* ((point (car a))
	     (b-start 0))
	(loop for b-pos = (position point b :start b-start :test #'vector=)
	   until (null b-pos)
	   thereis (eql-polygon a (rotate (copy-list b) (- b-pos)))
	   do (setf b-start (1+ b-pos))))))

(defun equal*-polygon (a b)
  "Return T if polygon A is equal-polygon to polygon B or its reverse."
  (or (equal-polygon a b) (equal-polygon a (reverse b))))

(defun convex-polygon2d-p (polygon)
  "Is POLYGON convex?"
  (let (sign-all)
    (mapl* 3
	   (lambda (a b c)
	     (let ((sign (vector-scalar
			  (vector2d-perpendicular (vector-sub a b))
			  (vector-sub a c))))
	       (if (null sign-all)
		   (setf sign-all sign)
		   (if (< (* sign sign-all) 0)
		       (return-from convex-polygon2d-p nil)))))
	   polygon))
  t)

(defun map-polygon (function polygon)
  (apply #'make-polygon (map 'list function polygon)))

(defun intersect2d-line-line (line1 line2)
  ;; what holds at the intersection of two lines?
  ;; a1 + f1*d1 = a2 + f2*d2
  ;; f1*d1 - f2*d2 = a2 - a1
  ;; f1*d1 + f2*-d2 = a2 - a1
  ;; (d1,-d2) * (f1,f2) = a2 - a1
  ;; (d1,-d2) means the matrix with d1,-d2 in the columns
  ;;
  ;;          f1           d1a  d1b
  ;;          f2           -d2a -d2b
  ;; d1a -d2a    <=> f1 f2
  ;; d1b -d2b
  ;; M means ((d1a d1b) (-d2a -d2b))
  ;; 
  ;; (f1,f2) * M = a2 - a1
  ;; (f1,f2) * M * M^-1 = (a2 - a1) * M^-1
  ;; (f1,f2) = (a2 - a1) * M^-1
  (let* ((m-1 (matrix-inverse
	       (make-matrix (line-direction line1)
			    (vector-sub (line-direction line2)))))
	 (origindiff (vector->list (vector-sub (line-origin line2)
					       (line-origin line1))))
	 (morigindiff (make-array '(1 2)
				  :initial-contents (list origindiff))))
    ;;(format t "m-1:~A origindiff:~A~%" m-1 origindiff)
    (if (null m-1)
	nil
	(matrix-row (matrix-* morigindiff m-1) 0))))

(defun test-intersect2d-line-line ()
  (and
   (equal (intersect2d-line-line (make-line (make-vector 0 0)
					    (make-vector 4 2))
				 (make-line (make-vector 1 2)
					    (make-vector 3 -3)))
	  '(1/2 1/3))
   (equal (intersect2d-line-line (make-line (make-vector 0 0)
					    (make-vector 4 2))
				 (make-line (make-vector 1 1)
					    (make-vector -1 -1)))
	  '(0 1))))

(defun cut-polygon2d (polygon line)
  "Cut POLYGON with LINE and return the two resulting polygons.
The resulting polygons have the same rotation direction as POLYGON.
Single vertices or lines are not considered as polygons.
ex: (defparameter +polygon+ '(#(0 0) #(1 2) #(2 3) #(2 1) #(2 0)))
    (cut-polygon2d +polygon+ (make-line (make-vector 0 1) (make-vector 1 0)))"
  (assert (convex-polygon2d-p polygon))
  (assert (line-p line))
  (let ((linedir-perp (vector2d-perpendicular (line-direction line)))
	neg-side
	pos-side)
    (labels ((side (p)
	       (sgn (vector-scalar linedir-perp
				   (vector-sub (line-origin line) p)))))
      (mapcar (lambda (a b)
		(let ((side-a (side a))
		      (side-b (side b)))
		  (ecase side-a
		    (-1 (push a neg-side))
		    (1 (push a pos-side))
		    (0 (progn (push a neg-side)
			      (push a pos-side))))
		  ;;(prind a side-a b side-b)
		  (if (= -1 (* side-a side-b))
		      (let* ((dir (vector-sub a b))
			     (abline (make-line b dir))
			     (factor (car (intersect2d-line-line abline
								 line))))
			(if (and (/= 0 factor) (/= 1 factor))
			    (let ((c (line-interpolate abline factor)))
			      (push c neg-side)
			      (push c pos-side)))))))
		  ;;(prind neg-side pos-side)))
	      (rotate (copy-list polygon))
	      polygon)
;;      (prind neg-side pos-side)
      (values (if (> (length neg-side) 2)
		  (nreverse neg-side))
	      (if (> (length pos-side) 2)
		  (nreverse pos-side))))))

(defun test-cut-polygon2d ()
  (labels ((test (polygon line neg-side pos-side)
	     (multiple-value-bind (neg pos)
		 (cut-polygon2d polygon line)
	       (and (equal-polygon neg-side neg)
		    (equal-polygon pos-side pos))))
	   (rot (polygon line neg-side pos-side)
	     (loop for i below (length polygon) always
		  (test (rotate (copy-list polygon) i)
			line neg-side pos-side)))
	   (inv (polygon line neg-side pos-side)
	     (and (rot polygon line neg-side pos-side)
		  (rot polygon (make-line (vector-add (line-origin line)
						      (line-direction line))
					  (vector-sub (line-direction line)))
		       pos-side neg-side)
		  (rot polygon (make-line (line-origin line)
					  (vector-sub (line-direction line)))
		       pos-side neg-side))))
    (let ((polygon '(#(0 0) #(1 2) #(2 3) #(2 1) #(2 0))))
      (and (inv polygon (make-line (make-vector 0 1) (make-vector 1 0))
		'(#(2 0) #(0 0) #(1/2 1) #(2 1))
		'(#(1/2 1) #(1 2) #(2 3) #(2 1))) ;; cut one point, one line
	   (inv polygon (make-line (make-vector 0 1) (make-vector 2 1))
		'(#(2 0) #(0 0) #(2/3 4/3) #(2 2) #(2 1))
		'(#(2/3 4/3) #(1 2) #(2 3) #(2 2))) ;; cut two lines
	   (inv polygon (make-line (make-vector 0 0) (make-vector 1 0))
		nil
		polygon) ;; cut two points
	   (inv polygon (make-line (make-vector 0 0) (make-vector 1 -1))
		nil
		polygon) ;; touch point
	   (inv polygon (make-line (make-vector -1 0) (make-vector 1 -1))
		nil
		polygon) ;; outside
	   ))))

