;;(asdf:operate 'asdf:load-op 'utils)

(in-package :utils)

;; (defmacro with-array (accessor-bindings &body forms)
;;   "with-array defines local array accessors, and executes FORMS using the local
;; accessors. ACCESSOR-BINDINGS is a list of MATRIX-ACCESSORS.
;; MATRIX-ACCESSORS is a list of the form: (M &key AREF ROW-MAJOR-AREF)
;; ACC is the symbol used as the accessor for the array A."
;;   `(macrolet ,@(loop for 
;; ((,acc (&rest subscripts

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

(defun matrix-invert (matrix)
  "Return the inverse matrix of MATRIX, or NIL of MATRIX is not invertible."
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
		   (return-from matrix-invert nil))
	       (loop
		  for s in solution
		  for j from 0 do
		    (setf (aref m-1 j i) s)))))
      m-1)))

(defun matrix-product (m n)
  "Return the matrix product of m * n."
  (let* ((m-dimensions (array-dimensions m))
	 (n-dimensions (array-dimensions n))
	 (m-rows (car m-dimensions))
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
    p))

(defun matrix-identity (dims)
  (let ((m (make-array (list dims dims) :element-type 'bit)))
    (loop for i below dims do
	 (setf (aref m i i) 1))
    m))

;;#+()
(defun test-2d ()
  (let ((a (make-array '(2 3) :initial-contents '((3 2 1) (5 6 7)))))
    (format t "new : ~A~&" a)
    ;(lineqsolve a)
    ;should be '(-1 2)
    ;(format t "proc: ~A~&" a)
    a))

(defun test-3d-1 ()
  (let ((a (make-array '(3 4) :initial-contents '((4 3 2 -4) (1 2 3 -6)
						   (1 1 1 -2)))))
    ;(lineqsolve a) should be nil (last row is 0 0 0 -1/5)
    a))

(defun test-3d-2 ()
  (let ((a (make-array '(3 4) :initial-contents '((4 3 2 -4) (1 2 3 -6)
						   (1 2 1 0)))))
    ;; (lineqsolve a) should be '(-1 2 -3)
    a))

(defun test-3d-3 ()
  (let ((a (make-array '(3 4) :initial-contents '((2 4 7 7155)
						  (3 6 6 7956)
						  (2 2 3 3561)))))
    ;; (lineqsolve a) should be '(292 563 617)
    a))

(defun test-3d-4 ()
  (let ((a (make-array '(3 4) :initial-contents '((4 4 2 2230)
						  (2 2 1 1115)
						  (5 1 7 3788)))))
    a))

;;#-()

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

(defun test-matrix-product-invert (dims &optional fn-determinant)
  (let ((m (make-array (list dims dims)))
	(d 1))
    (loop for r below dims do
	 (loop for c below dims do
	      (setf (aref m r c) (random 1000))))
    (if fn-determinant
	(setf d (funcall fn-determinant m)))
    (if (= d 0)
	t
	(let* ((m-1 (matrix-invert m))
	       (mp1 (matrix-product m m-1))
	       (mp2 (matrix-product m-1 m)))
	  (and (equal-array mp1 mp2)
	       (equal-array mp1 (matrix-identity dims)))))))
	       