(asdf:operate 'asdf:load-op 'utils)
(asdf:oos 'asdf:load-op 'series)

(defmacro let-matrix-dims (m &body body)
  (let ((flatbody (flatten body)))
    (labels ((find-in-body (symbol)
	       (find symbol flatbody)))
      (let ((cols (find-in-body 'cols))
	    (rows (find-in-body 'rows))
	    (maxcol (find-in-body 'maxcol))
	    (maxrow (find-in-body 'maxrow))
	    (arrdim (gensym)))
;;	(format t "cols:~A rows:~A maxcol:~A maxrow:~A~&body:~A"
;;		cols rows maxcol maxrow body)
	`(let* ,(append
		 `((,arrdim (array-dimensions ,m)))
		 (if (or cols maxcol)
		     `((cols (cadr ,arrdim))))
		 (if (or rows maxrow)
		     `((rows (car ,arrdim))))
		 (if maxcol
		     `((maxcol (1- cols))))
		 (if maxrow
		     `((maxrow (1- rows)))))
	   ,@body)))))


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
    (let-matrix-dims matrix
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
      (let-matrix-dims matrix
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
  (let-matrix-dims matrix 
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
  (let-matrix-dims matrix
    (if (>= diagonal maxrow)
	(lineqsolve-backward matrix
			     maxrow
			     nil)
	(let ((not0row (loop for i from diagonal upto maxrow
			  when (/= 0 (aref matrix i diagonal))
			  return i)))
;	  (format t "diagonal:~A not0row:~A matrix:~A~&" diagonal not0row matrix)
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

#+()
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

#-()

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


