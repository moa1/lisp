(asdf:oos 'asdf:load-op 'utils)

(defparameter initial '((T 4 T T T T T 5 T)
			(3 7 T 5 T T T 2 1)
			(T T 2 7 T 4 T 8 T)
			(T 1 3 6 T T T 7 T)
			(T T T T 8 T T 1 T)
			(T T 5 T T 1 2 T 8)
			(T T T T T 9 7 T T)
			(4 8 6 3 5 T T 9 T)
			(T 9 T T T 6 T T T)))

;; the solve-sudoku doesn't work for this sudoku!
;; there is/are a/some circle(s) of dependencies......... :(
(defparameter initial-2 '((7 T T 6 T 9 T T 3)
			  (T T T 7 3 8 T T T)
			  (T T 4 T T T 8 T T)
			  (T 2 T T 6 T T 9 T)
			  (6 T 1 T T T 5 T 2)
			  (T 5 T T 1 T T 7 T)
			  (T T 8 T T T 7 T T)
			  (T T T 1 8 2 T T T)
			  (5 T T 9 T 6 T T 1)))

(defparameter initial-3 '((4 T T T T T T T 1)
			  (T 2 T T 5 9 T T T)
			  (T T 9 T 8 1 5 T T)
			  (T T 4 T T 6 8 1 T)
			  (T T T T 2 T T T T)
			  (T T 3 8 T T T 6 T)
			  (8 T T T T T T T T)
			  (T T T T 3 T 6 5 T)
			  (T 6 T T T 4 T T 3)))

(defparameter initial-empty '((t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)
			      (t t t t t t t t t)))

(defparameter initial-bruteforce '((7 8 5 6 4 9 2 1 3)
				   (2 1 6 7 3 8 9 T T)
				   (3 9 4 5 2 1 8 6 7)
				   (T 2 7 3 6 5 1 9 T)
				   (6 T 1 8 9 7 5 T 2)
				   (T 5 T 2 1 4 T 7 T)
				   (1 6 8 4 5 3 7 2 9)
				   (T 7 T 1 8 2 T T T)
				   (5 T 2 9 7 6 T 8 1)))

(defun listlist->field (listlist)
  "Convert listlist sudoku description to array description."
  (make-array '(9 9)
	      :initial-contents (maptree (lambda (x) (if (eq x t) nil x))
					 listlist)))

(defparameter field (listlist->field initial-2))

(defun mapc-niner (function field x y)
  (let ((bx (truncate x 3))
	(by (truncate y 3)))
    (loop for x from (* 3 bx) upto (+ 2 (* 3 bx))
       do (loop for y from (* 3 by) upto (+ 2 (* 3 by))
	     do (funcall function (aref field y x))))))

(defun mapc-column (function field x y)
  (declare (ignore y))
  (loop for y from 0 upto 8 do
       (funcall function (aref field y x))))

(defun mapc-row (function field x y)
  (declare (ignore x))
  (loop for x from 0 upto 8 do
;;;       (format t "mapc-row|y:~A val:~A~%" y (aref field y x))
       (funcall function (aref field y x))))

(defun mapc-subregion (subregion-function function field x y)
  (funcall subregion-function function field x y))

(defun subregion-collector (subregion-function)
  "return a function that collects (as list) the subregion iterated by
subregion-function."
  (lambda (field x y)
    (let (result)
      (funcall subregion-function
	       (lambda (x)
		 (setf result (cons x result)))
	       field x y)
      (nreverse result))))

(defun niner (field x y)
  "return the niner (as list) that x,y is in"
  (funcall (subregion-collector #'mapc-niner) field x y))

(defun column (field x y)
  "return the column as list that x,y is in"
  (funcall (subregion-collector #'mapc-column) field x y))

(defun row (field x y)
  "return the row as list that x,y is in"
  (funcall (subregion-collector #'mapc-row) field x y))

(defun possible-in-niner-row-col (field x y)
  "return the one remaining possible number, considering all other numbers in
the respective niner, row, column. return nil otherwise.
undefined result for x,y already determined."
  (let* ((filtered (delete-if #'listp (append (niner field x y)
					      (column field x y)
					      (row field x y))))
	 (taken (flatten filtered))
	 (possible (list 1 2 3 4 5 6 7 8 9)))
    (mapc (lambda (x) (setf possible (delete x possible)))
	  taken)
    possible))

(defun fill-possible (field)
  (let ((result (make-array '(9 9))))
    (loop for x from 0 upto 8 do
	 (loop for y from 0 upto 8 do
	      (setf (aref result y x)
		    (if (listp (aref field y x))
			(possible-in-niner-row-col field x y)
			(aref field y x)))))
    result))

(defun one-in-determined? (possible-field x y)
  "return the one remaining possible number, considering all other numbers in
the respective niner, row, column. return nil otherwise.
undefined result for x,y already determined."
  (let ((possible (aref possible-field y x)))
    (if (and (listp possible) (= (length possible) 1))
	(car possible)
	possible)))

(defun fill-one-in-determined (possible-field)
  "Iterate over all positions in sudoku-field field, and fill out fields which
have obviously only one possible solution.
Note: If field is not yet completely solved and has a solution, then this
function should modify at least one position."
  (loop for x from 0 upto 8 do
       (loop for y from 0 upto 8 do
	    (setf (aref field y x) (one-in-determined? possible-field x y))))
  possible-field)

;; consider all the possibilities of not determined positions. is one possible
;; number unique in a specific niner, column, or row? if yes, then it must
;; be there.
(defun one-in-possible (subregion-function possible-field x y)
  "consider the possibilities of the position x,y. is one possible number
unique in the possibilities of a specific niner, column, or row? return the set
of all these unique possible numbers."
  (let* ((collect (subregion-collector subregion-function))
	 (subregion-possible (funcall collect possible-field x y))
	 (filtered-not-determined (delete-if (complement #'listp)
					     subregion-possible))
	 (flat-not-determined (flatten filtered-not-determined)))
;;;    (format t "subregion-function:~A~%one-in-possible subregion-possible:~A~%flat:~A~%" subregion-function subregion-possible flat-not-determined)
    (unique flat-not-determined :only t)))

(defun fill-one-in-possible (possible-field)
  (loop for y upto 8 do
       (loop for x upto 8 do
	  ;; it would be more efficient to precompute columns-unique,
	  ;; rows-unique and niner-unique.
	  ;; there are only 9 of each: 9*3=27, not 9*9*3=81*3
	    (if (listp (aref possible-field y x))
		(let* ((columns-unique (one-in-possible #'mapc-column
							possible-field
							x y))
		       (rows-unique (one-in-possible #'mapc-row
						     possible-field
						     x y))
		       (niner-unique (one-in-possible #'mapc-niner
						      possible-field
						      x y))
		       (possible (aref possible-field y x))
		       (unique-by-column (remove-if (complement
						     (lambda (x)
						       (find x columns-unique)))
						    possible))
		       (unique-by-row (remove-if (complement
						  (lambda (x)
						    (find x rows-unique)))
						 possible))
		       (unique-by-niner (remove-if (complement
						    (lambda (x)
						      (find x niner-unique)))
						   possible)))
;;;		  (format t "x:~A y:~A possible:~A~%" x y possible)
;;; 		  (format t "columns-unique:~A rows-unique:~A niner-unique:~A~%"
;;; 			  columns-unique rows-unique niner-unique)
;;; 		  (format t "possible-by-column:~A~%" unique-by-column)
;;; 		  (format t "possible-by-row:~A~%" unique-by-row)
;;; 		  (format t "possible-by-niner:~A~%" unique-by-niner)
		  (let ((unique (unique
				 (append unique-by-column
					 unique-by-row
					 unique-by-niner))))
;;;		    (format t "unique:~A~%" unique)
		    (assert (<= (length unique) 1))
		    (if (not (null unique))
			(progn
			  (setf (aref possible-field y x)
				(car unique))
			  ;; hmm... recalculating fill-possible might be cool
			  (return-from fill-one-in-possible
			    possible-field))))))))
  possible-field)


(defun bruteforce (possible-array)
  (loop for x upto 8 do
       (loop for y upto 8 do
	    (let ((el (aref possible-array y x)))
	      (if (listp el)
		  (loop for possible in el do
		       (let ((field (copy-array possible-array)))
			 (setf (aref field y x) possible)
			 (return-from bruteforce (bruteforce field))))))))
  possible-array)

(defun solved? (possible-array)
  "return nil if possible-array has any list as element, t otherwise."
  (mapc-array (lambda (x) (if (listp x)
			      (return-from solved? nil)))
	      possible-array)
  t)



(defun solve-sudoku-field (field)
  "Solve the 9x9-sudoku given as 2d-array field. Modifies field."
  (do nil ((solved? field) field)
    (let ((orig (copy-array field)))
      (format t "1) ~A~%" field)
      (setf field (fill-possible field))
      (format t "2) ~A~%" field)
      (fill-one-in-determined field)
      (format t "3) ~A~%" field)
      (setf field (fill-possible field))
      (format t "4) ~A~%" field)
      (fill-one-in-possible field)
      (format t "5) ~A~%" field)
      (if (equal-array orig field)
	  (setf field (bruteforce field))))))

(defun solve-sudoku (2d-list)
  (assert (equal (mapcar #'length 2d-list) '(9 9 9 9 9 9 9 9 9)))
  (solve-sudoku-field (make-array '(9 9) :initial-contents 2d-list)))

