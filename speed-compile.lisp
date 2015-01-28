(load "~/quicklisp/setup.lisp")
(ql:quickload 'utils)
(use-package 'utils)

(defparameter +fill-one-in-possible-head+
  `(lambda (possible-field)))
(defparameter +fill-one-in-possible-body+
  `((block fill-one-in-possible
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
				possible-field)))))))))
    possible-field))

(defun one-in-possible (&rest rest)
  (declare (ignore rest)))
(defun mapc-row (&rest rest)
  (declare (ignore rest)))
(defun mapc-column (&rest rest)
  (declare (ignore rest)))
(defun mapc-niner (&rest rest)
  (declare (ignore rest)))


#|
`(lambda (a b)
   (declare (optimize (compilation-speed ,compilation-speed)))
   (labels ((rec (a b)
	      (if (<= a 0) b (rec (1- a) (1+ b)))))
     (rec a b)))
|#

(defun timesec-compile (compilation-speed)
  (let ((fe (append +fill-one-in-possible-head+
		    `((declare (optimize (compilation-speed ,compilation-speed) (debug 0) (safety 0) (space 0) (speed 0))))
		    +fill-one-in-possible-body+)))
    (timesec (lambda () (eval fe)))))
;; there doesn't seem to be a big difference in compiling with (compilation-speed 3) or 0:
;; CL-USER> (list (nth-value 1 (timesec-compile 3)) (nth-value 1 (timesec-compile 0)))
;; (0.016d0 0.01625d0)
