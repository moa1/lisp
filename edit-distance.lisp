(defun edit-distance-matrix (s1 s2 match-score-fn gap-score)
  "Return the filled edit-distance matrix.
S1 and S2 are sequences, whose elements are compared with the function TEST.
MATCH-SCORE-FN is a function with two parameters that returns the match or mismatch score."
  (let* ((s1l (1+ (length s1)))
	 (s2l (1+ (length s2)))
	 (m (make-array (list s1l s2l) :initial-element 0))
	 (w (make-array (list s1l s2l) :initial-element nil)))
    (loop for x below s1l do
	 (setf (aref m x 0) (* x gap-score))
	 (setf (aref w x 0) (list 'left)))
    (setf (aref w 0 0) nil)
    (loop for y from 1 below s2l do
	 (setf (aref m 0 y) (* y gap-score))
	 (setf (aref w 0 y) (list 'up)))
    (loop for x from 1 below s1l do
	 (loop for y from 1 below s2l do
	      (let* ((fromup (+ gap-score (aref m x (1- y))))
		     (fromleft (+ gap-score (aref m (1- x) y)))
		     (fromdiag (+ (aref m (1- x) (1- y)) 
				  (funcall match-score-fn (elt s1 (1- x)) (elt s2 (1- y)))))
		     (max-score (max fromup fromleft fromdiag))
		     (dir nil))
		(setf (aref m x y) max-score)
		(if (= max-score fromup)
		    (setf dir (cons 'up dir)))
		(if (= max-score fromleft)
		    (setf dir (cons 'left dir)))
		(if (= max-score fromdiag)
		    (setf dir (cons 'diag dir)))
		(setf (aref w x y) dir))))
    (values m w)))

(defun make-edit-distance-match-score-fn (match-score mismatch-score &key (test #'eq))
  (lambda (a b)
    (if (funcall test a b) match-score mismatch-score)))

#|
(let ((score-fn (make-edit-distance-match-score-fn 3 -2)))
  (defparameter *edm* (nth-value 0 (edit-distance-matrix "abdbc" "abc" score-fn -1)))
  (defparameter *edw* (nth-value 1 (edit-distance-matrix "abdbc" "abc" score-fn -1))))
|#

(defun print-edit-distance-matrix (m)
  (let ((s1l (array-dimension m 0))
	(s2l (array-dimension m 1)))
    (loop for y below s2l do
	 (loop for x below s1l do
	      (format t "~A " (aref m x y)))
	 (format t "~%"))))

(defun edit-distance-tracebacks (w)
  "Return a list of the best-scoring alignments."
  (labels ((traceback (x y alignment)
	     (if (and (= 0 x) (= 0 y))
		 (list alignment)
		 (let ((alignments nil))
		   (dolist (dir (aref w x y) alignments)
		     (setf alignments (nconc (ecase dir
					       (up (traceback x (1- y) (cons dir alignment)))
					       (left (traceback (1- x) y (cons dir alignment)))
					       (diag (traceback (1- x) (1- y) (cons dir alignment))))
					     alignments)))))))
    (traceback (1- (array-dimension w 0)) (1- (array-dimension w 1)) nil)))

(defun edit-distance-score (m)
  "Return the score of the best-scoring alignment(s)."
  (aref m (1- (array-dimension m 0)) (1- (array-dimension m 1))))

(defun traceback-to-string-alignment (s1 s2 tb gap-char)
  "Return two strings representing the alignments of the two strings S1 and S2."
  (let* ((ali-length (length tb))
	 (ali1 (make-string ali-length))
	 (ali2 (make-string ali-length)))
    (labels ((rec (tb tbi i1 i2)
	       (if (null tb)
		   (values ali1 ali2)
		   (ecase (car tb)
		     (up (setf (schar ali1 tbi) gap-char)
			 (setf (schar ali2 tbi) (char s2 i2))
			 (rec (cdr tb) (1+ tbi) i1 (1+ i2)))
		     (left (setf (schar ali1 tbi) (char s1 i1))
			   (setf (schar ali2 tbi) gap-char)
			   (rec (cdr tb) (1+ tbi) (1+ i1) i2))
		     (diag (setf (schar ali1 tbi) (char s1 i1))
			   (setf (schar ali2 tbi) (char s2 i2))
			   (rec (cdr tb) (1+ tbi) (1+ i1) (1+ i2)))))))
      (rec tb 0 0 0))))

(defun edit-distance-score-and-string-alignments (s1 s2 match-score mismatch-score gap-score gap-char)
  "Return edit-distance score and alignments for the strings S1 and S2."
  (let ((score-fn (make-edit-distance-match-score-fn match-score mismatch-score)))
    (multiple-value-bind (m w) (edit-distance-matrix s1 s2 score-fn gap-score)
      (let* ((tbs (edit-distance-tracebacks w))
	     (score (edit-distance-score m)))
	(values score 
		(mapcar (lambda (tb) (multiple-value-list (traceback-to-string-alignment s1 s2 tb gap-char)))
			tbs))))))
