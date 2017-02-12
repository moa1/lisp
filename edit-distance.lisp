(defun make-score-fn (match-score mismatch-score &key (test #'eq))
  "Make a function that is passed two elements of two sequences.
If the two elements are equal under TEST, the function returns MATCH-SCORE, or MISMATCH-SCORE otherwise."
  (lambda (a b)
    (if (funcall test a b) match-score mismatch-score)))

(defun needleman-wunsch (s1 s2 match-score-fn gap-score)
  "This is the Needleman-Wunsch algorithm (edit distance of two sequences, global alignment).
S1 and S2 are sequences, whose elements are compared with the function MATCH-SCORE-FN.
MATCH-SCORE-FN is a function that is passed two elements, one from S1 and one from S2, and must return a REAL, the similarity score of the two elements.
GAP-SCORE is a REAL representing the score of inserting a gap.
Return two matrices of dimensions (LENGTH S1)x(LENGTH S2). The first matrix M contains in each element, (AREF M X Y), the best score achievable when aligning the subsequences of S1 and S2 up to this point, i.e. (SUBSEQ S1 0 (1+ X)) and (SUBSEQ S2 0 (1+ Y)). The second matrix W contains in each element (AREF W X Y) the list of directions that the best alignments up to (X,Y) came from."
  (declare (type sequence s1 s2)
	   (type (function (t t) real) match-score-fn)
	   (type real gap-score))
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
		(when (= max-score fromup)
		  (push 'up dir))
		(when (= max-score fromleft)
		  (push 'left dir))
		(when (= max-score fromdiag)
		  (push 'diag dir))
		(setf (aref w x y) dir))))
    (values m w)))

(defun print-score-matrix (m)
  (let ((s1l (array-dimension m 0))
	(s2l (array-dimension m 1)))
    (loop for y below s2l do
	 (loop for x below s1l do
	      (format t "~A " (aref m x y)))
	 (format t "~%"))))

(defun best-alignment-score (m)
  "Return the score of the best-scoring alignment(s)."
  (aref m (1- (array-dimension m 0)) (1- (array-dimension m 1))))

#|
(let ((score-fn (make-score-fn 3 -2)))
  (defparameter *edm* (nth-value 0 (needleman-wunsch "abdbc" "abc" score-fn -1)))
  (defparameter *edw* (nth-value 1 (needleman-wunsch "abdbc" "abc" score-fn -1))))
|#

(defun make-tracebacks (w)
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

(defun traceback-to-alignment (s1 s2 traceback gap-marker &optional (alignment-element-type 'symbol))
  "Return two vectors representing the alignments of the two sequences S1 and S2.
TRACEBACK is one of the tracebacks obtained using MAKE-TRACEBACKS.
GAP-MARKER is the element inserted in the alignment to indicate that there is a gap in the sequence.
ALIGNMENT-ELEMENT-TYPE is the type of the elements of the two alignment vectors."
  (declare (type sequence s1 s2)
	   (type list traceback)
	   (type t gap-marker))
  (let* ((ali-length (length traceback))
	 (ali1 (make-array ali-length :element-type alignment-element-type))
	 (ali2 (make-array ali-length :element-type alignment-element-type)))
    (labels ((rec (tb tbi i1 i2)
	       (if (null tb)
		   (values ali1 ali2)
		   (ecase (car tb)
		     (up (setf (aref ali1 tbi) gap-marker)
			 (setf (aref ali2 tbi) (elt s2 i2))
			 (rec (cdr tb) (1+ tbi) i1 (1+ i2)))
		     (left (setf (aref ali1 tbi) (elt s1 i1))
			   (setf (aref ali2 tbi) gap-marker)
			   (rec (cdr tb) (1+ tbi) (1+ i1) i2))
		     (diag (setf (aref ali1 tbi) (elt s1 i1))
			   (setf (aref ali2 tbi) (elt s2 i2))
			   (rec (cdr tb) (1+ tbi) (1+ i1) (1+ i2)))))))
      (rec traceback 0 0 0))))

(defun traceback-to-string-alignment (s1 s2 traceback gap-char)
  "Return two strings representing the alignments of the two strings S1 and S2.
TRACEBACK is one of the tracebacks obtained using MAKE-TRACEBACKS.
GAP-CHAR is the character inserted in the alignment to indicate that there is a gap in the string."
  (declare (type string s1 s2)
	   (type list traceback)
	   (type character gap-char))
  (traceback-to-alignment s1 s2 traceback gap-char 'character))

(defun edit-distance-score-and-alignments (s1 s2 match-score mismatch-score gap-score gap-marker &optional (alignment-element-type 'symbol))
  "Return edit-distance score and alignments for the sequences S1 and S2."
  (let ((score-fn (make-score-fn match-score mismatch-score)))
    (multiple-value-bind (m w) (needleman-wunsch s1 s2 score-fn gap-score)
      (let* ((tbs (make-tracebacks w))
	     (score (best-alignment-score m)))
	(values score 
		(mapcar (lambda (tb) (multiple-value-list (traceback-to-alignment s1 s2 tb gap-marker alignment-element-type)))
			tbs))))))

#|
(let ((seq1 '(a b d b c)) (seq2 '(a b c)))
  (multiple-value-bind (m w) (needleman-wunsch seq1 seq2 (make-score-fn 3 -2) -1)
    (traceback-to-alignment seq1 seq2 (cadr (make-tracebacks w)) '-)))
|#
