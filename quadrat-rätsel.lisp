;;Frage von Susi: gibt es eine Anordnung von Quadraten mit lauter verschiedenen Kantenlängen (bei dem das kleinste Quadrat Kantenlänge 2 hat, das größte Länge 50, und Kantenlängen dazwischen sind ganzzahlig), und die Kantenlänge des gesamten Quadrats ist 112.
;;Antwort von Susi: ja, und die minimale Kantenlänge des gesamten Quadrats, das aus der Summe von Quadraten mit kleinen Kantenlängen besteht, die alle durchgehend vorhanden sind, ist 112.
;; Dieses Skript soll diese Anordnung berechnen.
;; Die Lösung von Susi ist: 2, 4, 6, 7, 8, 9, 11, 15, 16, 17, 18, 19, 24, 25, 27, 29, 33, 35, 37, 42, 50.

;; 1. Frage: welche Kantenlängen geben zusammen 112?

(defparameter *smalls-solution* (list 2 4 6 7 8 9 11 15 16 17 18 19 24 25 27 29 33 35 37 42 50))

(defun enumerate-edge-sums (big &optional (smalls (loop for i from 2 upto (1- big) collect i)))
  "Enumerate all sets pairwise different integer numbers that have a sum of BIG. SMALLS is a list of the permissible (integer) lengths."
  (declare (type integer big) (type list smalls))
  (labels ((rec (sum used available)
	     "AVAILABLE must be sorted by length (in ascending order)."
	     (declare (type integer sum) (type list used available))
	     ;;(print (list sum used available))
	     ;;(when (< sum (* big 1/10))
	     ;;  (print (list "sum" sum "used" used)))
	     (if (> sum big)
		 nil
		 (if (= sum big)
		     (progn
		       ;;(print (list "found solution" used))
		       (list used))
		     (loop for a on available nconc
			  (let* ((new-square (car a))
				 (new-available (cdr a))
				 (new-used (cons new-square used)))
			    (rec (+ sum new-square) new-used new-available)))))))
    (rec 0 nil smalls)))

;; Now that enumerate-edge-sums can enumerate all solutions for the edges of the big square:
(defparameter *possible-edges* (enumerate-edge-sums 112 *smalls-solution*))
;; we need to find which small squares are possible in the corners of the big square.

(defun intersect-sets (set1 set2 &optional (test 'eql))
  "Given two lists SET1 and SET2, return the elements that occur in both lists."
  ;; TODO: implement accepting more than 2 sets.
  (let ((ht (make-hash-table :test test)))
    (loop for i in set1 do (setf (gethash i ht) t))
    (loop for i in set2 nconc
	 (multiple-value-bind (value present) (gethash i ht)
	   (declare (ignore value))
	   (if present
	       (list i)
	       nil)))))

(defun unique (list &key (test 'eql))
  "Return a new list with all duplicate entries in LIST removed."
  (let ((ht (make-hash-table :test test)))
    (loop for i in list do (setf (gethash i ht) t))
    (loop for k being each hash-key in ht collect k)))

;;;; Given the list of lists with (small) square edges, EDGES, enumerate all possible edges that have exactly corner pieces."
;; possibilities to do this
;; - find combinations of 4 edges which have exactly 4 overlapping (small square) edges. These 4 overlapping edges then must be the corner squares. (complicated to program.)

(defun enumerate-corners-adjacent (edges)
  "Given the list of lists with (small) square edges, EDGES, enumerate all possible edges that have exactly corner pieces."
  (declare (type list edges))
  ;; possibilities to do this
  ;; - make a list of all pairs which have exactly one overlap. These must be two adjacent edges. (not feasible: the pairs exhaust 512 MB memory.)
  (labels ((adjacent-edges (edges)
	     (loop for e on edges do
		  (let ((e1 (car e)))
		    (loop for e2 in (cdr e) do
			 (let ((i (intersect-sets e1 e2)))
			   (if (= 1 (length i))
			       (list (cons e1 e2))
			       nil)))))))
    (adjacent-edges edges)))

(defun enumerate-edge-pairs (edges)
  "Given the list of lists with (small) square edges, EDGES, enumerate all possible pairs of edges that share exactly 1 corner piece (and could therefore be next to each other)."
  (declare (type list edges))
  ;; possibilities to do this
  ;; - make a list of all small square edge lengths occurring in *possible-edges*, and store all *possible-edges* that have such an edge length in a list. Then, enumerate all pairs of edges sharing exactly one square length.
  (let ((ht (make-hash-table)))
    (loop for e in edges do
	 (loop for length in e do
	      (setf (gethash length ht) (cons e (gethash length ht nil)))))
    ;; now look at one slot of HT: enumerate all pairs of entries in the slot, and keep those that share only one square length.
    (let ((pairs-ht (make-hash-table :test 'equal)))
      (loop for length being the hash-key of ht using (hash-value edges) do
	   (loop for e on edges do
		(let ((e1 (car e)))
		  (loop for e2 in (cdr e) do
		       (when (= 1 (length (intersect-sets e1 e2)))
			 (multiple-value-bind (e1-ht present) (gethash e1 pairs-ht (make-hash-table :test 'equal))
			   (when (not present)
			     (setf (gethash e1 pairs-ht) e1-ht))
			   (setf (gethash e2 e1-ht) t))
			 (multiple-value-bind (e2-ht present) (gethash e2 pairs-ht (make-hash-table :test 'equal))
			   (when (not present)
			     (setf (gethash e2 pairs-ht) e2-ht))
			   (setf (gethash e1 e2-ht) t)))))))
      pairs-ht)))

(defparameter *possible-edge-pairs* (enumerate-edge-pairs *possible-edges*))

(defun enumerate-edge-quadruples (pairs-ht &key (count-only nil))
  "Given the hash-table with possible adjacent edge pairs, EDGES, enumerate all possible edges that share exactly 4 corner pieces."
  ;; now extend each of the two edges (which share one corner square) with another edge by looking them up in PAIRS-HT. First find 3-edge tuples that share only 2 square lengths, then enumerate the possible combinations of the found 3-edge tuples with another pair.
  (let ((quadruples nil)
	(quadruples-count 0))
    (loop for e1 being the hash-key in pairs-ht using (hash-value e1-ht) do
	 (let ((e2-or-e4-list (loop for e being the hash-key in e1-ht collect e)))
	   (loop for e24 on e2-or-e4-list for e2 = (car e24) do
		(loop for e4 in e24 do
		   ;; do e4, e1, and e2 share only 2 square lengths? we already know that e4 and e1 share exactly 1 length, and e1 and e2 share exactly 1 length. So we only need to check that e4 and e2 share 0 lengths.
		     (when (= 0 (length (intersect-sets e4 e2)))
		       ;; look for an edge that is both a neighbor of e2 and e4 (bot is not equal to e1), and shares no square length with e1.
		       (let ((e2-neighbors (loop for e being the hash-key in (gethash e2 pairs-ht) when (not (equal e e1)) collect e)))
			 (loop for possible-e3 in e2-neighbors do
			      (when (and (nth-value 1 (gethash possible-e3 (gethash e4 pairs-ht)))
					 (= 0 (length (intersect-sets possible-e3 e1))))
				(let ((q (list e1 e2 possible-e3 e4)))
				  (incf quadruples-count)
				  (when (= 0 (mod quadruples-count 10000))
				    (print quadruples-count))
				  (when (not count-only)
				    (push q quadruples)))))))))))
    (values quadruples quadruples-count)))

;; enumerate-edge-quadruples exhausts a heap of 512 MB.
