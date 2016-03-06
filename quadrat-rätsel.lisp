;;;; Quadrat-Rätsel

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-heap)
(ql:quickload :cl-cont)

;;Frage von Susi: gibt es eine Anordnung von Quadraten mit lauter verschiedenen Kantenlängen (bei dem das kleinste Quadrat Kantenlänge 2 hat, das größte Länge 50, und Kantenlängen dazwischen sind ganzzahlig. Es müssen aber nicht alle aufeinanderfolgenden (ganzzahligen) Kantenlängen vorhanden sein.), und die Kantenlänge des gesamten Quadrats ist 112.
;;Antwort von Susi: ja, und die minimale Kantenlänge des gesamten Quadrats, das aus der Summe von Quadraten mit kleinen Kantenlängen besteht, ist 112.
;; Dieses Skript soll die Anordnung der Zusammensetzung der Kanten des großen Quadrats berechnen.
;; Die Lösung: Die Kantenlängen der kleinen Quadrate, die zusammengesetzt ein Quadrat mit Kangenlänge 112 ergeben, sind: 2, 4, 6, 7, 8, 9, 11, 15, 16, 17, 18, 19, 24, 25, 27, 29, 33, 35, 37, 42, 50.

;;;; 1. Frage: welche Kantenlängen geben zusammen 112?

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
;; Note that this approach (of just testing which small squares are possible along the edges of the big square) is too simplistic (and is not able to enumerate all solutions, because there are too many solutions) : It doesn't take into account that some combinations are impossible because their small squares would overlap geometrically.

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


;;;; Gibt es eine Folge von ganzzahligen aufeinanderfolgenden Zahlen (von 1 ab), die, wenn man sie quadriert und die Quadrate summiert, wieder ein Quadrat einer ganzen Zahl ist?

(defun square?-buggy (x)
  "Apparently this function does not return correct values, for example (square? 298149294) returns T, but python says that sqrt(298149294)==17267.000144784848."
  (let* ((s (sqrt x)))
    (if (= (floor s) s)
	t
	nil)))

(defun square?-buggy2 (x)
  "Return T if X is a square of an integer number, NIL otherwise.
I'm not sure this is correct for all X. It probably doesn't work for large X, where (* (sqrt X) (sqrt X)) looses accuracy in the last digits due to rounding by sqrt."
  (let* ((s (sqrt x))
	 (si (floor s)))
    (if (= si s)
	(if (= (* si si) x)
	    t
	    nil)
	nil)))

(defun square? (x)
  "Return T if X is a square of an integer number, NIL otherwise."
  (let* ((s (isqrt x))
	 (p (* s s)))
    (if (= x p)
	t
	nil)))
;; This seems to be correct, since
;; (loop for i below 100000000 always (square? (* i i))) == T
;; (loop for i below 1000000 always (let ((j (random 100000000000000000000000))) (square? (* j j)))) == T

(defun sum-square-1 (upto)
  "Print two numbers (A B). Then the following holds: (= (loop for A below 25 sum (* i i)) (* B B)."
  (labels ((rec (n sum)
	     (when (>= n upto)
	       (return-from rec nil))
	     (let ((s (+ sum (* n n))))
	       ;;(print (list n sum s))
	       (when (square? s)
		 ;; verify that there was no floating-point error
		 (print (list n (floor (sqrt s)))))
	       (rec (1+ n) s))))
    (rec 1 0)))


;;;; Samis Lösung des ursprünglichen Rätsels von Susi.

(defstruct container
  (lowest-gaps (make-instance 'cl-heap:fibonacci-heap) :type cl-heap:fibonacci-heap)
  (sorted-gaps-in-y (make-hash-table)))

(defun init-container (container-edge-length)
  "Create a new container with edge length CONTAINER-EDGE-LENGTH."
  (let ((container (make-container)))
    ))
(defun add-square (edge-length gap container)
  "Add to CONTAINER the square with an edge length of EDGE-LENGTH and its leftmost lowest corner at x coordinate LEFT-LOWER-X and at y coordinate LEFT-LOWER-Y.
Return CONTAINER and a pointer to the newly created gap on top of the square."
  ;; remove GAP and create two gaps GAP1 and GAP2. (Only create gap2 if there is still space on the right in GAP.) Insert GAP1 and GAP2 into the heap and the hash-table. If there are adjacent gaps on the y-coordinate of GAP1, remove them and extend GAP1 on the left and/or right. Return GAP1.
  ;;(cl-heap:delete-from-heap 
  )
(defun remove-square (edge-length  square container)
  "Remove from CONTAINER the square with an edge length of EDGE-LENGTH and its leftmost lowest corner at x coordinate LEFT-LOWER-X and at y coordinate LEFT-LOWER-Y.
Return CONTAINER."
  ;; Find the gap on top of the square and remove it. Then split it into 3 gaps so that the middle gap covers the top of the square. Insert the left and right gap. Move the middle gap to the bottom of the square.
  )
(defun lowest-gap (container)
  "Return the leftmost corner of the lowest gap in the container CONTAINER as the list (LEFT-LOWER-X LEFT-LOWER-Y WIDTH)."
  (let ((gaps (cl-heap:peep-at-heap (container-lowest-gaps container))))
    (car gaps)))

(defun place ( rest-use rest-nouse)
  "Samis Lösung geht so: Lege den nächsten Klotz immer in die unterste Zeile, in der etwas frei ist, und dort möglichst weit links. Lege dann den nächsten möglichen Klotz. Fange dabei immer mit dem größten Klotz an, und wenn dieser nicht passt, probiere den nächsten. Wenn alle Klötze gelegt sind, das Rätsel aber nicht beendet ist (weil im großen Quadrat noch freie Stellen sind), mache einen Backtrack: entferne den zuletzt gelegten Klotz und ersetze ihn durch den Nächsten. Wenn es keinen Nächsten mehr gibt (weil alle zum Backtracken geführt haben), entferne den zweitletzt gelegten Klotz und ersetze ihn durch den nächsten, etc."
  (if (null rest-use)
      'no-solution
      ()))

;;;; Sami's Lösung mit seiner Datenstruktur
;; ist noch nicht fertig wenn ich mich richtig erinnere, TODO: fertigmachen

(defun place-squares (bigl smalls)
  (setf smalls (sort (copy-seq smalls) #'>))
  (print (list "smalls" smalls))
  (let* ((nsmalls (length smalls)))
    (labels ((insert (small-seq mini minh nparts partsw partsh)
	       (let* ((partsw1 (make-array bigl))
		      (partsh1 (make-array bigl))
		      (i 0))
		 (loop for i below mini do (setf (aref partsw1 i) (aref partsw i)))
		 (loop for i from mini for small in small-seq do
		      (setf (aref partsw1 i) small)
		      (setf (aref partsh1 i) (+ minh small)))
		 (loop for i from (+ mini (length small-seq)) for j from (1+ mini) below nparts do
		      (setf (aref partsw1 i) (aref partsw j))
		      (setf (aref partsh1 i) (aref partsh j)))
		 ;; TODO: compress partsw1, partsh1
		 (values partsw1 partsh1)))
	     (try (smalls nparts partsw partsh)
	       (loop until (null smalls) do
		    (labels ((valley ()
			       (let ((minh bigl)
				     (mini -1))
				 (do* ((i 0 (1+ i))
				       (h (aref partsh i) (aref partsh i)))
				      ((>= i nparts))
				   (print (list i h))
				   (when (< h minh)
				     (setf minh h)
				     (setf mini i)))
				 (values mini minh)))
			     (combinations (smalls w)
			       "Find a combination of lengths in SMALLS so that their sum equals W."
			       (cl-cont:with-call/cc
				 (labels ((rec (smalls result resultw)
					    ;;(print (list w smalls result resultw))
					    (cond
					      ((> resultw w) nil)
					      ((= resultw w) (cl-cont:call/cc (lambda (k) (values result k))))
					      (t (loop for rest-smalls on smalls for s = (car rest-smalls) do
						      (rec (cdr rest-smalls) (cons s result) (+ resultw s)))))))
				   (rec smalls nil 0)))))
		      (multiple-value-bind (mini minh) (valley)
			(let* ((minw (aref partsw mini)))
			  (multiple-value-bind (comb next-combination-function) (combinations smalls minw)
			    (loop while (not (null comb)) do
				 (print (list "minw" minw "comb" comb))
			       ;;(multiple-value-bind (partsw1 partsh1) (insert comb mini minh nparts partsw partsh)
				 (setf comb (funcall next-combination-function))
				 ))
			  (return-from place-squares)))))))
      (let ((nparts 1)
	    (partsw (make-array bigl :initial-element bigl))
	    (partsh (make-array bigl :initial-element 0)))
	(setf (aref partsw 0) (car smalls)
	      (aref partsw 1) (- bigl (car smalls))
	      (aref partsh 0) (car smalls)
	      (aref partsh 1) 0
	      nparts 2)
	(try smalls nparts partsw partsh)))))
