;;; organism implementation in lisp

;; TODO: speed up by finding frequent gene subsequences by randomly doing pairwise alignments and keeping the longest gap-free alignments in a most-recently-used cache. this establishes frequent instruction subsequences. now #"MAKE-ORGAP only needs to detect these frequent instruction subsequences (by some quick means) and replace these instructions with one long instruction. EVAL-ORGAP has to be recompiled from time to time to support the latest detected frequent instruction sequences and drop support for no longer frequent instruction sequences.

(defun frequent-instruction-subsequences (orgs min-length)
  (let* ((orgs (let ((a (make-array (hash-table-count orgs))))
		 (loop for v being the hash-value of orgs for i from 0 do (setf (aref a i) v))
		 a))
	 (n (length orgs))
	 (instruction-subseqs (make-hash-table :test #'equal :hash-function #'mru-cache:lsxhash)))
    (loop for i below (1- n) do
	 (let* ((j (+ i (random (- n i))))
		(org1-genes (orgap-genes (orgcont-orgap (aref orgs i))))
		(org2-genes (orgap-genes (orgcont-orgap (aref orgs j))))
		(alignments (nth-value 1 (edit-distance-score-and-alignments org1-genes org2-genes 1 -1 -1 nil))))
	   (loop for alignment in alignments do
		(let ((ali1 (car alignment))
		      (ali2 (cadr alignment))
		      (subseq nil))
		  (assert (= (length ali1) (length ali2)))
		  (loop for pos below (length ali1) do
		       (let ((ins1 (aref ali1 pos))
			     (ins2 (aref ali1 pos)))
			 (if (and (eq ins1 ins2) (not (null ins1)))
			     (push ins1 subseq)
			     (progn
			       (unless (null subseq)
				 (incf (gethash subseq instruction-subseqs 0)))
			       (setf subseq nil)))))))))
    (let ((is (make-array (hash-table-count instruction-subseqs))))
      (let ((i 0))
	(maphash (lambda (k v) (setf (aref is i) (cons v k)) (incf i)) instruction-subseqs))
      (sort is (lambda (a b) (> (car a) (car b))))
      (prind is))))

(defstruct (orgap
	     (:constructor make-orgap*))
  genes ;genes of the organism
  code ;compiled code
  markers ;ALIST of IPs by marker number
  functions ;ALIST of IPs by function number
  (ip 0)
  (wait 0)
  angle
  (target nil)
  (targeter nil)
  x
  y
  energy
  (skin 1)
  (off-genes nil)
  (off-length 0)
  (as nil)
  (bs nil)
  (cs nil)
  (an 0)
  (bn 0)
  (memory (make-array 2 :initial-element 0))
  (stack (make-array 2 :initial-element 0))
  (sp 0)
  genesx ;rest of the genes to be read
  )

(defmethod print-object ((org orgap) stream)
  (print-unreadable-object (org stream :type t :identity t)
    (format stream ":GENES ~S~%:IP ~S~%:WAIT ~S~%:ANGLE ~S~%:TARGET ~S~%:X ~S~%:Y ~S~%:ENERGY ~S~%:SKIN ~S~%:OFF-GENES ~S~%:AS ~S~%:BS ~S~%:CS ~S~%:AN ~S~%:BN ~S"
	    (orgap-genes org) (orgap-ip org) (orgap-wait org) (orgap-angle org) (when (orgap-target org) (orgcont-id (orgap-target org))) (orgap-x org) (orgap-y org) (orgap-energy org) (orgap-skin org) (orgap-off-genes org) (orgap-as org) (orgap-bs org) (orgap-cs org) (orgap-an org) (orgap-bn org))))

(defun make-orgap (genes x y angle energy)
  (declare (optimize (debug 3)))
  (labels ((return-defaults ()
	     (return-from make-orgap (make-orgap* :genes nil :code #() :markers nil :functions nil :x x :y y :angle angle :energy 0 :genesx nil))))
    (multiple-value-bind (code markers jumps functions calls)
	(let ((template genes)
	      (markers nil)
	      (jumps nil)
	      (functions nil)
	      (calls nil)
	      (code nil)
	      (code-ip 0))
	  (loop do
	       (when (null template)
		 (let* ((rev (nreverse code))
			(new (make-array (length rev) :initial-contents rev)))
		   ;; the RETURN exits the LOOP.
		   (return (values new markers jumps functions calls))))
	       (let* ((g (pop template))
		      (gs (string g)))
		 ;;(prind g code-ip)
		 (cond
		   ((equal (subseq gs 0 3) "MRK")
		    (let* ((rest (subseq gs 3 (position #\= gs)))
			   (pos (assoc rest markers)))
		      (when (null pos)
			(let ((label (if (equal "" rest) nil (read-from-string rest))))
			  (assert (typep label 'fixnum))
			  (setf markers (acons label code-ip markers))))))
		   ((equal (subseq gs 0 3) "JNE")
		    (let* ((rest (subseq gs 3 (position #\= gs)))
			   (pos (assoc rest markers)))
		      (when (null pos)
			(let ((label (if (equal "" rest) nil (read-from-string rest))))
			  (assert (typep label 'fixnum))
			  (push g code)
			  (push label jumps)))))
		   ((equal (subseq gs 0 3) "JMP")
		    (let* ((rest (subseq gs 3 (position #\= gs)))
			   (pos (assoc rest markers)))
		      (when (null pos)
			(let ((label (if (equal "" rest) nil (read-from-string rest))))
			  (assert (typep label 'fixnum))
			  (push g code)
			  (push label jumps)))))
		   ((and (>= (length gs) 4) (equal (subseq gs 0 4) "FUNC"))
		    (let* ((rest (subseq gs 4 (position #\= gs)))
			   (pos (assoc rest functions)))
		      (when (null pos)
			(let ((label (if (equal "" rest) nil (read-from-string rest))))
			  (assert (typep label 'fixnum))
			  (setf functions (acons label code-ip functions))))))
		   ((and (>= (length gs) 4) (equal (subseq gs 0 4) "CALL"))
		    (let* ((rest (subseq gs 4 (position #\= gs)))
			   (pos (assoc rest functions)))
		      (when (null pos)
			(let ((label (if (equal "" rest) nil (read-from-string rest))))
			  (assert (typep label 'fixnum))
			  (push g code)
			  (push label calls)))))
		   (t
		    (push g code)
		    (incf code-ip))))))
      ;;(prind code) (prind markers jumps) (prind functions calls)
      (loop for jump-label in jumps do
	 ;;(prind (assoc jump-label markers))
	   (when (null (assoc jump-label markers))
	     (return-defaults)))
      (loop for function-label in calls do
	 ;;(prind (assoc function-label markers))
	   (when (null (assoc function-label functions))
	     (return-defaults)))
      (make-orgap* :genes genes :code code :markers markers :functions functions :x x :y y :angle angle :energy energy :genesx genes))))

(defun clamp (x min max)
  (assert (>= max min))
  (min (max x min) max))

(defun my-mod (x y)
  ;;(nth-value 1 (round x y))
  (multiple-value-bind (quotient remainder) (floor x y)
    (if (and (= quotient -1) (>= (abs remainder) (abs y)))
	(+ remainder (* quotient y))
	remainder)))

(defun eval-orgap (iters org orgcont add-offspring-function)
  (declare (optimize (debug 3)))
  (with-slots (genes code markers functions ip wait angle target targeter x y energy skin off-genes off-length as bs cs an bn stack sp genesx) org
    (let* ((max-ip (1- (array-dimension code 0)))
	   (world-w (array-dimension *world* 0))
	   (world-h (array-dimension *world* 1))
	   (pi/2 (/ pi 2))
	   (ins-count 0))
      (labels ((die (reason)
		 (declare (ignorable reason))
		 ;;(prind "dies" reason org)
		 (let ((x (floor x))
		       (y (floor y)))
		   (incf (aref *world* x y) (max energy 0))
		   (setf energy 0))
		 (return-from eval-orgap (values :kill ins-count)))
	       (survive ()
		 (if (>= (orgcont-age orgcont) 100000000)
		     (die "too old")
		     (return-from eval-orgap (values :survive ins-count))))
	       (cell-division (off-x off-y off-energy)
		 (when (<= (random 100) 0)
		   (setf off-energy (random (1+ energy))))
		 (setf off-genes (nreverse off-genes))
		 (setf off-energy (clamp off-energy 0 energy))
		 (setf off-x (mod off-x world-w))
		 (setf off-y (mod off-y world-h))
		 ;;(format t "cell-division x:~A y:~A energy:~A~%" off-x off-y off-energy)
		 (let ((off-orgap (make-orgap off-genes off-x off-y angle off-energy)))
		   (incf (orgap-wait off-orgap) 20000)
		   (let ((off-orgcont (funcall add-offspring-function off-orgap)))
		     (setf (orgcont-energy-in-sum off-orgcont) off-energy))
		   (incf wait 20000))
		 (decf energy off-energy) (incf (orgcont-energy-out-sum orgcont) off-energy)
		 (incf (orgcont-offspring-energy-sum orgcont) off-energy)
		 (setf off-genes nil)
		 (setf off-length 0)
		 (setf genesx genes)))
	(loop for i below iters do
	     (when (> wait 0)
	       (let ((skip (min wait (- iters i))))
		 (decf wait skip)
		 (incf i skip))
	       (when (> wait 0)
		 (survive)))
	     (tagbody
	      next-ins
		(when (or (> ip max-ip))
		  (die "ip too large"))
		(when (<= energy 0)
		  (die "out of energy"))
		;;(when (<= (random 100000) 1)
		;;  (setf ip (random (length code))))
		(incf ins-count)
		(let* ((ins (aref code ip)))
		  ;;(prind energy ip ins as bs cs an bn off-genes)
		  (decf energy) (incf (orgcont-energy-out-sum orgcont))
		  (macrolet ((make-instructions (keyform &body body)
			       (let ((instructions-list (mapcar #'caar body))
				     (instruction-functions nil)
				     (cases nil))
				 (labels
				     ((get-code (ins)
					(let ((ins+code (find ins body
							      :test (lambda (x y) (eq x (caar y))))))
					  (assert (not (null ins+code)))
					  (cdr ins+code)))
				      (add-instruction (ins code)
					(push `((,ins) ,code) cases))
				      (unconditional-function-name (ins)
					(let* ((name (string ins))
					       (new-name (concatenate 'string "INS-" name))
					       (fun-name (intern new-name)))
					  fun-name))
				      (add-unconditional-function (ins)
					(let ((fun-name (unconditional-function-name ins)))
					  (push `(,fun-name () ,@(get-code ins)) instruction-functions)
					  (add-instruction ins `(,fun-name))))
				      (add-conditional-instruction (ins suffix test)
					(let* ((fun-name (unconditional-function-name ins))
					       (name (string ins))
					       ;; must be a suffix because #'MAKE-ORG looks for prefixes.
					       (new-name (concatenate 'string name suffix))
					       (new-ins (intern new-name))
					       (cond-code `(when ,test (,fun-name))))
					  (add-instruction new-ins cond-code))))
				   ;; add conditional instructions
				   (loop for ins in instructions-list do
					(add-conditional-instruction ins "-CSP" 'cs)
					(add-conditional-instruction ins "-CSN" '(not cs))
					(add-unconditional-function ins))
				   ;; compute auxiliary functions used by the CASES
				   (let* ((instructions-list (mapcar #'caar cases))
					  (instructions (make-array (length instructions-list) :initial-contents instructions-list))
					  (instructions-hash-table (make-hash-table :test 'eq))
					  (keyform-sym (gensym "KEYFORM")))
				     (loop for ins-case in cases do
					  (let ((ins (caar ins-case)))
					    (assert (null (gethash ins instructions-hash-table)) () "Instruction ~S defined more than once in MAKE-INSTRUCTION" ins)
					    (setf (gethash ins instructions-hash-table) t)))
				     (let ((code
					    `(labels ((random-ins ()
							(sample ,instructions))
						      (is-valid-ins (ins)
							(multiple-value-bind (val present)
							    (gethash ins ,instructions-hash-table)
							  (declare (ignore val))
							  present))
						      ,@instruction-functions)
					       (let ((,keyform-sym ,keyform))
						 (ecase ,keyform-sym
						   ,@cases
						   ((end))
						   ))
					       )))
				       ;;(format t "~&code:~S~%" code) ;(prind instructions)
				       (format t "number of instructions: ~A~%" (length cases))
				       code))))))
		    (make-instructions
		     ins
		     ((eat)
		      (let* ((x (floor x))
			     (y (floor y))
			     (meal (ash (aref *world* x y) -2)))
			(incf energy meal) (incf (orgcont-energy-in-sum orgcont) meal)
			(decf (aref *world* x y) meal)
			(setf wait 10000)))
		     ((set-as-nil)
		      (setf as nil))
		     ((set-bs-nil)
		      (setf bs nil))
		     ((set-as-random)
		      (setf as (random-ins)))
		     ((set-bs-random)
		      (setf bs (random-ins)))
		     ((read-as)
		      (setf as (if (<= (random 2000) 0) (random-ins) (car genesx))))
		     ((read-bs)
		      (setf bs (if (<= (random 2000) 0) (random-ins) (car genesx))))
		     ((read-next)
		      (setf genesx (cdr genesx)))
		     ((write-as)
		      (when (is-valid-ins as)
			(when (> off-length 100000)
			  (die "offspring too big"))
			(let ((r (random 2000)))
			  (cond ((<= r 1) (push (random-ins) off-genes) (push as off-genes) (incf off-length 2))
				((<= r 3) (push as off-genes) (push (random-ins) off-genes) (incf off-length 2))
				((<= r 6) nil)
				(t (push as off-genes) (incf off-length))))))
		     ((write-bs)
		      (when (is-valid-ins bs)
			(when (> off-length 100000)
			  (die "offspring too big"))
			(let ((r (random 2000)))
			  (cond ((<= r 1) (push (random-ins) off-genes) (push bs off-genes) (incf off-length 2))
				((<= r 3) (push bs off-genes) (push (random-ins) off-genes) (incf off-length 2))
				((<= r 6) nil)
				(t (push bs off-genes) (incf off-length))))))
		     ((cmp-as-as-bs)
		      (setf as (eq as bs)))
		     ((cmp-as-gt-an-bn)
		      (setf as (> an bn)))
		     ((cmp-as-gt-bn-an)
		      (setf as (> bn an)))
		     ((cmp-bs-as-bs)
		      (setf bs (eq as bs)))
		     ((cmp-bs-gt-an-bn)
		      (setf bs (> an bn)))
		     ((cmp-bs-gt-bn-an)
		      (setf bs (> bn an)))
		     ((cmp-cs-as-bs)
		      (setf cs (eq as bs)))
		     ((cmp-cs-gt-an-bn)
		      (setf cs (> an bn)))
		     ((cmp-cs-gt-bn-an)
		      (setf cs (> bn an)))
		     ((set-cs-nil)
		      (setf cs nil))
		     ((set-cs-t)
		      (setf cs t))
		     ((set-cs-not-cs)
		      (setf cs (not cs)))
		     ((set-cs-random)
		      (setf cs (let* ((abs-an (abs an)) (abs-bn (abs bn)) (sum (+ abs-an abs-bn 1))) (< (random sum) abs-an))))
		     ((mrk0=))
		     ((mrk1=))
		     ((mrk2=))
		     ((jne0=)
		      (when (not as)
			(let* ((jump-ip (cdr (assoc 0 markers)))) ;0 because of JNE_0_
			  (assert (not (null jump-ip)))
			  (setf ip jump-ip)
			  (if (<= 0 ip max-ip)
			      (go next-ins)
			      (die "invalid jump target")))))
		     ((jne1=)
		      (when (not as)
			(let* ((jump-ip (cdr (assoc 1 markers)))) ;0 because of JNE_0_
			  (assert (not (null jump-ip)))
			  (setf ip jump-ip)
			  (if (<= 0 ip max-ip)
			      (go next-ins)
			      (die "invalid jump target")))))
		     ((jne2=)
		      (when (not as)
			(let* ((jump-ip (cdr (assoc 2 markers)))) ;0 because of JNE_0_
			  (assert (not (null jump-ip)))
			  (setf ip jump-ip)
			  (if (<= 0 ip max-ip)
			      (go next-ins)
			      (die "invalid jump target")))))
		     ((jmp0=)
		      (let* ((jump-ip (cdr (assoc 0 markers)))) ;0 because of JNE_0_
			(assert (not (null jump-ip)))
			(setf ip jump-ip)
			(if (<= 0 ip max-ip)
			    (go next-ins)
			    (die "invalid jump target"))))
		     ((jmp1=)
		      (let* ((jump-ip (cdr (assoc 1 markers)))) ;0 because of JNE_0_
			(assert (not (null jump-ip)))
			(setf ip jump-ip)
			(if (<= 0 ip max-ip)
			    (go next-ins)
			    (die "invalid jump target"))))
		     ((jmp2=)
		      (let* ((jump-ip (cdr (assoc 2 markers)))) ;0 because of JNE_0_
			(assert (not (null jump-ip)))
			(setf ip jump-ip)
			(if (<= 0 ip max-ip)
			    (go next-ins)
			    (die "invalid jump target"))))
		     #| doesn't work yet (fails upon call to CALL0 due to invalid JUMP-IP)
		     ((func0=)
		     ;; TODO: modify #'MAKE-ORGAP to find next RET upon encountering FUNC0, and here, skip instructions until after the RET.
		      )
		     ((call0=)
		      (let* ((jump-ip (cdr (assoc 0 functions)))) ;0 because of JNE_0_
		        (assert (not (null jump-ip)))
			(setf (aref stack sp) (1+ ip))
			(setf sp (mod (1+ sp) (array-dimension stack 0)))
			(setf ip jump-ip)
			(prind "call0" ip)
			(if (<= 0 ip max-ip)
			    (go next-ins)
			    (die "invalid call target"))))
		     ((ret=)
		      (setf sp (mod (1- sp) (array-dimension stack 0)))
		      (let ((jump-ip (aref stack sp)))
			(setf ip jump-ip)
			(if (<= 0 ip max-ip)
			    (go next-ins)
			    (die "invalid ret target"))))
		     |#
		     ((set-an-1)
		      (setf an 1))
		     ((set-bn-1)
		      (setf bn 1))
		     ((set-an--1)
		      (setf an -1))
		     ((set-bn--1)
		      (setf bn -1))
		     ((set-an-to-bn)
		      (setf an bn))
		     ((set-bn-to-an)
		      (setf bn an))
		     ((set-an-to-energy)
		      (setf an energy))
		     ((set-bn-to-energy)
		      (setf bn energy))
		     ((set-an-to-angle)
		      (setf an (clamp (floor (* 128 angle)) most-negative-fixnum most-positive-fixnum)))
		     ((set-bn-to-angle)
		      (setf bn (clamp (floor (* 128 angle)) most-negative-fixnum most-positive-fixnum)))
		     ((set-angle-to-an)
		      (setf angle (/ an 128)))
		     ((set-angle-to-bn)
		      (setf angle (/ bn 128)))
		     ((add-to-an-bn)
		      (locally (declare (optimize (debug 0) (safety 0) (speed 3)) (type fixnum an bn))
			(setf an (the fixnum (+ an bn)))))
		     ((add-to-bn-an)
		      (locally (declare (optimize (debug 0) (safety 0) (speed 3)) (type fixnum an bn))
			(setf bn (the fixnum (+ an bn)))))
		     ((mul-to-an-bn)
		      (locally (declare (optimize (debug 0) (safety 0) (speed 3)) (type fixnum an bn))
			(setf an (the fixnum (* an bn)))))
		     ((mul-to-bn-an)
		      (locally (declare (optimize (debug 0) (safety 0) (speed 3)) (type fixnum an bn))
			(setf bn (the fixnum (* an bn)))))
		     ((sub-from-an-bn)
		      (locally (declare (optimize (debug 0) (safety 0) (speed 3)) (type fixnum an bn))
			(setf an (the fixnum (- an bn)))))
		     ((sub-from-bn-an)
		      (locally (declare (optimize (debug 0) (safety 0) (speed 3)) (type fixnum an bn))
			(setf bn (the fixnum (- bn an)))))
		     ((sign-an)
		      (setf an (signum an)))
		     ((sign-bn)
		      (setf bn (signum bn)))
		     ((in-an-energy-left)
		      (let* ((radius (clamp (/ an 128) 0 3))
			     (x (mod (+ x (* (cos (- angle pi/2)) radius)) world-w))
			     (y (mod (+ y (* (sin (- angle pi/2)) radius)) world-h)))
			(setf an (aref *world* (floor x) (floor y)))))
		     ((in-an-energy-right)
		      (let* ((radius (clamp (/ an 128) 0 3))
			     (x (mod (+ x (* (cos (+ angle pi/2)) radius)) world-w))
			     (y (mod (+ y (* (sin (+ angle pi/2)) radius)) world-h)))
			(setf an (aref *world* (floor x) (floor y)))))
		     ((in-bn-energy-left)
		      (let* ((radius (clamp (/ bn 128) 0 3))
			     (x (mod (+ x (* (cos (- angle pi/2)) radius)) world-w))
			     (y (mod (+ y (* (sin (- angle pi/2)) radius)) world-h)))
			(setf bn (aref *world* (floor x) (floor y)))))
		     ((in-bn-energy-right)
		      (let* ((radius (clamp (/ bn 128) 0 3))
			     (x (mod (+ x (* (cos (+ angle pi/2)) radius)) world-w))
			     (y (mod (+ y (* (sin (+ angle pi/2)) radius)) world-h)))
			(setf bn (aref *world* (floor x) (floor y)))))
		     ((in-an-energy-x+)
		      (let ((x (mod (+ x 3) world-w)))
			(setf an (aref *world* (floor x) (floor y)))))
		     ((in-an-energy-x-)
		      (let ((x (mod (- x 3) world-w)))
			(setf an (aref *world* (floor x) (floor y)))))
		     ((in-an-energy-y+)
		      (let ((y (mod (+ y 3) world-h)))
			(setf an (aref *world* (floor x) (floor y)))))
		     ((in-an-energy-y-)
		      (let ((y (mod (- y 3) world-h)))
			(setf an (aref *world* (floor x) (floor y)))))
		     ((in-bn-energy-x+)
		      (let ((x (mod (+ x 3) world-w)))
			(setf bn (aref *world* (floor x) (floor y)))))
		     ((in-bn-energy-x-)
		      (let ((x (mod (- x 3) world-w)))
			(setf bn (aref *world* (floor x) (floor y)))))
		     ((in-bn-energy-y+)
		      (let ((y (mod (+ y 3) world-h)))
			(setf bn (aref *world* (floor x) (floor y)))))
		     ((in-bn-energy-y-)
		      (let ((y (mod (- y 3) world-h)))
			(setf bn (aref *world* (floor x) (floor y)))))
		     ((set-as-an-gt0)
		      (setf as (> an 0)))
		     ((set-as-an-ge0)
		      (setf as (>= an 0)))
		     ((set-an-max-an-bn)
		      (setf an (max an bn)))
		     ((set-bn-max-an-bn)
		      (setf bn (max an bn)))
		     ((split-cell-an)
		      (when (and (>= an 0) (>= energy an))
			(cell-division x y an)))
		     ((split-cell-bn)
		      (when (and (>= bn 0) (>= energy bn))
			(cell-division x y bn)))
		     ((turn-cw-an)
		      (incf angle (/ an 128))
		      (setf wait 300))
		     ((turn-cw-bn)
		      (incf angle (/ bn 128))
		      (setf wait 300))
		     ((turn-ccw-an)
		      (decf angle (/ an 128))
		      (setf wait 300))
		     ((turn-ccw-bn)
		      (decf angle (/ bn 128))
		      (setf wait 300))
		     ((walk-an)
		      (let* ((speed (/ an 128))
			     (new-x (my-mod (+ x (* (cos angle) speed)) world-w))
			     (new-y (my-mod (+ y (* (sin angle) speed)) world-h)))
			(incf (orgcont-walk-sum orgcont) (abs speed)) (incf (orgcont-walk-count orgcont))
			(when (>= (aref *world* (floor new-x) (floor new-y)) 0)
			  (let ((cost (max 0 (floor (expt (* (abs speed) 16) 2)))))
			    (decf energy cost) (incf (orgcont-energy-out-sum orgcont) cost))
			  (setf x new-x y new-y)))
		      (setf wait 100))
		     ((walk-bn)
		      (let* ((speed (/ bn 128))
			     (new-x (my-mod (+ x (* (cos angle) speed)) world-w))
			     (new-y (my-mod (+ y (* (sin angle) speed)) world-h)))
			(incf (orgcont-walk-sum orgcont) (abs speed)) (incf (orgcont-walk-count orgcont))
			(when (>= (aref *world* (floor new-x) (floor new-y)) 0)
			  (let ((cost (max 0 (floor (expt (* (abs speed) 16) 2)))))
			    (decf energy cost) (incf (orgcont-energy-out-sum orgcont) cost))
			  (setf x new-x y new-y)))
		      (setf wait 100))
		     ((set-angle-right)
		      (setf angle 0))
		     ((set-angle-down)
		      (setf angle (/ pi 2)))
		     ((set-angle-left)
		      (setf angle pi))
		     ((set-angle-up)
		      (setf angle (* pi 3/2)))
		     ((wait-an)
		      (setf wait (clamp (floor an) 0 10000000))
		      )
		     ((wait-bn)
		      (setf wait (clamp (floor bn) 0 10000000))
		      )
		     ((set-target-near)
		      ;;(prind "set-target-near")
		      (let* ((other (nearest-org x y *orgs* :exclude-orgs (list orgcont))))
			(when other
			  (setf target other)
			  (setf (orgap-targeter (orgcont-orgap other)) orgcont))))
		     ((set-angle-to-target)
		      ;;(prind "set-angle-to-target")
		      (when target
			(let* ((target-orgap (orgcont-orgap target))
			       (tenergy (orgap-energy target-orgap)))
			  (when (< tenergy 0)
			    (setf target nil))
			  (when target
			    (let* ((diff-x (- (orgap-x target-orgap) x))
				   (diff-y (- (orgap-y target-orgap) y)))
			      (setf angle (atan diff-y diff-x)))))))
		     ((set-angle-to-targeter)
		      ;;(prind "set-angle-to-targeter")
		      (when targeter
			(let* ((target-orgap (orgcont-orgap targeter))
			       (tenergy (orgap-energy target-orgap)))
			  (when (< tenergy 0)
			    (setf targeter nil))
			  (when targeter
			    (let* ((diff-x (- (orgap-x target-orgap) x))
				   (diff-y (- (orgap-y target-orgap) y)))
			      (setf angle (atan diff-y diff-x)))))))
		     ))
		  (assert (and (typep an 'fixnum) (typep bn 'fixnum)) (an bn) "AN:~A or BN:~A are not of type FIXNUM (last INS:~A)." an bn ins)
		  )
		(incf ip)))
	(survive)
	))))

(defun orgap-code-length (org)
  (array-dimension (orgap-code org) 0))

;; the slot accessors for ORGAP do not have to be defined.

;;; end of organism implementation
