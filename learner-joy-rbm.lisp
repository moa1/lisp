(load "arbitrary-trees.lisp")
(load "joy-arch.lisp")
(load "rbm.lisp")

(defun position-top (sequence predicate &key from-end (start 0) (end nil) key)
  "Returns the position of the topmost element.
Does pairwise comparisons of the elements of SEQUENCE and the current topmost element using the predicate function PREDICATE.
If the predicate returns T, then the current element is the new topmost element.
If there are multiple topmost elements, then the position of the first found element is returned.
START, END, and KEY are like in the function POSITION.
Example: (position-top '(4 3 1 2) #'<) returns 2."
  ;; TODO: implement :from-end to make it possible to get the rightmost top element.
  (assert (null from-end))
  (let ((len (length sequence)))
    (when (null end)
      (setf end len))
    (assert (>= start 0))
    (assert (<= end len))
    (assert (<= start end))
    (if (null key)
	(let ((topmost (elt sequence start))
	      (topmost-pos start))
	  (loop for pos from start below end do
	       (let ((e (elt sequence pos)))
		 (when (funcall predicate e topmost)
		   (setf topmost e)
		   (setf topmost-pos pos))))
	  topmost-pos)
	(let ((topmost (funcall key (elt sequence start)))
	      (topmost-pos start))
	  (loop for pos from start below end do
	       (let ((e (funcall key (elt sequence pos))))
		 (when (funcall predicate e topmost)
		   (setf topmost e)
		   (setf topmost-pos pos))))
	  topmost-pos))))

(defun tree-to-list (tree &key (nest-sym 'nest) (unnest-sym 'unnest))
  "Converts the tree TREE into a list with NEST-SYM indicating the following list until UNNEST-SYM is a subtree.
Example: (tree-to-list '(1 (2 3))) == '(1 NEST 2 3 UNNEST)."
  (labels ((rec (tree l)
	     (if (null tree)
		 l
		 (let ((head (car tree))
		       (tail (cdr tree)))
		   (if (consp head)
		       (rec tail (nconc l (list nest-sym) (rec head nil) (list unnest-sym)))
		       (rec tail (nconc l (list head))))))))
    (rec tree nil)))

(defun list-to-tree (list &key (nest-sym 'nest) (unnest-sym 'unnest))
  "Converts the (flat) list LIST into a tree.
The symbol NEST-SYM occurring in the list indicates that the following list until UNNEST-SYM is a subtree.
If UNNEST-SYM occurs before NEST-SYM occurred, the result is as if the list was truncated before the UNNEST-SYM.
If there are missing UNNEST-SYMs at the end of the list, the result is as if they were appended.
Example: (list-to-tree '(1 NEST 2 3)) == '(1 (2 3))."
  (labels ((next ()
	     (if (null list)
		 (values nil nil)
		 (let ((head (car list)))
		   (setf list (cdr list))
		   (values head t))))
	   (rec (tree)
	     (multiple-value-bind (head more-p) (next)
	       (if (or (eq head unnest-sym) (not more-p))
		   tree
		   (if (eq head nest-sym)
		       (let ((subtree (rec nil))) ;subtree must be evaluated before the rest of the list.
			 (rec (nconc tree (list subtree))))
		       (rec (nconc tree (list head))))))))
    (rec nil)))

(let ((a '(1 2 3))
      (b '(1 (2 (3)) 4)))
  (assert (equal (list-to-tree (tree-to-list a)) a))
  (assert (equal (list-to-tree (tree-to-list b)) b)))

(defun sample-without-replacement (n sequence &key (result-type 'list))
  "Returns a list or vector (depending on RESULT-TYPE) with N different elements sampled from SEQUENCE."
  (assert (<= n (length sequence)))
  (let ((res (case result-type
	       (list (loop for i below n collect nil))
	       (vector (make-array n))))
	(len (length sequence)))
    (labels ((rec (n indices length-indices)
	       (if (<= n 0)
		   res
		   (let* ((pos (random length-indices))
			  (index (elt indices pos))
			  (new-indices (remove index indices :test #'eq)))
		     (setf (elt res (1- n)) (elt sequence index))
		     (rec (1- n) new-indices (1- length-indices))))))
      (rec n (loop for i below len collect i) len))
    res))

;; TODO: encode (and decode) the length of stk, exp and res

(defun encode-joy-input-and-result (stk exp res joy-ops nest-sym unnest-sym stk-length exp-length res-length)
  "Returns STK, EXP, and RES encoded as an vector containing floats.
STK and EXP are both a tree with the following possible values: NIL, T, a number, one element of JOY-OPS, or a subtree.
RES is a tree with the same possible values, or ERROR.
NEST-SYM and UNNEST-SYM are used by the function TREE-TO-LIST before encoding to an array.
The *-LENGTH-variables are the maximum lengths that the respective * variables may have, i.e. if STK-LENGTH is 5, then the stack has a maximum depth of 5.
JOY-OPS is the list of joy operations that are possible on the STK, EXP, and RES."
  (let* ((ext-joy-ops (cons nest-sym (cons unnest-sym joy-ops))) ;extend the joy-ops by nest-sym and unnest-sym
	 ;; The code consists of one binary neuron, one softmax neuron for each value and element in ext-joy-ops, and one number neuron for each value.
	 (val-length (+ (length ext-joy-ops) 3)) ;'(number-p nil t) plus the ext-joy-ops
	 ;; The code array is arranged in this order:
	 (res-errorp-binary-offset 0)
	 (res-errorp-binary-end (+ res-errorp-binary-offset 1))
	 (stk-softmax-offset res-errorp-binary-end)
	 (stk-softmax-end (+ stk-softmax-offset (* val-length stk-length)))
	 (exp-softmax-offset stk-softmax-end)
	 (exp-softmax-end (+ exp-softmax-offset (* val-length exp-length)))
	 (res-softmax-offset exp-softmax-end)
	 (res-softmax-end (+ res-softmax-offset (* val-length res-length)))
	 (stk-gaussian-offset res-softmax-end)
	 (stk-gaussian-end (+ stk-gaussian-offset stk-length))
	 (exp-gaussian-offset stk-gaussian-end)
	 (exp-gaussian-end (+ exp-gaussian-offset exp-length))
	 (res-gaussian-offset exp-gaussian-end)
	 (res-gaussian-end (+ res-gaussian-offset res-length))
	 (code-length res-gaussian-end))
    (labels ((set-code-val (val code softmax-offset gaussian-offset)
	       ;;(prind "enc" val code offset)
	       (cond
		 ((numberp val) (progn (setf (aref code gaussian-offset) (float val))
				       (setf (aref code (+ softmax-offset 0)) 1.0)))
		 ((null val) (setf (aref code (+ softmax-offset 1)) 1.0))
		 ((eq val t) (setf (aref code (+ softmax-offset 2)) 1.0))
		 (t (let ((pos (position val ext-joy-ops)))
		      ;;(prind ext-joy-ops val pos)
		      (setf (aref code (+ softmax-offset 3 pos)) 1.0))))
	       ;;(prind code)
	       )
	     (encode (stk exp res)
	       "Return a code."
	       (let* ((code (make-array code-length :element-type 'float :initial-element 0.0)))
		 (loop
		    for i below stk-length
		    for val in stk
		    for softmax-offset from stk-softmax-offset by val-length
		    for gaussian-offset from stk-gaussian-offset by 1
		    do
		      (set-code-val val code softmax-offset gaussian-offset))
		 (loop
		    for i below exp-length
		    for val in exp
		    for softmax-offset from exp-softmax-offset by val-length
		    for gaussian-offset from exp-gaussian-offset by 1
		    do
		      (set-code-val val code softmax-offset gaussian-offset))
 		 (if (eq res 'error)
		     (setf (aref code res-errorp-binary-offset) 1.0)
		     (loop
			for i below res-length
			for val in res
			for softmax-offset from res-softmax-offset by val-length
			for gaussian-offset from res-gaussian-offset by 1
			do
			  (set-code-val val code softmax-offset gaussian-offset)))
		 code)))
      (let ((stk-l (tree-to-list stk :nest-sym nest-sym :unnest-sym unnest-sym))
	    (exp-l (tree-to-list exp :nest-sym nest-sym :unnest-sym unnest-sym))
	    (res-l (if (eq res 'error)
			      'error
			      (tree-to-list res :nest-sym nest-sym :unnest-sym unnest-sym))))
	(encode stk-l exp-l res-l)))))

(defun decode-joy-input-and-result (code joy-ops nest-sym unnest-sym stk-length exp-length res-length)
  (let* ((ext-joy-ops (cons nest-sym (cons unnest-sym joy-ops)))
	 ;; The code consists of one binary neuron, one softmax neuron for each value and element in ext-joy-ops, and one number neuron for each value.

	 ;; TODO: rename val-length into n-softmax-units

	 (val-length (+ (length ext-joy-ops) 3))
	 (res-errorp-binary-offset 0)
	 (res-errorp-binary-end (+ res-errorp-binary-offset 1))
	 (stk-softmax-offset res-errorp-binary-end)
	 (stk-softmax-end (+ stk-softmax-offset (* val-length stk-length)))
	 (exp-softmax-offset stk-softmax-end)
	 (exp-softmax-end (+ exp-softmax-offset (* val-length exp-length)))
	 (res-softmax-offset exp-softmax-end)
	 (res-softmax-end (+ res-softmax-offset (* val-length res-length)))
	 (stk-gaussian-offset res-softmax-end)
	 (stk-gaussian-end (+ stk-gaussian-offset stk-length))
	 (exp-gaussian-offset stk-gaussian-end)
	 (exp-gaussian-end (+ exp-gaussian-offset exp-length))
	 (res-gaussian-offset exp-gaussian-end)
	 (res-gaussian-end (+ res-gaussian-offset res-length))
	 (code-length res-gaussian-end))
    (declare (ignore code-length))
    (labels ((get-code-val (code softmax-offset gaussian-offset softmax-max-pos)
	       (declare (ignore softmax-offset))
	       (assert (and (<= 0 softmax-max-pos) (< softmax-max-pos val-length)))
	       ;;(prind max-pos offset)
	       (case softmax-max-pos
		 (0 (aref code (+ gaussian-offset 0)))
		 (1 nil)
		 (2 t)
		 (t (elt ext-joy-ops (- softmax-max-pos 3)))))
	     (top-softmax-pos (softmax-offset)
	       (- (position-top code #'>
				:start softmax-offset
				:end (+ softmax-offset val-length))
		  softmax-offset))
	     (decode (code)
	       "Return stk, exp and res."
	       (let* (stk exp res)
		 (setf stk
		       (loop
			  for i below stk-length
			  for softmax-offset from stk-softmax-offset by val-length
			  for gaussian-offset from stk-gaussian-offset by 1
			  for softmax-max-pos = (top-softmax-pos softmax-offset)
			  collect
			    (get-code-val code softmax-offset gaussian-offset softmax-max-pos)))
		 (setf exp
		       (loop
			  for i below exp-length
			  for softmax-offset from exp-softmax-offset by val-length
			  for gaussian-offset from exp-gaussian-offset by 1
			  for softmax-max-pos = (top-softmax-pos softmax-offset)
			  collect
			    (get-code-val code softmax-offset gaussian-offset softmax-max-pos)))
		 (setf res
		       (if (>= (aref code res-errorp-binary-offset) 0.5)
			   'error
			   (loop
			      for i below res-length
			      for softmax-offset from res-softmax-offset by val-length
			      for gaussian-offset from res-gaussian-offset by 1
			      for softmax-max-pos = (top-softmax-pos softmax-offset)
			      collect
				(get-code-val code softmax-offset gaussian-offset softmax-max-pos))))
		 (values stk exp res))))
      (multiple-value-bind (stk exp res) (decode code)
	(let ((stk-t (list-to-tree stk :nest-sym nest-sym :unnest-sym unnest-sym))
	      (exp-t (list-to-tree exp :nest-sym nest-sym :unnest-sym unnest-sym))
	      (res-t (if (eq res 'error)
				'error
				(list-to-tree res :nest-sym nest-sym :unnest-sym unnest-sym))))
	  (list stk-t exp-t res-t))))))

(defun rbm-input-parameters-joy-input-and-result (joy-ops stk-length exp-length res-length)
  (let* ((ext-joy-ops(cons 'nest (cons 'unnest joy-ops)))
	 (val-length (+ (length ext-joy-ops) 3))
	 (res-errorp-binary-offset 0)
	 (res-errorp-binary-end (+ res-errorp-binary-offset 1))
	 (stk-softmax-offset res-errorp-binary-end)
	 (stk-softmax-end (+ stk-softmax-offset (* val-length stk-length)))
	 (exp-softmax-offset stk-softmax-end)
	 (exp-softmax-end (+ exp-softmax-offset (* val-length exp-length)))
	 (res-softmax-offset exp-softmax-end)
	 (res-softmax-end (+ res-softmax-offset (* val-length res-length)))
	 (stk-gaussian-offset res-softmax-end)
	 (stk-gaussian-end (+ stk-gaussian-offset stk-length))
	 (exp-gaussian-offset stk-gaussian-end)
	 (exp-gaussian-end (+ exp-gaussian-offset exp-length))
	 (res-gaussian-offset exp-gaussian-end)
	 (res-gaussian-end (+ res-gaussian-offset res-length))
	 (code-length res-gaussian-end))
    (declare (ignore code-length))
    (let* ((v-binary 1)
	   (v-softmax (append (loop for softmax-offset from stk-softmax-offset below stk-softmax-end by val-length collect val-length)
			      (loop for softmax-offset from exp-softmax-offset below exp-softmax-end by val-length collect val-length)
			      (loop for softmax-offset from res-softmax-offset below res-softmax-end by val-length collect val-length)))
	   (v-gaussian (- res-gaussian-end stk-gaussian-offset)))
      (values (+ v-binary (apply #'+ v-softmax) v-gaussian)
	      (list :v-binary v-binary
		    :v-softmax v-softmax
		    :v-gaussian v-gaussian)))))

(defun code-decode-equal (code-stk code-exp code-res decode-stk decode-exp decode-res)
  "Checks if coded and coded lists are equal under looking at the shortest common prefix."
  (labels ((list-eq (code decode)
	     (and (loop for c in code for d in decode always
		       (equalp c d))
		  ;; if decode is shorter than code then there are some unchecked values here
		  (loop for d in (nthcdr (length code) decode) always
		       (= 0.0 d)))))
    (and (list-eq code-stk decode-stk)
	 (list-eq code-exp decode-exp)
	 (if (eq code-res 'error)
	     (eq decode-res 'error)
	     (list-eq code-res decode-res)))))

(let* ((joy-ops '(+ dup nill t))
       (stk-length 3)
       (exp-length 3)
       (res-length 3)
       (stk '(0.0))
       (exp '(5.5 +))
       (res (joy-eval-handler '(0.0) '(5.5 +)))
       (code (encode-joy-input-and-result stk exp res joy-ops 'nest 'unnest stk-length exp-length res-length))
       (dec (decode-joy-input-and-result code joy-ops 'nest 'unnest stk-length exp-length res-length)))
  ;;(prind code dec)
  (assert (code-decode-equal stk exp res (car dec) (cadr dec) (caddr dec))))

(defun generate-joy-codes (n stk joy-ops nest-sym unnest-sym max-ticks max-seconds stk-length exp-length res-length)
  (let ((codes nil))
    (flet ((gen-code (exp)
	     (let* ((res (joy-eval-handler stk exp :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
		    (code (encode-joy-input-and-result stk exp res joy-ops nest-sym unnest-sym stk-length exp-length res-length))
		    (decode (decode-joy-input-and-result code joy-ops nest-sym unnest-sym stk-length exp-length res-length)))
	       ;;(prind stk (car decode))
	       ;;(prind exp (cadr decode))
	       ;;(prind res (caddr decode))
	       (assert (code-decode-equal stk exp res (car decode) (cadr decode) (caddr decode))
		       nil
		       "stk:~A d-stk:~A~%exp:~A d-exp:~A~%res:~A d-res:~A~%"
		       stk (car decode) exp (cadr decode) res (caddr decode)) ;;does not hold if the -LENGTH variables are too small
	       (setf codes (cons code codes)))))
      (enumerate-trees n joy-ops #'print #'gen-code))
    ;; convert codes into a vector
    (make-array (length codes) :initial-contents codes)))

(defun reconstruct-joy-step (stk exp res joy-ops stk-length exp-length res-length rbm)
  (let* ((code (encode-joy-input-and-result stk exp res joy-ops 'nest 'unnest stk-length exp-length res-length))
	 (code-vector (make-array (list 1 (length code)) :initial-contents (list code)))
	 (h (rbm-h-from-v code-vector rbm))
	 (v (rbm-v-from-h h rbm))
	 (v1 (make-array (rbm-n-v rbm) :displaced-to v))
	 (decode (decode-joy-input-and-result v1 joy-ops 'nest 'unnest stk-length exp-length res-length)))
    (print (list "rbm-h" h))
    (print (list "rbm-decode" decode))))

(defun learn-joy-step (n stk joy-ops max-ticks max-seconds stk-length exp-length res-length n-hidden hidden n-minibatch learn-rate momentum weight-cost max-iterations)
  (multiple-value-bind (n-visible visible)
      (rbm-input-parameters-joy-input-and-result joy-ops stk-length exp-length res-length)
    (let* ((codes (generate-joy-codes n stk joy-ops 'nest 'unnest max-ticks max-seconds stk-length exp-length res-length))
	   (rbm-parameters (append (list n-visible n-hidden) visible hidden))
	   (rbm (apply #'new-rbm rbm-parameters)))
      (flet ((get-data (iteration rbm)
	       ;;(declare (ignore iteration rbm))
	       ;; could return NIL to abort learning
	       (let* ((data (sample-without-replacement n-minibatch codes))
		      (decode (map 'list (lambda (code) (decode-joy-input-and-result code joy-ops 'nest 'unnest stk-length exp-length res-length)) data))
		      (decode-1 (car decode)))
		 (when (= 0 (rem iteration 10))
		   (print (list "decode" decode-1))
		   (reconstruct-joy-step (car decode-1) (cadr decode-1) (caddr decode-1) joy-ops stk-length exp-length res-length rbm))
		 (make-array (list n-minibatch n-visible) :element-type 'float :initial-contents data))))
	(rbm-learn #'get-data rbm learn-rate momentum weight-cost max-iterations)))))

;;(defparameter *rbm* (learn-joy-step 1 '(5.5) *joy-ops* 1000 .01 10 10 10 11 '(:h-binary 10 :h-gaussian 1) 1 .001 0 .0002 1000))
;; few joy-ops;
;;(defparameter *rbm* (learn-joy-step 1 '(5.5) '(pred succ) 1000 .01 2 2 2 3 '(:h-softmax (2) :h-binary 1 :h-gaussian 0) 1 .0005 .9 .0002 1000))
