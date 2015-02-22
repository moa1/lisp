(asdf:oos 'asdf:load-op :utils)
(asdf:oos 'asdf:load-op :lhstats)

(use-package :utils)
(use-package :alexandria)

;; one of the basic problems of evolution algorithms is specifying a good
;; fitness function: it should be 'smooth', meaning that its gradient gives a
;; hint where to evolve locally.
;; how to do that without having to think a lot?
;; maybe one way is to evolve a function that takes a simple, human-specified
;; function H, and outputs a smoother fitness function.
;; this could be accomplished by giving the evolving function F the ability
;; to call the fitness function with test values, and with a final value,
;; which should count as F's fitness.
;; another (or additional & complimentary) way could be to somehow let F access
;; the fitness function H. (e.g. in forth define it as a word, in sic map it
;; to some specific memory range)

(defstruct 4dict
  xt
  data)

(defmacro 4l (&body body)
  `(lambda (d c f)
     ,@body
     (values d c f)))

(defmacro m4di (name &body body)
  `(cons ',name (make-4dict :xt
			    (lambda (d c f)
			      (multiple-value-bind (d2 c2 f2)
				  (funcall (4l ,@body) d c f)
				(setq d d2 c c2 f f2))
			      (values d c f))
			    :data ',body)))

(defmacro m4dc (name &body body)
  `(cons ',name (make-4dict :xt (4l (error "compile-only"))
			    :data ',body)))

(let* ((ht (make-hash-table))
       (alists
	(list
	 (m4di + (push (+ (pop d) (pop d)) d))
	 (m4di - (push (- (pop d) (pop d)) d))
	 ;; * / /mod mod
	 ;; sf2.html
	 (m4di swap (rotatef (first d) (second d)))
	 (m4di dup (if (length>= d 1) (push (first d) d) (error "dup")))
	 (m4di over (if (length>= d 2) (push (second d) d) (error "over")))
	 (m4di rot (rotatef (third d) (second d) (first d)))
	 (m4di drop (if (length>= d 1) (pop d) (error "drop")))
	 (m4di 2swap (destructuring-bind (w x y z &rest r) d
		       (setf d (nconc (list y z w x) r))))
	 (m4di 2over (destructuring-bind (w x y z &rest r) d
		       (setf d (nconc (list y z w x y z) r))))
	 ;; 2drop 2dup
	 ;; sf4.html
	 (m4di = (if (length>= d 2) (push (if (eq (pop d) (pop d)) -1 0) d)
		     (error "=")))
	 (m4di < (push (let ((a (pop d)) (b (pop d)))
			 (if (and (typep a 'real) (typep b 'real))
			     (if (< a b) -1 0)
			     (error "<")))
		       d))
	 ;; > 0= 0< 0> invert and or
	 ;; sf7.html
	 (m4di number (if (length>= d 1) (let ((n (pop d)))
					   (if (realp n) (push n d)
					       (error "number")))))
	 ;; variables(symbols) create ,(Initializing an Array) sf8.html
	 ;; sf9.html
	 ;; ' ['] execute here sp@ sp0
	 ;; sf10.html
	 (m4di word (if (length>= c 1) (push (pop c) d) (error "word")))
	 ;; sf11.html
	 ;; cell+ immediate(immediate bit is a property of :) postpone [ ] literal
	 (m4di find-xt (if (length>= d 1)
			   (let* ((w (pop d))
				  dic)
			     (multiple-value-bind (d p)
				 (gethash w f)
			       (if p
				   (setq dic (4dict-xt d))
				   (multiple-value-bind (d p)
				       (gethash w ht)
				     (if p
					 (setq dic (4dict-xt d))
					 (setq dic w)))))
			     (push dic d))
			   (error "find-xt")))
	 (m4di execute (if (length>= d 1)
			   (multiple-value-bind (d2 c2 f2)
			       (funcall (pop d) d c f)
			     (setq d d2 c c2 f f2))
			   (error "execute")))
	 (m4di newlist (push nil d))
	 (m4di pushlist (if (length>= d 2)
			    (let ((o (pop d)) (l (pop d)))
			      (push (cons o l) d))
			    (error "pushlist")))
	 (m4di appendlist (if (length>= d 2)
			      (let ((o (pop d)) (l (pop d)))
				(push (nconc l (list o)) d))
			      (error "appendlist")))
	 (m4di set-xt
	   (if (length>= d 2)
	       (let* ((l (pop d))
		      (s (pop d))
		      (lc (mapcar (lambda (x)
				    (if (numberp x)
					`(push ',x d)
					`(progn
					   (push ',x d)
					   (multiple-value-bind (d2 c2 f2)
					       (funcall
						(4dict-xt
						 (gethash 'execute ,ht))
						d c f)
					     (setq d d2 c c2 f f2)))))
				  l))
		      (m (make-4dict)))
		 (setf (4dict-xt m)  (eval `(lambda (d c f)
					      (block nil
						,@lc
						(values d c f)))))
		 (setf (gethash s f) m))
	       (error "set-xt")))
	 (m4di while (if (length>= d 2)
			 (let* ((loop-xt (pop d))
				(stop-xt (pop d)))
			   (macrolet ((call ()
					`(multiple-value-bind (d2 c2 f2)
					     (funcall
					      (4dict-xt (gethash 'execute ht))
					      d c f)
					   (setq d d2 c c2 f f2))))
			     (do nil ((progn (push stop-xt d)
					     (call)
					     (= (pop d) 0)))
			       (push loop-xt d)
			       (call))))
			 (error "while")))
	 ;; (word f1 newlist set-xt word f2 newlist 0 pushlist 9
	 ;;  pushlist set-xt 1 word f1 find-xt word f2 find-xt while)
	 ))
       )
  (dolist (alist alists)
    (setf (gethash (car alist) ht) (cdr alist)))
  (defparameter *f* ht)
  (defparameter *insertions* (cons 1 (mapcar #'car alists))))

;; (inter nil
;;        '(word |'| newlist
;; 	 word word find-xt appendlist
;; 	 word find-xt find-xt appendlist
;; 	 set-xt)
;;        (make-hash-table))
;; (* (expt (* i p_word r) 4)
;;    (expt (* i p_|'| r) 1)
;;    (expt (* i p_newlist r) 1)
;;    (expt (* i p_find-xt r) 3)
;;    (expt (* i p_appendlist r) 2)
;;    (expt (* i p_set-xt r) 1))
;; what do the p_* have to be so that the product is maximal?
;; (under the condition that the sum of all p_* == 1)
;;
;; ex: (* (expt p_a 2) (expt p_b 1)) ==
;;     (* (expt p_a 2) (expt (- 1 p_a) 1)) ==
;;     (* (expt p_a 2) (- 1 p_a)) ==
;;     (- (expt p_a 2) (expt p_a 3)) where is this maximal? derivate
;;     (- (* 2 p_a) (* 3 (expt p_a 2))) = 0
;;     (* 2 p_a) = (* 3 (expt p_a 2))
;;
;; so applied to above text:
;; (* (expt (* i p_word r) 4)
;;    (expt (* i p_|'| r) 1)
;;    (expt (* i p_newlist r) 1)
;;    (expt (* i p_find-xt r) 3)
;;    (expt (* i p_appendlist r) 2)
;;    (expt (* i p_set-xt r) 1)) ==
;; (* (expt (* i p_word r) 4)
;;    (expt (* i p_find-xt r) 3)
;;    (expt (* i p_appendlist r) 2)
;;    (expt (* i p_1 r) 3))
;;
;; (defun fitness-p-exp-2 (s)
;;   (let* ((sum (reduce #'+ s))
;; 	 (sumnorm (if (= 0 sum) 1 sum))
;; 	 (scaled (mapcar (lambda (x) (/ x sumnorm)) s))
;; 	 (log-default (lambda (x) (if (<= x 0) -10000 (log x)))))
;;     (+ (funcall log-default (* (expt (elt scaled 0) 1)
;; 			       (expt (elt scaled 1) 1)
;; 			       (expt (elt scaled 2) 1)
;; 			       (expt (elt scaled 3) 2)
;; 			       (expt (elt scaled 4) 3)
;; 			       (expt (elt scaled 5) 4)))
;;        ;; todo: try to evolve programs that are more robust with
;;        ;; respect to the fitness function steepness
;;        ;; (i.e. without #'log above)
;;        (if (> sum 1) (/ sum) sum))))
;;
;; (defun offsp-p-exp (population)
;;   (let ((p1 (choice population)))
;;     (case (choice '(:mut :cross))
;;       (:mut (mapcar (lambda (x) (clamp (+ (- x .1) (random .2)) 0 1))
;; 		    p1))
;;       (:cross (let ((p2 (choice population)))
;; 		(mapcar (lambda (x y) (/ (+ x y) 2)) p1 p2))))))
;;
;; (genetic (repeat '(0 0 0 0 0 0) 2)
;; 	 #'fitness-p-exp-2
;; 	 500000
;; 	 #'offsp-p-exp :verbose nil)
;; (the best solution is '(1/12 1/12 1/12 2/12 3/12 4/12))


(defun inter (d c f)
  (declare (type list d c)
	   (type hash-table f))
  (if (endp c)
      (values d c f)
      (let ((ins (pop c))
	    fail)
	(if (integerp ins)
	    (push ins d)
	    (progn
	      (push ins c)
	      (handler-case
		  (dolist (doins '(word find-xt execute))
		    (multiple-value-bind (d2 c2 f2)
			(funcall (4dict-xt (gethash doins *f*)) d c f)
		      (setq d d2 c c2 f f2)))
		(error (condition)
		  (setf fail (format nil "error ~A~%ins:~A~%d:~A"
				     condition ins d))))))
	(if fail
	    (values :fail fail nil)
	    (inter d c f)))))

(defun inter2 (data code funs)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type hash-table funs))
  (if (endp code)
      (values data code funs)
      (let ((ins (pop code))
	    fail)
	(cond ((integerp ins)
	       (push ins data))
	      ((not (null (gethash ins funs)))
	       (inter2 data (append (gethash ins funs) code) funs))
	      (t
	       (multiple-value-bind (fun p)
		   (gethash ins *f*)
		 (declare (type function fun))
		 (if p
		     (multiple-value-bind (failed d c f)
			 (funcall fun ins data code funs)
		       (if (not failed)
			   (setq data d code c funs f)
			   (setf fail failed)))
		     (setf fail (format nil "unknown function ~A" ins))))))
	(if fail
	    (values :fail fail nil)
	    (inter2 data code funs)))))

(defun test-inter ()
  (let ((l `(
	     (nil nil)
	     ((1) (1))
	     ((1 2) (2 1))
	     ((,(gensym)) :fail)
	     ((1 1 +) (2))
	     ((1 1 -) (0))
	     ((1 2 swap) (1 2))
	     ((1 swap) :fail)
	     ((swap) :fail)
	     ((1 dup) (1 1))
	     ((dup) :fail)
	     ((1 2 over) (1 2 1))
	     ((1 over) :fail)
	     ((over) :fail)
	     ((1 2 3 rot) (1 3 2))
	     ((1 2 rot) :fail)
	     ((1 rot) :fail)
	     ((rot) :fail)
	     ((1 drop) ())
	     ((drop) :fail)
	     ((1 2 3 4 2swap) (2 1 4 3))
	     ((1 2 3 2swap) :fail)
	     ((1 2 2swap) :fail)
	     ((1 2swap) :fail)
	     ((2swap) :fail)
	     ((1 2 3 4 2over) (2 1 4 3 2 1))
	     ((1 2 3 2over) :fail)
	     ((1 2 2over) :fail)
	     ((1 2over) :fail)
	     ((2over) :fail)
	     ((1 1 =) (-1))
	     ((1 2 =) (0))
	     ((1 =) :fail)
	     ((=) :fail)
	     ((1 2 <) (0))
	     ((2 1 <) (-1))
	     ((1 <) :fail)
	     ((<) :fail)
	     ((word f1 newlist word + find-xt pushlist 5 pushlist set-xt 1 f1)
	      (6))
	     )))
    (let ((tests
	   (mapcar (lambda (par)
		     (let* ((code (car par))
			    (result (cadr par))
			    (data (inter nil code (make-hash-table))))
		       ;;(prind code data result (equal data result))
		       (if (equal data result)
			   t
			   (progn
			     (warn 
			      (format nil 
				      "inter-error code:~A data:~A result:~A"
				      code data result))
			     nil))))
		   l)))
      (format t "failed tests:~A/~A~%" (count nil tests) (length tests)))))

(test-inter)

(defun offsp (population)
  (declare (optimize (debug 3)))
  (labels ((ins (l i e &optional result)
	     (if (= i 0)
		 (nconc (nreverse result) (cons e l))
		 (ins (cdr l) (1- i) e (cons (car l) result))))
	   (del (l i)
	     (remove-if (constantly t) l :start i :count 1)))
    (let* ((s (choice population))
	   (mutation (if (length>= s 1) (random 3) 0))
	   (off (ecase mutation
		  (0 (ins s (random (1+ (length s))) (choice *insertions*)))
		  (1 (del s (random (length s))))
		  (2 (append (head s (random (length s)))
			     (let ((s (choice population))
				   (pos (if (length>= s 1)
					    (random (length s))
					    0)))
			       (nthcdr pos s) s))))))
      (if (< (random 1.0) .1)
	  (offsp (cons off nil))
	  (copy-list off)))))

(defun genetic (ancestors fitness generations foffspring &key verbose
		(unfit :serial))
  (let* ((population (copy-list ancestors))
	 (fit (mapcar fitness population)))
    (dotimes (gen generations (let ((i (max-index fit)))
				(values (elt population i) (elt fit i))))
      (when (not (null verbose))
	(prind gen)
	(when (and (numberp verbose) (>= verbose 2))
	  (prind population)))
      (ecase unfit
	(:serial
	 (let ((child (funcall foffspring population))
	       (index (min-index fit)))
	   (setf (elt population index) child)
	   (setf (elt fit index) (funcall fitness child))))
	((:parallel :parallel-strict)
	 (let ((minfit (apply #'min fit))
	       (oldpop (copy-list population)))
	   (dotimes (index (length population))
	     (if (= (elt fit index) minfit)
		 (let* ((child (funcall foffspring oldpop))
			(child-fit (funcall fitness child)))
		   (when (ecase unfit
			   (:parallel (>= child-fit minfit))
			   (:parallel-strict (> child-fit minfit)))
		     (setf (elt population index) child)
		     (setf (elt fit index) child-fit)))))))))))

(defun fit-find-number (number)
  (flet ((fit1 (s)
	   (multiple-value-bind (data)
	       (inter nil s (make-hash-table))
	     ;; (prind 'fit1 s data)
	     (if (or (eq data :fail)
		     (not (length>= data 1))
		     (not (numberp (car data))))
		 most-negative-fixnum
		 (- 0 (pow2 (- number (car data))) (length s))))))
    #'fit1))

(defparameter *success-find-number-1*
  ;; 1/3 each for (insertion deletion crossover)
  ;; .1 for (offspring child)
  ;; *insertions* == (1 + - SWAP DUP OVER ROT DROP 2SWAP 2OVER)
  ;; 1000 generations, 5 population size, start '((1) (1) (1) (1) (1))
  ;; fitness: (- 0 (pow2 (- number (car data))) (length s))
  ;;[number: target number]
  ;;[data: data stack upon exit]
  ;;[s: code stack upon start]
  '((4  174/1983 1983)
    (8  243/4094 4094)
    (16 66/1385 1385)
    (32 55/1569 1569)
    (64 38/1769 1769)
    (128  52/3914 3914)
    (4096 2/5022 5022)))


(defparameter *success-find-number-1*
  ;; 1/3 each for (insertion deletion crossover)
  ;; .1 for (offspring child)
  ;;*insertions* == (1 + - DUP OVER)
  ;; 1000 generations, 5 population size, start '((1) (1) (1) (1) (1))
  ;; fitness: (- 0 (pow2 (- number (car data))) (length s))
  ;;[number: target number]
  ;;[data: data stack upon exit]
  ;;[s: code stack upon start]
  '((4 312/855 855)
    (8 351/1140 1140)
    (16 264/1101 1101)
    (32 190/1037 1037)
    (64 174/1056 1056)
    (128 129/1035 1035)))

(defun binomial-success (success-find-number)
  (dolist (scen success-find-number)
    (destructuring-bind (number success-fraction n) scen
      (multiple-value-bind (lower upper)
	  (stats:binomial-probability-ci n success-fraction .05
					 :exact? nil)
	(prind number n success-fraction lower upper)))))

(defun inter-cont (c0 c1 &key d0 (f0 (make-hash-table)))
  (multiple-value-bind (d c f)
      (inter d0 c0 f0)
    (declare (ignore c))
    (if (eq d :fail)
	:fail
	(multiple-value-bind (d c f)
	    (inter d c1 f)
	  (values d c f)))))

(defun inter-more (c0 &key d0 (f0 (make-hash-table)))
  (multiple-value-bind (d1 c1 f1)
      (inter d0 c0 f0)
    (declare (ignore c1))
    (lambda (c &key (d d1) (f f1))
      (inter c d f))))

(defun |fit-find-'| (s)
  ;;  (let ((*insertions* '(word |'| newlist find-xt appendlist
  ;;			set-xt)))
  (multiple-value-bind (d c f)
      (inter-cont s '(|'| word))
    (declare (ignore c f))
    (if (eq d :fail)
	0
	(+ 1
	   (if (length>= s 100) 0 1)
	   (if (or (not (length>= d 1))
		   (not (functionp (car d))))
	       0
	       (if (equal (car d) (gethash 'word *f*))
		   2
		   1))))))

(defun sic-signed-masker (length)
  (declare (type fixnum length)
	   (optimize (speed 3) (safety 0)))
  (let* ((length/2 (floor length 2))
	 (maxval (1- (floor length 2)))
	 (minval (- (floor length 2))))
    (lambda (n)
      (declare (type fixnum n))
      (if (or (> n maxval) (< n minval))
	  (the fixnum (- (mod (+ n length/2) length) length/2))
	  n))))

(defun sic-signed (n sics ip)
  (declare (type fixnum n ip)
	   (type (simple-array fixnum *) sics))
  (do* ((i 0 (1+ i))
	(len (length sics))
	(sic-mask (sic-signed-masker len)))
       ((>= i n))
    (let* ((a_ n)
	   (b_ (mod (1+ n) len))
	   (c_ (mod (+ 2 n) len))
	   (a (elt sics a_))
	   (b (elt sics b_))
	   (c (elt sics c_))
	   (r_ (- a b))
	   (r (funcall sic-mask r_)))
      (setf (elt sics (+ a (floor len 2))) r)
      (if (< a b)
	  (setf ip (funcall sic-mask (+ c ip)))
	  (setf ip (funcall sic-mask (+ 3 ip))))))
  ip)

(defun sic-masker (length)
  (lambda (x) (mod x length)))

(defun sic (n sics ip &key verbose verbose-sics)
  (declare (type fixnum n ip)
	   (type (simple-array fixnum *) sics))
  (do* ((i 0 (1+ i))
	(len (length sics))
	(len/2 (floor len 2)))
       ((>= i n))
    (let* ((mask (sic-masker len))
	   (a_ (elt sics ip))
	   (b_ (elt sics (funcall mask (1+ ip))))
	   (c_ (elt sics (funcall mask (+ 2 ip))))
	   (a (elt sics a_))
	   (b (elt sics b_))
	   (c (elt sics c_))
	   (as (if (>= a len/2) (- a len) a))
	   (bs (if (>= b len/2) (- b len) b))
	   (rs (- as bs))
	   (r (funcall mask rs)))
      (when verbose (prind ip a_ b_ c_ a b c as rs r))
      (when verbose-sics (prind sics))
      (setf (elt sics a_) r)
      (if (< rs 0)
	  (setf ip c)
	  (setf ip (funcall mask (+ 3 ip))))))
  (values sics ip))

(defun make-sic (contents &optional (sic-masker #'sic-masker))
  (let ((mask (funcall sic-masker (length contents))))
    (values (make-array (length contents)
			:element-type 'fixnum
			:initial-contents (mapcar mask contents))
	    mask)))

(defun copy-sic (contents)
  (copy-array contents))

(defun add-product-cases (cases fun)
  "Build the product of CASES into (C0 C1...) and sum the results of
calling (FITFUN C0 C1...)."
  (reduce #'+
	  (apply #'map-product
		 fun
		 cases)))

(defparameter *fit-verbose* nil)

(defun fit-asic-2 (code)
  (destructuring-bind (ticks length siccode) code
    (add-product-cases (list (range length) (range length))
		       (lambda (a b)
			 (multiple-value-bind (sic mask)
			     (make-sic siccode)
			   (setf (ith -1 sic) (funcall mask a))
			   (setf (ith -2 sic) (funcall mask b))
			   (sic ticks sic 0)
			   (let ((is (ith -1 sic))
				 (goal (funcall mask (- a b))))
			     (when *fit-verbose*
			       (prind a b is goal))
			     (if (= is goal)
				 0 -1)))))))

(defun fit-asic+2 (code)
  (destructuring-bind (ticks length siccode) code
    (add-product-cases (list (range length) (range length))
		       (lambda (a b)
			 (multiple-value-bind (sic mask)
			     (make-sic siccode)
			   (setf (ith -1 sic) (funcall mask a))
			   (setf (ith -2 sic) (funcall mask b))
			   (sic ticks sic 0)
			   (let ((is (ith -1 sic))
				 (goal (funcall mask (+ a b))))
			     (when *fit-verbose*
			       (prind a b is goal))
			     (if (= is goal)
				 0 -1)))))))

(defun offsp-fit-asic-2 (population)
  (declare (optimize (debug 3)))
  (destructuring-bind (a-ticks a-length a-siccode) (choice population)
    (ecase (choice '(:crossover :mutate))
      (:mutate (let* ((n-ticks (clamp (+ a-ticks (choice '(-1 0 1))) 1 100))
		      (mcode (let ((e (random a-length))
				   (c (copy-list a-siccode)))
			       (incf (elt c e) (choice '(-1 0 1)))
			       c))
		      (n-siccode (head (append mcode (list (random a-length)))
				       (clamp (+ a-length (choice '(-1 0 1)))
					      3 100)))
		      (n-length (length n-siccode)))
		 (list n-ticks n-length n-siccode)))
      (:crossover (destructuring-bind (b-ticks b-length b-siccode)
		      (choice population)
		    (let* ((n-ticks (truncate (+ a-ticks b-ticks) 2))
			   (mcode (mapcar (lambda (x y) (choice (list x y)))
					  a-siccode b-siccode))
			   (g-siccode (if (> a-length b-length)
					  a-siccode b-siccode))
			   (rest-diff (- (max a-length b-length)
					 (length g-siccode)))
			   (rest-len (if (= rest-diff 0) 0 (random rest-diff)))
			   (n-siccode (append mcode (last g-siccode rest-len)))
			   (n-length (length n-siccode)))
		      (list n-ticks n-length n-siccode)))))))

(defmacro offsp-preproc (f-offsp gencode-pattern)
  ;;  (destructuring-bind (a c (x y)) gencode
  ;;    (destructuring-bind (a (x y)) (funcall f-offsp (list a (list x y)))
  ;;      (list a c (list x y))))
  (labels ((constp (s)
	     (and (listp s) (length>= s 2) (eq (car s) 'const)))
	   (rec (pat d1-ll d1-res d2-ll d2-res)
	     (prind pat)
	     (cond
	       ((null pat) (values d1-ll
				   (cons 'list d1-res)
				   d2-ll
				   (cons 'list d2-res)))
	       ((constp (car pat)) (with-gensyms (c)
				     (rec (cdr pat)
					  (append d1-ll (list c))
					  d1-res
					  d2-ll
					  (append d2-res (list c)))))
	       ((listp (car pat)) (multiple-value-bind
					(e1-ll e1-res e2-ll e2-res)
				      (rec (car pat) nil nil nil nil)
				    (rec (cdr pat)
					 (append d1-ll (list e1-ll))
					 (append d1-res (list e1-res))
					 (append d2-ll (list e2-ll))
					 (append d2-res (list e2-res)))))
	       (t (with-gensyms (v)
		    (rec (cdr pat)
			 (append d1-ll (list v))
			 (append d1-res (list v))
			 (append d2-ll (list v))
			 (append d2-res (list v))))))))
    (multiple-value-bind (d1-ll d1-res d2-ll d2-res)
	(rec gencode-pattern nil nil nil nil)
      (prind d1-ll d1-res)
      (prind d2-ll d2-res)
      (prind f-offsp)
      (with-gensyms (gencode population postproc child)
	`(lambda (,population)
	   (let* ((,postproc (mapcar (lambda (,gencode)
				       (destructuring-bind ,d1-ll ,gencode
					 ,d1-res))
				     ,population))
		  (,child (funcall ,f-offsp ,postproc)))
	     (prind ,postproc ,child)
	     (destructuring-bind ,d1-ll (car ,population)
	       (destructuring-bind ,d2-ll ,child
		 ,d2-res))))))))
;; 	(lambda (,gencode)
;; 	  (destructuring-bind ,d1-ll ,gencode
;; 	    (destructuring-bind ,d2-ll (funcall ,f-offsp ,d1-res)
;; 	      ,d2-res)))))))

;; (offsp-preproc #'offsp-fit-asic-2 (1 (const 3) (0 0 0)))

;; (genetic (repeat '(1 4 (0 0 0 0)) 2)
;; 		  #'fit-asic-2
;; 		  10000
;; 		  #'offsp-fit-asic-2
;; 		  :verbose 2)

(defun fit-sic- (length &key verbose)
  (flet ((fit (s_tick)
	   (let ((diff 0)
		 (mask (sic-masker length))
		 (ticks (car s_tick))
		 (s (cadr s_tick)))
	     (do* ((i 0 (1+ i))
		   (mem (make-sic s) (make-sic s)))
		  ((>= i length))
	       (setf (elt mem (1- length)) i)
	       (sic ticks mem 0)
	       (when verbose (prind i (elt mem (1- length))))
	       (setf diff (+ diff (if (= (elt mem (1- length))
					 (funcall mask (- i)))
				      0 1))))
	     (- diff))))
    #'fit))

(defun fit-sic-2 (length)
  (flet ((fit (s_tick)
	   (let ((diff 0)
		 (mask (sic-masker length))
		 (ticks (car s_tick))
		 (s (cadr s_tick)))
	     (do* ((j 0 (1+ j))) ((>= j length))
	       (do* ((i 0 (1+ i))
		     (mem (make-sic s) (make-sic s)))
		    ((>= i length))
		 (setf (elt mem (1- length)) i)
		 (setf (elt mem (- length 2)) j)
		 (sic ticks mem 0)
		 (setf diff (+ diff (if (= (elt mem (1- length))
					   (funcall mask (- i j)))
					0 1)))))
	     (- diff))))
    #'fit))

(defun fit-sic+ (length)
  (flet ((fit (s_tick)
	   (let ((diff 0)
		 (mask (sic-masker length))
		 (ticks (car s_tick))
		 (s (cadr s_tick)))
	     (do* ((j 0 (1+ j))) ((>= j length))
	       (do* ((i 0 (1+ i))
		     (mem (make-sic s) (make-sic s)))
		    ((>= i length))
		 (setf (elt mem (1- length)) i)
		 (setf (elt mem (- length 2)) j)
		 (sic ticks mem 0)
		 (setf diff (+ diff (if (= (elt mem (1- length))
					   (funcall mask (+ i j)))
					0 1)))))
	     (- diff))))
    #'fit))

(defun offsp-sic (length)
  (labels ((mask (n) (funcall (sic-masker length) n))
	   (offsp-mem (population case)
	     (let ((p1 (choice population)))
	       (ecase case
		 (:mut (map 'vector
			    (lambda (x)
			      (mask (+ (- x 1) (random 3))))
			    p1))
		 (:cross (let ((p2 (choice population))
			       (middle (random (1+ length))))
			   (replace (make-sic p1) p2
				    :start1 middle
				    :start2 middle))))))
	   (offsp-1 (population)
	     (ecase (choice '(:mut :cross))
	       (:mut (list (clamp (+ (car (choice population)) -5 (random 11))
				  0 1000)
			   (offsp-mem (mapcar #'cadr population) :mut)))
	       (:cross (list (floor (+ (car (choice population))
				       (car (choice population)))
				    2)
			     (offsp-mem (mapcar #'cadr population) :cross)))))
	   (offsp (population)
	     (let ((child (offsp-1 population)))
	       (if (= (random 10) 0)
		   (offsp (cons child population))
		   child))))
    #'offsp))

(defun fit-plus (s)
  (- (abs (- 10 (+ (car s) (cadr s))))))

(defun offsp-plus (pop-independent-p choice-independent-p)
  (labels ((mutate (n)
	     (+ -1 n (random 3)))
	   (cross (n m)
	     (floor (+ n m) 2))
	   (offsp* (c1 d1 c2 d2 case1 case2)
	     (mapcar (lambda (x y c)
		       (ecase c
			 (:mut (mutate x))
			 (:cross (cross x y))))
		     (list c1 d1)
		     (list c2 d2)
		     (list case1 case2)))
	   (offsp (population)
	     (let ((p1 (choice population))
		   (p2 (choice population))
		   (cases (choice '(:mut :cross))))
	       (if pop-independent-p
		   (if choice-independent-p
		       (offsp* (car (choice population))
			       (cadr (choice population))
			       (car (choice population))
			       (cadr (choice population))
			       (choice '(:mut :cross))
			       (choice '(:mut :cross)))
		       (offsp* (car (choice population))
			       (cadr (choice population))
			       (car (choice population))
			       (cadr (choice population))
			       cases cases))
		   (if choice-independent-p
		       (offsp* (car p1) (cadr p1)
			       (car p2) (cadr p2)
			       (choice '(:mut :cross))
			       (choice '(:mut :cross)))
		       (offsp* (car p1) (cadr p1)
			       (car p2) (cadr p2)
			       cases cases))))))
    #'offsp))

(defun freq-est-binomial-probability (nextf &key (ci .1) (alpha .01) max-n)
  (labels ((rec (n count-t)
	     (multiple-value-bind (minp maxp)
		 (stats:binomial-probability-ci n (/ count-t n) alpha)
	       (if (or (<= (- maxp minp) ci) (and max-n (>= n max-n)))
		   (values minp maxp n (/ count-t n))
		   (rec (1+ n) (if (funcall nextf)
				   (1+ count-t)
				   count-t))))))
    (rec 2 (+ (if (funcall nextf) 1 0)
	      (if (funcall nextf) 1 0)))))

(defun fit-sic+2 (length)
  (flet ((fit (ticks_s_repr)
	   (let ((diff 0)
		 (mask (sic-masker length))
		 (ticks (car ticks_s_repr))
		 (s (cadr ticks_s_repr)))
	     (do* ((j 0 (1+ j))) ((>= j length))
	       (do* ((i 0 (1+ i))
		     (mem (make-sic s) (make-sic s)))
		    ((>= i length))
		 (setf (elt mem (1- length)) i)
		 (setf (elt mem (- length 2)) j)
		 (sic ticks mem 0)
		 (setf diff (+ diff (if (= (elt mem (1- length))
					   (funcall mask (+ i j)))
					0 1)))))
	     (- diff))))
    #'fit))

(defun offsp-sic+2 (length)
  (let ((masker (sic-masker length)))
    (labels ((mask (n) (funcall masker n))
	     (offsp-mem (population case)
	       (let ((p1 (choice population)))
		 (ecase case
		   (:mut (map 'vector
			      (lambda (x)
				(mask (+ (- x 1) (random 3))))
			      p1))
		   (:cross (let ((p2 (choice population))
				 (middle (random (1+ length))))
			     (replace (make-sic p1) p2
				      :start1 middle
				      :start2 middle))))))
	     (offsp-1 (population)
	       (ecase (choice '(:mut :cross))
		 (:mut (list (clamp (+ (car (choice population)) -5 (random 11))
				    0 1000)
			     (offsp-mem (mapcar #'cadr population) :mut)))
		 (:cross (list (floor (+ (car (choice population))
					 (car (choice population)))
				      2)
			       (offsp-mem (mapcar #'cadr population) :cross)))))
	     (offsp (population)
	       (let ((child (offsp-1 population)))
		 (if (= (random 10) 0)
		     (offsp (cons child population))
		     child))))
      #'offsp)))


;; (defstruct sicfo
;;   ancestor
;;   normalize)

;; (defun sicfo.iid (&rest rest)
;;   (lambda (c)
;;     (declare (ignore c))
;;     (make-sicfo :ancestor 0
;; 		:normalize (constantly (constantly nil)))))

;; (defun sicfo.const (value)
;;   (lambda (c)
;;     (declare (ignore c))
;;     (make-sicfo :ancestor value
;; 		:normalize (lambda (pos)
;; 			     (lambda (mem &rest rest)
;; 			       (declare (ignore rest))
;; 			       (setf (elt mem pos) value))))))

;; (defun sicfo.forall (name &rest list)
;;   (lambda (c)
;; ;; (values name
;; ;; 	  (let ((value 0))
;; ;; 	    (lambda (size)
;; ;; 	      (incf value)
;; ;; 	      (if (>= value size) nil value)))))    
    
;; ))

;; (defstruct sictype
;;   memsize
;;   masker)
  

;; ;; genetic-sicfo must be a macro, otherwise the length of the sic must be given explicitly
;; (defmacro genetic-sicfo (list &key sic-masker)
;;   (let* ((masker (or sic-masker #'sic-masker))
;; 	 (c (make-sictype :memsize (length list)
;; 			  :masker masker))
;; 	 ancestors
;; 	 fitness
;; 	 generations
;; 	 foffspring)
;;     (mapc (lambda (x)
;; 	    (let* ((sicfo-inst (funcall x c))
;; 		   (ancestor (sicfo-ancestor sicfo-inst))
;; 		   (normalize (sicfo-normalize sicfo-inst)))
;; 	      ;; generate ancestors, fitness, foffspring
;; 	      ....
;; 	      ))

;; 	  list)))






;; (genetic-sic (genetic-sicfo (append (repeat (sicfo.iid) (* 3 2))
;; 				    (sicfo.forall x
;; 						  (sicfo.io-slot 
;; 						   :input x
;; 						   :goal (1+ x)))
;; 				    (sicfo.const 0))
;; 			    ))


;; ;;(defun make-genetic-sic (sic-content)
;; ;;   (defstruct
;; ;;   (labels ((iid (&rest rest)
;; ;; 	     (make-sicfo :ancestor 0
;; ;; 			 :normalize (constantly nil)))
;; ;; 	   (forall (name)
;; ;; 	     (values name
;; ;; 		     (let ((value 0))
;; ;; 		       (lambda (size)
;; ;; 			 (incf value)
;; ;; 			 (if (>= value size) nil value)))))
;; ;; 	   (test (&key :input 
	 

  
  

;; (defclass sicfo.iid ()
;;   ())

;; (defmethod sicfo-ancestor ((i sicfo.iid) (size fixnum))
;;   (declare (ignore i size))
;;   0)


  
  
;; ;; goal:
;; (genetic-sic (make-genetic-sic (append (repeat (sicfo.iid) (* 3 2))
;; 				       (forall x
					       
;; 					 (test :input (forall x)
;; 					  :goal (1+ x))
;; 					 (const 0)))))

(defun genetic-sic (ancestors fitness generations foffspring &key verbose
		    (unfit :serial))
  (let* ((population (copy-list ancestors))
	 (fit (mapcar fitness population)))
    (dotimes (gen generations (let ((i (max-index fit)))
				(values (elt population i) (elt fit i))))
      (when (not (null verbose))
	(prind gen)
	(when (and (numberp verbose) (>= verbose 2))
	  (prind population)))
      (ecase unfit
	(:serial
	 (let ((child (funcall foffspring population))
	       (index (min-index fit)))
	   (setf (elt population index) child)
	   (setf (elt fit index) (funcall fitness child))))
	((:parallel :parallel-strict)
	 (let ((minfit (apply #'min fit))
	       (oldpop (copy-list population)))
	   (dotimes (index (length population))
	     (if (= (elt fit index) minfit)
		 (let* ((child (funcall foffspring oldpop))
			(child-fit (funcall fitness child)))
		   (when (ecase unfit
			   (:parallel (>= child-fit minfit))
			   (:parallel-strict (> child-fit minfit)))
		     (setf (elt population index) child)
		     (setf (elt fit index) child-fit)))))))))))
