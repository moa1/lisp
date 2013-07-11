;(in-package #:joy-arch) ;does this require an .asd file?

;(sb-ext:restrict-compiler-policy 'debug 3)
;(declaim (optimize speed))

(defclass joy ()
  ((stack :accessor joy-stack
	  :initform nil
	  :initarg :stack)
   (code :accessor joy-code
	 :initform nil
	 :initarg :code)
   (steps :accessor joy-steps
	  :initform 0
	  :initarg :steps)))

(define-condition joy-error (error)
  ((text :initarg :text :reader :text)))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun zip-array (result-type &rest arrays)
  (apply #'map result-type #'list arrays))

(defun proper-list-p (l)
  (if (null l)
      t
      (if (consp l)
	  (proper-list-p (cdr l))
	  nil)))

(defun make-counter (&optional (counter 0))
  (declare (type fixnum counter))
  (if (<= counter 0)
      (lambda () 1)
      (let ((c counter))
	(lambda () (decf c)))))

(defun make-countdown (&optional (seconds .01))
  "Returns a function that, when called, yields the number of internal run time units remaining until the timer expires."
  (declare (type single-float seconds))
  (if (<= seconds 0)
      (lambda () 1)
      (let ((c (+ (get-internal-run-time) (* seconds internal-time-units-per-second))))
	(lambda () (- c (get-internal-run-time))))))

(defun mean (seq)
  (/ (apply #'+ seq) (length seq)))

(defun square (x)
  (* x x))

(defun var-corr (seq)
  (let ((m (mean seq)))
    (/ (loop for x in seq sum (square (- x m))) (- (length seq) 1))))

(defun stddev-corr (seq)
  (sqrt (var-corr seq)))

(defun deterministic-fun-cacher (fun slots)
  (let ((ht (make-hash-table :test 'equal :size (* 3/2 slots))))
    (lambda (&rest rest)
      (multiple-value-bind (v p) (gethash rest ht)
	(if p
	    v
	    (let ((val (apply fun rest)))
	      (when (>= (hash-table-count ht) slots)
		(print "clrhash")
		(clrhash ht))
	      (setf (gethash rest ht) val)))))))

(defun sample (seq)
  (let ((l (length seq)))
    (elt seq (random l))))

(defun weighted-sample-index (w)
  "Returns the index of the picked weight from w.
Elements of w are numbers and represent the relative chance of picking that item.
The sum of w must not be 0."
  (let* ((s (reduce #'+ w :from-end T))) ;:from-end T is necessary because otherwise (weighted-sample-index '(0.99 0.989622 0.9895308 0.9893857 0.9898456 0.98966277 0.9894621 0.99 0.9889513 0.9896954)) fails with a high probability in 10.000.000 cases. probably a corner case of float arithmetic. with :from-end we add in the same order in that we are subtracting in rec.
    (labels ((rec (x v i)
	       (let ((x0 (- x (car v))))
		 (if (< x0 0)
		     i
		     (rec x0 (cdr v) (1+ i))))))
      (rec (random s) w 0)))) ; will (correctly) give an error if s = 0

(defun weighted-sample (w seq)
  (elt seq (weighted-sample-index w)))

(defun chance (p)
  (if (< (random 1.0) p)
      t
      nil))

(defun unique (l &optional (test #'eql))
  "Return the unique (under equality test) elements in list l in random order."
  (let ((ht (make-hash-table :test test))
	(u nil))
    (mapc (lambda (x) (setf (gethash x ht) t)) l)
    (maphash (lambda (k v) (declare (ignore v)) (setf u (cons k u))) ht)
    u))

;; '((1) 1 DEFINE 1)
(defun joy-eval (stk exp &key (heap (make-hash-table)) (c (make-counter 0)) (cd (make-countdown 0.0)))
  (declare (optimize (debug 0) (compilation-speed 0) (speed 3) (space 0))
	   (type (function () fixnum) c cd))
  ;; note that this function does not fail for the same inputs as the joy implementation by Manfred von Thun, e.g. '(branch) returns nil, but would fail for the real implementation.
  ;; however, it should raise an error for cases when the stack becomes a non-list.
  (let ((c (funcall c)) (cd (funcall cd)))
    ;;(print (list "stk" stk "exp" exp "c" c "cd" cd))
    (when (<= c 0)
      (return-from joy-eval 'overrun))
    (when (<= cd 0)
      (return-from joy-eval 'timeout)))
  (if (null exp)
      stk
      (joy-eval
       (case (car exp)
	 (+       (cons (+ (cadr stk) (car stk)) (cddr stk))) ;add
	 (and     (cons (and (car stk) (cadr stk)) (cddr stk)))
	 (branch  (if (caddr stk)
		      (joy-eval (cdddr stk) (cadr stk) :heap heap :c c :cd cd)
		      (joy-eval (cdddr stk) (car stk) :heap heap :c c :cd cd)))
	 (concat  (cons (append (cadr stk) (car stk)) (cddr stk)))
	 (cons    (cons (cons (cadr stk) (let ((a (car stk))) (if (listp a) a (error (make-condition 'type-error :datum a :expected-type 'list))))) (cddr stk))) ; same as papply
	 (dip     (cons (cadr stk) (joy-eval (cddr stk) (car stk) :heap heap :c c :cd cd)))
	 (/       (cons (/ (cadr stk) (car stk)) (cddr stk))) ;divide
	 (dup     (cons (car stk) stk))
	 (equal   (cons (equal (cadr stk) (car stk)) (cddr stk)))
	 (i       (joy-eval (cdr stk) (car stk) :heap heap :c c :cd cd)) ;same as apply
	 (ifte    (if (car (joy-eval (cdddr stk) (caddr stk) :heap heap :c c :cd cd)) ; similar to branch
		      (joy-eval (cdddr stk) (cadr stk) :heap heap :c c :cd cd)
		      (joy-eval (cdddr stk) (car stk) :heap heap :c c :cd cd)))
	 (*       (cons (* (car stk) (cadr stk)) (cddr stk))) ;multiply
	 (nill    (cons nil stk)) ;same as false
	 (not     (cons (not (car stk)) (cdr stk))) ; can be emulated by branch
	 (or      (cons (or (car stk) (cadr stk)) (cddr stk)))
	 (pop     (cdr stk))
	 (pred    (cons (1- (car stk)) (cdr stk)))
	 (quote   (cons (list (car stk)) (cdr stk)))
	 (rem     (cons (mod (cadr stk) (car stk)) (cddr stk)))
	 (<       (cons (< (cadr stk) (car stk)) (cddr stk))) ;smaller
	 (stack   (cons stk stk))
	 (step    (let ((res (cddr stk)))
		    (loop for i in (cadr stk) do
			 (setf res (joy-eval (cons i res) (car stk) :heap heap :c c :cd cd)))
		    res))
	 (-       (cons (- (cadr stk) (car stk)) (cddr stk))) ;subtract
	 (succ    (cons (1+ (car stk)) (cdr stk)))
	 (swap    (cons (cadr stk) (cons (car stk) (cddr stk))))
	 (times   (let ((res (cddr stk)) (n (cadr stk)))
		    (dotimes (i n res)
		      (setf res (joy-eval res (car stk) :heap heap :c c :cd cd)))))
	 (true    (cons t stk))
	 (uncons  (cons (cdar stk) (cons (caar stk) nil)))
	 (unstack (let ((a (car stk))) (if (listp a) (car stk) (error (make-condition 'type-error :datum a :expected-type 'list)))))
	 (while   (let ((res (cddr stk)))
		    (do () ((not (car (joy-eval res (cadr stk) :heap heap :c c :cd cd))))
		      (setf res (joy-eval res (car stk) :heap heap :c c :cd cd)))
		    res))
	 ;; define is special
	 (define  (if (null heap) (error "define doesn't work for a nil heap")
		      (if (or (null (cadr exp)) (not (symbolp (cadr exp))))
			  (error (make-condition 'type-error :datum (cadr exp) :expected-type '(and (not null) symbolp)))
			  (progn
			    (setf (gethash (cadr exp) heap) (car stk))
			    (setf exp (cdr exp))
			    (cdr stk)))))
	 ;; implement an "undefine", which ends the scope of a "define"d program, but leaves defined programs (and programs on the stack) using the to be "undefine"d program running intact. this would require replacing the "define"d name with an anonymous name.
	 (t 
	  (if (null heap)
	      (cons (car exp) stk)
	      (multiple-value-bind (value present-p) (gethash (car exp) heap)
		(if present-p
		    (progn (setf exp (append '(1) value (cdr exp))) stk)
		    (cons (car exp) stk))))))
       (cdr exp) :heap heap :c c :cd cd)))

;; For example, the step combinator can be used to access all elements of an aggregate in sequence. For strings and lists this means the order of their occurrence, for sets it means the underlying order. The following will step through the members of the second list and swons them into the initially empty first list. The effect is to reverse the non-empty list, yielding [5 6 3 8 2].  
;;        []  [2 8 3 6 5]  [swons]  step

;; step 	A [P] -> ...
;;	Sequentially putting members of aggregate A onto stack, executes P for each member of A.

;; cons 	X A -> B
;;	Aggregate B is A with a new member X (first member for sequences).

;; swons 	A X -> B
;;	Aggregate B is A with a new member X (first member for sequences).

;; uncons 	A -> F R
;;	F and R are the first and the rest of non-empty aggregate A.

;; unswons 	A -> R F
;;	R and F are the rest and the first of non-empty aggregate A.

;; stack 	.. X Y Z -> .. X Y Z [Z Y X ..]
;;	Pushes the stack as a list.

;; unstack 	[X Y ..] -> ..Y X
;;	The list [X Y ..] becomes the new stack.

;; filter 	A [B] -> A1
;;	Uses test B to filter aggregate A producing sametype aggregate A1.
;;      helpdetail == [help-list [first =] filter [help-print-tentry] map] map pop;

;; 1 2 3 4 5 6 7 [pop pop stack [1] equal not] [stack put pop] while .
;; [7 6 5 4 3 2 1] [6 5 4 3 2 1] [5 4 3 2 1] [4 3 2 1] 3

(defun joy-test (stk exp res)
  (let ((r (joy-eval stk exp :heap (make-hash-table))))
    (if (not (equal r res))
	(error (format nil "joy-test failed for stk:~A exp:~A res:~A r:~A"
		       stk exp res r)))))

(joy-test nil '(nil nil) '(nil nil))
(joy-test nil '(5 4 +) '(9))
(joy-test nil '(nil 5 and) '(nil))
(joy-test nil '(3 5 and) '(3))
(joy-test nil '(true (1) (2) branch) '(1))
(joy-test nil '(nill (1) (2) branch) '(2))
(joy-test nil '(0 (1 2 3) (4 5 6) concat) '((1 2 3 4 5 6) 0))
(joy-test nil '(4 (3) cons) '((4 3)))
(joy-test nil '(1 2 5 (+) dip) '(5 3))
(joy-test nil '(5 2 /) '(5/2))
(joy-test nil '(3 dup) '(3 3))
(joy-test nil '(3 3 equal) '(t))
(joy-test nil '(3 true equal) '(nil))
(joy-test nil '((3) (3) equal) '(t))
(joy-test nil '((3) (true) equal) '(nil))
(joy-test nil '(2 (3 +) i) '(5))
(joy-test nil '((true) (1) (2) ifte) '(1))
(joy-test nil '((nill) (1) (2) ifte) '(2))
(joy-test nil '((0 true) (1) (2) ifte) '(1))
(joy-test nil '(3 4 *) '(12))
(joy-test nil '(nill) '(()))
(joy-test nil '(true not) '(nil))
(joy-test nil '(nil 5 or) '(5))
(joy-test nil '(nil nil or) '(nil))
(joy-test nil '(5 4 pop) '(5))
(joy-test nil '(5 pred) '(4))
(joy-test nil '(5 quote) '((5)))
(joy-test nil '(9 4 rem) '(1))
(joy-test nil '(1 2 <) '(t))
(joy-test nil '(2 1 <) '(nil))
(joy-test nil '(1 2 stack) '((2 1) 2 1))
(joy-test nil '((swap cons) define swons 0 nil (1 2 3) (swons) step) '((3 2 1) 0))
(joy-test nil '(4 5 -) '(-1))
(joy-test nil '(3 succ) '(4))
(joy-test nil '(1 2 swap) '(1 2))
(joy-test nil '(5 (1) times) '(1 1 1 1 1))
(joy-test nil '(2 5 (2 *) times) '(64))
(joy-test nil '(true) '(t))
(joy-test nil '(4 5 quote cons uncons) '((5) 4))
(joy-test nil '(0 (1 2) unstack) '(1 2))
(joy-test nil '(1 2 3 4 5 6 7 (pop pop stack (1) equal not) (pop) while) '(3 2 1))
;; 5 [0 < not] [[1] dip pred] while stack . ;; puts -1 and 6 ones on the stack
;; (joy-eval '(5) '((pred dup 0 < () ((1) dip a) branch) define a a))
;; define is special
(joy-test nil '(2 (dup +) define superman) '(2))
(joy-test nil '(2 (1) define superman superman) '(1 2))
(joy-test nil '((1) define a a (2) define a a 1) '(1 2 1))
(joy-test nil '(1 a) '(a 1))
;; own defines. maybe write fitness tests for letting them find
(joy-test nil '((0 equal) define null 0 null) '(t))
(joy-test nil '((0 equal) define null 1 null) '(nil))
(joy-test nil '((pred) define dec 5 dec) '(4))
(joy-test nil '((succ) define inc 5 inc) '(6))
(joy-test nil '((swap cons) define swons (2) 1 swons) '((1 2)))
(joy-eval nil
	  '(((dup cons) swap concat dup cons i) define y
	    (((pop null) (pop succ) ((dup pred) dip i *) ifte) y) define fac
	    1 fac) :heap (make-hash-table))


(defun joy-eval-handler (stk exp &key (heap (make-hash-table)) (c (make-counter)) (cd (make-countdown)))
  (handler-case (joy-eval stk exp :heap heap :c c :cd cd)
    #+CMU (simple-error () 'error)
    #+CMU (arithmetic-error () 'error)
    (simple-type-error () 'error)
    (type-error () 'error)
    (division-by-zero () 'error)
    (floating-point-invalid-operation () 'error)
    (floating-point-overflow () 'error)))

(defparameter *joy-ops* 
  '(+ and branch concat cons dip / dup equal i * nill not or pop pred quote rem < stack step - succ swap times true uncons unstack while define))

(defun mutate (exp debranch-p p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 
	       q1 q2 q3 q4 q5 q6 q7 q8 q9 q10
	       &rest ops-p)
;;  (print (list "exp" exp))
  (flet ((random-final (p)
	   (if (< (random 1.0) p)
	       (weighted-sample (list q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
				'(a b c d e 1 2 4 8 16))
	       (weighted-sample ops-p *joy-ops*))))
    (cond
      ((null exp) (if (< (random 1.0) p1) ;extend
		      (cons (random-final p2)
			    nil)))
      ((listp exp) (if (and debranch-p (< (random 1.0) p3))
		       (random-final p4) ; de-branch
		       (cons (apply #'mutate (car exp) t p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 ops-p)
			     (if (and (= 1 (length (cdr exp)))
				      (< (random 1.0) p5))
				 nil ; shorten
				 (apply #'mutate (cdr exp) nil p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 ops-p)))))
      (t (if (< (random 1.0) p6)
	     (if (< (random 1.0) p7)
		 (cons (if (< (random 1.0) p8)
			   (random-final p9) ; branch
			   exp)
		       nil)	
		 (random-final p10)) ; substitute
	     exp)))))
   
(defun mutate-rec (exp &rest probs)
;;  (print (list "exp" exp "p0" p0))
  (let ((p0 (car probs))
	(prest (cdr probs)))
    (assert (< p0 .9))
    (if (< (random 1.0) p0)
	(apply #'mutate-rec (apply #'mutate exp t prest) probs)
	(apply #'mutate exp t prest))))

(defun mutate-mutate (p0 probs)
  (labels ((rec (probs mut-probs)
	     (if (null probs)
		 (nreverse mut-probs)
		 (if (<= (random 1.0) p0)
		     (rec (cdr probs) (cons (random .99) mut-probs)) ;previously, (random .99) was a random walk +- 0.1. I think mutation parameter smoothness should be provided by mutate-crossover.
		     (rec (cdr probs) (cons (car probs) mut-probs))))))
    (rec (cdr probs) (cons (random .9) nil)))) ; recursion prob < 0.9

(defun mutate-crossover (probs1 probs2)
  (labels ((rec (probs1 probs2 acc)
	     (if (null probs1)
		 (progn
		   (assert (null probs2))
		   (nreverse acc))
		 (rec (cdr probs1) (cdr probs2)
		      (cons (/ (+ (car probs1) (car probs2)) 2) acc)))))
    (rec probs1 probs2 nil)))

;; I commented out the two lines starting with (sb-kernel::
(defun find-mutate (exp prob times)
  (print (list "(find-mutate" exp prob times ")"))
  (let ((hits 0))
    (dotimes (i times (/ hits times 1.0))
      (handler-case
	  (progn
	    (destructuring-bind ((v1 v2) v3) (mutate-rec exp prob) (print (list (list v1 v2) v3)))
	    (incf hits))
	;;(division-by-zero () (format t "division-by-zero~%") 1)
;	(sb-kernel::arg-count-error () t)
;	(sb-kernel::defmacro-bogus-sublist-error () t)
	(type-error () t)))))

(defun subexps (exp subs)
  ;; speed this function up by tail recursion
  (if (null exp)
      subs
      (if (consp exp)
	  (if (consp (car exp))
	      (nconc (subexps (car exp) (cons (car exp) nil))
		     (subexps (cdr exp) subs))
	      (subexps (cdr exp) subs))
	  subs)))

(defun mapexps (function exp)
  "Recreate the EXP from the first result of FUNCTION called with each subexp.
The second result of FUNCTION indicates, for cases where the argument of
FUNCTION is a subexp, whether the subexp should be recursed.
Example: (mapexps (lambda (x) (values (print x) t)) '(1 (2) (3 (4))))"
  ;; speed this up by using nconcs and (nreverse acc)
  (labels ((rec (exp acc)
	     (if (null exp)
		 acc
		 (if (consp (car exp))
		     (multiple-value-bind (v r) (funcall function (car exp))
		       (if r
			   (append acc
				   (cons (rec (car exp) nil) nil)
				   (rec (cdr exp) nil))
			   (append acc
				   (cons v nil)
				   (rec (cdr exp) nil))))
		     (rec (cdr exp) (append acc
					    (cons (funcall function (car exp)) nil)))))))
    (rec exp nil)))

(defun crossover (exp1 exp2 p0 p1)
  (let* ((se (subexps exp2 (cons exp2 nil))))
	(mapexps (lambda (x)
		   (if (consp x)
		       (if (chance p0)
			   (values (sample se) nil)
			   (values x t))
		       (if (chance p1)
			   (sample se)
		       x)))
		 exp1)))

(defun crossover-and-mutate (exp1 exp2 &rest probs)
  (let* ((p1 (elt probs 1))
	 (p2 (elt probs 2))
	 (prest (cons (car probs) (cdddr probs)))
	 (cross (crossover exp1 exp2 p1 p2))
	 (new (apply #'mutate-rec cross prest)))
    new))

(defun make-arbitrary-tree-sampler (nodes)
  "Return a function that, upon calling, returns a randomly sampled tree with a node maximum count of NODES (and a minimum node count of 1, i.e. '(A))."
  (let* ((structs-nested (loop for i from 1 upto nodes collect (enumerate-tree-structures i)))
	 (structs (apply #'nconc structs-nested)))
    (lambda (symbol-f)
      "Return a randomly sampled tree and each symbol is the returned symbol of the nullary function SYMBOL-F."
      (let* ((s (sample structs))
	     (syms (count-symbols-in-tree s))
	     (res (replace-symbols-in-tree s (loop for i below syms collect (funcall symbol-f)))))
	res))))

;; (defun align (exp1 exp2 &optional r)
;;   (cond
;;     ((and (null exp1) (null exp2)) r)
;;     ((null exp1) (if (< (random 1.0) 0.5)
;; 		     (append r exp2)
;; 		     r))
;;     ((null exp2) (if (< (random 1.0) 0.5)
;; 		     (append r exp1)
;; 		     r))
;;     ((and (listp (car exp1))
;; 	  (listp (car exp2))) (if (< (random 1.0) 0.5)
;; 				  (intersect (cdr exp1) (cdr exp2)
;; 					     (append (list (car exp1)) r))
;; 				  (intersect (cdr exp1) (cdr exp2)
;; 					     (append (list (car exp2)) r))))
;;     ((listp (car exp1)) 
     
(defun fitness-max (o)
  (let ((res (joy-eval-handler nil o :c (make-counter 100) :cd (make-countdown .01))))
    (if (and (listp res) (numberp (car res)))
	(car res)
	0)))

(defun fitness-reverse (o)
  (let ;;((rev (joy-eval-handler nil o :c (make-counter 1000))))
      ((rev o))
    (if (not (listp rev))
	0
	(labels ((valid-reverse (len)
		   (let* ((l (loop for i below len collect 
				  (sample '(a b c d e f 1 2 3))))
			  (r (joy-eval-handler (list l) rev :c (make-counter 1000) :cd (make-countdown .01)))
			  (lr (reverse l)))
;;		     (format t "rev:~A r:~A lr:~A~%" rev r lr)
		     (if (or (not (listp r)) (not (proper-list-p (car r)))
			     (not (= (length (car r)) len)))
			 0
			 (loop for i in (car r) for j in lr
			    sum (if (eq i j) (/ 1 len) 0))))))
	  (loop for i in '(1
			   2
			   3
			   6 6 6 6 6
			   10 10 100)
	     sum (valid-reverse i))))))

;; Previously, there was a function here that tried to check if an expression is valid by looking at the values immediately before an instruction.
;; This function cannot exclude expressions for instructions that expect list(s), since the instruction before could leave list(s) on the stack but are not lists themselves.
;; In addition, the function worked recursively on sub-expressions. Therefore, the expression '(+) cannot be excluded since it is valid in '(1 2 (+) i).

;; the next try for syntactically excluding expression is creating a database that contains the expected and remaining types on the stack.
;; the order of the types is in the same order as in "A Joy Library for Online Help Generation.html", i.e. in the order in that the objects appear on the expression stream.
;; :any means the instruction can modify the stack in any way.
(defparameter *joy-ops-types*
  '((+       (number number) (number))
    (and     (t t) (boolean))
    (branch  (t list list) (:any))
    (concat  (list list) (list))
    (cons    (t list) (list))
    (dip     (t list) (:any))
    (/       (number number) (number))
    (dup     (t) (t t))
    (equal   (t t) (boolean))
    (i       (list) (:any))
    (ifte    (list list list) (:any))
    (*       (number number) (number))
    (nill    () (list))
    (not     (t) (boolean))
    (or      (t t) (boolean))
    (pop     (t) nil)
    (pred    (number) (number))
    (quote   (t) (list))
    (rem     (number number) (number))
    (<       (number number) (boolean))
    (stack   nil (list))
    (step    (list list) (:any))
    (-       (number number) (number))
    (succ    (number) (number))
    (swap    (t t) (t t))
    (times   (number list) (:any))
    (true    () (boolean))
    (uncons  (list) (t list)) ;t is top of stack, list is 2nd of stack
    (unstack (list) (:any))
    (while   (list list) (:any))
    (define  (list) nil)))

(defun make-expected-remaining-mappings ()
  (let ((ht-ex (make-hash-table :test 'eq :size (length *joy-ops-types*)))
	(ht-re (make-hash-table :test 'eq :size (length *joy-ops-types*))))
    (loop for types in *joy-ops-types* do
	 (let ((op (car types))
	       (ex (cadr types))
	       (re (caddr types)))
	   (setf (gethash op ht-ex) ex)
	   (setf (gethash op ht-re) re)))
    (values ht-ex ht-re)))

(defparameter *joy-ops-types-expected* (nth-value 0 (make-expected-remaining-mappings)))
(defparameter *joy-ops-types-remaining* (nth-value 1 (make-expected-remaining-mappings)))

(defun typep-symbol (type-symbol type)
  "Return whether the type-symbol is of type type.
For example 'float is of type 'number, but 'number is not of type 'float.
This should hold: (typep val type) = (typep-symbol (type-of val) type), but doesn't always, because e.g. (type-of 5) returns (INTEGER 0 4611686018427387903)."
  ;;(format t "type-symbol:~A type:~A~%" type-symbol type)
  (or (eq type t)
      (eq type-symbol type)
      (ecase type-symbol
	(float (or (eq type 'number) (eq type 'float) (eq type 'short-float) (eq type 'single-float) (eq type 'double-float) (eq type 'long-float)))
	(number nil) ;there is no type more general than number, except t, which was tested above
	(list nil)
	(boolean nil)
	;;more types to be implemented as necessary
	)))

(defun typep-symbol-joy (type-symbol type)
  "Returns whether type-symbol is of type type or type-symbol is T."
  (or (eq type-symbol t) (typep-symbol type-symbol type)))

(defun expect-stk-type (ex stk)
  "Returns whether ex, the list of expected types is present in stk, the list of present types.
For example, (expect-stk-type '(list :any number) '(list list)) is T, but (expect-stk-type '(list number) '(list list)) is NIL.
As a second value, the remainder of stk is returned, or :error if the stack didn't contain the expected types."
  (if (null ex)
      (values t stk) ;nothing expected
      (if (null stk)
	  (values nil :error) ;there is an expected type, but stk is empty
	  (if (eq (car stk) :any)
	      (values t stk) ;there is an expected type and stk could be anything
	      (and (typep-symbol-joy (car stk) (car ex))
		   (expect-stk-type (cdr ex) (cdr stk)))))))

(defun valid-joy-exp (exp &optional (stk nil))
  "Return whether exp can be a syntactically valid joy expression."
  (labels ((rec (exp stk)
	     ;;(format t "exp:~A stk:~A~%" exp stk)
	     (if (null exp)
		 t
		 (if (listp (car exp))
		     (and (rec (car exp) '(:any)) ;stack could be anything for subexpressions
			  (rec (cdr exp) (cons 'list stk)))
		     (let ((op (car exp)))
		       (multiple-value-bind (ex op-p)
			   (gethash op *joy-ops-types-expected*)
			 (if op-p
			     (multiple-value-bind (matches stk-nthcdr)
				 (expect-stk-type ex stk)
			       (and matches
				    (let ((re (gethash op *joy-ops-types-remaining*)))
				      (rec (cdr exp) (append re stk-nthcdr))))) ;this can also append (:any)
			     (if (numberp op)
				 (rec (cdr exp) (cons 'number stk))
				 (rec (cdr exp) (cons :any stk)))))))))) ;func call
    (rec exp stk)))

(assert (eq t (valid-joy-exp nil))) ;test nil
(assert (eq t (valid-joy-exp '(1 2 +)))) ;test number matching
(assert (eq nil (valid-joy-exp '(true 2 +))))
(assert (eq nil (valid-joy-exp '(1 i)))) ;test list matching
(assert (eq t (valid-joy-exp '(() i))))
(assert (eq t (valid-joy-exp '(1 2 true pop +)))) ;test pop
(assert (eq t (valid-joy-exp '((1 2 +) i)))) ;test sub-expressions
(assert (eq nil (valid-joy-exp '((1 true +) i))))
(assert (eq nil (valid-joy-exp '(1 () +))))
(assert (eq t (valid-joy-exp '(1 2 swap () ifte)))) ;test type conversion
(assert (eq t (valid-joy-exp '(() uncons cons uncons)))) ;test multiple type order matching
(assert (eq nil (valid-joy-exp '(() cons uncons))))
(assert (eq t (valid-joy-exp '(1 2 swap nil ifte))))
(assert (eq t (valid-joy-exp '((1) define a a (2) define a a 1))))
(assert (eq t (valid-joy-exp '((1) define (2)))))
(assert (eq nil (valid-joy-exp '(1 define (2)))))

(defun absdiff (a b)
  (abs (- a b)))

(defun fitness-randomized-tests-generate (values)
  "Generate (LENGTH VALUES) test cases."
  (cons (let ((a (elt values 0))
	      (b (elt values (1- (length values)))))
	  (+ a (random (- b a))))
	(loop for i below (1- (length values))
	   collect (let* ((a (elt values i))
			  (b (elt values (mod (1+ i) (length values)))))
		     (+ a (random (- b a)))))))

(defun identity-1 (values)
  values)

(defparameter *fitness-invalid* -1000000) ;score for invalid joy-expressions or invalid results

(defun fitness-oneval-diff-score (r goal exp)
  "calculates the score of the joy-eval-result r against the goal.
r should be a list of one value, otherwise *fitness-invalid* is returned."
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      *fitness-invalid*
      (- (absdiff goal (car r)))))

(defstruct test-cases
  (values nil :type list :read-only t)
  (generate (lambda () (error "generate undefined")) :type function :read-only t)
  (goal (lambda () (error "goal undefined")) :type function :read-only t)
  (score (lambda () (error "score undefined")) :type function :read-only t))

(defparameter *fitness-sqrt-test*
  (make-test-cases :values '(1.0 25.0 100.0 225.0 400.0 625.0 1000.0)
		   :generate #'fitness-randomized-tests-generate
		   :goal #'sqrt
		   :score #'fitness-oneval-diff-score))

(defparameter *fitness-expt2-test*
  (make-test-cases :values '(1 5 10)
		   :generate #'fitness-randomized-tests-generate
		   :goal (lambda (x) (expt 2 x))
		   :score #'fitness-oneval-diff-score))
;;'(pred 2 swap (2 *) times)

(defun fitness-stacklength-score (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (proper-list-p r)))
      *fitness-invalid*
      (- (absdiff (length r) goal))))

(defparameter *fitness-stacklength-test*
  (make-test-cases :values '(1 5 10)
		   :generate #'fitness-randomized-tests-generate
		   :goal (lambda (x) x)
		   :score #'fitness-stacklength-score))
; '((0) times)

(defparameter *fitness-stackcount-test*
  (make-test-cases :values '((7) (5 6 3 1 2) (4 9 1 0 2 3 5 6 5 1))
		   :generate #'identity-1
		   :goal #'length
		   :score #'fitness-oneval-diff-score))

(defun generate-fitness-systematicmapping-oks (exp-nodes)
  (let ((goal-c 0)
	(ok-c 0)
	(error-c 0))
    (values 
     (make-test-cases :values '(0)
		      :goal (lambda (x) (declare (ignore x)) (incf goal-c) 0)
		      :score (lambda (r goal exp) (declare (ignore goal))
				     (if (= exp-nodes (count-tree-nodes exp))
					 (if (eq r 'error) 
					     (progn (incf error-c) *fitness-invalid*)
					     (progn (incf ok-c) 0))
					 (if (eq r 'error)
					     *fitness-invalid*
					     0))))
     (lambda () (values goal-c error-c ok-c)))))

(defun fitness-generate-test-goal-values (fitness)
  (let* ((v (test-cases-values fitness))
	 (generate-fn (test-cases-generate fitness))
	 (goal-fn (test-cases-goal fitness))
	 (test-values (funcall generate-fn v)))
    (loop for test in test-values collect
	 (let* ((goal (funcall goal-fn test)))
	   (list test goal)))))

(defun fitness-score-test-values (fitness test-goal-values exp max-ticks max-seconds)
  (let ((score-fn (test-cases-score fitness)))
    (loop for x in test-goal-values
       sum (destructuring-bind (test goal) x
	     (let* ((r (joy-eval-handler (list test) exp :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
		    (fit (funcall score-fn r goal exp)))
	       fit)))))

(defun joy-program-show-fitness (o fitness)
  (flet ((f (v)
	   (let* ((res (joy-eval-handler (list v) o))
		  (goal (funcall (test-cases-goal fitness) v))
		  (score (funcall (test-cases-score fitness) res goal o)))
	     (values v goal score res))))
    (mapcar (lambda (v) (format t "~A~%" (multiple-value-list (f v))))
	    (test-cases-values fitness))
    (format t "sum:~A~%" (fitness-score-test-values fitness (mapcar (lambda (x) (list x (funcall (test-cases-goal fitness) x)))
								    (test-cases-values fitness)) o 0 0.0))))
;(joy-program-show-fitness '(DUP (DUP (16 +) DIP REM 16 + D AND) DIP REM SUCC) *fitness-sqrt-test*)

(defparameter *mut-length* (+ 11 10 (length *joy-ops*)))

(defun tournament-new (o size cycles fitness max-ticks max-seconds)
  (let* ((pop (make-array size :initial-element o))
	 (mut (make-array size :initial-element (loop for i below *mut-length* collect .1))))
;;    (dotimes (s size) (setf (aref mut s) (loop for i below 11 collect (random .9))))
    (tournament pop mut cycles fitness max-ticks max-seconds)))

(defun tournament-res (res cycles fitness max-ticks max-seconds)
  (let* ((size (length res))
	 (pop (make-array size :initial-contents (loop for i in res collect (car i))))
	 (mut (make-array size :initial-contents (loop for i in res collect (caddr i)))))
    (tournament pop mut cycles fitness max-ticks max-seconds)))

(defun tournament (pop mut cycles fitness max-ticks max-seconds)
  (assert (= (length pop) (length mut)))
  (let* ((size (length pop))
	 (n (loop for i below size collect i))
	 (fit (make-array size :initial-element 0))
	 (logstream (open "/tmp/log.txt" :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)))
    (let ((test-goal-values (fitness-generate-test-goal-values fitness)))
      (dotimes (s size) (setf (aref fit s) (fitness-score-test-values fitness test-goal-values (aref pop s) max-ticks max-seconds))))
    (dotimes (c cycles)
      (let* ((test-goal-values (fitness-generate-test-goal-values fitness)) ;test with the same test-goal value pairs
	     (c1 (sample n))
	     (c2 (sample n))
	     ;;(c2-fit (elt fit c2))
	     (c2-fit (fitness-score-test-values fitness test-goal-values (elt pop c2) max-ticks max-seconds)) ;; fairer for randomized fitnesses
	     (c1-mut (elt mut c1))
	     (c2-mut (elt mut c2))
	     (new-mut (mutate-mutate .1 (mutate-crossover c1-mut c2-mut)))
;;	     (new-mut c2-mut)
	     (new (apply #'crossover-and-mutate (elt pop c1) (elt pop c2)
			 new-mut)))
	(when (and (listp new) (valid-joy-exp new '(:any)))
	  (let ((new-fit (fitness-score-test-values fitness test-goal-values new max-ticks max-seconds)))
	    ;;(print (list "c1" c1 "c2" c2 "c2-fit" c2-fit "new-fit" new-fit))
	    (when (> new-fit c2-fit)
	      (setf (elt pop c2) new)
	      (setf (elt fit c2) new-fit)
	      (setf (elt mut c2) new-mut)
	      (print (list "new" new "new-fit" new-fit "new-mut" new-mut "c2-fit" c2-fit)))))
	(when (= 0 (mod c 1000))
	  (print (list c c2-fit)))
	(when (= 0 (mod c 10000))
	  (log-mut-stats (sort (zip-array 'list pop fit mut) #'< :key #'second) logstream))))
    (close logstream)
    (sort (zip-array 'list pop fit mut) #'< :key #'second)))

(defun mut-stats (res)
  (let ((mut (mapcar #'caddr res)))
    (loop for i below *mut-length* collect
	 (let ((pn (mapcar (lambda (x) (nth i x)) mut)))
;;	   (print (list "pn" pn))
	   (list (mean pn) (stddev-corr pn))))))

(defun log-mut-stats (res logstream)
  (let* ((fits (mapcar #'cadr res))
	 (fit-mean (mean fits))
	 (fit-stddev (stddev-corr fits))
	 (ms (mut-stats res)))
    (format logstream "~A ~A" (float fit-mean) fit-stddev)
    (loop for i in ms do (format logstream " ~A ~A" (car i) (cadr i)))
    (format logstream "~%"))
  (finish-output logstream))

(defun store-res (res filename)
  (with-open-file (stream filename :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)
    (print res stream))
  t)

(defun restore-res (filename)
  (with-open-file (stream filename :direction :input)
    (read stream)))

;; systematic program fitness measurement

(defun copy-hash-table (ht)
  "returns a copy of ht. (keys and) values are not copied."
  (let ((copy (make-hash-table :test (hash-table-test ht) 
			       :size (hash-table-size ht)
			       :rehash-size (hash-table-rehash-size ht)
			       :rehash-threshold
			       (hash-table-rehash-threshold ht))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) ht)
    copy))

(defun extend-and-evaluate (l-exp l-1-stk l-1-heap ins goal-value fitness-fn max-ticks max-seconds)
  "appends ins to l-1-exp and calculates the fitness for each possibility.
l-1-stk and l-1-heap must be the stack and heap returned when executing l-1-exp."
  (declare (ignorable l-1-heap) (type (function (list number list) number) fitness-fn))
  (let* ((lins (list ins))
	 (heap (copy-hash-table l-1-heap)) ; this takes a lot of time
	 ;;(heap l-1-heap)
	 (res (joy-eval-handler l-1-stk lins :heap heap :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
	 (fit (funcall fitness-fn res goal-value l-exp)))
    ;;(format t "eae. l-1-stk:~A ins:~A res:~A fit:~A~%" l-1-stk ins res fit)
    ;;(format t "exp:~A ret:~A~%" l-exp res)
    (values res heap fit)))

(defun evaluate-tests (ins l-exp l-1-stks l-1-heaps l-1-fits goal-values score-fn max-ticks max-seconds)
  "Run ins on the joy-machines specified by l-1-stks and l-1-heaps and calculate the new fitness, and whether an error was returned.
l-1-fits must be a list of fitnesses which must be nil if the previous calls yielded no error."
  (declare (ignorable l-exp))
  (let (l-stks l-heaps l-fits (fit-sum 0) 
	       (valid-exp (valid-joy-exp l-exp '(:any))) ;this approximately halves search time
	       ;(valid-exp t)
	       (pursue nil))
    (loop
       for l-1-stk in l-1-stks
       for l-1-heap in l-1-heaps
       for l-1-fit in l-1-fits
       for goal-value in goal-values
       do
	 (if (or (not (null l-1-fit)) (not valid-exp)) ;error in level above or invalid expression
	     (progn
	       (setf l-stks (cons l-1-stk l-stks))
	       (setf l-heaps (cons l-1-heap l-heaps))
	       (setf l-fits (cons l-1-fit l-fits))
	       ;;(setf fit-sum (+ fit-sum l-1-fit))) ;if the shorter expression failed, so will this longer one
	       (setf fit-sum (+ fit-sum *fitness-invalid*)))
	     (multiple-value-bind (res heap fit) ;no error in level above
		 (extend-and-evaluate l-exp l-1-stk l-1-heap ins goal-value score-fn max-ticks max-seconds)
	       ;;(format t "goal-value:~A stk:~A fit:~A~%" goal-value res fit)
	       (setf l-stks (cons res l-stks))
	       (setf l-heaps (cons heap l-heaps))
	       (if (not (eq res 'error)) ;no error
		   (progn
		     (setf l-fits (cons nil l-fits))
		     (setf pursue t))
		   (setf l-fits (cons fit l-fits)))
	       (setf fit-sum (+ fit-sum fit)))))
    ;; pursue is nil if all tests failed with an error, t otherwise
    (values (nreverse l-stks) (nreverse l-heaps) (nreverse l-fits) fit-sum pursue)))

;; TODO: add computing the systematic mapping for more than one test-value
(defun systematicmapping (maxlevel fitness-test-case test-values joy-ops max-ticks max-seconds)
  "systematically walks through the joy expression possibilities (taken from joy-ops) of a certain length maxlevel and saves the best performing expression under a certain fitness-test-case generator for a test-value.
max-ticks is the maximum joy-eval counter.
max-seconds is the maximum joy-eval timeout."
  (let ((goal-values (mapcar (test-cases-goal fitness-test-case) test-values))
	(score-fn (test-cases-score fitness-test-case)))
    (labels ((score (exp heap test-value goal-value)
	       (funcall score-fn
			(joy-eval-handler (list test-value) exp :heap heap)
			goal-value))
	     (score-sum (exp heaps)
	       (reduce #'+ (mapcar (lambda (test-value goal-value heap)
				     (score exp heap test-value goal-value))
				   test-values goal-values heaps))))
      (let* ((best-exp nil)
	     (best-heaps (loop for i below (length test-values) collect (make-hash-table)))
	     (best-fit (score-sum best-exp best-heaps))
	     (start-fits (loop for i below (length test-values) collect nil)))
	(labels ((rec (l l-1-exp l-1-stks l-1-heaps l-1-fits)
		   (if (>= l maxlevel)
		       nil
		       (loop for ins in joy-ops do
			    (let ((l-exp (append l-1-exp (list ins))))
			      ;;(format t "l-exp:~A l-1-stks:~A l-1-fits:~A goal-values:~A~%" l-exp l-1-stks l-1-fits goal-values)
			      (multiple-value-bind
				    (l-stks l-heaps l-fits fit-sum pursue)
				  (evaluate-tests ins l-exp l-1-stks l-1-heaps l-1-fits goal-values score-fn max-ticks max-seconds)
				(format t "l-exp:~A fit-sum:~A pursue:~A~%" l-exp fit-sum pursue)
				(when (> fit-sum best-fit)
				  (setf best-exp l-exp)
				  (setf best-fit fit-sum))
				(when pursue
				  (rec (1+ l) l-exp l-stks l-heaps l-fits))))))))
	  (rec 0 best-exp (mapcar #'list test-values) best-heaps start-fits)
	  (values best-exp best-fit))))))

;;(systematicmapping 3 *fitness-sqrt-test* '(0 1 5) (cons 1 *joy-ops*) 1000)

(defun extend-exp-and-test (max-ext-nodes fitness-test-case l-1-exp l-1-stks l-1-heaps l-1-fits goal-values collectfit joy-ops max-ticks max-seconds)
  "Extend l-1-exp with an arbitrary tree of at most max-ext-nodes nodes.
Test with all fitness-test-cases and extend those expressions whose result had no error.
l-1-fits must be a list of fitnesses which must be nil if the previous level yielded no error."
  (when (> max-ext-nodes 0)
    (let* ((ext-structs (unique (mapcar #'car (enumerate-tree-structures max-ext-nodes))
				#'equal)) ;extension tree structures
	   (score-fn (test-cases-score fitness-test-case)))
      ;;(format t "ext-structs:~A~%" ext-structs)
      (loop for ext-struct in ext-structs do
	   (let ((ext-nodes (count-tree-nodes ext-struct))
		 (ext-symbols (count-symbols-in-tree ext-struct))
		 (enum-fill-start-time (get-internal-real-time)))
	     (when (> max-ext-nodes 3)
	       (format t "max-ext-nodes:~A l-1-exp:~A ext-struct:~A ext-nodes:~A ext-symbols:~A~%" max-ext-nodes l-1-exp ext-struct ext-nodes ext-symbols)
	       )
	     (flet ((f1 (ins-set)
		      (let* ((ins (replace-symbols-in-tree ext-struct ins-set))
			     (l-exp (append l-1-exp (list ins))))
			(multiple-value-bind
			      (l-stks l-heaps l-fits fit-sum pursue)
			    (evaluate-tests ins l-exp l-1-stks l-1-heaps l-1-fits goal-values score-fn max-ticks max-seconds)
			  ;;(format t "  l-exp:~A fit-sum:~A      #sym:~A #nodes:~A~%" l-exp fit-sum (count-symbols-in-tree l-exp) (count-tree-nodes l-exp))
			  (funcall collectfit l-exp fit-sum)
			  (when pursue
			    (extend-exp-and-test (- max-ext-nodes ext-nodes) fitness-test-case l-exp l-stks l-heaps l-fits goal-values collectfit joy-ops max-ticks max-seconds))))))
	       (let* ((ins-sets (loop for i below ext-symbols collect joy-ops)))
		 (enumerate-set-combinations ins-sets #'f1)))
	     (when (> max-ext-nodes 3)
	       (let ((elapsed-seconds (float (/ (- (get-internal-real-time) enum-fill-start-time) internal-time-units-per-second))))
		 (format t "max-ext-nodes:~A l-1-exp:~A ext-struct:~A ext-nodes:~A ext-symbols:~A elapsed-seconds:~A~%" max-ext-nodes l-1-exp ext-struct ext-nodes ext-symbols elapsed-seconds))))))))

(defun systematicmapping2 (maxlevel fitness-test-case joy-ops max-ticks max-seconds)
  (let* ((test-values (test-cases-values fitness-test-case))
	 (goal-values (mapcar (test-cases-goal fitness-test-case) test-values))
	 (l0-heaps (loop for i below (length test-values) collect (make-hash-table :size 0)))
	 (l0-exps (mapcar #'list test-values))
	 (l0-stks (mapcar (lambda (e h) (joy-eval-handler nil e :heap h)) l0-exps l0-heaps))
	 (fitn (mapcar (test-cases-score fitness-test-case) l0-stks goal-values l0-exps))
	 (l0-fits (mapcar (lambda (s f) (if (not (eq s 'error)) nil f)) l0-stks fitn))
	 (best-fit (apply #'+ fitn))
	 (best-exp (list nil)))
    (flet ((collectfit (exp fit)
	     (if (> fit best-fit)
		 (progn (setf best-fit fit) (setf best-exp (list exp)))
		 (when (= fit best-fit)
		   (setf best-exp (cons exp best-exp))))))
      (format t "nil fitn:~A best-fit:~A~%" fitn best-fit)
      (extend-exp-and-test maxlevel fitness-test-case nil l0-stks l0-heaps l0-fits goal-values #'collectfit joy-ops max-ticks max-seconds)
      (values best-exp best-fit))))

;;(time (systematicmapping2 4 *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))

;;(require :sb-sprof)
;;(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil)
;;  (systematicmapping2 4 *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))
;;(sb-sprof:with-profiling (:max-samples 1000 :mode :alloc :report :flat)
;;  (systematicmapping2 4 *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))

;;(time (systematicmapping2 4 *fitness-sqrt-test* '(1 5 10) (remove 'define *joy-ops-with-duplicates*) 1000))
;;auf Susi's PC
;; (1 SWAP REM INC) 11.219
;; (1 INC SWAP REM INC) 279.084 seconds
;; (DUP DEC DEC REM INC) 7951.128 seconds

; without heap
; 0.050 0.637 5.773 136.033
; with heap
; 0.070 0.878 7.364 161.580

(defun count-error-joy-expressions (n stk joy-ops max-ticks max-seconds)
  (let ((errors 0) (oks 0))
    (flet ((try (exp)
	     (let ((ret (joy-eval-handler stk exp :c (make-counter max-ticks) :cd (make-countdown max-seconds))))
	       ;;(format t "exp:~A ret:~A~%" exp ret)
	       (if (eq ret 'error)
		   (incf errors)
		   (incf oks)))))
      (enumerate-trees n joy-ops #'print #'try))
    (values errors oks)))

; (count-error-joy-expressions 5 '(0) *joy-ops* 1000 .01)
; errors: 12 592 23077 850875 30945104
; oks:    19 400 9628  257189 7524718

;(multiple-value-bind (test-cases get-counts)
;	     (generate-fitness-systematicmapping-oks 2)
;	   (systematicmapping2 1 test-cases *joy-ops* 1000 .01)
;	   (funcall get-counts))