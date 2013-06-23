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

(defun choice (seq)
  (let ((l (length seq)))
    (elt seq (random l))))

(defun weighted-choice-index (w)
  "Returns the index of the picked weight from w. Elements of w are numbers and
represent the relative chance of picking that item. Sum of w must not be 0."
  (let ((s (reduce #'+ w)))
    (labels ((rec (x w i)
	       (let ((x0 (- x (car w))))
		 (if (< x0 0)
		     i
		     (rec x0 (cdr w) (1+ i))))))
      (rec (random s) w 0)))) ; will (correctly) give an error if s = 0

(defun weighted-choice (w seq)
  (elt seq (weighted-choice-index w)))

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
  (declare (optimize (debug 0) (compilation-speed 0) (speed 3) (space 0)))
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
	 (+       (cons (+ (cadr stk) (car stk)) (cddr stk)))
	 (and     (cons (and (car stk) (cadr stk)) (cddr stk)))
	 (apply   (joy-eval (cdr stk) (car stk) :heap heap :c c :cd cd)) ;same as i
	 (compose (cons (append (car stk) (cadr stk)) (cddr stk))) ; almost same as concat (swapped arguments)
	 (cons    (cons (cons (cadr stk) (car stk)) (cddr stk))) ; same as papply
	 (dip     (cons (cadr stk) (joy-eval (cddr stk) (car stk) :heap heap :c c :cd cd)))
	 (/       (cons (/ (cadr stk) (car stk)) (cddr stk)))
	 (dup     (cons (car stk) stk))
	 (equal   (cons (equal (cadr stk) (car stk)) (cddr stk)))
	 (false   (cons nil stk))
	 (ifte    (if (car (joy-eval (cdddr stk) (caddr stk) :heap heap :c c :cd cd)) ; similar to branch
		      (joy-eval (cdddr stk) (cadr stk) :heap heap :c c :cd cd)
		      (joy-eval (cdddr stk) (car stk) :heap heap :c c :cd cd)))
	 (list    (cons (list (car stk)) (cdr stk))) ; same as quote
	 (*       (cons (* (car stk) (cadr stk)) (cddr stk)))
	 (not     (cons (not (car stk)) (cdr stk))) ; can be emulated by branch
	 (or      (cons (or (car stk) (cadr stk)) (cddr stk)))
	 (papply  (cons (cons (cadr stk) (car stk)) (cddr stk)))
	 (pop     (cdr stk))
	 (pred    (cons (1- (car stk)) (cdr stk)))
	 (quote   (cons (list (car stk)) (cdr stk)))
	 (rem     (cons (mod (cadr stk) (car stk)) (cddr stk)))
	 (-       (cons (- (cadr stk) (car stk)) (cddr stk)))
	 (succ    (cons (1+ (car stk)) (cdr stk)))
	 (swap    (cons (cadr stk) (cons (car stk) (cddr stk))))
	 (true    (cons t stk))
	 (uncons  (cons (cdar stk) (cons (caar stk) nil)))
	 ;; my own definitions
	 (<       (cons (< (cadr stk) (car stk)) (cddr stk)))
	 (branch  (if (caddr stk)
		      (joy-eval (cdddr stk) (cadr stk) :heap heap :c c :cd cd)
		      (joy-eval (cdddr stk) (car stk) :heap heap :c c :cd cd)))
	 (concat  (cons (append (cadr stk) (car stk)) (cddr stk)))
	 (i       (joy-eval (cdr stk) (car stk) :heap heap :c c :cd cd))
	 (stack   (cons stk stk))
	 (step    (let ((res (cddr stk)))
		    (loop for i in (cadr stk) do
			 (setf res (joy-eval (cons i res) (car stk) :heap heap :c c :cd cd)))
		    res))
	 (unstack (car stk))
	 (while   (let ((res (cddr stk)))
		    (do () ((not (car (joy-eval res (cadr stk) :heap heap :c c :cd cd))))
		      (setf res (joy-eval res (car stk) :heap heap :c c :cd cd)))
		    res))
	 ;; define is special
	 (define  (if (null heap) (error "define doesn't work for a nil heap")
		      (progn (setf (gethash (car stk) heap) (cadr stk)) (cddr stk))))
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
(joy-test nil '(5 4 (+) apply) '(9))
(joy-test nil '((+) (-) compose) '((- +)))
(joy-test nil '(4 (3) cons) '((4 3)))
(joy-test nil '(1 2 5 (+) dip) '(5 3))
(joy-test nil '(5 2 /) '(5/2))
(joy-test nil '(3 dup) '(3 3))
(joy-test nil '(3 3 equal) '(t))
(joy-test nil '(3 true equal) '(nil))
(joy-test nil '((3) (3) equal) '(t))
(joy-test nil '((3) (true) equal) '(nil))
(joy-test nil '(false) '(nil))
(joy-test nil '((true) (1) (2) ifte) '(1))
(joy-test nil '((false) (1) (2) ifte) '(2))
(joy-test nil '((0 true) (1) (2) ifte) '(1))
(joy-test nil '(3 list) '((3)))
(joy-test nil '(3 4 *) '(12))
(joy-test nil '(true not) '(nil))
(joy-test nil '(nil 5 or) '(5))
(joy-test nil '(nil nil or) '(nil))
(joy-test nil '(1 (equal) papply) '((1 equal)))
(joy-test nil '(5 4 pop) '(5))
(joy-test nil '(5 pred) '(4))
(joy-test nil '(5 quote) '((5)))
(joy-test nil '(9 4 rem) '(1))
(joy-test nil '(4 5 -) '(-1))
(joy-test nil '(3 succ) '(4))
(joy-test nil '(1 2 swap) '(1 2))
(joy-test nil '(true) '(t))
(joy-test nil '(4 5 list cons uncons) '((5) 4))
;; own tests
(joy-test nil '(1 2 <) '(t))
(joy-test nil '(2 1 <) '(nil))
(joy-test nil '(true (1) (2) branch) '(1))
(joy-test nil '(false (1) (2) branch) '(2))
(joy-test nil '(0 (1 2 3) (4 5 6) concat) '((1 2 3 4 5 6) 0))
(joy-test nil '(2 (3 +) i) '(5))
(joy-test nil '(1 2 stack) '((2 1) 2 1))
(joy-test nil '((swap cons) swons define 0 nil (1 2 3) (swons) step) '((3 2 1) 0))
(joy-test nil '(0 (1 2) unstack) '(1 2))
(joy-test nil '(1 2 3 4 5 6 7 (pop pop stack (1) equal not) (pop) while) '(3 2 1))
;; 5 [0 < not] [[1] dip pred] while stack . ;; puts -1 and 6 ones on the stack
;; define is special
(joy-test nil '(2 (dup +) superman define) '(2))
(joy-test nil '(2 (1) superman define superman) '(1 2))
(joy-test nil '(1 a) '(a 1))
;; own defines
(joy-test nil '((0 equal) null define 0 null) '(t))
(joy-test nil '((0 equal) null define 1 null) '(nil))
(joy-test nil '((pred) dec define 5 dec) '(4))
(joy-test nil '((succ) inc define 5 inc) '(6))
(joy-test nil '((swap cons) swons define (2) 1 swons) '((1 2)))
(joy-eval nil
	  '(((dup cons) swap concat dup cons i) y define
	    (((pop null) (pop succ) ((dup pred) dip i *) ifte) y) fac define
	    1 fac) :heap (make-hash-table))


(defun joy-eval-handler (stk exp &key (heap (make-hash-table)) (c (make-counter)) (cd (make-countdown)))
  (handler-case (joy-eval stk exp :heap heap :c c :cd cd)
    (simple-type-error () 'error)
    (type-error () 'error)
    (division-by-zero () 'error)
    (floating-point-invalid-operation () 'error)
    (floating-point-overflow () 'error)))

(defparameter *joy-ops* '(+ and apply compose cons dip / dup equal false
			  list * not or pop pred rem - succ swap true
			  uncons < branch stack step unstack while define))

(defun mutate (exp debranch-p p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 
	       q1 q2 q3 q4 q5 q6 q7 q8 q9 q10
	       &rest ops-p)
;;  (print (list "exp" exp))
  (flet ((random-final (p)
	   (if (< (random 1.0) p)
	       (weighted-choice (list q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
				'(a b c d e 1 2 4 8 16))
	       (weighted-choice ops-p *joy-ops*))))
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
  (declare (optimize (debug 3)))
;;  (print (list "exp" exp "p0" p0))
  (let ((p0 (car probs))
	(prest (cdr probs)))
    (if (< (random 1.0) p0)
	(apply #'mutate-rec (apply #'mutate exp t prest) probs)
	(apply #'mutate exp t prest))))

(defun mutate-mutate (p0 probs)
  (labels ((rnd (a)
	     (let ((b (+ a (- (random .2) -.1))))
	       (if (< b 0.0)
		   0
		   (if (>= b 0.99)
		       0.99
		       b))))
	   (rec (probs mut-probs)
	     (if (null probs)
		 (nreverse mut-probs)
		 (if (<= (random 1.0) p0)
		     (rec (cdr probs) (cons (rnd (car probs)) mut-probs))
;;		     (rec (cdr probs) (cons (random .99) mut-probs))
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
			   (values (choice se) nil)
			   (values x t))
		       (if (chance p1)
			   (choice se)
		       x)))
		 exp1)))

(defun crossover-and-mutate (exp1 exp2 &rest probs)
  (let* ((p1 (elt probs 0))
	 (p2 (elt probs 1))
	 (prest (cddr probs))
	 (cross (crossover exp1 exp2 p1 p2))
	 (new (apply #'mutate-rec cross prest)))
    new))

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
				  (choice '(a b c d e f 1 2 3))))
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

(defun absdiff (a b)
  (abs (- a b)))

(defun fitness-smallpref-generate ()
  "generate one test case."
  (let* ((c '(0 1 5 10 20 40 80 100 200 250 1000 10000 100000))
	 (r (random (1- (length c))))
	 (a (elt c r))
	 (b (elt c (1+ r))))
    (+ a (random (- b a)))))

(defun fitness-oneval-diff-score (r goal)
  "calculates the score of the joy-eval-result r against the goal.
r should be a list of one value, otherwise -1000 is returned."
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      -1000
      (- (absdiff goal (car r)))))

(defstruct test-cases
  (values nil :type list :read-only t)
  (generate (constantly nil) :type function :read-only t)
  (goal nil :type function :read-only t)
  (score nil :type function :read-only t))

(defparameter *fitness-sqrt-test*
  (make-test-cases :values '(1.0 5.0 10.0 1000.0)
		   :generate #'fitness-smallpref-generate
		   :goal #'sqrt
		   :score #'fitness-oneval-diff-score))

;; re-definition of fitness-sqrt using *fitness-sqrt-test*-functions
(defun fitness-generator (fitness-test-case)
  (lambda (o &optional (print nil))
    (loop for x in (test-cases-values fitness-test-case)
       sum (let* ((value (funcall (test-cases-goal fitness-test-case) x))
		  (r (joy-eval-handler (list x) o :c (make-counter 1000) :cd (make-countdown .01)))
		  (fit (funcall (test-cases-score fitness-test-case) r value)))
	     (when print
	       (print (list "x" x "value" value "r" r "fit" fit)))
	     fit))))

(defparameter *fitness-sqrt* (fitness-generator *fitness-sqrt-test*))

(defparameter *fitness-expt2-test*
  (make-test-cases :values '(1 5 10)
		   :generate #'fitness-smallpref-generate
		   :goal (lambda (x) (expt 2 x))
		   :score #'fitness-oneval-diff-score))

(defparameter *fitness-expt2* (fitness-generator *fitness-expt2-test*))

(defun fitness-stacklength-score (r goal)
  (if (or (not (listp r)) (not (proper-list-p r)))
      -1000
      (- (absdiff (length r) goal))))

(defparameter *fitness-stacklength-test*
  (make-test-cases :values '(1 5 10)
		   :goal (lambda (x) x)
		   :score #'fitness-stacklength-score))

(defparameter *fitness-stacklength* (fitness-generator *fitness-stacklength-test*))

;; (INC DUP * DUP *)

(defparameter *mut-length* (+ 11 10 (length *joy-ops*)))

(defun tournament-new (o size cycles fitness)
  (let* ((pop (make-array size :initial-element o))
	 (mut (make-array size :initial-element (loop for i below *mut-length* collect .1))))
;;    (dotimes (s size) (setf (aref mut s) (loop for i below 11 collect (random .9))))
    (tournament pop mut cycles fitness)))

(defun tournament-res (res cycles fitness)
  (let* ((size (length res))
	 (pop (make-array size :initial-contents (loop for i in res collect (car i))))
	 (mut (make-array size :initial-contents (loop for i in res collect (caddr i)))))
    (tournament pop mut cycles fitness)))
(defun tournament (pop mut cycles fitness)
  (assert (= (length pop) (length mut)))
  (let* ((size (length pop))
	 (n (loop for i below size collect i))
	 (fit (make-array size :initial-element 0))
	 (logstream (open "/tmp/log.txt" :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)))
    (dotimes (s size) (setf (aref fit s) (funcall fitness (aref pop s))))
    (dotimes (c cycles)
      (let* ((c1 (choice n))
	     (c2 (choice n))
	     ;;(c2-fit (elt fit c2))
	     (c2-fit (funcall fitness (elt pop c2))) ;; fairer for randomized fitnesses
	     (c1-mut (elt mut c1))
	     (c2-mut (elt mut c2))
	     (new-mut (mutate-mutate .1 (mutate-crossover c1-mut c2-mut)))
;;	     (new-mut c2-mut)
	     (new (apply #'crossover-and-mutate (elt pop c1) (elt pop c2)
			 new-mut))
	     (new-fit (funcall fitness new)))
	;;(print (list "c1" c1 "c2" c2 "c2-fit" c2-fit "new-fit" new-fit))
	(if (> new-fit c2-fit)
	    (progn (setf (elt pop c2) new)
		   (setf (elt fit c2) new-fit)
		   (setf (elt mut c2) new-mut)
		   (print (list "new" new "new-fit" new-fit "new-mut" new-mut "c2-fit" c2-fit))))
	(when (= 0 (mod c 1000))
	  (print (list c new-fit c2-fit)))
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
    (format logstream "~A ~A" fit-mean fit-stddev)
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

(defun extend-and-evaluate (l-1-stk l-1-heap ins goal-value fitness-fn max-ticks max-seconds)
  "appends ins to l-1-exp and calculates the fitness for each possibility.
l-1-stk and l-1-heap must be the stack and heap returned when executing l-1-exp."
  (declare (ignorable l-1-heap) (type (function (list number) number) fitness-fn))
  (let* ((lins (list ins))
	 (heap (copy-hash-table l-1-heap)) ; this takes a lot of time
	 ;;(heap l-1-heap)
	 (res (joy-eval-handler l-1-stk lins :heap heap :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
	 (fit (funcall fitness-fn res goal-value)))
;;    (format t "eae. l-1-stk:~A ins:~A res:~A fit:~A~%" l-1-stk ins res fit)
    (values res heap fit)))

(defun evaluate-tests (ins l-1-stks l-1-heaps l-1-fits goal-values score-fn max-ticks max-seconds)
  "Run ins on the joy-machines specified by l-1-stks and l-1-heaps and calculate the new fitness, and whether an error was returned.
l-1-fits must be a list of fitnesses which must be nil if the previous calls yielded no error."
  (let (l-stks l-heaps l-fits (fit-sum 0) pursue)
    (loop
       for l-1-stk in l-1-stks
       for l-1-heap in l-1-heaps
       for l-1-fit in l-1-fits
       for goal-value in goal-values
       do
	 (if (null l-1-fit) ;no error in level above
	     (multiple-value-bind (res heap fit)
		 (extend-and-evaluate l-1-stk l-1-heap ins goal-value score-fn max-ticks max-seconds)
	       ;;(format t "goal-value:~A stk:~A fit:~A~%" goal-value res fit)
	       (setf l-stks (cons res l-stks))
	       (setf l-heaps (cons heap l-heaps))
	       (if (listp res) ;no error
		   (progn
		     (setf l-fits (cons nil l-fits))
		     (setf pursue t))
		   (setf l-fits (cons fit l-fits)))
	       (setf fit-sum (+ fit-sum fit)))
	     (progn
	       (setf l-stks (cons l-1-stk l-stks))
	       (setf l-heaps (cons l-1-heap l-heaps))
	       (setf l-fits (cons l-1-fit l-fits))
	       (setf fit-sum (+ fit-sum l-1-fit)))))
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
				  (evaluate-tests ins l-1-stks l-1-heaps l-1-fits goal-values score-fn max-ticks max-seconds)
				(format t "l-exp:~A fit-sum:~A pursue:~A~%" l-exp fit-sum pursue)
				(when (> fit-sum best-fit)
				  (setf best-exp l-exp)
				  (setf best-fit fit-sum))
				(when pursue
				  (rec (1+ l) l-exp l-stks l-heaps l-fits))))))))
	  (rec 0 best-exp (mapcar #'list test-values) best-heaps start-fits)
	  (values best-exp best-fit))))))

;;(systematicmapping 3 *fitness-sqrt-test* '(0 1 5) (cons 1 *joy-ops*) 1000)

(defun extend-exp-and-test (max-ext-nodes fitness-test-case l-1-exp l-1-stks l-1-heaps l-1-fits goal-values best-exp best-fit joy-ops max-ticks max-seconds)
  "Extend l-1-exp with an arbitrary tree of at most max-ext-nodes nodes.
Test with all fitness-test-cases and extend those expressions whose result had no error.
l-1-fits must be a list of fitnesses which must be nil if the previous level yielded no error."
  (if (= 0 max-ext-nodes)
      (values best-exp best-fit)
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
				(l-stks l-heaps l-fits fit-sum)
			      (evaluate-tests ins l-1-stks l-1-heaps l-1-fits goal-values score-fn max-ticks max-seconds)
			    ;;(format t "  l-exp:~A fit-sum:~A      #sym:~A #nodes:~A~%" l-exp fit-sum (count-symbols-in-tree l-exp) (count-tree-nodes l-exp))
			    (when (> fit-sum best-fit)
			      (setf best-exp l-exp)
			      (setf best-fit fit-sum))
			    (multiple-value-bind (best-exp+ best-fit+)
				(extend-exp-and-test (- max-ext-nodes ext-nodes) fitness-test-case l-exp l-stks l-heaps l-fits goal-values best-exp best-fit joy-ops max-ticks max-seconds)
			      (when (> best-fit+ best-fit)
				(setf best-exp best-exp+)
				(setf best-fit best-fit+)))))))
		 (let* ((ins-sets (loop for i below ext-symbols collect joy-ops)))
		   (enumerate-set-combinations ins-sets #'f1)))
	       (when (> max-ext-nodes 3)
		 (let ((elapsed-seconds (float (/ (- (get-internal-real-time) enum-fill-start-time) internal-time-units-per-second))))
		   (format t "max-ext-nodes:~A l-1-exp:~A ext-struct:~A ext-nodes:~A ext-symbols:~A elapsed-seconds:~A~%" max-ext-nodes l-1-exp ext-struct ext-nodes ext-symbols elapsed-seconds))
		 )
	       ))
	(values best-exp best-fit))))

(defun systematicmapping2 (maxlevel fitness-test-case joy-ops max-ticks max-seconds)
  (let* ((test-values (test-cases-values fitness-test-case))
	 (goal-values (mapcar (test-cases-goal fitness-test-case) test-values))
	 (l0-heaps (loop for i below (length test-values) collect (make-hash-table :size 0)))
	 (l0-stks (mapcar (lambda (s h) (joy-eval-handler nil (list s) :heap h)) test-values l0-heaps))
	 (fitn (mapcar (test-cases-score fitness-test-case) l0-stks goal-values))
	 (l0-fits (mapcar (lambda (s f) (if (listp s) nil f)) l0-stks fitn))
	 (best-fit (apply #'+ fitn)))
    (format t "nil fitn:~A best-fit:~A~%" fitn best-fit)
    (extend-exp-and-test maxlevel fitness-test-case nil l0-stks l0-heaps l0-fits goal-values nil best-fit joy-ops max-ticks max-seconds)))

;;(time (systematicmapping2 4 *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))

;;(require :sb-sprof)
;;(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil)
;;  (systematicmapping2 4 *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))
;;(sb-sprof:with-profiling (:max-samples 1000 :mode :alloc :report :flat)
;;  (systematicmapping2 4 *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))

;;(time (systematicmapping2 4 *fitness-sqrt-test* '(1 5 10) (remove 'define *joy-ops-with-duplicates*) 1000))
;; (1 SWAP REM INC) 11.219
;; (1 INC SWAP REM INC) 279.084 seconds
;; (DUP DEC DEC REM INC) 7951.128 seconds

; without heap
; 0.050 0.637 5.773 136.033
; with heap
; 0.070 0.878 7.364 161.580
