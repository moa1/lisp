;(in-package #:joy-arch) ;does this require an .asd file?

;(sb-ext:restrict-compiler-policy 'debug 3)
;(declaim (optimize speed))

(load "~/lisp/arbitrary-trees.lisp")
(load "~/lisp/multitree.lisp")
(load "~/lisp/refal-patmat.lisp")
;;(load "~/quicklisp/setup.lisp") ;is in multitree.lisp
;;(ql:quickload :cl-custom-hash-table)
;;(use-package :cl-custom-hash-table)

(define-custom-hash-table-constructor make-lsxhash-equal-hash-table
    ;; equalp required when hashing hash tables
    :test equal :hash-function lsxhash)

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
  ; TODO: this function should reject circular lists
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
	(check-type c fixnum)
	(lambda () (decf c)))))

(defun make-countdown (&optional (seconds .01))
  "Returns a function that, when called, yields the number of internal run time units remaining until the timer expires."
  (declare (type single-float seconds))
  (if (<= seconds 0)
      (lambda () 1)
      (let ((c (+ (get-internal-run-time) (ceiling (* seconds internal-time-units-per-second)))))
	(check-type c fixnum)
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

(defun absdev (seq)
  (let ((m (mean seq)))
    (mean (mapcar (lambda (x) (abs (- x m))) seq))))

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

(defun identity-1 (values)
  values)

(defun list-replace-symbols (list plist)
  "Recursively replace (non-destructively) all occurrences of the symbols in PLIST in LIST with the values stored in PLIST.
Example: (list-replace-symbols '(a b (c a (d)) e) '(a 1 e nil)) == '(1 B (C 1 (D)) NIL)."
  (labels ((replace-symbol (symbol)
	     (let ((cdr (plist-cdr symbol plist)))
	       (if (null cdr)
		   symbol
		   (cadr cdr))))
	   (rec (list accum)
	     (if (null list)
		 (nreverse accum)
		 (let ((el (car list)))
		   (if (consp el)
		       (rec (cdr list) (cons (rec el nil) accum))
		       (rec (cdr list) (cons (replace-symbol el) accum)))))))
    (rec list nil)))

;; '((1) 1 DEFINE 1)
(defun joy-eval (stk exp &key (heap (make-hash-table)) (c (make-counter 0)) (cd (make-countdown 0.0)))
  (declare (optimize (debug 0) (compilation-speed 0) (speed 3) (space 0))
	   (type (function () fixnum) c cd))
  "Note that this function does not fail for the same inputs as the joy implementation by Manfred von Thun, e.g. '(branch) returns nil, but would fail for the real implementation.
However, it should raise an error for cases when the stack becomes a non-list.
This function must not modify stk, only copy it (otherwise test values might be modified)."
  (let ((c (funcall c)) (cd (funcall cd)))
    ;;(print (list "stk" stk "exp" exp "c" c "cd" cd))
    (when (<= c 0)
      (return-from joy-eval 'overrun))
    (when (<= cd 0) ;if this check doesn't do what it is supposed to, check the output type of (funcall cd) and whether it is identical to the cd type declaration of joy-eval!
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
	 (cons    (cons (cons (cadr stk) (let ((a (car stk))) (if (proper-list-p a) a (error (make-condition 'type-error :datum a :expected-type 'list))))) (cddr stk))) ; same as papply
	 (dip     (cons (cadr stk) (joy-eval (cddr stk) (car stk) :heap heap :c c :cd cd)))
	 (/       (cons (/ (cadr stk) (car stk)) (cddr stk)))
	 ;; #+sbcl
	 ;; (if (= 0.0 (car stk)) ;work around SBCL bug (/ 1.0 0.0)
	 ;;     (error (make-condition 'divison-by-zero :operands (list (cadr stk) (car stk))))
	 ;;     (cons (/ (cadr stk) (car stk)) (cddr stk))) ;divide
	 ;; #-sbcl
	 ;; (cons (/ (cadr stk) (car stk)) (cddr stk)) ;divide
	 ;; )
	 (dup     (cons (car stk) stk))
	 (equal   (cons (equal (cadr stk) (car stk)) (cddr stk)))
	 (i       (joy-eval (cdr stk) (car stk) :heap heap :c c :cd cd)) ;same as apply
	 (ifte    (if (car (joy-eval (cdddr stk) (caddr stk) :heap heap :c c :cd cd)) ; similar to branch
		      (joy-eval (cdddr stk) (cadr stk) :heap heap :c c :cd cd)
		      (joy-eval (cdddr stk) (car stk) :heap heap :c c :cd cd)))
	 (list    (cons (listp (car stk)) (cdr stk)))
	 (*       (cons (* (car stk) (cadr stk)) (cddr stk))) ;multiply
	 (nill    (cons nil stk)) ;same as false
	 (not     (cons (not (car stk)) (cdr stk))) ; can be emulated by branch
	 (or      (cons (or (car stk) (cadr stk)) (cddr stk)))
	 (patmat  (let ((vars (caddr stk)) (exp (cadr stk)) (pat (car stk)))
		    (cons
		     (alist-to-plist (patmat vars exp pat))
		     (cdddr stk))))
	 (patsub  (cons (list-replace-symbols (car stk) (cadr stk)) (cddr stk)))
	 (pop     (cdr stk))
	 (pred    (cons (1- (car stk)) (cdr stk)))
	 (quote   (cons (list (car stk)) (cdr stk)))
	 (rem     (cons (mod (cadr stk) (car stk)) (cddr stk)))
	 ;; #+sbcl
	 ;; (if (= 0.0 (car stk)) ;work around SBCL bug (mod 1.0 0.0)
	 ;;     (error (make-condition 'divison-by-zero :operands (list (cadr stk) (car stk))))
	 ;;     (cons (mod (cadr stk) (car stk)) (cddr stk)))
	 ;; #-sbcl
	 ;; (cons (mod (cadr stk) (car stk)) (cddr stk))
	 ;; )
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
	 (uncons  (cons (cdar stk) (cons (caar stk) (cdr stk))))
	 (unstack (let ((a (car stk))) (if (proper-list-p a) (car stk) (error (make-condition 'type-error :datum a :expected-type 'list)))))
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
	  (if (or (not (symbolp (car exp))) (null heap))
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
(joy-test nil '(0 1 list) '(nil 0))
(joy-test nil '(0 (1) list) '(t 0))
(joy-test nil '(3 4 *) '(12))
(joy-test nil '(nill) '(()))
(joy-test nil '(true not) '(nil))
(joy-test nil '((a 1 b 2) (a c) patsub) '((1 c)))
;;(joy-test nil '(((a) (b) (c)) (1 2) (a b c) patmat) '((c nil b 2 a 1)))
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
(joy-test nil '(3 4 5 quote cons uncons) '((5) 4 3))
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
    (floating-point-overflow () 'error)
    #+SBCL (SB-KERNEL::ARG-COUNT-ERROR () 'error)))

(defparameter *joy-ops* 
  '(+ and branch concat cons dip / dup equal i ifte list * nill not or patmat patsub pop pred quote rem < stack step - succ swap times true uncons unstack while define))

(defun mutate (joy-ops exp debranch-p p1 p2 p3 p4 p5 p6
	       &rest ops-p)
  (assert (= (length joy-ops) (length ops-p)))
  (flet ((random-final ()
	   (weighted-sample ops-p joy-ops)))
    (cond
      ((null exp) (if (chance p1) ;extend
		      (cons (random-final)
			    nil)))
      ((listp exp) (if (and debranch-p (chance p2))
		       (random-final) ; de-branch
		       (cons (apply #'mutate joy-ops (car exp) t p1 p2 p3 p4 p5 p6 ops-p)
			     (if (and (= 1 (length (cdr exp)))
				      (chance p3))
				 nil ; shorten
				 (apply #'mutate joy-ops (cdr exp) nil p1 p2 p3 p4 p5 p6 ops-p)))))
      (t (if (chance p4)
	     (if (chance p5)
		 (cons (if (chance p6)
			   (random-final) ; branch
			   exp)
		       nil)	
		 (random-final)) ; substitute
	     exp)))))
   
(defun mutate-rec (joy-ops exp &rest probs)
;;  (print (list "exp" exp "p0" p0))
  (let ((p0 (car probs))
	(prest (cdr probs)))
    (assert (< p0 .9))
    (if (chance p0)
	(apply #'mutate-rec joy-ops (apply #'mutate joy-ops exp t prest) probs)
	(apply #'mutate joy-ops exp t prest))))

(defun mutate-mutate (p0 probs)
  (labels ((rec (probs mut-probs)
	     (if (null probs)
		 (nreverse mut-probs)
		 (if (chance p0)
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
      (let ((exp1 (apply #'mutate-rec exp prob)))
	;;(print exp1)
	(handler-case
	    (progn
	      (destructuring-bind ((v1 v2) v3) exp1 (print (list (list v1 v2) v3)))
	      (incf hits))
	  ;;(division-by-zero () (format t "division-by-zero~%") 1)
	  ;;(sb-kernel::arg-count-error () t)
	  ;;(sb-kernel::defmacro-bogus-sublist-error () t)
	  (type-error () t))))))

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

(defun crossover-and-mutate (joy-ops exp1 exp2 &rest probs)
  (let* ((p1 (elt probs 1))
	 (p2 (elt probs 2))
	 (prest (cons (car probs) (cdddr probs)))
	 (cross (crossover exp1 exp2 p1 p2))
	 (new (apply #'mutate-rec joy-ops cross prest)))
    new))

;; (defun align (exp1 exp2 &optional r)
;;   (cond
;;     ((and (null exp1) (null exp2)) r)
;;     ((null exp1) (if (chance 0.5)
;; 		     (append r exp2)
;; 		     r))
;;     ((null exp2) (if (chance 0.5)
;; 		     (append r exp1)
;; 		     r))
;;     ((and (listp (car exp1))
;; 	  (listp (car exp2))) (if (chance 0.5)
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
    (list    (t) (boolean))
    (*       (number number) (number))
    (nill    () (list))
    (not     (t) (boolean))
    (or      (t t) (boolean))
    (patmat  (list list list) (list))
    (patsub  (list list) (list))
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
This should hold: (typep val type) = (typep-symbol (type-of val) type), but doesn't always, because e.g. (type-of 5) returns (INTEGER 0 4611686018427387903).
Maybe this function should be replaced with calls to the function subtypep."
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
				 (expect-stk-type (reverse ex) stk)
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
(assert (eq t (valid-joy-exp '(1 2 swap () ifte)))) ;test type conversion (by swap in this case)
(assert (eq t (valid-joy-exp '(true (1) (2) branch)))) ;test single operation type order matching
(assert (eq t (valid-joy-exp '(() uncons cons uncons)))) ;test multiple type order matching
(assert (eq nil (valid-joy-exp '(() cons uncons))))
(assert (eq t (valid-joy-exp '((1) define a a (2) define a a 1)))) ;test define
(assert (eq t (valid-joy-exp '((1) define (2)))))
(assert (eq nil (valid-joy-exp '(1 define (2)))))

;; I'll have another go at a type predictor.
;; This one uses a function which receives the current stk and exp (and heap?) and computes the types on the stack.
;; It shall use the same type notation as Common Lisp, plus some additions for describing regular stacks, like (REPEAT NUMBER BOOLEAN), which describes a sequence of alternating numbers and booleans, or (REPEAT (INTEGER -1 1) LIST 0), an instance of which would be (0 (a b c) 0 -1 (a) 0).
;; It also shall support describing lists, like (LIST SYMBOL (NUMBER 0 5)), an instance of which would be (a 2).
;; Supporting REPEATs and LISTs becomes problematic when instructions like UNCONS or POP can happen, because it is unclear how to describe a REPEAT or LIST of which an unknown number of elements have been removed.

;; A different approach is to try to evolve a joy program that returns whether an input joy program has type errors in it.


;; examples of equivalent joy expressions:
;; (joy-eval nil '(1024 nill (succ) dip (swap 1 < not) ((pred dup 64 rem 0 equal) dip swap ((dup) dip cons) () branch) while))
;; (joy-eval nil '(1024 nill (64 +) dip (swap 1 < not) ((64 - dup) dip cons) while))

(defun fitness-generate-test-goal-values (fitness)
  (let* ((v (test-cases-values fitness))
	 (generate-fn (test-cases-generate fitness))
	 (goal-fn (test-cases-goal fitness))
	 (test-values (funcall generate-fn v)))
    (loop for test in test-values collect
	 (let* ((goal (funcall goal-fn test)))
	   (cons test goal)))))

(defun fitness-score-test-values (fitness test-goal-values exp max-ticks max-seconds)
  (let ((score-fn (test-cases-score fitness)))
    (loop for (test . goal) in test-goal-values
       sum
	 (let* ((r (joy-eval-handler nil (append test exp) :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
		(fit (funcall score-fn r goal exp)))
	   fit))))

(defun joy-show-fitness (o fitness)
  (flet ((eval-test (v)
	   (let* ((res (joy-eval-handler nil (append v o)))
		  (goal (funcall (test-cases-goal fitness) v))
		  (score (funcall (test-cases-score fitness) res goal o)))
	     (list v goal score res))))
    (let* ((fitsum (fitness-score-test-values fitness 
					      (mapcar (lambda (x) (cons x (funcall (test-cases-goal fitness) x)))
						      (test-cases-values fitness)) o 0 0.0)))
      (mapcar (lambda (p) (destructuring-bind (v goal score res) p
			    (format t "v(exp):~A goal:~A score:~A res:~A~%" v goal score res) score))
	      (mapcar (lambda (v) (eval-test v)) (test-cases-values fitness)))
      (format t "fitsum:~A~%" fitsum)
      fitsum)))

(defun absdiff (a b)
  (abs (- a b)))

(defun generate-randomized-tests (values)
  "Generate (LENGTH VALUES) test cases."
  (cons (let ((a (car (elt values 0)))
	      (b (car (elt values (1- (length values))))))
	  (list (+ a (random (- b a)))))
	(loop for i below (1- (length values))
	   collect (let* ((a (car (elt values i)))
			  (b (car (elt values (mod (1+ i) (length values))))))
		     (list (+ a (random (- b a))))))))

(defparameter *fitness-invalid* -1000000) ;score for invalid joy-expressions or invalid results

(defun score-one-value (r goal exp)
  "calculates the score of the joy-eval-result r against the goal.
r should be a list of one value, otherwise *fitness-invalid* is returned."
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      *fitness-invalid*
      (- (absdiff goal (car r)))))

(defstruct test-cases
  (values nil :type list :read-only t) ;the values that are prepended to a joy-expression.
  (generate (lambda (&rest r) (declare (ignore r)) (error "generate undefined")) :type function :read-only t)
  (goal (lambda (&rest r) (declare (ignore r)) (error "goal undefined")) :type function :read-only t)
  (score (lambda (&rest r) (declare (ignore r)) (error "score undefined")) :type function :read-only t))

(defparameter *test-cases-sqrt*
  (make-test-cases :values '((1.0) (25.0) (100.0) (225.0) (400.0) (625.0) (1000.0))
		   :generate #'generate-randomized-tests
		   :goal (lambda (x) (sqrt (car x)))
		   :score #'score-one-value))
;; TODO: write a joy program that computes the sqrt. Therefore, write fitnesses that provide solutions for required ops, like the joy operation 'rotate.
;;(time (systematicmapping 4 '() *test-cases-sqrt* '(+ dip / dup < - while) 1000 .01))

(defparameter *test-cases-expt2*
  (make-test-cases :values '((1) (5) (10))
		   :generate #'generate-randomized-tests
		   :goal (lambda (x) (expt 2 (car x)))
		   :score #'score-one-value))
(assert (eq 0 (joy-show-fitness '(pred 2 swap (2 *) times) *test-cases-expt2*)))

(defun score-stacklength (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (proper-list-p r)))
      *fitness-invalid*
      (- (absdiff (length r) goal))))

(defparameter *test-cases-stacklength*
  (make-test-cases :values '((1) (5) (10))
		   :generate #'generate-randomized-tests
		   :goal #'car
		   :score #'score-stacklength))
(assert (eq 0 (joy-show-fitness '((0) times) *test-cases-stacklength*)))

(defparameter *test-cases-stackcount*
  (make-test-cases :values '(((7)) ((5 6 3 1 2)) ((4 9 1 0 2 3 5 6 5 1)))
		   :generate #'identity-1
		   :goal (lambda (x) (length (car x)))
		   :score #'score-one-value))
(assert (eq 0 (joy-show-fitness '(0 (swap) (succ (uncons swap pop) dip) while swap pop) *test-cases-stackcount*)))

(defparameter *letter-symbols* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun score-list-similarity (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (proper-list-p r)))
      *fitness-invalid*
      (if (equal r goal)
	  0
	  (- (+ 10 (abs (- (length r) (length goal)))))))) ;replace this with the edit-distance between r and goal

(defparameter *test-cases-rotate*
  (make-test-cases :values '((a b c) (x y z d e f) ((1) (2 3) j k l))
		   :generate (lambda (v) (declare (ignore v)) (let ((l (random 10)) (a (sample *letter-symbols*)) (b (sample *letter-symbols*)) (c (sample *letter-symbols*)))
					   (list (nconc (list a b c) (loop for i below l collect (random 10))))))
		   :goal (lambda (v) (destructuring-bind (a b c &rest r) (reverse v) (nconc (list c b a) r)))
		   :score #'score-list-similarity))
(assert (eq 0 (joy-show-fitness '((swap) dip swap (swap) dip) *test-cases-rotate*)))
(assert (eq 0 (joy-show-fitness '(SWAP QUOTE CONS DIP) *test-cases-rotate*)))

(defun score-one-symbol-equal (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      *fitness-invalid*
      (if (equal (car r) goal) 0 -10)))

(defparameter *test-cases-at*
  (make-test-cases :values '(((a b c) 2) ((x y z d e f) 4) (((1) (2 3) j k l) 1))
		   :generate (lambda (v)
				     (loop for i below (length v) collect
					  (let ((l (1+ (random 10))))
					    (list (loop for i below l collect (sample *letter-symbols*))
						  (random l)))))
		   :goal (lambda (v) (elt (car v) (cadr v)))
		   :score #'score-one-symbol-equal))

;;(defparameter *test-cases-abs* ;;trains to be able to compute (abs x) for every x

(defparameter *test-cases-list-positive0*
  (make-test-cases :values '((4 2 0 -1 -2) (6 4 3 1 0 -3) (1 -4 -7 -9))
		   :generate (lambda (v) (loop for i below (length v) collect
					      (do ((a (1+ (random 10)) (- a (random 3))) (b (- -5 (random 5))) (l nil)) ((< a b) (nreverse l))
						(setf l (cons a l)))))
		   :goal (lambda (v) (let ((r (reverse v))) (nthcdr (position-if (lambda (x) (>= x 0)) r) r)))
		   :score #'score-list-similarity))
(assert (eq 0 (joy-show-fitness '((0 <) (pop) while) *test-cases-list-positive0*)))

(defparameter *test-cases-identity4*
  (make-test-cases :values '((1 1.0) (1 1.5) (1 2.0) (1 2.5) (1 3.0) (1 3.5)) ;the 1 is a bias neuron
		   :generate (lambda (vs) (mapcar (lambda (x) (list 1 (car x))) (generate-randomized-tests (mapcar (lambda (x) (list (cadr x))) vs))))
		   :goal #'cadr
		   :score #'score-one-value))

(defparameter *test-cases-identity1024*
  (make-test-cases :values '((1 0.0) (1 204.6) (1 409.2) (1 613.8) (1 818.4) (1 1023.0)) ;the 1 is a bias neuron
		   :generate (lambda (vs) (mapcar (lambda (x) (list 1 (car x))) (generate-randomized-tests (mapcar (lambda (x) (list (cadr x))) vs))))
		   :goal #'cadr
		   :score #'score-one-value))

(defun generate-test-cases-systematicmapping-oks (exp-nodes)
  "Return a test-cases instance and a function to retrieve the number of successful joy programs."
  (let ((goal-c 0)
	(ok-c 0)
	(error-c 0))
    (values 
     (make-test-cases :values '((0))
		      :goal (lambda (x) (declare (ignore x)) (incf goal-c) '(0))
		      :score (lambda (r goal exp) (declare (ignore goal))
				     (if (= exp-nodes (count-tree-nodes exp))
					 (if (eq r 'error) 
					     (progn (incf error-c) *fitness-invalid*)
					     (progn (incf ok-c) 0))
					 (if (eq r 'error)
					     *fitness-invalid*
					     0))))
     (lambda () (values goal-c error-c ok-c)))))

(defun golden-ratio (n &optional (initial 1))
  (if (= 0 n)
      initial
      (golden-ratio (1- n) (1+ (/ 1 initial)))))

(defmacro def-test-cases-one-value (test-cases-name value)
  "Define a test-cases named TEST-CASES-NAME whose goal is it to obtain the value VALUE."
  (let ((s (gensym)))
    `(defparameter ,test-cases-name
       (let ((,s ,value))
	 (make-test-cases :values '(nil)
			  :generate (lambda (vs) vs)
			  :goal (lambda (x) (declare (ignore x)) ,s)
			  :score #'score-one-value)))))

(def-test-cases-one-value *test-cases-golden-ratio-value* (float (golden-ratio 20)))
;;(systematicmapping 6 '() *test-cases-golden-ratio-value* (cons 1 *joy-ops*) 1000 .01 nil)
;; yields ((1 DUP SUCC / SUCC) (1 1 SUCC / SUCC)) -0.118034005 14430760
;; (tournament-new '(1 dup succ / succ) 200 1000000 *test-cases-golden-ratio-value* *joy-ops* 1000 .01) yields a joy expression with many nested I's and -2.1576881e-5 as error,
;; but (tournament-new '() 200 1000000 *test-cases-golden-ratio-value* *joy-ops* 1000 .01) yields '(2) as joy expression and consequently -0.381966 as error.

(def-test-cases-one-value *test-cases-pi-value* pi)

(defparameter *test-cases-golden-ratio-sequence*
  (make-test-cases :values '((0 1.0) (1 1.0) (2 1.0) (3 1.0) (6 1.0) (12 1.0))
		   :generate (lambda (vs) vs)
		   :goal (lambda (x) (golden-ratio (car x) (cadr x)))
		   :score #'score-one-value))
;; (joy-eval-handler '(12 1.0) '((1 swap / succ) times))

(defun generate-test-cases-enumerate-all-exps-and-results (exp-nodes initial-stk)
  "Return a test-cases instance (to be used with systematicmapping, not tournament-new) and a function to retrieve the list of enumerated joy programs (with EXP-NODES maximal nodes) together with their results, when executed on INITIAL-STACK."
  (let ((exps-and-results nil))
    (values 
     (make-test-cases :values (list initial-stk) ;one initial stack
		      :goal (lambda (x) (declare (ignore x)) '(0))
		      :score (lambda (r goal exp) (declare (ignore goal))
				     (if (= exp-nodes (count-tree-nodes exp))
					 (progn
					   (push (cons exp r) exps-and-results)
					   (if (eq r 'error)
					       *fitness-invalid*
					       0))
					 (if (eq r 'error)
					     *fitness-invalid*
					     0))))
     (lambda () exps-and-results))))

;; Try to evolve a joy program that returns whether an input joy program has type errors in it.
;; Do this by first generating all joy-exps of a certain length, and determine whether they return an error or not.
;; Then define a test-case that in turn feeds all generated joy-exps to the joy program to be tested, and sums up the number of correct predictions.
;; This means the test-case will be expensive to run.
(defun generate-test-cases-check-valid-joy-exps (exp-nodes initial-stk joy-ops max-ticks max-seconds)
  (multiple-value-bind (test-cases-all-exps get-exps-and-results)
      (generate-test-cases-enumerate-all-exps-and-results exp-nodes initial-stk)
    (systematicmapping exp-nodes nil test-cases-all-exps joy-ops max-ticks max-seconds nil)
    (let* ((exps-and-results (funcall get-exps-and-results))
	   (joy-exp-to-result (make-lsxhash-equal-hash-table)))
      (mapcar (lambda (exp-and-result)
		(destructuring-bind (exp . result) exp-and-result
		  (setf (gethash exp joy-exp-to-result) (if (eq 'error result) nil t))))
	      exps-and-results)
      (make-test-cases :values (mapcar (lambda (x) (list (car x))) exps-and-results)
		       :generate #'identity-1
		       :goal (lambda (val) (gethash (car val) joy-exp-to-result))
		       :score (lambda (r goal exp)
				(declare (ignore exp))
				(if (symbolp r)
				    ;; FIXME: sometimes, R is 'TIMEOUT due to GC or so, which could lead to a wrong fitness.
				    *fitness-invalid*
				    (if goal
					(if (car r) 0 -1)
					(if (car r) -1 0))))))))

;(let ((tc (generate-test-cases-check-valid-joy-exps 2 '(0) *joy-ops* 1000 .01))
;      (i '(pop t))
;      (i '(dup (+) equal (pop nill) (pop t) branch))
;      (i '(dup (+) equal (pop nill)      (dup (-)    equal (pop nill) (pop t) branch) branch))
;      (i '(DUP (+) EQUAL (POP NILL SWAP) (DUP (CONS) EQUAL (POP NILL) (DUP (-) EQUAL (UNCONS) NILL BRANCH) BRANCH) BRANCH))
;      )
;(tournament-new i 20 10000 tc *joy-ops* 1000 .01)
;;;(joy-show-fitness i tc)
;)

;; Add a test-cases that scores joy programs by the value they return.
;; The length of the joy programs is also scored, programs of length larger than a predefined number receive a very low fitness.
;; Maybe this evolves joy programs that implement new creative higher-level (control) structures.

(defun generate-test-cases-maxvalue-maxprognodes (maxnodes)
  (let ((log2 (log 2)))
    (make-test-cases :values '((0))
		     :generate (lambda (vs) vs)
		     :goal (constantly nil)
		     :score (lambda (r goal exp)
			      (declare (ignore goal))
			      (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)) (> (count-tree-nodes exp) maxnodes))
				  *fitness-invalid*
				  (let ((v (car r)))
				    (if (<= v 0)
					-1
					(/ (log v) log2))))))))
;;(time (let ((tc (generate-test-cases-maxvalue-maxprognodes 11)))
;;	(systematicmapping 11 nil tc '(succ dup times) 1000 .01 nil)))
;; == ((SUCC SUCC SUCC DUP (DUP (SUCC) TIMES) TIMES)) 4.5849624 4954369 (481 seconds)

(defun generate-test-cases-list (list)
  ;; TODO: extend this function to accept a list of lists, which all should be rewarded.
  (make-test-cases :values '(nil)
		   :generate (lambda (vs) vs)
		   :goal (constantly list)
		   :score #'score-list-similarity))
;;(time (let ((tc (generate-test-cases-list '(cons cons)))
;;	    (joy-ops '(concat cons dup list nill pop quote swap)))
;;	(systematicmapping 6 nil tc joy-ops 1000 .01 nil)))
;; == (((CONS) UNCONS POP DUP))


;;;; tournament selection

(defparameter *mut-length-const* (+ 6 3))

(defun tournament-new (o size cycles fitness joy-ops max-ticks max-seconds)
  (let* ((pop (make-array size :initial-element o))
	 (mut (make-array size :initial-element (loop for i below (+ *mut-length-const* (length joy-ops)) collect .1))))
;;    (dotimes (s size) (setf (aref mut s) (loop for i below 11 collect (random .9))))
    (tournament pop mut cycles fitness joy-ops max-ticks max-seconds)))

(defun tournament-res (res cycles fitness joy-ops max-ticks max-seconds)
  (let* ((size (length res))
	 (pop (make-array size :initial-contents (loop for i in res collect (car i))))
	 (mut (make-array size :initial-contents (loop for i in res collect (caddr i)))))
    (tournament pop mut cycles fitness joy-ops max-ticks max-seconds)))

(defun tournament (pop mut cycles fitness joy-ops max-ticks max-seconds)
  (assert (= (length pop) (length mut)))
  (let* ((size (length pop))
	 (n (make-array size :element-type 'fixnum :initial-contents (loop for i below size collect i)))
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
	     (new (apply #'crossover-and-mutate joy-ops (elt pop c1) (elt pop c2)
			 new-mut)))
	(when (and (listp new) (valid-joy-exp new '(:any)))
	  ;;(print (list "new" new))
	  (let ((new-fit (fitness-score-test-values fitness test-goal-values new max-ticks max-seconds)))
	    ;;(print (list "c1" c1 "c2" c2 "c2-fit" c2-fit "new-fit" new-fit))
	    (when (or (> new-fit c2-fit) (and (= new-fit c2-fit) (chance .5)))
	      ;; sometimes, c2-fit can become very low, because of a TIMEOUT in joy-eval due to garbage collection or so.
	      ;; then, c2 is replaced by a new-mut with potentially lower fitness.
	      ;; therefore, make the population size big enough to guard against losing good genomes.
	      (setf (elt pop c2) new)
	      (setf (elt fit c2) new-fit)
	      (setf (elt mut c2) new-mut)
	      (when (> new-fit c2-fit)
		(print (list "new" new "new-fit" new-fit "new-mut" new-mut "c2-fit" c2-fit))))))
	(when (= 0 (mod c 1000))
	  (print (list c c2-fit)))
	(when (= 0 (mod c 10000))
	  (log-mut-stats (sort (zip-array 'list pop fit mut) #'< :key #'second) logstream))))
    (close logstream)
    (sort (zip-array 'list pop fit mut) #'< :key #'second)))

(defun mut-stats (res)
  (let* ((mut (mapcar #'caddr res))
	 (mut-length (length (car mut))))
    (loop for i below mut-length collect
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
    (with-hash-table-iterator (next ht)
      (labels ((rec ()
		 (multiple-value-bind (present-p key value) (next)
		   (unless present-p (return-from rec))
		   (setf (gethash key copy) value))))
	(rec)))
    copy))

(defmacro defun-list-cmp (name value-null-a-b value-null-a value-null-b number-cmp string-cmp)
  "A macro for defining a lexicographic comparison function of two lists."
  (declare (type symbol name number-cmp string-cmp)
	   (type (or null t) value-null-a-b value-null-a value-null-b))
  ;; TODO: also implement it for sequences
  `(defun ,name (list-a list-b)
     (declare (type list list-a list-b))
     (declare (values (or null t)))
     (if (null list-a)
	 (if (null list-b)
	     ,value-null-a-b
	     ,value-null-a)
	 (if (null list-b)
	     ,value-null-b
	     (let ((a (car list-a))
		   (b (car list-b)))
	       (etypecase a
		 (number (if (= a b) (,name (cdr list-a) (cdr list-b)) (,number-cmp a b)))
		 (symbol (if (string= a b) (,name (cdr list-a) (cdr list-b)) (,string-cmp (string a) (string b))))
		 (list (,name a b))))))))

(defun-list-cmp list=  t   nil nil =  string=)
(defun-list-cmp list/= nil t   t   /= string/=)
(defun-list-cmp list<  nil t   nil <  string<)
(defun-list-cmp list>  nil nil t   >  string>)
(defun-list-cmp list<= t   t   nil <= string<=)
(defun-list-cmp list>= t   nil t   >= string>=)

(defun hash-table-to-alist (ht)
  "Return the alist that stores the same key-value pairs as hash table HT."
  (let* ((k-v nil))
    (with-hash-table-iterator (next ht)
      (labels ((rec ()
		 (multiple-value-bind (present-p key value) (next)
		   (unless present-p (return-from rec))
		   (setf k-v (acons key value k-v))
		   (rec))))
	(rec)))
    ;; sort by key
    (setf k-v (sort k-v #'list< :key #'car))
    k-v))

(defun extend-and-evaluate (l-exp l-1-stk l-1-heap ins goal-value fitness-fn max-ticks max-seconds)
  "appends ins to l-1-exp and calculates the fitness for each possibility.
l-1-stk and l-1-heap must be the stack and heap returned when executing l-1-exp."
  (declare (ignorable l-1-heap) (type (function (list number list) number) fitness-fn))
  (let* ((lins (list ins))
	 (heap (if (null l-1-heap) nil (copy-hash-table l-1-heap))) ; this takes a lot of time
	 ;;(heap l-1-heap)
	 (res (joy-eval-handler l-1-stk lins :heap heap :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
	 (fit (funcall fitness-fn res goal-value l-exp)))
    ;;(when (equal l-exp '((swap) dip swap (swap) dip))
    ;;  (format t "eae. l-1-stk:~W ins:~W~%" l-1-stk ins)
    ;;  (format t "res:~W fit:~W~%" res fit)
    ;;  (format t "exp:~W~%" l-exp)
    ;;  (format t "res:~W~%" res))
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

(defun extend-exp-and-test (max-ext-nodes fitness-test-case l-1-exp l-1-stks l-1-heaps l-1-fits goal-values collectfit joy-ops max-ticks max-seconds)
  "Extend l-1-exp with an arbitrary tree of at most max-ext-nodes nodes.
Test with all fitness-test-cases and extend those expressions whose result had no error.
l-1-fits must be a list of fitnesses which must be nil if the previous level yielded no error."
  (when (> max-ext-nodes 1)
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
			  (when (and pursue (funcall collectfit l-stks l-heaps l-exp fit-sum))
			    (extend-exp-and-test (- max-ext-nodes ext-nodes) fitness-test-case l-exp l-stks l-heaps l-fits goal-values collectfit joy-ops max-ticks max-seconds))))))
	       (let* ((ins-sets (loop for i below ext-symbols collect joy-ops)))
		 (enumerate-set-combinations ins-sets #'f1)))
	     (when (> max-ext-nodes 3)
	       (let ((elapsed-seconds (float (/ (- (get-internal-real-time) enum-fill-start-time) internal-time-units-per-second))))
		 (format t "max-ext-nodes:~A l-1-exp:~A ext-struct:~A ext-nodes:~A ext-symbols:~A elapsed-seconds:~A~%" max-ext-nodes l-1-exp ext-struct ext-nodes ext-symbols elapsed-seconds))))))))

(defun systematicmapping (maxlevel exp0 fitness-test-case joy-ops max-ticks max-seconds cache)
  (declare (type (or null (integer 1 #.most-positive-fixnum)) cache))
  (let* ((test-values (test-cases-values fitness-test-case))
	 (goal-values (mapcar (test-cases-goal fitness-test-case) test-values))
	 (l0-heaps (loop for i below (length test-values) collect (make-hash-table :size 0)))
	 ;;(l0-heaps (loop for i below (length test-values) collect nil))
	 (l0-exps (mapcar (lambda (v) (append v exp0)) test-values))
	 (l0-stks (mapcar (lambda (e h) (joy-eval-handler nil e :heap h)) l0-exps l0-heaps))
	 (fitn (mapcar (test-cases-score fitness-test-case) l0-stks goal-values l0-exps))
	 (l0-fits (mapcar (lambda (s f) (if (not (eq s 'error)) nil f)) l0-stks fitn))
	 (best-fit (apply #'+ fitn))
	 (best-exp (list nil))
	 (number-trees (count-labelled-trees maxlevel (length joy-ops)))
	 (results-seen-cache (when cache (make-mru-cache (min number-trees cache))))
	 (trees-evaluated 0))
    (flet ((collectfit (stks heaps exp fit)
	     (incf trees-evaluated)
	     (when (= 0 (mod trees-evaluated 1000))
	       (format t "percent visited (of total possible):~A~%" (float (/ trees-evaluated number-trees))))
	     (if (> fit best-fit)
		 (progn (setf best-fit fit) (setf best-exp (list (append exp0 exp))))
		 (when (= fit best-fit)
		   (setf best-exp (cons (append exp0 exp) best-exp))))
	     (if (not cache)
		 t
		 ;; if the results (stks and heaps) are in the cache, we don't have to pursue, since the same stk and extension ins will give same results.
		 ;; however, if the exp that led to the results in the cache is longer than the one we found, then we have to pursue.
		 (let* ((heap-alists (loop for h in heaps collect (if (null h) nil (hash-table-to-alist h)))) ;;convert to alist to save space
			(res (cons stks heap-alists)))
		   (multiple-value-bind (exp-old present)
		       (mru-cache-get-value results-seen-cache res)
		     (let* ((exp-nodes (count-tree-nodes exp))
			    (exp-old-nodes (count-tree-nodes exp-old))
			    (pursue (or (not present) (< exp-nodes exp-old-nodes))))
		       (when pursue
			 (mru-cache-set results-seen-cache res exp))
		       ;;(when (and present (< exp-nodes exp-old-nodes))
		       ;;  (format t "exp-old:~A exp:~A~%" exp-old exp))
		       ;;(when present
		       ;;  (format t "collision exp-old:~A exp:~A res:~A lsxhash:~A (not present):~A~%" exp-old exp res (lsxhash res) (not present)))
		       pursue))))))
      (format t "nil fitn:~A best-fit:~A~%" fitn best-fit)
      (extend-exp-and-test maxlevel fitness-test-case nil l0-stks l0-heaps l0-fits goal-values #'collectfit joy-ops max-ticks max-seconds)
      (values best-exp best-fit trees-evaluated))))

;;(time (systematicmapping 4 '() *test-cases-sqrt* (append '(1 A) *joy-ops*) 1000 .01 nil))

;;(require :sb-sprof)
;;(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil)
;;  (systematicmapping 4 '() *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))
;;(sb-sprof:with-profiling (:max-samples 1000 :mode :alloc :report :flat)
;;  (systematicmapping 4 '() *fitness-sqrt-test* (cons '(1 A) *joy-ops*) 1000 .01))

;; computer-name results [score exp-evaluations] runtime
;;(time (systematicmapping 6 '() *test-cases-sqrt* *joy-ops* 1000 .01))
;; pura ((DUP / SUCC SUCC SUCC) (DUP / SUCC DUP *) (DUP / SUCC DUP +)) 878.140 sec
;; Bobo ((DUP / SUCC SUCC SUCC) (DUP / SUCC DUP *) (DUP / SUCC DUP +)) -85.62277 4173780 562.001
;; Bobo (((DUP / SUCC) I)) -95.62277 857933 136.662 w/ cache
;;(time (systematicmapping 6 '() *test-cases-sqrt* (subseq *joy-ops* 20 32) 1000 .01))
;; Bobo ((STACK (-) STEP SUCC) (STACK (- SUCC) STEP)) -100.62277 64832 13.577
;; Bobo ((STACK (-) STEP SUCC) (STACK (- SUCC) STEP)) -100.62277 51340 11.941 w/ cache


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
      (enumerate-labelled-trees n joy-ops #'print #'try))
    (values errors oks)))

; (count-error-joy-expressions 5 '(0) *joy-ops* 1000 .01)
; errors: 12 592 23077 850875 30945104
; oks:    19 400 9628  257189 7524718

;(multiple-value-bind (test-cases get-counts)
;    (generate-test-cases-systematicmapping-oks 2)
;  (systematicmapping 2 '() test-cases *joy-ops* 1000 .01)
;  (funcall get-counts))
