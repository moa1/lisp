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
  (if (<= counter 0)
      (lambda () 1)
      (let ((c counter))
	(lambda () (decf c)))))

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
  (let ((s (reduce (lambda (a b) (+ a b)) w)))
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

;; '((1) 1 DEFINE 1)
(defun joy-eval (stk exp &key (p (make-hash-table)) (c (make-counter 0)))
;;  (print (list "stk" stk "exp" exp))
  (declare (optimize (debug 0) (compilation-speed 0) (speed 3) (space 0)))
  (if (<= (funcall c) 0)
      (return-from joy-eval 'overrun))
  (if (null exp)
      stk
      (joy-eval
       (case (car exp)
	 (+       (cons (+ (cadr stk) (car stk)) (cddr stk)))
	 (and     (cons (and (car stk) (cadr stk)) (cddr stk)))
	 (apply   (joy-eval (cdr stk) (car stk) :p p :c c))
	 (compose (cons (append (car stk) (cadr stk)) (cddr stk)))
	 (cons    (cons (cons (cadr stk) (car stk)) (cddr stk)))
	 (dec     (cons (- (car stk) 1) (cdr stk)))
	 (dip     (cons (cadr stk) (joy-eval (cddr stk) (car stk) :p p :c c)))
	 (/       (cons (/ (cadr stk) (car stk)) (cddr stk)))
	 (dup     (cons (car stk) stk))
	 (eq      (cons (eq (cadr stk) (car stk)) (cddr stk)))
	 (false   (cons nil stk))
	 (ifte    (if (car (joy-eval (cdddr stk) (caddr stk) :p p :c c))
		      (joy-eval (cdddr stk) (cadr stk) :p p :c c)
		      (joy-eval (cdddr stk) (car stk) :p p :c c)))
	 (inc     (cons (+ (car stk) 1) (cdr stk)))
	 (list    (cons (list (car stk)) (cdr stk)))
	 (*       (cons (* (car stk) (cadr stk)) (cddr stk)))
	 (not     (cons (not (car stk)) (cdr stk)))
	 (or      (cons (or (car stk) (cadr stk)) (cddr stk)))
	 (papply  (cons (cons (cadr stk) (car stk)) (cddr stk)))
	 (pop     (cdr stk))
	 (quote   (cons (list (car stk)) (cdr stk)))
	 (rem     (cons (mod (cadr stk) (car stk)) (cddr stk)))
	 (-       (cons (- (cadr stk) (car stk)) (cddr stk)))
	 (swap    (cons (cadr stk) (cons (car stk) (cddr stk))))
	 (true    (cons t stk))
	 (uncons  (cons (cdar stk) (cons (caar stk) nil)))
	 ;; my own definitions
	 (concat  (cons (append (cadr stk) (car stk)) (cddr stk)))
	 (i       (joy-eval (cdr stk) (car stk) :p p :c c))
	 (step    (let ((res (cddr stk)))
		    (loop for i in (cadr stk) do
			 (setf res (joy-eval (cons i res) (car stk) :p p :c c)))
		    res))
	 (define  (setf (gethash (car stk) p) (cadr stk)) (cddr stk))
	 ;; implement a "undefine", which ends the scope of a "define"d program, but leaves defined programs (and programs on the stack) using the to be "undefine"d program running intact. this would require replacing the "define"d name with an anonymous name.
	 (t
	  (multiple-value-bind (value present-p) (gethash (car exp) p)
	    (if present-p
		(progn (setf exp (append '(1) value (cdr exp))) stk)
		(cons (car exp) stk)))))
       (cdr exp) :p p :c c)))

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

(defun joy-test (stk exp res)
  (let ((r (joy-eval stk exp)))
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
(joy-test nil '(5 dec) '(4))
(joy-test nil '(1 2 5 (+) dip) '(5 3))
(joy-test nil '(5 2 /) '(5/2))
(joy-test nil '(3 dup) '(3 3))
(joy-test nil '(3 3 eq) '(t))
(joy-test nil '(3 t eq) '(nil))
(joy-test nil '(false) '(nil))
(joy-test nil '((true) (1) (2) ifte) '(1))
(joy-test nil '((false) (1) (2) ifte) '(2))
(joy-test nil '((0 true) (1) (2) ifte) '(1))
(joy-test nil '(3 inc) '(4))
(joy-test nil '(3 list) '((3)))
(joy-test nil '(3 4 *) '(12))
(joy-test nil '(true not) '(nil))
(joy-test nil '(nil 5 or) '(5))
(joy-test nil '(nil nil or) '(nil))
(joy-test nil '(1 (eq) papply) '((1 eq)))
(joy-test nil '(5 4 pop) '(5))
(joy-test nil '(5 quote) '((5)))
(joy-test nil '(9 4 rem) '(1))
(joy-test nil '(4 5 -) '(-1))
(joy-test nil '(1 2 swap) '(1 2))
(joy-test nil '(true) '(t))
(joy-test nil '(4 5 list cons uncons) '((5) 4))
;; own tests
(joy-test nil '(0 (1 2 3) (4 5 6) concat) '((1 2 3 4 5 6) 0))
(joy-test nil '(2 (3 +) i) '(5))
(joy-test nil '((swap cons) swons define 0 nil (1 2 3) (swons) step) '((3 2 1) 0))
(joy-test nil '(2 (dup +) superman define) '(2))
(joy-test nil '(2 (1) superman define superman) '(1 2))
(joy-test nil '(1 a) '(a 1))
;; own defines
(joy-test nil '((0 eq) null define 0 null) '(t))
(joy-test nil '((0 eq) null define 1 null) '(nil))
(joy-test nil '((dec) pred define 5 pred) '(4))
(joy-test nil '((inc) succ define 5 succ) '(6))
(joy-test nil '((swap cons) swons define (2) 1 swons) '((1 2)))
(joy-eval nil
	  '(((dup cons) swap concat dup cons i) y define
	    (((pop null) (pop inc) ((dup dec) dip i *) ifte) y) fac define
	    1 fac))


(defun joy-eval-handler (stk exp &key (p (make-hash-table)) (c (make-counter)))
  (handler-case (joy-eval stk exp :p p :c c)
    (simple-type-error () 'error)
    (type-error () 'error)
    (division-by-zero () 'error)))

(defparameter joy-ops '(+ and apply compose cons dec dip / dup eq false ifte
			inc list * nil not or papply pop quote rem - swap true
			uncons concat i step define))

(defun mutate (exp p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 
	       q1 q2 q3 q4 q5 q6 q7 q8 q9 q10
	       r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18
	       r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30
	       &optional (debranch-p t))
;;  (print (list "exp" exp))
  (flet ((random-final (p)
	   (if (< (random 1.0) p)
	       (weighted-choice (list q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
				'(a b c d e 1 2 4 8 16))
	       (weighted-choice (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30)
				joy-ops))))
    (cond
      ((null exp) (if (< (random 1.0) p1) ;extend
		      (cons (random-final p2)
			    nil)))
      ((listp exp) (if (and debranch-p (< (random 1.0) p3))
		       (random-final p4) ; de-branch
		       (cons (mutate (car exp) p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30)
			     (if (and (= 1 (length (cdr exp)))
				      (< (random 1.0) p5))
				 nil ; shorten
				 (mutate (cdr exp) p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 nil)))))
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
	(apply #'mutate-rec (apply #'mutate exp prest) probs)
	(apply #'mutate exp prest))))

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

(defun find-mutate (exp prob times)
  (print (list "(find-mutate" exp prob times ")"))
  (let ((hits 0))
    (dotimes (i times (/ hits times 1.0))
      (handler-case
	  (progn
	    (destructuring-bind ((v1 v2) v3) (mutate-rec exp prob) (print (list (list v1 v2) v3)))
	    (incf hits))
	;;(division-by-zero () (format t "division-by-zero~%") 1)
	(sb-kernel::arg-count-error () t)
	(sb-kernel::defmacro-bogus-sublist-error () t)
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
  (let ((res (joy-eval-handler nil o :c (make-counter 100))))
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
			  (r (joy-eval-handler (list l) rev :c (make-counter 1000)))
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

(defun fitness-sqrt (o)
  (flet ((fun (x) (sqrt x))
	 (valdiff (a b) (- (abs (- a b)))))
    (loop for x in '(0 1 5 10 20 40 80 100 200 250 1000 10000 100000)
       sum (let* ((value (fun x))
		  (r (joy-eval-handler (list x) o :c (make-counter 1000))))
;;	     (print (list "x" x "value" value "r" r))
	     (if (or (not (listp r)) (not (numberp (car r))) (not (= (length r) 1)))
		 -1000
		 (valdiff value (car r)))))))

(defun tournament-new (o size cycles fitness)
  (let* ((pop (make-array size :initial-element o))
	 (mut (make-array size :initial-element ;;(loop for i below 53 collect .1))))
			  '(0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1))))
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
;;	(print (list "c1" c1 "c2" c2 "c2-fit" c2-fit "new-fit" new-fit))
	(if (> new-fit c2-fit)
	    (progn (setf (elt pop c2) new)
		   (setf (elt fit c2) new-fit)
		   (setf (elt mut c2) new-mut)
		   (print (list "new" new "new-fit" new-fit "new-mut" new-mut "c2-fit" c2-fit))))
	(when (= 0 (mod c 1000))
	  (print (list c new-fit c2-fit)))
	(when (= 0 (mod c 10000))
	  (log-mut-stats (sort (zip-array 'list pop fit mut) #'< :key (lambda (x) (nth 1 x))) logstream))))
    (close logstream)
    (sort (zip-array 'list pop fit mut) #'< :key (lambda (x) (nth 1 x)))))

(defun mut-stats (res)
  (let ((mut (mapcar #'caddr res)))
    (loop for i below 53 collect
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
