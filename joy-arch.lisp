;; (in-package #:joy-arch) ;does this require an .asd file?

;; (sb-ext:restrict-compiler-policy 'debug 3)
;; (declaim (optimize speed))

(load "~/quicklisp/setup.lisp")
(ql:quickload 'alexandria)
(use-package 'alexandria)
(ql:quickload 'ltree)
(use-package 'ltree)
(ql:quickload 'cl-heap)

(load "~/lisp/arbitrary-trees.lisp")
;;(load "~/lisp/multitree.lisp")
(handler-case (load "~/lisp/multitree.lisp")
  (SB-PCL::SLOTD-INITIALIZATION-ERROR (c) (continue c))) ;how to avoid it?
(load "~/lisp/refal-patmat.lisp")
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

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun zip-array (result-type &rest arrays)
  (apply #'map result-type #'list arrays))

;; in alexandria
;; (defun proper-list-p (l)
;;   ; TODO: this function should reject circular lists
;;   (if (null l)
;;       t
;;       (if (consp l)
;; 	  (proper-list-p (cdr l))
;; 	  nil)))

(defun make-counter (&optional (counter 0))
  (declare (type fixnum counter))
  (if (<= counter 0)
      (values (lambda () 1)
	      (lambda (counter-delta) (declare (ignore counter-delta))))
      (let ((c counter))
	(check-type c fixnum)
	(values (lambda () (decf c))
		(lambda (counter-delta) (incf c counter-delta))))))

(defun make-countdown (&optional (seconds 0.0))
  "Returns two functions.
The first, when called, yields the number of internal run time units remaining until the timer expires.
The second, called with a number of seconds, postpones the timer by the number of seconds."
  (declare (type single-float seconds))
  (if (<= seconds 0)
      (values (lambda () 1)
	      (lambda (seconds-delta) (declare (ignore seconds-delta))))
      (let ((c (+ (get-internal-run-time) (ceiling (* seconds internal-time-units-per-second)))))
	(check-type c (and integer unsigned-byte))
	(values	(lambda () (- c (get-internal-run-time)))
		(lambda (seconds-delta) (incf c (* seconds-delta internal-time-units-per-second)))))))

(define-condition time-error (condition)
  ((time :initarg :time :reader time-error-time)) ;the value describing the time when the error was detected
  (:documentation "An error having to do with time."))

;; in alexandria
;;(defun mean (seq)
;;  (/ (apply #'+ seq) (length seq)))

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
  (let* ((s (reduce #'+ w))) ;:from-end T was here for the following reason: is necessary because otherwise (weighted-sample-index '(0.99 0.989622 0.9895308 0.9893857 0.9898456 0.98966277 0.9894621 0.99 0.9889513 0.9896954)) fails with a high probability in 10.000.000 cases. probably a corner case of float arithmetic. with :from-end we add in the same order in that we are subtracting in rec. However, it doesn't help in all cases. Therefore, s is artificially reduced to be smaller than the sum.
    (assert (> s 0))
    (labels ((rec (x v i)
	       (if (null v) ;this should never happen, but due to floating-point inaccuracies, it does.
		   (rec (random s) w 0) ;then just try another (random s).
		   (let ((x0 (- x (car v))))
		     (if (< x0 0)
			 i
			 (rec x0 (cdr v) (1+ i)))))))
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

(defmacro time2 (&body body)
  "Execute BODY and return the number of seconds passed.
The first return value is the number of seconds passed as measured by get-internal-run-time, the second as measured by get-internal-real-time."
  (let ((run (gensym)) (real (gensym)))
    `(let ((,run (get-internal-run-time))
	   (,real (get-internal-real-time)))
       ,@body
       (let ((,run (- (get-internal-run-time) ,run))
	     (,real (- (get-internal-real-time) ,real)))
	 (values (float (/ ,run internal-time-units-per-second))
		 (float (/ ,real internal-time-units-per-second)))))))

(defun seconds-per-call (function &key (mintime 0.01) (next-loops-measurable (lambda (x) (* 2 x))) (next-loops-measure next-loops-measurable))
  "Return the average number of seconds per call of function FUNCTION.
First find out how often the nullary FUNCTION has to be run in a loop until its run-time becomes measurable.
This works by running it once, and, unless its run-time is measurable (i.e. greater than 0.0 seconds), running it (funcall NEXT-LOOPS-MEASURABLE 1) times.
It is then run this number of times in a loop, and this is repeated until the run-time of the loop is measurable.
Let the number of calls that were measurable be minloop.
Then, FUNCTION is run minloop times in a loop, and its run-time is measured.
If the run-time is at least MINTIME, the number of calls per second is returned, otherwise MINTIME is set to (funcall NEXT-LOOPS-MEASURE minloop), and the measuring is repeated."
  (flet ((measure-loop (iterations)
	   (let ((start (get-internal-run-time)))
	     (loop for i below iterations do (funcall function))
	     (/ (- (get-internal-run-time) start) internal-time-units-per-second))))
    (let* ((loops 0)
	   (elapsed 0.0))
      ;;; increase loops until run-time is measurable.
      (do () ((> elapsed 0.0) loops)
	(setf loops (if (zerop loops)
			1
			(funcall next-loops-measurable loops)))
	(setf elapsed (measure-loop loops)))
      ;;(print (list "loops" loops "elapsed" elapsed))
      ;;; increase loops until run-time is at least MINTIME.
      (do () ((> elapsed mintime) (float (/ elapsed loops)))
	(setf loops (funcall next-loops-measure loops))
	(setf elapsed (measure-loop loops))))))

(defvar +patmat-seconds-per-node+ (let ((stk '(((NIL) NIL) ((NIL) NIL))))
					 (/ (seconds-per-call (lambda () (patmat stk stk)) :mintime 0.5) (count-tree-nodes stk))))

(defun list-replace-symbols (list plist)
  "Recursively replace (non-destructively) all occurrences of the symbols in PLIST in LIST with the values stored in PLIST.
Example: (list-replace-symbols '(a b (c a (d)) e) '(a 1 e nil)) == '(1 B (C 1 (D)) NIL)."
  ;; FIXME: I think this function caused a heap overflow, at least when backtracing in LDB the function REC of this function showed up.
  ;;(print (list "list" list "plist" plist))
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

(defvar +list-replace-symbols-seconds-per-node+ (let ((l '(a b (c a (d)) e)) (bind '(a 1 e nil)))
						  (/ (seconds-per-call (lambda () (list-replace-symbols l bind)) :mintime 0.5) (+ (count-tree-nodes l) (count-tree-nodes bind)))))

(define-condition joy-error (error)
  ((stk :initarg :stk :reader joy-error-stk) ;the joy stack before the operation causing the error.
   (exp :initarg :exp :reader joy-error-exp) ;the joy expressions before the operation causing the error. (this means (car exp) is the operation causing the error.)
   (heap :initarg :heap :reader joy-error-heap)) ;the joy heap before the operation causing the error.
  (:documentation "An error in a joy program."))

(define-condition joy-eval-2-error (joy-error)
  ((stks :initarg :stks :reader joy-eval-2-error-stks)
   (exps :initarg :exps :reader joy-eval-2-error-exps))
  (:documentation "An error in a joy program during evaluation with joy-eval-2."))

(define-condition joy-counter-error (joy-error time-error)
  ()
  (:documentation "An error signaling too many computation steps of a joy program."))

(define-condition joy-countdown-error (joy-error time-error)
  ()
  (:documentation "An error signaling a too long runtime of a joy program."))

(define-condition joy-eval-2-counter-error (joy-eval-2-error time-error)
  ()
  (:documentation "An error signaling too many computation steps of a joy program evaluated with joy-eval-2."))

(define-condition joy-eval-2-countdown-error (joy-eval-2-error time-error)
  ()
  (:documentation "An error signaling a too long runtime of a joy program evaluated with joy-eval-2."))

(define-condition unknown-op-error (error)
  ((op :initarg :op :reader unknown-op-error-op))
  (:documentation "An error signaling an unknown operation."))

;; '((1) 1 DEFINE 1)
(defun joy-eval (stk exp &key (heap (make-hash-table)) (c (make-counter 0)) (cd (make-countdown 0.0)))
  (declare (optimize (debug 0) (compilation-speed 0) (speed 3) (space 0))
	   (type (function () fixnum) c cd))
  "Note that this function does not fail for the same inputs as the joy implementation by Manfred von Thun, e.g. '(branch) returns nil, but would fail for the real implementation.
However, it should raise an error for cases when the stack becomes a non-list.
This function must not modify stk, only copy it (otherwise test values might be modified)."
  (let ((c-value (funcall c)) (cd-value (funcall cd)))
    ;;(print (list "stk" stk "exp" exp "c-value" c-value "cd-value" cd-value))
    (when (<= c-value 0)
      (restart-case
	  (error (make-condition 'joy-counter-error :stk stk :exp exp :heap heap :time c-value))
	(continue ()
	  :report "Continue, ignoring the counter overrun this step.")))
    (when (<= cd-value 0.0) ;if this check doesn't do what it is supposed to, check the output type of (funcall cd) and whether it is identical to the cd type declaration of joy-eval!
      (restart-case
	  (error (make-condition 'joy-countdown-error :stk stk :exp exp :heap heap :time cd-value))
	(continue ()
	  :report "Continue, ignoring the countdown timeout this step."))))
  (if (null exp)
      stk
      (joy-eval
       ;; A restart-case form which allows setting the result of the case statement below in case of error would be nice here. However, such a restart is much too slow. (see restart-vs-no-restart in speed.lisp.)
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
	 (gensym  (cons (gensym) stk))
	 (i       (joy-eval (cdr stk) (car stk) :heap heap :c c :cd cd)) ;same as apply
	 (ifte    (if (car (joy-eval (cdddr stk) (caddr stk) :heap heap :c c :cd cd)) ; similar to branch
		      (joy-eval (cdddr stk) (cadr stk) :heap heap :c c :cd cd)
		      (joy-eval (cdddr stk) (car stk) :heap heap :c c :cd cd)))
	 (list    (cons (listp (car stk)) (cdr stk)))
	 (*       (cons (* (car stk) (cadr stk)) (cddr stk))) ;multiply
	 (nill    (cons nil stk)) ;same as false
	 (not     (cons (not (car stk)) (cdr stk))) ; can be emulated by branch
	 (or      (cons (or (car stk) (cadr stk)) (cddr stk)))
	 (patmat  (let ((exp (cadr stk)) (pat (car stk)) (cd-value (funcall cd)))
		    (if (> (* +patmat-seconds-per-node+ (+ (count-tree-nodes exp) (count-tree-nodes pat)))
			   cd-value)
			(error (make-condition 'joy-countdown-error :stk stk :exp exp :heap heap :cd-value cd-value))
			(cons
			 (alist-to-plist (patmat exp pat))
			 (cdddr stk)))))
	 (patsub  (let ((l (car stk)) (bind (cadr stk)) (cd-value (funcall cd)))
		    (if (> (* +list-replace-symbols-seconds-per-node+ (+ (count-tree-nodes l) (count-tree-nodes bind)))
			   cd-value)
			(error (make-condition 'joy-countdown-error :stk stk :exp exp :heap heap :cd-value cd-value))
			(cons (list-replace-symbols l bind) (cddr stk)))))
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
	 (sample  (cons (sample (car stk)) (cdr stk)))
	 (si      (cons (joy-eval (cadr stk) (car stk) :heap heap :c c :cd cd) (cddr stk)))
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
	 ;; TODO: remove define or let it have another meaning, see below.
	 (define  (if (null heap) (error "define doesn't work for a nil heap")
		      (if (not (listp (car stk)))
			  (error (make-condition 'type-error :datum (car stk) :expected-type 'list))
			  (if (not (symbolp (caar stk)))
			      (error (make-condition 'type-error :datum (caar stk) :expected-type 'symbol))
			      (progn
				(setf (gethash (caar stk) heap) (cadr stk)) ;(cdar stk) is not used.
				(cddr stk))))))
	 ;; TODO: add a "constant" joy operation, which defines a constant and signals an error if a re-definition defines it differently. (maybe create a new const-heap, which stores the constants.)
	 ;; TODO: implement an "undefine", which ends the scope of a "define"d program, but leaves defined programs (and programs on the stack) using the to be "undefine"d program running intact. this would require replacing the "define"d name with an anonymous name.
	 (t
	  (if (or (numberp (car exp)) (listp (car exp)))
	      (cons (car exp) stk)
	      (multiple-value-bind (value present-p) (when (not (null heap)) (gethash (car exp) heap))
		(if present-p
		    (progn (setf exp (append '(1) value (cdr exp))) stk)
		    (error (make-condition 'unknown-op-error :op (car exp))))))))
       (cdr exp) :heap heap :c c :cd cd)))

(defun default-no-op (op stk exp stks exps heap)
  (declare (ignore stk exp stks exps heap))
  (error (make-condition 'unknown-op-error :op op)))

(defun joy-eval-2 (stk exp stks exps &key (heap (make-hash-table)) (c (make-counter 0)) (cd (make-countdown 0.0)) (no-op #'default-no-op))
  (declare (optimize (debug 3) (compilation-speed 0) (speed 3) (space 0))
	   (type (function () fixnum) c cd))
  "Note that this function does not fail for the same inputs as the joy implementation by Manfred von Thun, e.g. '(branch) returns nil, but would fail for the real implementation.
However, it should raise an error for cases when the stack becomes a non-list.
This function must not modify stk, only copy it (otherwise test values might be modified)."
  ;; A restart-case form which allows setting the result of the case statements below in case of error would be nice here. However, such a restart is much too slow. (see restart-vs-no-restart in speed.lisp.)
  ;;(print (list "stk" stk "exp" exp "stks" stks "exps" exps))
  (let ((c-value (funcall c)) (cd-value (funcall cd)))
    ;;(print (list "stk" stk "exp" exp "c-value" c-value "cd-value" cd-value))
    (when (<= c-value 0)
      ;; FIXME: instead of restart-case I should use throw/catch, b/c it's much faster (see speed.lisp).
      (restart-case
	  (error (make-condition 'joy-eval-2-counter-error :stk stk :exp exp :stks stks :exps exps :heap heap :time c-value))
	(continue ()
	  :report "Continue, ignoring the counter overrun this evaluation step.")))
    (when (<= cd-value 0.0) ;if this check doesn't do what it is supposed to, check the output type of (funcall cd) and whether it is identical to the cd type declaration of joy-eval!
      (restart-case
	  (error (make-condition 'joy-eval-2-countdown-error :stk stk :exp exp :stks stks :exps exps :heap heap :time cd-value))
	(continue ()
	  :report "Continue, ignoring the countdown timeout this evaluation step."))))
  (macrolet ((rec (stk exp stks exps)
	       `(joy-eval-2 ,stk ,exp ,stks ,exps :heap heap :c c :cd cd :no-op no-op)))
    (if (null exp)
	(if (null exps)
	    (progn
	      (assert (null stks))
	      stk)
	    (ecase (car exps)
	      ;;(branch) is completely handled below.
	      (dip    (rec (cons (car stks) stk) (cadr exps) (cdr stks) (cddr exps)))
	      (i      (rec stk (cadr exps) stks (cddr exps)))
	      (ifte   (if (car stk)
			  (rec (cdddar stks) (cadar stks) (cdr stks) (cons 'i (cdr exps)))
			  (rec (cdddar stks) (caar stks) (cdr stks) (cons 'i (cdr exps)))))
	      (si     (rec (cons stk (car stks)) (cadr exps) (cdr stks) (cddr exps)))
	      (step   (if (null (cdar stks))
			  (rec stk (cadr exps) (cdr stks) (cddr exps))
			  (rec (cons (cadar stks) stk) (caar stks)
			       (cons (cons (caar stks) (cddar stks)) (cdr stks))
			       exps)))
	      (times  (if (> (cadar stks) 0)
			  (rec stk (caar stks)
			       (cons (cons (caar stks) (cons (1- (cadar stks)) (cddar stks))) (cdr stks))
			       exps)
			  (rec stk (cadr exps) (cdr stks) (cddr exps))))
	      ;;(while  (let* ((con (cadar stks))
	      ;;	     (step (caar stks))
	      ;;	     (newstk (cons nil (cons step (cons con stk)))))
	      ;;	(rec stk con
	      ;;	     (cons newstk (cons nil (cons (car stks) (cdr stks))))
	      ;;	     (cons 'ifte (cons nil (cons 'while (cdr exps)))))))
	      ))
	(case (car exp)
	  (branch (if (caddr stk)
		      (rec (cdddr stk) (cadr stk) stks (cons 'i (cons (cdr exp) exps)))
		      (rec (cdddr stk) (car stk) stks (cons 'i (cons (cdr exp) exps)))))
	  (dip    (rec (cddr stk) (car stk) (cons (cadr stk) stks) (cons 'dip (cons (cdr exp) exps))))
	  (i      (rec (cdr stk) (car stk) stks (cons 'i (cons (cdr exp) exps))))
	  (ifte   (rec (cdddr stk) (caddr stk) (cons stk stks) (cons 'ifte (cons (cdr exp) exps))))
	  (si     (rec (cadr stk) (car stk) (cons (cddr stk) stks) (cons 'si (cons (cdr exp) exps))))
	  (step   (rec (cddr stk) nil (cons (cons (car stk) (cadr stk)) stks) (cons 'step (cons (cdr exp) exps))))
	  (times  (rec (cddr stk) nil (cons stk stks) (cons 'times (cons (cdr exp) exps))))
	  ;;(while  (rec (cddr stk) (cadr stk) (cons (cons nil stk) (cons nil (cons stk stks))) (cons 'ifte (cons nil (cons 'while (cons (cdr exp) exps))))))
	  (t (rec
	      (case (car exp)
		;;(1=      (cons (= (car stk) 1) (cdr stk)))
		(+       (cons (+ (cadr stk) (car stk)) (cddr stk))) ;add
		(and     (cons (and (car stk) (cadr stk)) (cddr stk)))
		(concat  (cons (append (cadr stk) (car stk)) (cddr stk)))
		(cons    (cons (cons (cadr stk) (let ((a (car stk))) (if (proper-list-p a) a (error (make-condition 'type-error :datum a :expected-type 'list))))) (cddr stk))) ; same as papply
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
		(gensym  (cons (gensym) stk))
		(list    (cons (listp (car stk)) (cdr stk)))
		(*       (cons (* (car stk) (cadr stk)) (cddr stk))) ;multiply
		(nill    (cons nil stk)) ;same as false
		(not     (cons (not (car stk)) (cdr stk))) ; can be emulated by branch
		(or      (cons (or (car stk) (cadr stk)) (cddr stk)))
		(patmat  (let ((exp (cadr stk)) (pat (car stk)) (cd-value (funcall cd)))
			   (if (> (* +patmat-seconds-per-node+ (+ (count-tree-nodes exp) (count-tree-nodes pat)))
				  cd-value)
			       (error (make-condition 'joy-countdown-error :stk stk :exp exp :heap heap :cd-value cd-value))
			       (cons
				(alist-to-plist (patmat exp pat))
				(cdddr stk)))))
		(patsub  (let ((l (car stk)) (bind (cadr stk)) (cd-value (funcall cd)))
			   (if (> (* +list-replace-symbols-seconds-per-node+ (+ (count-tree-nodes l) (count-tree-nodes bind)))
				  cd-value)
			       (error (make-condition 'joy-countdown-error :stk stk :exp exp :heap heap :cd-value cd-value))
			       (cons (list-replace-symbols l bind) (cddr stk)))))
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
		(sample  (cons (sample (car stk)) (cdr stk)))
		(<       (cons (< (cadr stk) (car stk)) (cddr stk))) ;smaller
		(stack   (cons stk stk))
		(-       (cons (- (cadr stk) (car stk)) (cddr stk))) ;subtract
		(succ    (cons (1+ (car stk)) (cdr stk)))
		(swap    (cons (cadr stk) (cons (car stk) (cddr stk))))
		(true    (cons t stk))
		(uncons  (cons (cdar stk) (cons (caar stk) (cdr stk))))
		(unstack (let ((a (car stk))) (if (proper-list-p a) (car stk) (error (make-condition 'type-error :datum a :expected-type 'list)))))
		;; define is special
		(define  (if (null heap) (error "define doesn't work for a nil heap")
			     (if (not (listp (car stk)))
				 (error (make-condition 'type-error :datum (car stk) :expected-type 'list))
				 (if (not (symbolp (caar stk)))
				     (error (make-condition 'type-error :datum (caar stk) :expected-type 'symbol))
				     (progn
				       (setf (gethash (caar stk) heap) (cadr stk)) ;(cdar stk) is not used.
				       (cddr stk))))))
		;; implement an "undefine", which ends the scope of a "define"d program, but leaves defined programs (and programs on the stack) using the to be "undefine"d program running intact. this would require replacing the "define"d name with an anonymous name.
		(t
		 (if (or (numberp (car exp)) (listp (car exp)) (null heap))
		     (cons (car exp) stk)
		     (multiple-value-bind (value present-p) (when (not (null heap)) (gethash (car exp) heap))
		       (if present-p
			   (progn (setf exp (append '(1) value (cdr exp))) stk)
			   (multiple-value-bind (new-stk new-exp new-stks new-exps new-heap)
			       (funcall no-op (car exp) stk (cdr exp) stks exps heap)
			     (setf stk new-stk)
			     (setf exp (cons nil new-exp))
			     (setf stks new-stks)
			     (setf exps new-exps)
			     (setf heap new-heap)
			     stk))))))
	      (cdr exp) stks exps))))))

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
		       stk exp res r))))
  (let* ((joy-while '(((stack) dip dup (si uncons pop) dip) dip
		      dup (swap ((dup) dip swap) dip swap) dip swap
		      (((nill branch) dip) dip) dip
		      (while) (pop pop) branch))
	 (heap (make-hash-table)))
    (setf (gethash 'while heap) joy-while)
    (let ((r (joy-eval-2 stk exp nil nil :heap heap)))
      (if (not (equal r res))
	  (error (format nil "joy-test2 failed for stk:~A exp:~A res:~A r:~A"
			 stk exp res r))))))

(joy-test nil '(nill nill) '(nil nil))
(joy-test nil '(5 4 +) '(9))
(joy-test nil '(nill 5 and) '(nil))
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
(joy-test nil '(nill 5 or) '(5))
(joy-test nil '(nill nill or) '(nil))
(joy-test nil '(5 4 pop) '(5))
(joy-test nil '(5 pred) '(4))
(joy-test nil '(5 quote) '((5)))
(joy-test nil '(9 4 rem) '(1))
(joy-test nil '(1 2 (3 4 5 6) (+ -) si) '((-2 6) 2 1))
(joy-test nil '(1 2 <) '(t))
(joy-test nil '(2 1 <) '(nil))
(joy-test nil '(1 2 stack) '((2 1) 2 1))
(joy-test nil '((swap cons) (swons) define 0 nill (1 2 3) (swons) step) '((3 2 1) 0))
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
;; (joy-eval '(5) '((pred dup 0 < () ((1) dip a) branch) (a) define a))
;; define is special
(joy-test nil '(2 (dup +) (superman) define) '(2))
(joy-test nil '(2 (1) (superman) define superman) '(1 2))
(joy-test nil '((1) (a) define a (2) (a) define a 1) '(1 2 1))
(joy-test nil '(gensym quote dup (5) swap define i) '(5))
;; own defines. maybe write test-cases for letting them find
(joy-test nil '((0 equal) (null) define 0 null) '(t))
(joy-test nil '((0 equal) (null) define 1 null) '(nil))
(joy-test nil '((pred) (dec) define 5 dec) '(4))
(joy-test nil '((succ) (inc) define 5 inc) '(6))
(joy-test nil '((swap cons) (swons) define (2) 1 swons) '((1 2)))
;; development of while as a recursive program:
(joy-eval-2 '((pop) (pop pop stack (1) equal not) 7 6 5 4 3 2 1)
	    '(((stack) dip dup (si uncons pop) dip) dip) ;apply the while-test.
	    nil nil)
(joy-eval-2 '((pop) (pop pop stack (1) equal not) 7 6 5 4 3 2 1)
	    '(((stack) dip dup (si uncons pop) dip) dip
	      dup (swap ((dup) dip swap) dip swap) dip swap) ;duplicate boolean and while-program and place them.
	    nil nil)
(joy-eval-2 '((pop) (pop pop stack (1) equal not) 7 6 5 4 3 2 1)
	    '(((stack) dip dup (si uncons pop) dip) dip
	      dup (swap ((dup) dip swap) dip swap) dip swap
	      (((nill branch) dip) dip) dip ;apply while-program.
	      (nill) nill branch) ;prepare test for recursion and recursion.
	    nil nil)
(joy-eval-2 '((pop) (pop pop stack (1) equal not) 7 6 5 4 3 2 1)
	    '((((stack) dip dup (si uncons pop) dip) dip ;finished version
	       dup (swap ((dup) dip swap) dip swap) dip swap
	       (((nill branch) dip) dip) dip
	       (while) (pop pop) branch)
	      (while) define while)
	    nil nil)

;; compare (count-tree-nodes EXP) for the following EXPs:
;;34: (joy-eval '() '((pred dup 0 < (dup 0 equal) dip or (pop) (dup (*) dip fak-1) branch) (fak-1) define
;;		(succ 1 swap fak-1) (fak) define 4 fak))
;; compare this to:
;;25: (labels ((fak (x)
;;	   (if (>= x 1)
;;	       (* x (fak (1- x)))
;;	       1)))
;;  (fak 5))
;; or
;;22: (do ((x 5 (1- x)) (r 1 (* r x))) ((< x 1) r))
;; compare this to:
;;21: (fak (1 = 1)
;;     (s.1 = < * s.1 < fak < - s.1 1 > > >))

(defun joy-eval-handler (stk exp &key (heap (make-hash-table)) (c (make-counter)) (cd (make-countdown)))
  (handler-case (joy-eval stk exp :heap heap :c c :cd cd)
    (joy-counter-error () 'counter-error)
    (joy-countdown-error () 'countdown-error)
    (unknown-op-error () 'unknown-op-error)
    #+CMU (simple-error () 'error)
    #+CMU (arithmetic-error () 'error)
    (simple-type-error () 'error)
    (type-error () 'error)
    (division-by-zero () 'error)
    (floating-point-invalid-operation () 'error)
    (floating-point-overflow () 'error)
    #+SBCL (SB-KERNEL::ARG-COUNT-ERROR () 'error)))

(defparameter *joy-ops* 
  '(+ and branch concat cons dip / dup equal gensym i ifte list * nill not or patmat patsub pop pred quote rem si sample < stack step - succ swap times true uncons unstack while define))

(defstruct joy-program
  (program))

(defstruct refal-program
  (program))

(define-constant +mut0-max+ 0.8)

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
    (assert (< p0 +mut0-max+))
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
    (rec (cdr probs) (cons (random +mut0-max+) nil)))) ; recursion prob < +mut0-max+

(defun mutate-crossover (probs1 probs2)
  (labels ((rec (probs1 probs2 acc)
	     (if (null probs1)
		 (progn
		   (assert (null probs2))
		   (nreverse acc))
		 (rec (cdr probs1) (cdr probs2)
		      ;;(cons (/ (+ (car probs1) (car probs2)) 2) acc)))))
		      (cons (if (chance .5) (car probs1) (car probs2)) acc)))))
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

(defmethod crossover-and-mutate (joy-ops (prg1 joy-program) (prg2 joy-program) &rest probs)
  (let* ((p1 (elt probs 1))
	 (p2 (elt probs 2))
	 (prest (cons (car probs) (cdddr probs)))
	 (exp1 (joy-program-program prg1))
	 (exp2 (joy-program-program prg2))
	 (cross (crossover exp1 exp2 p1 p2))
	 (new (apply #'mutate-rec joy-ops cross prest)))
    (make-joy-program :program new)))

(defmethod crossover-and-mutate (joy-ops (prg1 refal-program) (prg2 refal-program) &rest probs)
  (let* ((p1 (elt probs 1))
	 (p2 (elt probs 2))
	 (prest (cons (car probs) (cdddr probs)))
	 (exp1 (refal-program-program prg1))
	 (exp2 (refal-program-program prg2))
	 (cross (crossover exp1 exp2 p1 p2))
	 (new (apply #'mutate-rec joy-ops cross prest)))
    (make-refal-program :program new)))

(define-constant +mut-length-const+ (+ 6 3))

(defun generate-mut (joy-ops)
  (append
   (list (random +mut0-max+))
   (loop for i below (1- +mut-length-const+) collect (random 1.0))
   (loop for i below (length joy-ops) collect (random 1.0))))

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
    (gensym  () (symbol))
    (i       (list) (:any))
    (ifte    (list list list) (:any))
    (list    (t) (boolean))
    (*       (number number) (number))
    (nill    () (list))
    (not     (t) (boolean))
    (or      (t t) (boolean))
    (patmat  (list list) (list))
    (patsub  (list list) (list))
    (pop     (t) nil)
    (pred    (number) (number))
    (quote   (t) (list))
    (rem     (number number) (number))
    (sample  (list) (t))
    (si      (list list) (list))
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
    (define  (list list) nil)))

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
	(symbol nil)
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
(assert (eq nil (valid-joy-exp '(gensym 2 +))))
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
(assert (eq t (valid-joy-exp '((1) (a) define a (2) (a) define a 1)))) ;test define
(assert (eq t (valid-joy-exp '((1) ((2)) define))))
(assert (eq nil (valid-joy-exp '(1 ((2)) define))))
(assert (eq nil (valid-joy-exp '((1) (a nill +) define)))) ;should be valid, but isn't (define looks only at a in 2nd list)

;; I'll have another go at a type predictor.
;; This one uses a function which receives the current stk and exp (and heap?) and computes the types on the stack.
;; It shall use the same type notation as Common Lisp, plus some additions for describing regular stacks, like (REPEAT NUMBER BOOLEAN), which describes a sequence of alternating numbers and booleans, or (REPEAT (INTEGER -1 1) LIST 0), an instance of which would be (0 (a b c) 0 -1 (a) 0).
;; It also shall support describing lists, like (LIST SYMBOL (NUMBER 0 5)), an instance of which would be (a 2).
;; Supporting REPEATs and LISTs becomes problematic when instructions like UNCONS or POP can happen, because it is unclear how to describe a REPEAT or LIST of which an unknown number of elements have been removed.

;; A different approach is to try to evolve a joy program that returns whether an input joy program has type errors in it.


;; examples of equivalent joy expressions:
;; (joy-eval nil '(1024 nill (succ) dip (swap 1 < not) ((pred dup 64 rem 0 equal) dip swap ((dup) dip cons) () branch) while))
;; (joy-eval nil '(1024 nill (64 +) dip (swap 1 < not) ((64 - dup) dip cons) while))

;;;; test-cases

;; TODO: generalize test-cases to other machines besides joy programs. This can be done by (defclass io-machine), which has methods (defgeneric eval). Then derive the joy-machine from IO-MACHINE.

(defun generate-exam (test-cases)
  "Generate test-goal-values for the test-cases TEST-CASES."
  (let* ((v (test-cases-values test-cases))
	 (generate-fn (test-cases-generate test-cases))
	 (goal-fn (test-cases-goal test-cases))
	 (test-values (funcall generate-fn v)))
    (loop for test in test-values collect
	 (let* ((goal (funcall goal-fn test)))
	   (cons test goal)))))

(defmethod score-exam-helper (score-fn test-goal-values (prg joy-program) max-ticks max-seconds)
  (let ((exp (joy-program-program prg)))
    (loop for (test . goal) in test-goal-values
       sum
	 (let* ((r (joy-eval-handler nil (append test exp) :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
		(fit (funcall score-fn r goal exp)))
	   fit))))

(defmethod score-exam-helper (score-fn test-goal-values (prg refal-program) max-ticks max-seconds)
  (let ((prg (refal-program-program prg)))
    (format *debug-stream* "~A~%" max-ticks)
    (loop for (test . goal) in test-goal-values
       sum
	 (let* ((r (refal-eval prg test :c (make-counter max-ticks)))
		(fit (funcall score-fn r goal prg)))
	   ;;(print (list prg test goal r fit))
	   fit))))

(defun score-exam (test-cases test-goal-values exp max-ticks max-seconds)
  "Return the score points for the test-goal-values generated by GENERATE-EXAM, for joy expression EXP evalated for maximal MAX-TICKS ticks and maximal MAX-SECONDS seconds."
  (let ((score-fn (test-cases-score test-cases)))
    (score-exam-helper score-fn test-goal-values exp max-ticks max-seconds)))

(defun score-exam-in-01 (test-cases test-goal-values exp max-ticks max-seconds)
  "Return the score points in [0,1] for the test-goal-values generated by GENERATE-EXAM, for joy expression EXP evalated for maximal MAX-TICKS ticks and maximal MAX-SECONDS seconds."
  (let ((score-fn (test-cases-score01 test-cases)))
    (score-exam-helper score-fn test-goal-values exp max-ticks max-seconds)))

(defun joy-show-test-cases-score (o test-cases)
  (flet ((eval-test (v)
	   (let* ((prg (make-joy-program :program (append v o)))
		  (res (joy-eval-handler nil prg))
		  (goal (funcall (test-cases-goal test-cases) v))
		  (score (funcall (test-cases-score test-cases) res goal o))) ;pass o as list, not joy-program, so that scoring functions still work.
	     (list v goal score res))))
    (let* ((fitsum (score-exam test-cases
			       (mapcar (lambda (x) (cons x (funcall (test-cases-goal test-cases) x)))
				       (test-cases-values test-cases))
			       (make-joy-program :program o)
			       0 0.0)))
      (mapcar (lambda (p) (destructuring-bind (v goal score res) p
			    (format t "v(exp):~A~%goal:~A~%score:~A~%res:~A~%" v goal score res) score))
	      (mapcar (lambda (v) (eval-test v)) (test-cases-values test-cases)))
      (format t "fitsum:~A~%" fitsum)
      fitsum)))

(defun absdiff (a b)
  (abs (- a b)))

(defun generate-randomized-tests (values)
  "Generate (LENGTH VALUES) test cases.
The test-cases' test values must be ordered increasingly."
  (cons (let ((a (car (elt values 0)))
	      (b (car (elt values (1- (length values))))))
	  (list (+ a (random (- b a)))))
	(loop for i below (1- (length values))
	   collect (let* ((a (car (elt values i)))
			  (b (car (elt values (mod (1+ i) (length values))))))
		     (list (+ a (random (- b a))))))))

(define-constant +test-cases-invalid+ -1000000) ;score for invalid joy-expressions or invalid results

(defun score-one-value (r goal exp)
  "calculates the score of the joy-eval-result r against the goal.
r should be a list of one value, otherwise +test-cases-invalid+ is returned."
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      +test-cases-invalid+
      (- (absdiff goal (car r)))))

(defun score01-one-value (r goal exp)
  "Calculates the score in [0,1] of the joy-eval-result r against the goal.
r should be a list of one value, otherwise +test-cases-invalid+ is returned."
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      0
      (/ 1 (1+ (absdiff goal (car r))))))

(defstruct test-cases
  (values nil :type list :read-only t) ;the values that are prepended to a joy-expression.
  (generate (lambda (&rest r) (declare (ignore r)) (error "generate undefined")) :type function :read-only t)
  (goal (lambda (&rest r) (declare (ignore r)) (error "goal undefined")) :type function :read-only t)
  (score (lambda (&rest r) (declare (ignore r)) (error "score undefined")) :type function :read-only t)
  (score01 (lambda (&rest r) (declare (ignore r)) (error "score01 undefined")) :type function :read-only t))

(defparameter *test-cases-sqrt*
  (make-test-cases :values '((1.0) (25.0) (100.0) (225.0) (400.0) (625.0) (1000.0))
		   :generate #'generate-randomized-tests
		   :goal (lambda (x) (sqrt (car x)))
		   :score #'score-one-value
		   :score01 #'score01-one-value))
;; TODO: write a joy program that computes the sqrt. Therefore, write test-cases that provide solutions for required ops, like the joy operation 'rotate.
;;(time (systematicmapping 4 '() *test-cases-sqrt* '(+ dip / dup < - while) 1000 .01))
;; (joy-show-exam-score '(5
;; 		       (dup) dip dup (/) dip + 2 /
;; 		       (dup) dip dup (/) dip + 2 /
;; 		       (dup) dip dup (/) dip + 2 /
;; 		       (dup) dip dup (/) dip + 2 /
;; 		       (dup) dip dup (/) dip + 2 /
;; 		       )
;; 		     (make-exam-from-test-cases *test-cases-sqrt*))

(defun my-sqrt (x)
  (labels ((rec (r)
	     (let ((r2 (/ (+ r (/ x r)) 2)))
	       (if (< (absdiff r2 r) 0.0001)
		   r2
		   (rec r2)))))
    (rec (float x))))

(defparameter *test-cases-expt2*
  (make-test-cases :values '((1) (5) (10))
		   :generate #'generate-randomized-tests
		   :goal (lambda (x) (expt 2 (car x)))
		   :score #'score-one-value
		   :score01 #'score01-one-value))
(assert (eq 0 (joy-show-test-cases-score '(pred 2 swap (2 *) times) *test-cases-expt2*)))

(defun score-stacklength (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (proper-list-p r)))
      +test-cases-invalid+
      (- (absdiff (length r) goal))))

(defparameter *test-cases-stacklength*
  (make-test-cases :values '((1) (5) (10))
		   :generate #'generate-randomized-tests
		   :goal #'car
		   :score #'score-stacklength))
(assert (eq 0 (joy-show-test-cases-score '((0) times) *test-cases-stacklength*)))

(defparameter *test-cases-stackcount*
  (make-test-cases :values '(((7)) ((5 6 3 1 2)) ((4 9 1 0 2 3 5 6 5 1)))
		   :generate #'identity-1
		   :goal (lambda (x) (length (car x)))
		   :score #'score-one-value
		   :score01 #'score01-one-value))
(assert (eq 0 (joy-show-test-cases-score '(0 (swap) (succ (uncons swap pop) dip) while swap pop) *test-cases-stackcount*)))

(define-constant +letter-symbols+ '(a b c d e f g h i j k l m n o p q r s t u v w x y z) :test 'equal
		 :documentation "All 26 letters as symbols.")

(defun score-list-similarity (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (proper-list-p r)))
      +test-cases-invalid+
      (if (equal r goal)
	  0
	  (- (+ 10 (abs (- (length r) (length goal)))))))) ;replace this with the edit-distance between r and goal

(defparameter *test-cases-rotate*
  (make-test-cases :values '(((a b c) unstack) ((x y z d e f) unstack) (((1) (2 3) j k l) unstack))
		   :generate (lambda (v) (declare (ignore v)) (let ((l (random 10)) (a (gensym)) (b (gensym)) (c (gensym)))
								(list (list (nconc (list a b c) (loop for i below l collect (random 10))) 'unstack))))
		   :goal (lambda (v) (destructuring-bind (a b c &rest r) (car v) (nconc (list c b a) r)))
		   :score #'score-list-similarity))
(assert (eq 0 (joy-show-test-cases-score '((swap) dip swap (swap) dip) *test-cases-rotate*)))

(defun score-one-symbol-equal (r goal exp)
  (declare (ignorable exp))
  (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)))
      +test-cases-invalid+
      (if (equal (car r) goal) 0 -10)))

(defparameter *test-cases-at*
  (make-test-cases :values '(((a b c) 2) ((x y z d e f) 4) (((1) (2 3) j k l) 1))
		   :generate (lambda (v)
				     (loop for i below (length v) collect
					  (let ((l (1+ (random 10))))
					    (list (loop for i below l collect (sample +letter-symbols+))
						  (random l)))))
		   :goal (lambda (v) (elt (car v) (cadr v)))
		   :score #'score-one-symbol-equal))

(defparameter *test-cases-abs*
  (make-test-cases :values '((-8) (-3) (0) (7))
		   :generate #'generate-randomized-tests
		   :goal (lambda (v) (abs (car v)))
		   :score #'score-one-value
		   :score01 #'score01-one-value))
(assert (eq 0 (joy-show-test-cases-score '(dup 0 < (0 swap -) () branch) *test-cases-abs*)))
;; (systematicmapping 10 '() *test-cases-abs* '(0 < swap - ifte) 1000 .01 nil) ==
;;((0 SWAP (<) (SWAP) NIL IFTE -) (0 (SWAP) (<) SWAP NIL IFTE -)
;; (0 (<) NIL (SWAP) IFTE SWAP -) (0 (<) NIL (SWAP) SWAP IFTE -)
;; (0 (<) (SWAP) NIL IFTE -) (0 (<) (SWAP -) (-) IFTE)
;; (0 (SWAP <) NIL (SWAP) IFTE -) ((<) 0 SWAP (SWAP) NIL IFTE -)
;; ((0 <) (0 SWAP -) NIL IFTE))
;;0 37344379 (percent visited of total possible: 0.15903935)

(defparameter *test-cases-list-positive0*
  (make-test-cases :values '((4 2 0 -1 -2) (6 4 3 1 0 -3) (1 -4 -7 -9))
		   :generate (lambda (v) (loop for i below (length v) collect
					      (do ((a (1+ (random 10)) (- a (random 3))) (b (- -5 (random 5))) (l nil)) ((< a b) (nreverse l))
						(setf l (cons a l)))))
		   :goal (lambda (v) (let ((r (reverse v))) (nthcdr (position-if (lambda (x) (>= x 0)) r) r)))
		   :score #'score-list-similarity))
(assert (eq 0 (joy-show-test-cases-score '((0 <) (pop) while) *test-cases-list-positive0*)))

(defparameter *test-cases-identity4*
  (make-test-cases :values '((1 1.0) (1 1.5) (1 2.0) (1 2.5) (1 3.0) (1 3.5)) ;the 1 is a bias neuron
		   :generate (lambda (vs) (mapcar (lambda (x) (list 1 (car x))) (generate-randomized-tests (mapcar (lambda (x) (list (cadr x))) vs))))
		   :goal #'cadr
		   :score #'score-one-value
		   :score01 #'score01-one-value))

(defparameter *test-cases-identity1024*
  (make-test-cases :values '((1 0.0) (1 204.6) (1 409.2) (1 613.8) (1 818.4) (1 1023.0)) ;the 1 is a bias neuron
		   :generate (lambda (vs) (mapcar (lambda (x) (list 1 (car x))) (generate-randomized-tests (mapcar (lambda (x) (list (cadr x))) vs))))
		   :goal #'cadr
		   :score #'score-one-value
		   :score01 #'score01-one-value))

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
					     (progn (incf error-c) +test-cases-invalid+)
					     (progn (incf ok-c) 0))
					 (if (eq r 'error)
					     +test-cases-invalid+
					     0))))
     (lambda () (values goal-c error-c ok-c)))))

(defun golden-ratio (n &optional (initial 1))
  (if (= 0 n)
      initial
      (golden-ratio (1- n) (1+ (/ 1 initial)))))

(defun generate-test-cases-one-value (value)
  "Return a test-cases whose goal it is to obtain the value VALUE."
  (make-test-cases :values '(nil)
		   :generate (lambda (vs) vs)
		   :goal (lambda (x) (declare (ignore x)) value)
		   :score #'score-one-value
		   :score01 #'score01-one-value))

(defparameter *test-cases-golden-ratio-value* (generate-test-cases-one-value (float (golden-ratio 20))))
;;(systematicmapping 6 '() *test-cases-golden-ratio-value* (cons 1 *joy-ops*) 1000 .01 nil)
;; yields ((1 DUP SUCC / SUCC) (1 1 SUCC / SUCC)) -0.118034005 14430760
;; (tournament-new-joy '(1 dup succ / succ) 200 1000000 *test-cases-golden-ratio-value* *joy-ops* 1000 .01) yields a joy expression with many nested I's and -2.1576881e-5 as error,
;; but (tournament-new-joy '() 200 1000000 *test-cases-golden-ratio-value* *joy-ops* 1000 .01) yields '(2) as joy expression and consequently -0.381966 as error.

(defparameter *test-cases-pi-value* (generate-test-cases-one-value pi))

(defparameter *test-cases-golden-ratio-sequence*
  (make-test-cases :values '((0 1.0) (1 1.0) (2 1.0) (3 1.0) (6 1.0) (12 1.0))
		   :generate (lambda (vs) vs)
		   :goal (lambda (x) (golden-ratio (car x) (cadr x)))
		   :score #'score-one-value
		   :score01 #'score01-one-value))
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
					       +test-cases-invalid+
					       0))
					 (if (eq r 'error)
					     +test-cases-invalid+
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
				    ;; FIXME: sometimes, R is 'TIMEOUT due to GC or so, which could lead to a wrong score.
				    +test-cases-invalid+
				    (if goal
					(if (car r) 0 -1)
					(if (car r) -1 0))))))))

;(let ((tc (generate-test-cases-check-valid-joy-exps 2 '(0) *joy-ops* 1000 .01))
;      (i '(pop t))
;      (i '(dup (+) equal (pop nill) (pop t) branch))
;      (i '(dup (+) equal (pop nill)      (dup (-)    equal (pop nill) (pop t) branch) branch))
;      (i '(DUP (+) EQUAL (POP NILL SWAP) (DUP (CONS) EQUAL (POP NILL) (DUP (-) EQUAL (UNCONS) NILL BRANCH) BRANCH) BRANCH))
;      )
;(tournament-new-joy i 20 10000 tc *joy-ops* 1000 .01)
;;;(joy-show-test-cases-score i tc)
;)

(defparameter *test-cases-joy-eval-and-joy-eval-2-equivalence*
  (make-test-cases :values '(())
		   :generate (lambda (vs) vs)
		   :goal (constantly nil)
		   :score (lambda (r goal exp)
			    (declare (ignore goal))
			    (let* ((joy-while '(((stack) dip dup (si uncons pop) dip) dip
						dup (swap ((dup) dip swap) dip swap) dip swap
						(((nill branch) dip) dip) dip
						(while) (pop pop) branch))
				   (heap (make-hash-table)))
			      (setf (gethash 'while heap) joy-while)
			      (let ((r2 (handler-case (joy-eval-2 nil exp nil nil :heap heap :c (make-counter 1000))
					  (joy-eval-2-counter-error () 'counter-error)
					  (joy-eval-2-countdown-error () 'coundown-error)
					  (unknown-op-error () 'unknown-op-error)
					  #+CMU (simple-error () 'error)
					  #+CMU (arithmetic-error () 'error)
					  (simple-type-error () 'error)
					  (type-error () 'error)
					  (division-by-zero () 'error)
					  (floating-point-invalid-operation () 'error)
					  (floating-point-overflow () 'error)
					  #+SBCL (SB-KERNEL::ARG-COUNT-ERROR () 'error))))
				(assert (equal r r2) (r r2) "assertion failed. r:~A != r2:~A for exp:~A" r r2 exp)))
			    0)))
;;(tournament-new-joy '() 200 1000000 *test-cases-joy-eval-and-joy-eval-2-equivalence* (remove 'sample (remove 'gensym *joy-ops*)) 1000 0.0)
;; NOTE: joy-eval and joy-eval-2 with DEFINEd WHILE differ, because it can be redefined, but the built-in WHILE cannot.

;; Add a test-cases that scores joy programs by the value they return.
;; The length of the joy programs is also scored, programs of length larger than a predefined number receive a very low score.
;; Maybe this evolves joy programs that implement new creative higher-level (control) structures.

(defun generate-test-cases-maxvalue-maxprognodes (maxnodes)
  (let ((log2 (log 2)))
    (make-test-cases :values '((0))
		     :generate (lambda (vs) vs)
		     :goal (constantly nil)
		     :score (lambda (r goal exp)
			      (declare (ignore goal))
			      (if (or (not (listp r)) (not (numberp (car r))) (not (eq (cdr r) nil)) (> (count-tree-nodes exp) maxnodes))
				  +test-cases-invalid+
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

(defun score-tree-equal-prefix (l1 l2 &optional (score 0))
  "Example: (score-tree-equal-prefix '(1 2 (3 4) 5) '(1 2 (3) 5)) == -1."
  ;;(format t "l1:~A l2:~A score:~A~%" l1 l2 score)
  (if (null l1)
      (if (null l2)
	  score
	  (- score (* 1 (count-tree-nodes l2))))
      (if (null l2)
	  (- score (* 1 (count-tree-nodes l1)))
	  (if (or (not (consp l1)) (not (consp l2)))
	      (if (equal l1 l2)
		  score
		  (1- score))
	      (score-tree-equal-prefix (cdr l1) (cdr l2)
				       (score-tree-equal-prefix (car l1) (car l2) score))))))

(defparameter *test-cases-ifte*
  (make-test-cases :values '((true) (nill))
		   :generate (lambda (vs) vs)
		   :goal (lambda (v) (if (eq (car v) 'true) '(1 t) '(0 nil)))
		   :score (lambda (r goal exp)
			    (declare (ignore exp))
			    (if (or (not (listp r)) (not (proper-list-p r)))
				+test-cases-invalid+
				(score-tree-equal-prefix r goal)))))
;; (systematicmapping 7 '() *test-cases-ifte* *joy-ops* 1000 .01 nil) exhausts heap, because the best exps are collected.
(assert (eq 0 (joy-show-test-cases-score '(() (1) (0) ifte) *test-cases-ifte*)))

(defparameter *test-cases-branch*
  (make-test-cases :values '((true) (nill))
		   :generate (lambda (vs) vs)
		   :goal (lambda (v) (if (eq (car v) 'true) '(1) '(0)))
		   :score (lambda (r goal exp)
			    (declare (ignore exp))
			    (if (or (not (listp r)) (not (proper-list-p r)))
				+test-cases-invalid+
				(score-tree-equal-prefix r goal)))))
;; does (systematicmapping 6 '() *test-cases-branch* *joy-ops* 1000 .01 nil) exhaust heap?
(assert (eq 0 (joy-show-test-cases-score '((1) (0) branch) *test-cases-branch*)))

(defun generate-test-cases-stack (stack)
  "Return a test-cases whose goal it is to obtain the tree TREE."
  (make-test-cases :values '(nil)
		   :generate (lambda (vs) vs)
		   :goal (lambda (x) (declare (ignore x)) stack)
		   :score (lambda (r goal exp)
			    (declare (ignore exp))
			    (if (or (not (listp r)) (not (proper-list-p r)))
				+test-cases-invalid+
				(score-tree-equal-prefix r goal)))))

(defparameter *test-cases-10-stack* (generate-test-cases-stack (loop for i below 10 collect '+)))
(defparameter *test-cases-200-stack* (generate-test-cases-stack (loop for i below 200 collect '+)))

(defun generate-test-cases-stack-small (stack node-cost)
  "Return a test-cases whose goal it is to obtain the tree TREE and small programs are better."
  (make-test-cases :values '(nil)
		   :generate (lambda (vs) vs)
		   :goal (lambda (x) (declare (ignore x)) stack)
		   :score (lambda (r goal exp)
			    (if (or (not (listp r)) (not (proper-list-p r)))
				+test-cases-invalid+
				(- (score-tree-equal-prefix r goal) (* node-cost (count-tree-nodes exp)))))))

(defparameter *test-cases-10-stack-small* (generate-test-cases-stack-small (loop for i below 10 collect '+) .1))
(defparameter *test-cases-200-stack-small* (generate-test-cases-stack-small (loop for i below 200 collect '+) 1))

;; this test-cases should compress a list of 200 "+".
(defun generate-test-cases-compress-list (decoder-maxticks decoder-max-seconds)
  ;; generalize this: compute all possible different outputs from joy-programs of length up to N nodes, and let them be learned.
  (let ;;((lists1 (loop for symbol in '(+ - * /) collect (loop for i below 200 collect symbol))))
      ((lists1 (loop for symbol in '(1) collect (loop for i below 200 collect symbol))))
    (flet ((score-list-prefix (r goal exp)
	     (declare (ignorable exp))
	     (if (or (not (listp r)) (not (proper-list-p r)) (not (listp (car r))) (not (proper-list-p (car r))) (not (eq nil (cdr r))))
		 +test-cases-invalid+
		 (let ((decoded (joy-eval-handler nil (car r) :c (make-counter decoder-maxticks) :cd (make-countdown decoder-max-seconds))))
		   (if (or (not (listp decoded)) (not (proper-list-p decoded)) (not (eq (cdr decoded) nil)) (not (listp (car decoded))) (not (proper-list-p (car decoded))))
		       +test-cases-invalid+
		       (let ((diff (score-tree-equal-prefix (car decoded) (car goal) 0))
			     (code-length (count-tree-nodes r)))
			 ;;(print (list "r" r "goal" goal "exp" exp "decoded" decoded "diff" diff "code-length" code-length))
			 (- (* 2 diff) (* 1 code-length))))))))
      (make-test-cases :values (mapcar (lambda (x) (list x)) (append lists1))
		       :generate (lambda (vs) vs)
		       :goal (lambda (v) v)
		       :score #'score-list-prefix))))
;;(let ((tc (generate-test-cases-compress-list 1000 .01)))
;;  (joy-show-test-cases-score '(pop (nill 200 (1 swap cons) times)) tc))
;; the following was a problem, because some joy operations worked on the deeply nested list generated by (200 (cons dup) times):
;;(let ((tc (generate-test-cases-compress-list 1000 .01))
;;      (joy-ops (append '(1 200) (remove 'stack *joy-ops*))))
;;  (systematicmapping 2 '(200 (cons dup) times) tc joy-ops 1000 .01 30000))
;; it was solved by allowing count-tree-nodes to efficiently count self-similar trees, and estimating the run time of the operations.

(defun enumerate-permutations (l k)
  "Return all possible permutations of taking K items out of list L.
E.g. (enumerate-permutations '(1 2 3) 2) == '((1 2) (1 3) (2 1) (2 3) (3 1) (3 2))."
  (assert (<= 0 k (length l)))
  (let ((len (length l)))
    (labels ((copy-omit-nth (l n &optional (result nil))
	       "Copy list L, leaving out the N-th element. The returned list may share structure with L."
	       ;; If this function is made a separate DEFUN, then error-check if (null L) before (= n 0).
	       (if (= n 0)
		   (nconc (nreverse result) (cdr l))
		   (copy-omit-nth (cdr l) (1- n) (cons (car l) result))))
	     (rec (l l-len k)
	       (if (= 0 k)
		   (list nil) ;the list of the empty permutation.
		   (let ((result nil))
		     (loop for i below l-len do
			  (let* ((e (nth i l))
				 (l-1 (copy-omit-nth l i))
				 (l-1-perms (rec l-1 (1- l-len) (1- k))))
			    (loop for l-1-perm in l-1-perms do
				 (push (cons e l-1-perm) result))))
		     result))))
      (rec l len k))))
;; Using enumerate-permutations, write a test-cases that finds all programs that yield any permutation of the stack. Do this by checking the returned stack against a pre-computed hash-table of permutations.

(defun generate-test-cases-permute-stack (stack-length)
  (let* ((stk (loop for i below stack-length collect
		   (gensym (format nil "H-~A-" i))))
	 (stks-perm (apply #'nconc (loop for i upto stack-length collect
				       (enumerate-permutations stk i))))
	 (stks-perm-w/o-id (remove stk stks-perm :test #'equal)) ;TODO: Currently this doesn't handle superfluous computation for other permutations, e.g. (nil pop swap),(swap nil pop) etc. Rather than removing the identity, have an additional scoring variable representing the total time required.
	 (stk-tail (loop for i below 10 collect (gensym "T")))
	 (stks-hashtable (make-lsxhash-equal-hash-table)))
    ;; insert all permuted stacks to be recognized into stks-hashtable.
    (dolist (stk-head stks-perm-w/o-id)
      (let ((stk-whole (append stk-head stk-tail)))
	(setf (gethash stk-whole stks-hashtable) t)))
    (flet ((score-stk (r goal exp)
	     (declare (ignore exp goal))
	     (if (or (not (listp r)) (not (proper-list-p r)))
		 +test-cases-invalid+
		 (if (nth-value 1 (gethash r stks-hashtable))
		     1
		     0))))
      (make-test-cases :values (list (append (list (append stk stk-tail)) '(unstack)))
		       :generate (lambda (vs) vs)
		       :goal (lambda (v) v) ;;TODO: actually we have many goals (all permuted stacks), but can't specify them here.
		       :score #'score-stk))))
;;(let ((tc (generate-test-cases-permute-stack 3)))
;;  (joy-show-test-cases-score '(pop) tc))
;; (let ((tc (generate-test-cases-permute-stack 3))
;;       (joy-ops '(concat cons dip dup i list pop quote si stack swap uncons unstack)))
;;   (systematicmapping 4 '() tc joy-ops 1000 .01 nil))

(defun make-exam-from-test-cases (test-cases &optional values)
  "Return an exam.
An exam is a list of tasks.
Each task consists of a list consisting of the input for the task, and a scoring function accepting a result as parameter and returning the score for the task."
  ;; Idea how to include description into a task: The input of a task could be a list consisting of a description of the task (e.g. simply a constant denoting the task type), and the inputs of the task in a suitable format (e.g. a list).
  (when (null values)
    (setf values (test-cases-values test-cases)))
  (let ((exam nil)
	(goal-fn (test-cases-goal test-cases))
	(score-fn (test-cases-score test-cases)))
    (loop
       for value in values
       do
	 (let ((goal (funcall goal-fn value)))
	   (flet ((scoring-fn (result)
		    (funcall score-fn result goal (make-condition 'error "exp not available"))))
	     (let ((task (list value #'scoring-fn)))
	       (push task exam)))))
    (nreverse exam)))

(defun joy-show-exam-score (o exam)
  (loop for (inp scorer) in exam collect
       (let* ((stk (joy-eval-handler nil (append inp o)))
	      (score (funcall scorer stk)))
	 (format t "inp:~A stk:~A score:~A~%" inp stk score)
	 score)))

;;;; Keeping track of scores in multiple test-cases
;; TODO: Rewrite test-cases (and maybe systematicmapping, tournament, and competition) to support multiple goals. Then rewrite generate-test-cases-permute-stack to use it. (Although test-cases-permute-stack would be a rather uninteresting test-cases, since it only finds completely disjunct joy programs, i.e. there is no joy program which finds multiple permuted stacks at the same time.)
;; How to support multiple goals: An joy program is scored according to N test-cases, so there are N scores. We want to remember only those programs that are better than all other programs in at least one category. Another criterium would be to forget all those programs which are worse than another program in all categories. Are these two criteria equivalent?

(defun multiscore-add-new (new-scores scores-list &key key)
  "Return the new dominating scores list when adding the scores NEW-SCORES to the old dominating SCORES-LIST.
Dominating is defined as being better in at least one category.
Every scores in SCORES-LIST must be dominating all other scores in SCORES-LIST.
This function tries to optimize future calls to itself by not reordering SCORES-LIST except in the following situation: when a scores in SCORES-LIST dominates NEW-SCORES, the scores is put at the beginning of the returned scores list. (The reasoning is that the score will probably dominate other NEW-SCORES in future calls to this function.)
When KEY is not NIL, it is used to access the scores of SCORES-LIST and NEW-SCORES, otherwise they are used directly."
  (flet ((better-p (scores-1 scores-2)
	   "Return T if SCORES-1 is better than SCORES-2 in at least one category, NIL otherwise."
	   (let ((equal t))
	     (loop for s1 in scores-1 for s2 in scores-2 do
		  (if (< s1 s2)
		      (return-from better-p nil)
		      (if (> s1 s2)
			  (setf equal nil))))
	     (not equal))))
    ;; TODO: specialize on key==NIL.
    (setf key (if (null key) (lambda (x) x) key))
    (let ((ret nil)
	  (new-scores-obj new-scores)
	  (new-scores (funcall key new-scores)))
      (loop for scores in scores-list for cdr-scores-list on scores-list do
	   (let ((scores-obj scores)
		 (scores (funcall key scores)))
	     (if (better-p scores new-scores)
		 (return-from multiscore-add-new
		   ;; put scores-obj in front in the hope that it will dominiate other new-scores in the future.
		   ;; if new-scores is dominated, we don't need to return it, even if it was better than others in some category.
		   (cons scores-obj (nconc (nreverse ret) (cdr cdr-scores-list))))
		 (if (better-p new-scores scores)
		     nil
		     (push scores-obj ret)))))
      (cons new-scores-obj (nreverse ret)))))

(assert (equal (multiscore-add-new '(9 5) '((9 4) (7 9))) '((9 5) (7 9))))
(assert (equal (multiscore-add-new '(9 3) '((9 4) (7 9))) '((9 4) (7 9))))
(assert (equal (multiscore-add-new '(9 4) '((9 4) (7 9))) '((9 4) (9 4) (7 9))))

;; (let ((scores (loop for i below 10 collect (list (random 10) (random 10))))
;;       (dominators nil))
;;   (loop for s in scores do
;;        (setf dominators (multiscore-add-new s dominators))
;;        (print (list "s" s "dominators" dominators))))

;;;; tournament selection

(defun tournament-new-joy (o size cycles test-cases joy-ops max-ticks max-seconds)
  (tournament-new (make-joy-program :program o) size cycles test-cases joy-ops max-ticks max-seconds))

(defun tournament-new-refal (o size cycles test-cases joy-ops max-ticks max-seconds)
  (tournament-new (make-refal-program :program o) size cycles test-cases joy-ops max-ticks max-seconds))

(defun tournament-new (o size cycles test-cases joy-ops max-ticks max-seconds)
  (let* ((pop (make-array size :initial-element o))
	 (mut (make-array size)))
    (dotimes (s size) (setf (aref mut s) (generate-mut joy-ops)))
    (tournament pop mut cycles test-cases joy-ops max-ticks max-seconds)))

(defun tournament-res (res cycles test-cases joy-ops max-ticks max-seconds)
  (let* ((size (length res))
	 (pop (make-array size :initial-contents (loop for i in res collect (car i))))
	 (mut (make-array size :initial-contents (loop for i in res collect (caddr i)))))
    (tournament pop mut cycles test-cases joy-ops max-ticks max-seconds)))

(defmethod valid-program ((program joy-program))
  (let ((exp (joy-program-program program)))
    (and (listp exp) (valid-joy-exp exp '(:any)))))

(defmethod valid-program ((program refal-program))
  ;; parsing is done by refal-eval, so just return T here.
  ;; (let ((exp (refal-program-program program)))
  ;;   (not (null (parse-program* exp)))))
  t)

(defun tournament (pop mut cycles test-cases joy-ops max-ticks max-seconds)
  (assert (= (length pop) (length mut)))
  (let* ((size (length pop))
	 (n (make-array size :element-type 'fixnum :initial-contents (loop for i below size collect i)))
	 (fit (make-array size :initial-element 0))
	 (logstream (open "/tmp/log.txt" :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)))
    (let ((test-goal-values (generate-exam test-cases)))
      (dotimes (s size) (setf (aref fit s) (score-exam test-cases test-goal-values (aref pop s) max-ticks max-seconds))))
    (dotimes (c cycles)
      (let* ((test-goal-values (generate-exam test-cases)) ;test with the same test-goal value pairs
	     (c1 (sample n))
	     (c2 (sample n))
	     ;;(c2-fit (elt fit c2))
	     (c2-fit (score-exam test-cases test-goal-values (elt pop c2) max-ticks max-seconds)) ;; re-scoring is fairer for randomized goals
	     (c1-mut (elt mut c1))
	     (c2-mut (elt mut c2))
	     (new-mut (mutate-mutate .1 (mutate-crossover c1-mut c2-mut)))
;;	     (new-mut c2-mut)
	     (new (apply #'crossover-and-mutate joy-ops (elt pop c1) (elt pop c2)
			 new-mut)))
	(when (valid-program new)
	  ;;(print (list "new" new))
	  (let ((new-fit (score-exam test-cases test-goal-values new max-ticks max-seconds)))
	    ;;(print (list "c1" c1 "c2" c2 "c2-fit" c2-fit "new-fit" new-fit))
	    (when (or (> new-fit c2-fit) (and (= new-fit c2-fit) (chance .5)))
	      ;; sometimes, c2-fit can become very low, because of a countdown timeout in joy-eval due to garbage collection or so.
	      ;; then, c2 is replaced by a new-mut with potentially lower score.
	      ;; therefore, make the population size big enough to guard against losing good genomes.
	      (setf (elt pop c2) new)
	      (setf (elt fit c2) new-fit)
	      (setf (elt mut c2) new-mut)
	      ;;(when (> new-fit c2-fit)
		;;(print (list "new" new "new-fit" new-fit "new-mut" new-mut "c2-fit" c2-fit)))
	      )))
	(when (= 0 (mod c 1000))
	  (print (list "c" c "c2-fit" c2-fit)))
	(when (= 0 (mod c 10000))
	  (let ((res (sort (zip-array 'list pop fit mut) #'< :key #'second)))
	    (log-mut-stats res logstream)
	    (print (list "best-pop" (car (car (last res))) "best-fit" (cadr (car (last res)))))))))
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
	 (fit-max (apply #'max fits))
	 (fit-mean (mean fits))
	 (fit-stddev (stddev-corr fits))
	 (ms (mut-stats res)))
    (format logstream "~A ~A ~A" (float fit-max) (float fit-mean) fit-stddev)
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

;; systematic program score measurement

;; in alexandria
;; (defun copy-hash-table (ht)
;;   "returns a copy of ht. (keys and) values are not copied."
;;   (let ((copy (make-hash-table :test (hash-table-test ht)
;; 			       :size (hash-table-size ht)
;; 			       :rehash-size (hash-table-rehash-size ht)
;; 			       :rehash-threshold
;; 			       (hash-table-rehash-threshold ht))))
;;     (with-hash-table-iterator (next ht)
;;       (labels ((rec ()
;; 		 (multiple-value-bind (present-p key value) (next)
;; 		   (unless present-p (return-from rec))
;; 		   (setf (gethash key copy) value))))
;; 	(rec)))
;;     copy))

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

(defun extend-and-evaluate (l-exp l-1-stk l-1-heap ins goal-value test-cases max-ticks max-seconds)
  "appends ins to l-1-exp and calculates the score for each possibility.
l-1-stk and l-1-heap must be the stack and heap returned when executing l-1-exp."
  (declare (ignorable l-1-heap) (type (function (list number list) number) test-cases))
  (let* ((lins (list ins))
	 (heap (if (null l-1-heap) nil (copy-hash-table l-1-heap))) ; this takes a lot of time
	 ;;(heap l-1-heap)
	 (res (joy-eval-handler l-1-stk lins :heap heap :c (make-counter max-ticks) :cd (make-countdown max-seconds)))
	 (score (funcall test-cases res goal-value l-exp)))
    ;;(when (equal l-exp '((swap) dip swap (swap) dip))
    ;;  (format t "eae. l-1-stk:~W ins:~W~%" l-1-stk ins)
    ;;  (format t "res:~W fit:~W~%" res fit)
    ;;  (format t "exp:~W~%" l-exp)
    ;;  (format t "res:~W~%" res))
    (values res heap score)))

(defun evaluate-tests (ins l-exp l-1-stks l-1-heaps l-1-scores goal-values score-fn max-ticks max-seconds)
  "Run ins on the joy-machines specified by l-1-stks and l-1-heaps and calculate the new test-cases scores, and whether an error was returned.
l-1-scores must be a list of test-cases scores which must be nil if the previous calls yielded no error."
  (declare (ignorable l-exp))
  (let (l-stks l-heaps l-fits (fit-sum 0) 
	       (valid-exp (valid-joy-exp l-exp '(:any))) ;this approximately halves search time
	       ;(valid-exp t)
	       (pursue nil))
    (loop
       for l-1-stk in l-1-stks
       for l-1-heap in l-1-heaps
       for l-1-fit in l-1-scores
       for goal-value in goal-values
       do
	 (if (or (not (null l-1-fit)) (not valid-exp)) ;error in level above or invalid expression
	     (progn
	       (setf l-stks (cons l-1-stk l-stks))
	       (setf l-heaps (cons l-1-heap l-heaps))
	       (setf l-fits (cons l-1-fit l-fits))
	       ;;(setf fit-sum (+ fit-sum l-1-fit))) ;if the shorter expression failed, so will this longer one
	       (setf fit-sum (+ fit-sum +test-cases-invalid+)))
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

(defun extend-exp-and-test (max-ext-nodes test-cases l-1-exp l-1-stks l-1-heaps l-1-scores goal-values collectfit joy-ops max-ticks max-seconds)
  "Extend l-1-exp with an arbitrary tree of at most max-ext-nodes nodes.
Test with all test-cases and extend those expressions whose result had no error.
l-1-scores must be a list of test-cases scores which must be nil if the previous level yielded no error."
  (when (> max-ext-nodes 1)
    (let* ((ext-structs (unique (mapcar #'car (enumerate-tree-structures max-ext-nodes))
				#'equal)) ;extension tree structures
	   (score-fn (test-cases-score test-cases)))
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
			    (evaluate-tests ins l-exp l-1-stks l-1-heaps l-1-scores goal-values score-fn max-ticks max-seconds)
			  ;;(format t "  l-exp:~A fit-sum:~A      #sym:~A #nodes:~A~%" l-exp fit-sum (count-symbols-in-tree l-exp) (count-tree-nodes l-exp))
			  (when (and pursue (funcall collectfit l-stks l-heaps l-exp fit-sum))
			    (extend-exp-and-test (- max-ext-nodes ext-nodes) test-cases l-exp l-stks l-heaps l-fits goal-values collectfit joy-ops max-ticks max-seconds))))))
	       (let* ((ins-sets (loop for i below ext-symbols collect joy-ops)))
		 (enumerate-set-combinations ins-sets #'f1)))
	     (when (> max-ext-nodes 3)
	       (let ((elapsed-seconds (float (/ (- (get-internal-real-time) enum-fill-start-time) internal-time-units-per-second))))
		 (format t "max-ext-nodes:~A l-1-exp:~A ext-struct:~A ext-nodes:~A ext-symbols:~A elapsed-seconds:~A~%" max-ext-nodes l-1-exp ext-struct ext-nodes ext-symbols elapsed-seconds))))))))

(defun systematicmapping (maxlevel exp0 test-cases joy-ops max-ticks max-seconds cache)
  (declare (type (or null (integer 1 #.most-positive-fixnum)) cache))
  (let* ((test-values (test-cases-values test-cases))
	 (goal-values (mapcar (test-cases-goal test-cases) test-values))
	 (l0-heaps (loop for i below (length test-values) collect (make-hash-table :size 0)))
	 ;;(l0-heaps (loop for i below (length test-values) collect nil))
	 (l0-exps (mapcar (lambda (v) (append v exp0)) test-values))
	 (l0-stks (mapcar (lambda (e h) (joy-eval-handler nil e :heap h)) l0-exps l0-heaps))
	 (fitn (mapcar (test-cases-score test-cases) l0-stks goal-values l0-exps))
	 (l0-fits (mapcar (lambda (s f) (if (not (eq s 'error)) nil f)) l0-stks fitn))
	 (best-fit (apply #'+ fitn))
	 (best-exp (list nil))
	 (number-trees (count-labelled-trees maxlevel (length joy-ops)))
	 (results-seen-cache (when cache (make-mru-cache (min number-trees cache))))
	 (trees-evaluated 0))
    (flet ((collectfit (stks heaps exp fit)
	     (incf trees-evaluated)
	     (when (= 0 (mod trees-evaluated 10000))
	       (format t "trees:~A/~A exp:~A best-fit:~A #best-exp:~A~%" trees-evaluated number-trees exp best-fit (length best-exp)))
	     (with-open-file (stream "/tmp/systematicmapping-last.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create)
	       (print exp stream))
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
      (extend-exp-and-test maxlevel test-cases nil l0-stks l0-heaps l0-fits goal-values #'collectfit joy-ops max-ticks max-seconds)
      (values best-exp best-fit trees-evaluated))))

;;(time (systematicmapping 4 '() *test-cases-sqrt* (append '(1 A) *joy-ops*) 1000 .01 nil))

;;(require :sb-sprof)
;;(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil)
;;  (systematicmapping 4 '() *test-cases-sqrt* (cons '(1 A) *joy-ops*) 1000 .01))
;;(sb-sprof:with-profiling (:max-samples 1000 :mode :alloc :report :flat)
;;  (systematicmapping 4 '() *test-cases-sqrt* (cons '(1 A) *joy-ops*) 1000 .01))

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

(defun joy-eval-trace (stk exp &key (heap (make-hash-table)) (c (make-counter)) (cd (make-countdown)) (fun nil))
  "Evaluate the joy expression EXP on joy stack STK and joy heap HEAP, and call function FUN with the current tick, STK, EXP, and HEAP before each tick.
By default, FUN prints tick, stk and exp.
Signal the same errors that JOY-EVAL would."
  (when (null fun)
    (setf fun (lambda (count stk exp heap)
		(declare (ignore heap))
		(format t "count:~A stk:~A exp:~A~%" count stk exp))))
  (let ((count 0))
    (funcall fun count stk exp heap)
    (multiple-value-bind (counter counter-delta) (make-counter 2) ;not 1 for some reason.
      (handler-bind
	  ((joy-counter-error (lambda (condition)
				(let ((c-value (funcall c)))
				  (when (> c-value 0)
				    (incf count)
				    (funcall fun count (joy-error-stk condition) (joy-error-exp condition) (joy-error-heap condition))
				    (funcall counter-delta 1)
				    (invoke-restart 'continue))))))
	(joy-eval stk exp :heap heap :c counter :cd cd)))))

;;;; Competition: In a competition, genomes can replicate actively, submit solutions, and communicate.

(defun make-set ()
  "Return a new vector, useable as a set."
  (make-array 0 :adjustable t :fill-pointer 0))

(defun set-delete (vector i)
  "Delete element I by moving the last item to position I and decreasing the fill-pointer by 1."
  (let* ((new-length (1- (length vector))))
    (setf (elt vector i) (elt vector new-length))
    (setf (fill-pointer vector) new-length)))

(defun set-add (vector e)
  "Add element E to end of vector VECTOR, adjusting it if neccessary, and return the new vector."
  (let ((fp (fill-pointer vector))
	(dim (array-dimension vector 0)))
    (when (>= fp dim)
      (setf vector (adjust-array vector (1+ dim))))
    (setf (fill-pointer vector) (1+ fp))
    (setf (elt vector fp) e)
    vector))

(defclass competition (standard-object)
  ((cycle :initform 0 :type (and unsigned-byte integer) :initarg :cycle :accessor competition-cycle)
   (test-cases :initform nil :type test-cases :initarg :test-cases :reader competition-test-cases)
   (pop :initform (make-set) :type vector :initarg :pop :accessor competition-pop) ;the genomes.
   (state :initform (make-set) :type vector :initarg :state :accessor competition-state) ;the state genomes are in currently.
   (claim :initform (make-set) :type vector :initarg :claim :accessor competition-claim) ;the claims handed in by the genomes.
   (fit :initform (make-set) :type vector :initarg :fit :accessor competition-fit)
   (bday :initform (make-set) :type vector :initarg :bday :accessor competition-bday)
   (par :initform nil :initarg :par :accessor competition-par)
   (n-born :initform 0 :type (and unsigned-byte fixnum) :initarg :n-born :accessor competition-n-born)
   (n-died :initform 0 :type (and unsigned-byte fixnum) :initarg :n-died :accessor competition-n-died)))

(defparameter *competition-initial-claim* nil)

(defparameter *competition-initial-fit* 0)

(defgeneric make-offspring (competition i1 i2)
  (:documentation "In competition COMPETITION, use the genomes with indices I1 and I2 to make one offspring."))

(defgeneric execute (competition i test-goal-values)
  (:documentation "In competition COMPETITION, continue executing the I-th entry in (competition-state COMPETITION)."))

(defgeneric initial-state (genome)
  (:documentation "Return the initial state of GENOME."))

(defgeneric competition-new-cycle (competition cyclei)
  (:documentation "Inform the competition that the cycle with number CYCLEI has started."))

(defmethod competition-new-cycle ((comp competition) cyclei)
  (with-slots (cycle n-born n-died) comp
    (setf cycle cyclei)
    (setf n-born 0)
    (setf n-died 0)))

(defgeneric competition-organism-die (competition i)
  (:documentation "Mark the organism with id I in the competition COMPETITION as dead so that it will be removed when COMPETITION-REMOVE-DEAD is called."))

(defmethod competition-organism-die ((comp competition) i)
  (with-slots (fit n-died) comp
    (incf n-died)
    (setf (elt fit i) :dead)))

(defgeneric competition-new-organism (competition new-pop)
  (:documentation "Add a new organism with the given properties to COMPETITION and return its identifier."))

(defmethod competition-new-organism ((comp competition) new-pop)
  (with-slots (cycle pop state claim fit bday n-born) comp
    (incf n-born)
    (set-add pop new-pop)
    (set-add state (initial-state new-pop))
    (set-add claim *competition-initial-claim*)
    (set-add fit *competition-initial-fit*)
    (set-add bday cycle)
    (1- (length pop))))

(defgeneric competition-remove-dead (competition)
  (:documentation "Remove dead organisms. Will reorder the indices."))

(defmethod competition-remove-dead ((comp competition))
  (with-slots (pop state claim fit bday) comp
    (loop for i from (1- (length pop)) downto 0 do
	 (when (eq (elt fit i) :dead)
	   (set-delete pop i)
	   (set-delete state i)
	   (set-delete claim i)
	   (set-delete fit i)
	   (set-delete bday i)))))

(defgeneric competition-add-score (competition score-factor)
  (:documentation "Increase the fitnesses of the organisms in the competition."))

(defstruct joy-state
  (stk nil :type list)
  (exp nil :type list)
  (stks nil :type list)
  (exps nil :type list)
  (heap nil :type hash-table)
  (msgs (dlist) :type t))

(defstruct joy-default-pop
  (genome nil :type list)
  (mut nil :type list))

(defstruct joy-default-par
  (joy-ops nil :type list)
  (schedule-ticks 0 :type (and fixnum unsigned-byte))
  (schedule-seconds 0.0 :type float)
  (score-ticks 0 :type (and fixnum unsigned-byte))
  (score-seconds 0 :type float))

(defclass competition-joy-default (competition)
  ;; TODO: override the slots of COMPETITION with slots of correct type.
  ())

(defmethod make-offspring ((comp competition-joy-default) i1 i2)
  (with-slots (pop par) comp
    (let* ((joy-ops (joy-default-par-joy-ops par))
	   (pop1 (elt pop i1))
	   (pop2 (elt pop i2))
	   (mut1 (joy-default-pop-mut pop1))
	   (mut2 (joy-default-pop-mut pop2))
	   (new-mut (mutate-mutate .1 (mutate-crossover mut1 mut2)))
	   (genome1 (joy-default-pop-genome pop1))
	   (genome2 (joy-default-pop-genome pop2))
	   (new (apply #'crossover-and-mutate joy-ops genome1 genome2 new-mut)))
      (if (and (valid-genome new) (valid-mut new-mut (length joy-ops)))
	  (make-joy-default-pop :genome new :mut new-mut)
	  nil))))

(defmethod initial-state ((genome joy-default-pop))
  (make-joy-state :stk nil :exp (joy-default-pop-genome genome) :stks nil :exps nil :heap (make-hash-table) :msgs (dlist)))

(defmethod competition-add-score ((comp competition-joy-default) score-factor)
  ;; Maybe give one fitness award for each different solution, i.e. there can be multiple awards for one fitness. This would require saving all solutions together with their fitnesses in a hash-table/mru-cache.
  (with-slots (test-cases pop claim fit par) comp
    (let ((test-goal-values (generate-exam test-cases)) ;test with the same test-goal value pairs
	  (score-ticks (joy-default-par-score-ticks par))
	  (score-seconds (joy-default-par-score-seconds par)))
      (dotimes (i (length pop))
	(incf (elt fit i)
	      (* score-factor
		 (score-exam-in-01 test-cases test-goal-values (elt claim i) score-ticks score-seconds)))))))

(defun joy-eval-competition (stk exp stks exps heap c cd offspring-fun give-fun score-fun claim-fun send-fun receive-fun)
  "(ADD-OFFSPRING-FUN genome mut) adds the pair GENOME, MUT as a new organism, and leaves an organism id on the stack.
 (GIVE-FUN target-id fitness) transfers INITIAL-FIT of fitness to the TARGET-ID organism.
 (SCORE-FUN program) returns the score of joy program PROGRAM.
 (CLAIM-FUN program) submits the joy program PROGRAM as claim."
  (flet ((new-ops (op stk exp stks exps heap)
	   (values
	    (case op
	      ;; TODO: add hash-table copier for heap.
	      ;;(new-heap  (cons (make-hash-table) stk))
	      ;;(copy-heap (cons (copy-hash-table (car stk)) stk))
	      (offspring (cons (funcall offspring-fun (car stk) (cadr stk)) (cddr stk)))
	      (give      (funcall give-fun (cadr stk) (car stk)) (cddr stk))
	      (score     (cons (funcall score-fun (car stk)) (cdr stk)))
	      (claim     (funcall claim-fun (car stk)) (cdr stk))
	      (send      (funcall send-fun (car stk)) (cdr stk))
	      (receive   (multiple-value-bind (msg-p msg) (funcall receive-fun)
			   (cons msg-p (cons msg stk))))
	      (t (default-no-op op stk exp stks exps heap)))
	    exp stks exps heap)))
    ;;TODO: (handler-bind catch time-errors
    (joy-eval-2 stk exp stks exps :heap heap :c c :cd cd :no-op #'new-ops)))

(defparameter *joy-competition-ops* (append '(offspring give score claim send receive) (remove 'while *joy-ops*)))

(defun valid-genome (genome)
  (and (listp genome) (valid-joy-exp genome)))

(defun valid-mut (mut length-joy-ops)
  (and (listp mut)
       (= (+ +mut-length-const+ length-joy-ops) (length mut))
       (loop for m in mut always (and (realp m) (<= 0 m 1)))
       (<= (elt mut 0) +mut0-max+)))

(defmethod execute ((comp competition-joy-default) i test-goal-values)
  ;; Problem is, we need to give the genomes a bit time to submit some program, so that scores can be compared fairly. Maybe add a parameter (to COMPETITION-PAR) describing the minimum run time before they may be kicked out. But then flooding with calls to ADD-OFFSPRING-FUN could fill the whole population with random genomes, which would kick out all the good evolved organisms. Maybe make it so that a newly created organism receives an initial amount of fitness from its parent (whose fitness is reduced by the same amount).
  (with-slots (test-cases pop state claim fit par) comp
    (let ((joy-ops (joy-default-par-joy-ops par))
	  (score-ticks (joy-default-par-score-ticks par))
	  (score-seconds (joy-default-par-score-seconds par)))
      (flet ((offspring-fun (genome mut)
	       ;; check that (and (valid-genome genome) (valid-mut mut)), otherwise don't add the genome, and return the own i as new organism id (so that fitness transfers won't go into the void).
	       ;; TODO: decouple organism ID from position in the comp-sets. (count organism-ID from 0 and make it belong to this organism forever.)
	       (if (and (valid-genome genome) (valid-mut mut (length joy-ops)))
		   (competition-new-organism comp (make-joy-default-pop :genome genome :mut mut)))
		   i)
	     (give-fun (target-id fitness)
	       (when (and (<= 0 target-id (1- (length pop))) (not (null (elt fit target-id))))
		 (let ((f (max 0 (min (elt fit i) fitness))))
		   (decf (elt fit i) f)
		   (incf (elt fit target-id) f))))
	     (score-fun (joy-program)
	       (if (and (listp joy-program) (valid-joy-exp joy-program '(:any)))
		   (score-exam-in-01 test-cases test-goal-values joy-program score-ticks score-seconds)
		   0))
	     (claim-fun (joy-program)
	       (setf (elt claim i) joy-program))
	     (send-fun (message)
	       ;; send to all genomes
	       (loop for s across state do
		    (dlist-push message (joy-state-msgs s) :at-end t)))
	     (receive-fun ()
	       ;; Returns two values: MESSAGE-PRESENT-P and MESSAGE, which is NIL if there was no message.
	       (if (null (joy-state-msgs (elt state i)))
		   (values nil nil)
		   (values t (dlist:dlist-pop (joy-state-msgs (elt state i))))))
	     )
	(let* ((istate (elt state i))
	       (stk (joy-state-stk istate))
	       (exp (joy-state-exp istate))
	       (stks (joy-state-stks istate))
	       (exps (joy-state-exps istate))
	       (heap (joy-state-heap istate))
	       ;;(ticks (let ((ticks (joy-default-par-schedule-ticks par)) (f (ceiling (elt fit i)))) (if (< f ticks) (max 0 f) ticks)))
	       (ticks (joy-default-par-schedule-ticks par))
	       (seconds (joy-default-par-schedule-seconds par)))
	  (labels ((die (c)
		     (declare (ignorable c))
		     ;;(print (list "organism i" i "dies. reason=" c))
		     (competition-organism-die comp i))
		   (save-state (c)
		     (decf (elt fit i) ticks)
		     (if (<= (elt fit i) 0)
			 (progn
			   ;;(die )
			   )
			 (setf (elt state i) (make-joy-state :stk (joy-error-stk c)
							     :exp (joy-error-exp c)
							     :stks (joy-eval-2-error-stks c)
							     :exps (joy-eval-2-error-exps c)
							     :heap (joy-error-heap c)
							     :msgs (joy-state-msgs (elt state i)))))))
	    ;;(print (list "execute" i ticks))
	    (handler-case
		(joy-eval-competition stk exp stks exps heap (make-counter ticks) (make-countdown seconds) #'offspring-fun #'give-fun #'score-fun #'claim-fun #'send-fun #'receive-fun)
	      (joy-eval-2-counter-error (c) (save-state c))
	      (joy-eval-2-countdown-error (c) (save-state c))
	      (unknown-op-error (c) (die c))
	      (simple-type-error (c) (die c))
	      (type-error (c) (die c))
	      (division-by-zero (c) (die c))
	      (floating-point-invalid-operation (c) (die c))
	      (floating-point-overflow (c) (die c))
	      #+SBCL (SB-KERNEL::ARG-COUNT-ERROR (c) (die c))
	      (:no-error (stk) (declare (ignorable stk)) (die (list :no-error stk))))
	    ))))))

(defun new-joy-default-competition (o size cycles test-cases schedule-ticks schedule-seconds joy-ops score-ticks score-seconds score-factor max-births)
  (labels ((generate-mut ()
	     (append
	      (list (random +mut0-max+))
	      (loop for i below (1- +mut-length-const+) collect (random 1.0))
	      (loop for i below (length joy-ops) collect (random 1.0))))
	   (generate-pop ()
	     (let* ((mut (generate-mut))
		    (pop (make-joy-default-pop :genome o :mut mut)))
	       pop)))
    (let* ((pop (make-array size :initial-contents (loop for i below size collect (generate-pop))))
	   (par (make-joy-default-par :joy-ops joy-ops :schedule-ticks schedule-ticks :schedule-seconds schedule-seconds :score-ticks score-ticks :score-seconds score-seconds)))
      (competition 'competition-joy-default pop par cycles test-cases score-factor max-births))))

;;(new-joy-default-competition '(((1) claim 51 (0.5) times stack (0) offspring a) (a) define a) 10 10000 *test-cases-pi-value* 100 .01 (cons 0 *joy-competition-ops*) 1000 .01 1000 1)

(defun min-index (s)
  (reduce (lambda (i j) (let ((a (elt s i))
			      (b (elt s j)))
			  (if (< a b) i j)))
	  (loop for i below (length s) collect i)))

(defun competition (competition-type pop par cycles test-cases score-factor max-births)
  "Same as function TOURNAMENT, but different..."
  (declare (optimize (debug 3) (safety 3)))
  (let* ((size (length pop))
	 (comp (make-instance competition-type :test-cases test-cases :par par))
	 (logstream (open "/tmp/log.txt" :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)))
    (loop for p across pop do
	 (competition-new-organism comp p))
    (with-slots (pop claim fit bday n-born n-died) comp
      (dotimes (cycle cycles)
	(competition-new-cycle comp cycle)
	(let ((test-goal-values (generate-exam test-cases))) ;test with the same test-goal value pairs
	  ;;(print (list "before execute. size=" (length pop)))
	  (dotimes (i (length pop))
	    (execute comp i test-goal-values))
	  (competition-remove-dead comp)
	  (competition-add-score comp score-factor)
	  ;; of the organisms which are not new, remove the un-fittest ones so that there are (1- SIZE) old organisms.
	  ;;(print (list "size=" (length pop)))
	  (let* ((oldi (loop for i from 0 for b across bday when (< b cycle) collect i))
		 (z (loop for i in oldi for f across fit collect (cons i f))))
	    (sort z #'> :key #'cdr)
	    ;;(print (list "(1- size)" (1- size) "(length z)" (length z)))
	    (loop for i from (1- size) below (length z)
	       for ze in z do
		 (print (list "(car ze)" (car ze)))
		 (competition-organism-die comp (car ze)))
	    (competition-remove-dead comp))
	  ;;(print (list "remove un-fittest. size=" (length pop)))
	  ;; add one organism, so that there are now SIZE old organisms.
	  ;;(print (list "(length pop)" (length pop)))
	  (let* ((size (length pop))
		 (new (make-offspring comp (random size) (random size))))
	    (when (not (null new))
	      (competition-new-organism comp new)))
	  ;; TODO: randomly kill new organisms, so that there remain max MAX-BIRTHS new organisms.
	  )
	(when (= 0 (mod cycle 1))
	  (let ((n-old (loop for b across bday sum (if (< b cycle) 1 0))))
	    (print (list "cycle" cycle "max-fit" (loop for f across fit maximize f) "n-old" n-old "n-born" n-born "n-dead" n-died))))
	(when (= 0 (mod cycle 100))
	  (let ((res (sort (zip-array 'list pop fit) #'< :key #'second)))
	    (competition-log-mut-stats res logstream)
	    (print (list "best-pop" (car (car (last res))) "best-fit" (cadr (car (last res))))))))
      (close logstream)
      (sort (zip-array 'list pop fit) #'< :key #'second))))

(defun competition-log-mut-stats (res logstream)
  (let* ((fits (mapcar #'cadr res))
	 (fit-max (apply #'max fits))
	 (fit-mean (mean fits))
	 (fit-stddev (stddev-corr fits)))
    (format logstream "~A ~A ~A" (float fit-max) (float fit-mean) fit-stddev)
    (format logstream "~%"))
  (finish-output logstream))

;;;; ltree auxiliary functions

(defun ltree-reduce-leaves (ltree reduce-function initial-value &key sort-children-predicate)
  "Visit the leaves of LTREE.
Initialize VALUE with INITIAL-VALUE.
For each leaf, call REDUCE-FUNCTION with the current leaf and VALUE, and set VALUE to the result.
Iterate until all leaves are visited, and return VALUE."
  (flet ((node-function (node children-value)
	   (when children-value ;non-leaves are constantly nil
	     (setf initial-value (funcall reduce-function node initial-value)))))
   (ltree-reduce ltree (constantly nil) t #'node-function :sort-children-predicate sort-children-predicate))
  initial-value)

(defun ltree-leaves (ltree &key sort-children-predicate)
  (let ((leaves nil))
    (flet ((reduce-function (node v)
	     (declare (ignore v))
	     (push node leaves)))
      (ltree-reduce-leaves ltree
			   #'reduce-function
			   nil
			   :sort-children-predicate sort-children-predicate)
      leaves)))

(defun ltree-visit-nodes (ltree node-function)
  "Visit all the nodes of LTREE in arbitrary order, calling NODE-FUNCTION on each.
Return NIL."
  (ltree-reduce ltree (constantly nil) nil (lambda (node value) (declare (ignore value)) (funcall node-function node))))

;; TODO: search through the joy tree of possible programs. Only extend the currently fastest program by all possible extensions. This needs rewriting joy trees as lists using tree-to-list and list-to-tree (see learner-joy-rbm.lisp).
;; TODO: search through the joy tree of possible programs. Only extend the currently best-scoring programs by all possible extensions.
;; TODO: think hard about how to find out if two joy programs are equivalent. Use this to reduce the search space for joy programs. this might be related to caching implemented in systematicmapping, but also might not.
;; What is equal? Two programs are equal if they yield the same output for any input. Since we can't try all inputs, we might limit ourselves to inputs not exceeding a certain length.
;; Are there other approaches for testing program equality? Maybe there's a symbolic approach? Or a test-only-special-values, like if there's an IF, then we only have to test the values that make it execute the true-branch and those values that make it execute the false-branch.
;; Maybe most important is to prefer programs that have local variable accesses instead of programs that access variables very deep in the stack. Maybe I should consider implementing an evaluator for The Mill (computer architecture).
;; Also prefer programs which minimize maximal memory use. This should be related to prefering variable accesses close to the stack top. (Related in the sense that programs with local variable access also should automatically have lower maximal memory usage.)
;;;; To cache the results of joy programs, I could use an ltree instead of a hash-table. This should be much more space efficient. However, I don't yet know if it's possible that a joy-result contains cycles. In this case storing an ltree is probably not possible.
;;;; I could assess the similarity of two joy programs by checking if they have similar stacks at any time. To test for similar stacks, I could flatten the stacks (or not), then sort their values (or sub-trees) lexicographically, keep unique values, and see if they have the same unique sorted values. If they do, they can be transferred into each other by a "trivial" joy program that only duplicates and permutes its values, and are thus similar.
;; I need a system where programs predict each others outputs. Each program could have a builder part and an approximator. The system works like this: Two programs A and B are selected randomly. A random input (or none at all) is put on the stack of the builder program of A, it computes A builder's output (called X). Program B receives the input (or none at all) and the builder program of A on the stack of the approximator (of B) and computes its output (called Y). Then we need some measure to compute the similarity of the two outputs. But now that I think about it, it probably won't work, because the approximator will just be '(SI) or something like that, and thus be 100% correct. And the builders will just become very complex (one could of course have a fitness value preferring short builders).
;; I need a system which iteratively improves itself. Since I want it to learn programming, it should improve its understanding of how program code produces outputs from inputs. How to achieve the iterative self-improving? What does it mean to understand program code? What could the fitness function be?

;;;; Breadth-first-search

(defstruct evalled
  (states nil)
  (scores nil :type list))

(defun make-evaluator (exam-list)
  "Return as values an evaluator function and the nil-evalled."
  (let ((stk-init (mapcar #'car exam-list))
	(scorers (mapcar #'cadr exam-list)))
    (labels ((joy-eval-handler-counter (stk exp &key (heap (make-hash-table)))
	       (let* ((counter (make-counter most-positive-fixnum))
		      (countdown (make-countdown (float most-positive-fixnum)))
		      (stk (joy-eval-handler stk exp :heap heap :c counter :cd countdown)))
		 (let* ((elapsed-counter (- most-positive-fixnum (funcall counter) 2)) ;-2 because evaluating nil at the end costs 1 tick, and (funcall counter) costs 1 tick
			(elapsed-countdown (- most-positive-fixnum (funcall countdown))))
		   (values stk heap elapsed-counter (/ elapsed-countdown internal-time-units-per-second)))))
	     (evaluator (old-evalled exp)
	       "Return the evalled resulting from applying exp to old-evalled."
	       (let* ((states (evalled-states old-evalled))
		      (scores nil)
		      (new-states (loop
				     for state in states
				     for scorer in scorers
				     collect
				       (destructuring-bind (o-stk o-elapsed-counter o-elapsed-countdown) state
					 (multiple-value-bind (stk heap elapsed-counter elapsed-countdown)
					     (joy-eval-handler-counter o-stk exp)
					   (declare (ignore heap))
					   (let ((elapsed-counter (+ elapsed-counter o-elapsed-counter))
						 (elapsed-countdown (+ elapsed-countdown o-elapsed-countdown)))
					     ;;(print (list "exp" exp "o-stk" o-stk "stk" stk))
					     (setf scores (cons (funcall scorer stk) (cons (- elapsed-counter) scores)))
					     (list stk elapsed-counter elapsed-countdown)))))))
		 ;;(print (list "old-evalled" old-evalled "exp" exp "scores" scores))
		 (make-evalled :states new-states
			       :scores scores))))
    (values #'evaluator
	    (let* ((states (loop for stk in stk-init collect (list stk 0 0.0)))
		   (nil-evalled (evaluator (make-evalled :states states
							 :scores nil)
					   nil))
		   (nil-states (evalled-states nil-evalled)))
	      (make-evalled :states nil-states
			    :scores (evalled-scores nil-evalled)))))))

(defstruct bfs
  (ops nil :type list)
  (evaluator)
  (tree nil) ;tree of already visited joy programs
  )

(define-ltree-type-with-children-array-storage ltree-visited init-ltree-visited make-ltree-visited)
(defparameter *bfs-joy-ops* (append *joy-ops* '([ ] 0 1)))
(defparameter *bfs-joy-ops-nobrackets* (remove-if (lambda (x) (find x '(define [ ]))) *bfs-joy-ops*))
(init-ltree-visited *bfs-joy-ops*)

(defun bfs-new (joy-ops test-cases-list)
  (let ((tree (make-ltree-visited)))
    (multiple-value-bind (evaluator nil-evalled) (make-evaluator (loop for test-cases in test-cases-list append
								      (make-exam-from-test-cases test-cases)))
      (setf (ltree-value tree) nil-evalled)
      (make-bfs :ops joy-ops :evaluator evaluator :tree tree))))

(defun make-extender-bestscore (bfs)
  ;; Greedily extend the best scores
  (let ((queues nil))
    (labels ((register (node)
	       (loop
		  for queue in queues
		  for s in (evalled-scores (ltree-value node))
		  do
		    (cl-heap:enqueue queue node s)))
	     (init (tree)
	       "Initialize the queues with the leaves' nodes, prioritized according to the scores."
	       (flet ((leaf-function (node v)
			(declare (ignore v))
			(when (null queues)
			  (let* ((evalled (ltree-value node))
				 (scores (evalled-scores evalled)))
			    (setf queues (mapcar (lambda (s)
						   (declare (ignore s))
						   (make-instance 'cl-heap:priority-queue :sort-fun #'>))
						 scores)))
			  (register node))))
		 (ltree-reduce-leaves tree #'leaf-function nil)))
	     (extender ()
	       (unique (loop for queue in queues collect
			    (cl-heap:dequeue queue)))))
      (init (bfs-tree bfs))
      (values #'extender #'register))))

(defun count-unique (sequence &key (test #'eql))
  "Return (ELEMENT . COUNT) for the unique (under equality TEST) elements in SEQUENCE."
  (let ((ht (make-hash-table :test test))
	(result))
    (map nil
	 (lambda (x)
	   (incf (gethash x ht 0) 1))
	 sequence)
    (maphash (lambda (x c)
	       (push (cons x c) result))
	     ht)
    result))

(defun make-extender-dominators (bfs)
  ;; Extend one of the dominators. If all dominators have been extended, extend a random leaf of a dominator.
  (let ((dominators nil))
    (labels ((register (node)
	       (flet ((key (node)
			(evalled-scores (ltree-value node))))
		 ;;(print (list "register" "node" (ltree-path-from-root node) "node-scores" (funcall #'key node) "dominators" (mapcar #'key dominators)))
		 (setf dominators (multiscore-add-new node dominators :key #'key))
		 ;;(print (list "dominators" (mapcar #'key dominators) "nodes" (mapcar #'ltree-path-from-root dominators)))
		 ))
	     (init (tree)
	       "Initialize the dominators with all nodes."
	       (ltree-visit-nodes tree #'register))
	     (extender ()
	       ;;TODO: "Return the leaves of the dominators that have the shortest distance from its dominator root."
	       ;;(print (list "extender" "dominators" dominators))
	       (let* ((leaves (apply #'nconc (mapcar #'ltree-leaves dominators)))
		      (leaf-count (count-unique leaves :test #'eq))
		      (max-count (loop for (leaf . count) in leaf-count maximizing count))
		      (max-leaf-count (remove-if (lambda (x) (/= (cdr x) max-count)) leaf-count)))
		 (if (chance 0.5)
		     (mapcar #'car max-leaf-count)
		     leaves)
	       )))
      (init (bfs-tree bfs))
      (format t "init done. ~A~%" dominators)
      (values #'extender #'register))))

(defun bfs-continue (bfs make-extender-fn number-extensions)
  (multiple-value-bind (extender-fn register-fn) (funcall make-extender-fn bfs)
    (let ((ops (bfs-ops bfs))
	  (evaluator (bfs-evaluator bfs)))
      (dotimes (i number-extensions)
	(let* ((best-nodes (funcall extender-fn)))
	  (loop
	     for best-node in best-nodes
	     do
	       (format t "~A: extend ~A scores ~A~%" i (ltree-path-from-root best-node) (evalled-scores (ltree-value best-node)))
	       (dolist (op ops)
		 (let* ((evalled (funcall evaluator (ltree-value best-node) (list op)))
			(child-node (ltree-set-new-child best-node op evalled)))
		   (funcall register-fn child-node))))))
;;      bfs)))
      (print (list "best-nodes"
		   (let ((best-nodes (funcall extender-fn)))
		     ;;(print (list "best-nodes" best-nodes))
		     (mapcar (lambda (best-node)
			       (list (ltree-path-from-root best-node)
				     (evalled-scores (ltree-value best-node))))
			     best-nodes))))))
  bfs)

;;(bfs-continue (bfs-new *bfs-joy-ops-nobrackets* (list *test-cases-sqrt*)) #'make-extender-bestscore 1000)
;;(bfs-continue (bfs-new '(succ pred) (list *test-cases-sqrt*)) #'make-extender-dominators 1)

;; Arimaa (see wikipedia) might be a good game to learn on.
