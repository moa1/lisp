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

(defun make-counter (&optional (counter 0))
  (if (<= counter 0)
      (lambda () 1)
      (let ((c counter))
	(lambda () (decf c)))))

(defun joy-eval (stk exp &key (p (make-hash-table)) (c (make-counter 0)))
;;  (print (list "stk" stk "exp" exp))
  (if (<= (funcall c) 0)
      (return-from joy-eval 'overrun))
  (if (null exp)
      stk
      (joy-eval
       (case (car exp)
	 (+       (cons (+ (car stk) (cadr stk)) (cddr stk)))
	 (and     (cons (and (car stk) (cadr stk)) (cddr stk)))
	 (apply   (joy-eval (cdr stk) (car stk) :p p :c c))
	 (compose (cons (append (car stk) (cadr stk)) (cddr stk)))
	 (cons    (cons (cons (car stk) (cadr stk)) (cddr stk)))
	 (dec     (cons (- (car stk) 1) (cdr stk)))
	 (dip     (cons (cadr stk) (joy-eval (cddr stk) (car stk) :p p :c c)))
	 (/       (cons (/ (cadr stk) (car stk)) (cddr stk)))
	 (dup     (cons (car stk) stk))
	 (eq      (cons (eq (cadr stk) (car stk)) (cddr stk)))
	 (false   (cons nil stk))
	 (ifte    (if (caddr stk)
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
	 (-       (cons (- (car stk) (cadr stk)) (cddr stk)))
	 (swap    (cons (cadr stk) (cons (car stk) (cddr stk))))
	 (true    (cons t stk))
	 (uncons  (cons (caar stk) (cons (cdar stk) nil)))
	 (define  (setf (gethash (car stk) p) (cadr stk)) (cddr stk))
	 (t
	  (multiple-value-bind (value present-p) (gethash (car exp) p)
	    (if present-p
		(progn (setf exp (append '(1) value (cdr exp))) stk)
		(cons (car exp) stk)))))
       (cdr exp) :p p :c c)))

(defun joy-test (stk exp res)
  (let ((r (joy-eval stk exp)))
    (if (not (equal r res))
	(error (format nil "joy-test failed for stk:~A exp:~A res:~A r:~A"
		       stk exp res r)))))

(joy-test nil '(5 4 +) '(9))
(joy-test nil '(nil 5 and) '(nil))
(joy-test nil '(3 5 and) '(3))
(joy-test nil '(5 4 (+) apply) '(9))
(joy-test nil '((+) (-) compose) '((- +)))
(joy-test nil '((3) 4 cons) '((4 3)))
(joy-test nil '(5 dec) '(4))
(joy-test nil '(1 2 5 (+) dip) '(5 3))
(joy-test nil '(5 2 /) '(5/2))
(joy-test nil '(3 dup) '(3 3))
(joy-test nil '(3 3 eq) '(t))
(joy-test nil '(3 t eq) '(nil))
(joy-test nil '(false) '(nil))
(joy-test nil '(true (1) (2) ifte) '(1))
(joy-test nil '(false (1) (2) ifte) '(2))
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
(joy-test nil '(4 5 -) '(1))
(joy-test nil '(1 2 swap) '(1 2))
(joy-test nil '(true) '(t))
(joy-test nil '(5 list 4 cons uncons) '(4 (5)))
(joy-test nil '(2 (dup +) superman define) '(2))
(joy-test nil '(2 (1) superman define superman) '(1 2))
(joy-test nil '(1 a) '(a 1))

(defun joy-eval-handler (stk exp &key (p (make-hash-table)) (c (make-counter)))
  (handler-case (joy-eval stk exp :p p :c c)
    (simple-type-error () 'error)
    (type-error () 'error)
    (division-by-zero () 'error)))

;;(defparameter joy-ops '(+ and apply compose cons dec dip / dup eq false ifte
;;			inc list * not or papply pop quote rem - swap true
;;			uncons define))
(defparameter joy-ops '(+ and apply compose cons dec dip dup eq false ifte
			inc list not or papply pop quote rem - swap true
			uncons define))

(defun choice (seq)
  (let ((l (length seq)))
    (elt seq (random l))))

(defun mutate (exp prob &optional (debranch-p t))
  (cond
    ((null exp) (if (< (random 1.0) prob) ;extend
		    (cons (if (< (random 1.0) .1)
			      (choice '(a b c d e 1 2 4 8 16))
			      (choice joy-ops))
			  nil)))
    ((listp exp) (if (and debranch-p (< (random 1.0) prob))
		     (if (< (random 1.0) .1) ; de-branch
			 (choice '(a b c d e 1 2 4 8 16))
			 (choice joy-ops))
		     (cons (mutate (car exp) prob)
			   (if (and (= 1 (length (cdr exp)))
				    (< (random 1.0) .1))
			       nil ; shorten
			       (mutate (cdr exp) prob nil)))))
    (t (if (< (random 1.0) prob)
	   (if (< (random 1.0) .1)
	       (cons (if (< (random 1.0) prob)
			 (if (< (random 1.0) .1) ; branch
			     (choice '(a b c d e 1 2 4 8 16))
			     (choice joy-ops))
			 exp)
		     nil)
	       (if (< (random 1.0) .1) ; substitute
		   (choice '(a b c d e 1 2 4 8 16))
		   (choice joy-ops)))
	   exp))))

(defun mutate-rec (exp prob)
  (if (< (random 1.0) prob)
      (mutate-rec (mutate exp prob) prob)
      (mutate exp prob)))

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
     
  

(defun fitness-max (exp &key (steps 1000))
  (let ((res (joy-eval-handler nil exp :c (make-counter steps))))
    (if (and (listp res) (= (length res) 1) (numberp (car res)))
	(car res)
	0)))

(defun fitness (exp)
  (


(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun zip-array (result-type &rest arrays)
  (apply #'map result-type #'list arrays))

(defun tournament (exp size cycles)
  (let* ((n (loop for i below size collect i))
	 (pop (make-array size :initial-element exp))
	 (fitexp (fitness exp))
	 (fit (make-array size :initial-element fitexp)))
    (dotimes (c cycles)
      (let* ((c1 (choice n))
	     (c2 (choice n))
	     (c2-fit (elt fit c2))
	     (new (mutate-rec (elt pop c1) .1))
	     (new-fit (fitness new)))
	(if (> new-fit c2-fit)
	    (progn (setf (elt pop c2) new)
		   (setf (elt fit c2) new-fit)
		   (print (list "new" new))))
	(if (= 0 (mod c 1000))
	    (print (list c new-fit c2-fit)))))
    (sort (zip-array 'list pop fit) #'< :key (lambda (x) (nth 1 x)))))

