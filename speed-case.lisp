;; this file tests if joy-eval can be sped up by looking up the symbols with a hash-table.
;; it doesn't look at binary trees, which could be used to look up the (string (car exp)).
;; in slime, execute every function in order with ctrl-cc (compiling with ctrl-ck doesn't work)

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

(defparameter *table* '((+       (cons (+ (cadr stk) (car stk)) (cddr stk))) ;add
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
				   (cons (car exp) stk)))))))

(defmacro make-joy-eval-normal ()
  `(defun joy-eval (stk exp &key (heap (make-hash-table)) (c (make-counter 0)) (cd (make-countdown 0.0)))
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
	    ,@*table*)
	  (cdr exp) :heap heap :c c :cd cd))))

(defun table-to-hashtable ()
  (flet ((list-to-setf (x)
	   `(setf (gethash ',(car x) *joy-eval-ht*)
		  (lambda (stk exp heap c cd) 
		    (progn (let ((stk ,@(cdr x)))
			     (values stk exp)))))))
    (mapcar #'list-to-setf *table*)))

(defmacro make-joy-eval-ht ()
  (let ((mc (table-to-hashtable)))
    `(progn
       (defparameter *joy-eval-ht* (make-hash-table :test 'eq))
       ,@mc
       (setf *joy-eval-t* (gethash t *joy-eval-ht*)))))

(defmacro make-joy-eval-hash ()
  `(defun joy-eval (stk exp &key (heap (make-hash-table)) (c (make-counter 0)) (cd (make-countdown 0.0)))
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
	 (progn
	   (multiple-value-bind (stk-new exp-new)
	       (funcall (gethash (car exp) *joy-eval-ht* *joy-eval-t*) stk exp heap c cd)
	     (joy-eval stk-new (cdr exp-new) :heap heap :c c :cd cd))))))


(defparameter *loops* 100000)
(make-joy-eval-normal)
(time (loop for i below *loops* do
	   (joy-eval nil '(1 2 3 4 5 6 7 (pop pop stack (1) equal not) (pop) while))))
(make-joy-eval-ht)
(make-joy-eval-hash)
(time (loop for i below *loops* do
	   (joy-eval nil '(1 2 3 4 5 6 7 (pop pop stack (1) equal not) (pop) while))))
