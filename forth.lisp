(asdf:oos 'asdf:load-op 'utils)

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
			      (handler-case
				  (multiple-value-bind (d2 c2 f2)
				      (funcall (4l ,@body) d c f)
				    (setq d d2 c c2 f f2))
				(error (condition)
				  (format nil "error ~A~%ins:~A~%d:~A"
					  condition ',name d))
				(:no-error (x)
				  (declare (ignore x))
				  (values nil d c f))))
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
			   (error "find")))
	 (m4di execute (if (length>= d 1)
			   (multiple-value-bind (failed d2 c2 f2)
			       (funcall (pop d) d c f)
			     (if failed
				 (error failed)
				 (setq d d2 c c2 f f2)))
			   (error "execute")))
	 (m4di newlist (push nil d))
	 (m4di pushlist (if (length>= d 2)
			    (let ((o (pop d)) (l (pop d)))
			      (push (cons o l) d))
			    (error "pushlist")))
	 (m4di set-xt
	   (if (length>= d 2)
	       (let* ((l (pop d))
		      (s (pop d))
		      (lc (mapcar (lambda (x)
				    (if (numberp x)
					`(push ',x d)
					`(progn
					   (push ',x d)
					   (multiple-value-bind (failed d2 c2 f2)
					       (funcall
						(4dict-xt
						 (gethash 'execute ,ht))
						d c f)
					     (if failed
						 (return (values failed d c f))
						 (setq d d2 c c2 f f2))))))
				  l))
		      (m (make-4dict)))
		 (setf (4dict-xt m)  (eval `(lambda (d c f)
					      (block nil
						,@lc
						(values nil d c f)))))
		 (setf (gethash s f) m))
	       (error "set-xt")))
	 ))
       )
  (dolist (alist alists)
    (setf (gethash (car alist) ht) (cdr alist)))
  (defparameter *f* ht)
  (defparameter *insertions* (cons 1 (mapcar #'car alists))))

(defun inter (d c f)
  (if (endp c)
      (values d c f)
      (let ((ins (pop c))
	    fail)
	(if (integerp ins)
	    (push ins d)
	    (progn
	      (push ins c)
	      (dolist (doins '(word find-xt execute))
		(multiple-value-bind (failed d2 c2 f2)
		    (funcall (4dict-xt (gethash doins *f*)) d c f)
		  (setq fail failed d d2 c c2 f f2)
		  (if failed
		      (return))))))
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
	     ((word f1 newlist word + find-xt pushlist 5 pushlist set-xt 1 f1) (6))
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

(defun genetic (ancestors fitness generations foffspring)
  (let ((size (length ancestors))
	(population (copy-list ancestors))
	(bestf (lambda (p) (reduce (lambda (x y)
				     (if (> (funcall fitness x)
					    (funcall fitness y)) x y)) p))))
    
    (dotimes (gen generations (funcall bestf population))
      (prind gen)
      (let* ((fit (loop for i below size collect
		       (let ((s (elt population i)))
			 (cons s (funcall fitness s)))))
	     (worst (reduce (lambda (x y) (if (< (cdr x) (cdr y)) x y)) fit)))
	(setf population
	      (let ((p (mapcar #'car (remove worst fit))))
		(cons (funcall foffspring p) p)))
	))))

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

;;  (let ((data (4m-data machine))
;;	(code (4m-code machine)))
