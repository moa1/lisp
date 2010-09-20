(asdf:oos 'asdf:load-op 'utils)

(defstruct 4m
  data
  code)

(defmacro 4l (&body body)
  `(lambda (d c f)
     ,@body
     (values d c f)))

(let ((alists
       `(
	 (+ . ,(4l (push (+ (pop d) (pop d)) d)))
	 (- . ,(4l (push (- (pop d) (pop d)) d)))
	 ;; * / /mod mod
	 (swap . ,(4l (rotatef (first d) (second d))))
	 (dup . ,(4l (if (length>= d 1) (push (first d) d) (error "dup"))))
	 (over . ,(4l (if (length>= d 2) (push (second d) d) (error "over"))))
	 (rot . ,(4l (rotatef (third d) (second d) (first d))))
	 (drop . ,(4l (if (length>= d 1) (pop d) (error "drop"))))
	 (2swap . ,(4l (destructuring-bind (w x y z &rest r) d
			 (setf d (nconc (list y z w x) r)))))
	 (2over . ,(4l (destructuring-bind (w x y z &rest r) d
			 (setf d (nconc (list y z w x y z) r)))))
	 ;; 2drop 2dup sf2.html
	 (= . ,(4l (if (length>= d 2) (push (if (eq (pop d) (pop d)) -1 0) d)
		       (error "="))))
	 (< . ,(4l (push (let ((a (pop d)) (b (pop d)))
			   (if (and (typep a 'real) (typep b 'real))
			       (if (< a b) -1 0)
			       (error "<")))
			 d)))
	 ;; > 0= 0< 0> invert and or sf4.html
	 
	 
	 ;; word sf10.html
	 ;; variables(symbols) create ,(Initializing an Array) sf8.html
	 ;; ' ['] execute here sp@ sp0 sf9.html
	 ;; cell+ immediate(immediate bit is a property of :) postpone [ ] literal sf11.html
	 ;;
	 ))
      (ht (make-hash-table)))
  (dolist (alist alists)
    (setf (gethash (car alist) ht)
	  (lambda (ins d c f)
	    (handler-case
		(multiple-value-bind (d2 c2 f2)
		    (funcall (cdr alist) d c f)
		  (setq d d2 c c2 f f2))
	      (error (condition) (format nil "error ~A~%ins:~A~%d:~A"
					 condition ins d))
	      (:no-error (x) (declare (ignore x)) (values nil d c f)))
	    )))
  (defparameter *f* ht)
  (defparameter *insertions* (cons 1 (mapcar #'car alists))))

(defun inter (data code funs)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type hash-table funs))
  (if (endp code)
      (values data code funs)
      (let ((ins (pop code))
	    fail)
	(cond ((integerp ins)
	       (push ins data))
	      ((not (null (gethash ins funs)))
	       (inter data (append (gethash ins funs) code) funs))
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
	    (inter data code funs)))))

(defun test-inter ()
  (let ((test-data `(
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
		     ((1 1 =) (1))
		     ((1 2 =) (0))
		     ((1 =) :fail)
		     ((=) :fail)
		     ((1 2 <) (0))
		     ((2 1 <) (1))
		     ((1 <) :fail)
		     ((<) :fail))))
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
		   test-data)))
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
      (let* ((fit (loop for i below size collect
		       (let ((s (elt population i)))
			 (cons s (funcall fitness s)))))
	     (worst (reduce (lambda (x y) (if (< (cdr x) (cdr y)) x y)) fit)))
	(setf population
	      (let ((p (mapcar #'car (remove worst fit))))
		(cons (funcall foffspring p) p)))
	;;(prind fit)
	))))

(defun fit-find-number (number)
  (flet ((fit1 (s)
	   (multiple-value-bind (data)
	       (inter nil s (make-hash-table))
	     ;; (prind 'fit1 s data)
	     (if (or (eq data :fail) (not (length>= data 1)))
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
