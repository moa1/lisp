(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :iterate)
(use-package :iter)

;;;; 3.4 On-line Help

(display-iterate-clauses)

;;;; 2.1 Drivers

;; IN-SEQUENCE (for sequences) is like IN (for lists)
(let ((l '(1 2 3 4)))
  (iter (for i in-sequence l) (collect i)))
;; indices
(let ((l '(1 2 3 4)))
  (iter (for i index-of-sequence l) (collect i)))
;; index-of-sequence doesn't check the bounds of L
(let ((l '(1 2 3 4)))
  (iter (for i index-of-sequence l below 10) (collect i)))
;; only first two elements
(let ((l '(1 2 3 4)))
  (iter (for i in-sequence l below 2) (collect i)))
;; omit first 2 elements
(let ((l '(1 2 3 4)))
  (iter (for i in-sequence l from 2) (collect i)))
;; seems to not do anything since the result of the second iter is dropped, but see ";; collect in outer ITER" below (in ";;;; 3.2 Named Blocks")
(iter (for i below 5)
      (iter (for j below 5)
	    (collect (cons i j))))
;; list of lists of conses
(iter (for i below 5)
      (collect
	  (iter (for j below 5)
		(collect (cons i j)))))
;; same iteration variable
(iter (for i below 5)
      (collect
	  (iter (for i below 5)
		(collect (cons i i)))))
;; next updates the whole generator
(iter (generating (key . item) in '((a . 1) (b . 2) (c . 3)))
      (collect (next key))
      (collect (next item)))
;; the second call to FORMAT is interrupted by the termination of the generator
(iter (generating (key . item) in '((a . 1) (b . 2) (c . 3)))
      (format t "~S ~S~%" (next key) (next item)))
;; previous value
(iter (for current in '(1 2 3 4))
      (for prev previous current)
      (collect (cons current prev)))
;; previous value back 2 iterations
(iter (for current in '(1 2 3 4))
      (for prev previous current back 2)
      (collect (cons current prev)))
;;;; 2.3 Gathering Clauses

;; sum is produced into X, but X is not used anywhere (and not returned)
(iter (for el in '(1 2 3 4))
      (sum el into x))
;; now it is returned
(iter (for el in '(1 2 3 4))
      (sum el into x)
      (finally (return x)))
;; returning multiple values
(iter (for el in '(1 2 3 4))
      (sum el into x)
      (finally (return (values x 5))))
;; using intermediate results
(iter (for el in '(1 2 3 4))
      (sum el into x)
      (print x)
      (finally (return (values x 5))))
;; incrementing the counter by 1 every time (ODDP EL) is non-NIL.
(iter (for el in '(1 2 3 4))
      (counting (oddp el)))
;; return the collection in reverse order (because the element is inserted at the beginning, not end)
(iter (for i from 1 to 5)
      (collect i at start))
;; finding an element
(iter (for el in '(1 2 3 4))
      (finding el such-that (= el 3)))
;; default value if element is not found
(iter (for el in '(1 2 3 4))
      (finding el such-that (= el 6) on-failure nil))
;; the failure-value of ON-FAILURE should be thought of as a default start value instead of a value evaluated only if the element is never found.
(let ((x 0))
  (values
   (iter (for el in '(1 2 3 4))
	 (finding el such-that (= el 3) on-failure (incf x)))
   x))
;; MAXIMIZING finds the element that maximizes an expression
(iter (for el in '(1 2 3 4))
      (finding el maximizing (if (evenp el) (- el) el)))
;; ALWAYS, NEVER and THEREIS like in LOOP
(iter (for el in '(1 2 3 4))
      (always (> el 0)))

;;;; 2.4. Control-flow

;; (FINISH) (LEAVE) (NEXT-ITERATION) (WHILE expr) (UNTIL expr) (IF-FIRST-TIME first-iteration-expr other-iterations-expr)

;;;; 2.5 Predicates

;; (FIRST-ITERATION-P) is true on the first iteration, NIL on all others
(iter (for el in '(nil 1 2 nil 3))
      (when el
	(unless (first-iteration-p)
	  (princ ", "))
	(princ el)))
;; => , 1, 2, 3
;; (FIRST-TIME-P) is true the first time it is executed, NIL else
(iter (for el in '(nil 1 2 nil 3))
      (when el
	(unless (first-time-p)
	  (princ ", "))
	(princ el)))
;; => 1, 2, 3

;;;; 2.6 Code Placement

;; (INITIALLY forms...) (AFTER-EACH forms...) (ELSE forms...) (FINALLY forms...) (FINALLY-PROTECTED forms...)
;; ELSE executes at the end of ITER if it is never executed
(iter (for el in '(nil 1 2 nil 3))
	       (finish)
	       (else (print "else")))
;; => prints "else"
CL-USER> (iter (for el in '(nil 1 2 nil 3))
	       (else (print "else"))
	       (finish))
;; does not print "else"

;;;; 3.1 Multiple Accumulations

(iter (for i from 1 to 10)
      (collect i into nums)
      (collect (sqrt i) into nums)
      (finally (return nums)))

(iter (for i from 1 to 10)
      (maximize (* i i))
      (maximize (* i i i)))

;;;; 3.2 Named Blocks

(iter fred
      (for i from 1 to 10)
      (iter barney
	    (for j from i to 10)
	    (if (> (* i j) 17)
		(return-from fred j))))

;; collect in outer ITER
(iter outer (for i below 10)
      (iter (for j below 5)
	    (in outer (collect (cons i j)))))

;;;; 4 Types and Declarations
