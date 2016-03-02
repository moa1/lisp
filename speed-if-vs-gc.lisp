;; Package ITERATE produces code like this:
;; CL-USER> (macroexpand '(iter (for i from 1 to 10)
;; 			(collect i)))
;; (LET* ((I NIL) (#:RESULT26 NIL) (#:END-POINTER27 NIL) (#:TEMP28 NIL))
;;  ...
;;         ;; the loop's body:
;;         (SETQ #:TEMP28 (LIST I))
;;         (SETQ #:END-POINTER27
;;                 (IF #:RESULT26
;;                     (SETF (CDR #:END-POINTER27) #:TEMP28)
;;                     (SETQ #:RESULT26 #:TEMP28)))
;;  ...
;;     #:RESULT26))
;; I.e. it initializes the RESULT with NIL and modifies its CDR, which doesn't work in the first iteration. So it checks whether the RESULT is NIL or already a cons, and in the case it is NIL it overwrites it with a cons.
;; I thought that this IF could maybe be avoided in two ways: 1. move the first iteration out of and before the loop. However, this doesn't work if the first iteration doesn't yet fill the RESULT, e.g. as in '(ITER (FOR EL IN LIST) (IF (AND (NUMBERP EL) (ODDP EL)) (COLLECT EL))). 2. Initialize the RESULT not with NIL, but with (CONS NIL NIL) and instead of returning the whole RESULT, return only (CDR RESULT). This means that the first cons has to be garbage-collected.
;; Which method is more efficient and under which circumstances? (I.e. probably there is a cutoff of number of iterations after which the second method is more efficient because the IF has to be executed in every loop, but the CONS only has to be garbage-collected once. But then again, the processor can predict the conditional jump of the IF correctly in every iteration except the first two (or so), so it may be negligible.)

(defun collect-using-if (iterations)
  (declare (type fixnum iterations))
  (let ((result nil)
	(end-pointer nil))
    (do ((i 0 (1+ i)))
	((>= i iterations))
      (let ((temp (list i)))
	(setf end-pointer
	      (if result
		  (setf (cdr end-pointer) temp)
		  (setf result temp)))))
    result))

(defun collect-using-cons (iterations)
  (declare (type fixnum iterations))
  (let* ((result (cons nil nil))
	 (end-pointer result))
    (do ((i 0 (1+ i)))
	((>= i iterations))
      (let ((temp (list i)))
	(setf end-pointer
	      (setf (cdr end-pointer) temp))))
    (cdr result)))

(defun compare-collect-if-vs-cons (n)
  (utils:timediff (collect-using-if n) (collect-using-cons n) :showtimes t :maxtime 2))

;; On eckplatz2, the results are (reproducibly) that #'COLLECT-USING-IF is faster for short loops, and there is no difference for long loops:
;; CL-USER> (compare-collect-if-vs-cons 5)
;; TIMES1:(0.043 0.044 0.043 0.035 0.043 0.043) 
;; TIMES2:(0.049 0.048 0.048 0.049 0.048 0.049) 
;; Body1 mean 0.00007979074 ms. Body2 mean 0.00009250641 ms.
;; Body1 is 0.000012715666 ms faster (1.2 speedup) per call than Body2.
;; (0.004966606300175618d0 T)
;; (-0.0066666715 -1.2715667e-8)
;; (0.04183333 0.003371449 6)
;; (0.0485 5.4772163e-4 6)
;; 524288
;; CL-USER> (compare-collect-if-vs-cons 10000)
;; TIMES1:(0.029 0.036 0.03 0.035 0.03 0.036 0.029 0.036 0.029 0.035 0.035 0.035
;;         0.036 0.035 0.035 0.035 0.036 0.035 0.036 0.035 0.035 0.029 0.048 0.029
;;         0.036 0.029 0.036 0.03 0.035 0.029) 
;; TIMES2:(0.049 0.036 0.035 0.036 0.036 0.035 0.036 0.036 0.036 0.036 0.036 0.03
;;         0.036 0.029 0.035 0.03 0.035 0.029 0.035 0.029 0.036 0.035 0.037 0.037
;;         0.036 0.036 0.036 0.036 0.036 0.036) 
;; Body1 mean 0.13203125 ms. Body2 mean 0.1375 ms.
;; (0.15899260563491446d0 NIL)
;; (-0.0014000013 -5.4687553e-6)
;; (0.033800002 0.003986183 30)
;; (0.035200004 0.0036045944 30)
;; 256

(defun compare2-collect-if-vs-cons (n)
  (utils:timesec (lambda () (collect-using-if n))))
