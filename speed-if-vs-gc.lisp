(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :utils)

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

(defun time-collect-using-if (n repeats)
  (utils:timeit (repeats)
    (collect-using-if n)))

(defun time-collect-using-cons (n repeats)
  (utils:timeit (repeats)
    (collect-using-cons n)))

;; On zweihorn:
;; CL-USER> (time-collect-using-if 5 1000000)
;; 0.885
;; CL-USER> (time-collect-using-if 5 1000000)
;; 0.945
;; CL-USER> (time-collect-using-if 5 1000000)
;; 0.885
;; CL-USER> (time-collect-using-cons 5 1000000)
;; 0.99
;; CL-USER> (time-collect-using-cons 5 1000000)
;; 1.008
;; CL-USER> (time-collect-using-cons 5 1000000)
;; 1.01
;; CL-USER> (time-collect-using-if 5000 1000)
;; 0.61
;; CL-USER> (time-collect-using-if 5000 1000)
;; 0.675
;; CL-USER> (time-collect-using-if 5000 1000)
;; 0.629
;; CL-USER> (time-collect-using-cons 5000 1000)
;; 0.592
;; CL-USER> (time-collect-using-cons 5000 1000)
;; 0.546
;; CL-USER> (time-collect-using-cons 5000 1000)
;; 0.611

;; On eckplatz2:
;; CL-USER> (time-collect-using-if 5 10000000)
;; 0.741
;; CL-USER> (time-collect-using-if 5 10000000)
;; 0.741
;; CL-USER> (time-collect-using-if 5 10000000)
;; 0.743
;; CL-USER> (time-collect-using-cons 5 10000000)
;; 0.859
;; CL-USER> (time-collect-using-cons 5 10000000)
;; 0.856
;; CL-USER> (time-collect-using-cons 5 10000000)
;; 0.859
;; CL-USER> (time-collect-using-if 5000 10000)
;; 0.668
;; CL-USER> (time-collect-using-if 5000 10000)
;; 0.668
;; CL-USER> (time-collect-using-if 5000 10000)
;; 0.673
;; CL-USER> (time-collect-using-cons 5000 10000)
;; 0.673
;; CL-USER> (time-collect-using-cons 5000 10000)
;; 0.716
;; CL-USER> (time-collect-using-cons 5000 10000)
;; 0.64

(defun measure-times ()
  (defparameter *time-if-1*
    (progn
      (gc :full t)
      (loop for i below 100 collect (time-collect-using-if 5 1000000))))
  (defparameter *time-cons-1*
    (progn
      (gc :full t)
      (loop for i below 100 collect (time-collect-using-cons 5 1000000))))
  (defparameter *time-if-2*
    (progn
      (gc :full t)
      (loop for i below 100 collect (time-collect-using-if 5000 1000))))
  (defparameter *time-cons-2*
    (progn
      (gc :full t)
      (loop for i below 100 collect (time-collect-using-cons 5000 1000)))))

(defun all-vs-all (list1 list2)
  "Compare all times in LIST1 with all times in LIST2.
Return 1. what fraction of comparisons had a faster time in LIST1 than LIST2 and 2. in those cases, what the average time difference was between time2 and time1, and 3. in the other cases what the average time difference was between time2 and time1. (This means that the first average time difference is always positive or NIL, and the second average time difference is always negative or NIL.)"
  (let ((count-faster-1 0)
	(abstime-faster-1 0.0)
	(abstime-faster-2 0.0))
    ;; Compare all times in LIST1 with all times in LIST2 and count how often a time in LIST1 was faster than one in LIST2 and by what amount.
    (loop for time1 in list1 do
	 (loop for time2 in list2 do
	      (if (< time1 time2)
		  (progn
		    (incf count-faster-1)
		    (incf abstime-faster-1 (- time2 time1)))
		  (progn
		    (incf abstime-faster-2 (- time2 time1))))))
    ;; Return 1. what fraction of comparisons had a faster time in LIST1 than LIST2 and 2. in those cases, what the average time difference was between time2 and time1, and 3. in the other cases what the average time difference was between time2 and time1. (This means that the first average time difference is always positive or NIL, and the second average time difference is always negative or NIL.)
    (let* ((length1 (length list1))
	   (length2 (length list2))
	   (comparisons (* length1 length2))
	   (count-faster-1-frac (float (/ count-faster-1 comparisons)))
	   (count-faster-2 (- comparisons count-faster-1))
	   (abstime-faster-1-avg (when (> count-faster-1 0)
				   (float (/ abstime-faster-1 count-faster-1))))
	   (abstime-faster-2-avg (when (> count-faster-2 0)
				   (float (/ abstime-faster-2 count-faster-2)))))
      (values count-faster-1-frac abstime-faster-1-avg abstime-faster-2-avg))))

;; On eckplatz2:
;; CL-USER> (measure-times)
;; *TIME-CONS-2*
;; CL-USER> (all-vs-all *time-if-1* *time-cons-1*)
;; 1.0
;; 0.011950243
;; NIL
;; CL-USER> (all-vs-all *time-if-2* *time-cons-2*)
;; 0.3647
;; 0.003707433
;; -0.0025532895
