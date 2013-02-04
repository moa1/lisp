;; Restricted Boltzmann Machine

(load "numeric.lisp")

(defun sigmoid (x)
  (cond
    ((< x -88) 0.0)
    (t (/ 1 (1+ (exp (- x)))))))

(defun h-from-v (v w)
;;  (format t "h-from-v~%")
  (let ((h-size (cdr (matrix-dim w)))
	(v-size (length v)))
    (loop for j below h-size collect
	 (sigmoid (loop for i below v-size for wi in w for vi in v sum
		       (progn
;;			 (format t "i:~A j:~A wij:~A~%" i j (elt wi j))
			 (* vi (elt wi j))))))))

(defun v-from-h (w h)
;;  (format t "v-from-h~%")
  (let ((h-size (cdr (matrix-dim w)))
	(v-size (car (matrix-dim w))))
;;	(v-size (length v)))
    (loop for i below v-size for wi in w collect
	 (sigmoid (loop for j below h-size for hj in h sum
		       (progn 
;;			 (format t "i:~A j:~A wij:~A~%" i j (elt wi j))
			 (* hj (elt wi j))))))))

(defun learn (v w rate)
  ;; v are the input layer values, w is the connection matrix
  (let* ((h (h-from-v v w))
	 (pos (outer v h))
	 (v1 (v-from-h w h))
	 (h1 (h-from-v v1 w))
	 (neg (outer v1 h1))
	 (update (num* rate (num- pos neg))))
    update))

(defun recall (v w &optional (verbose nil))
  (let* ((h (h-from-v v w)))
    (when verbose
      (format t "h:~A~%" h))
    (v-from-h w h)))

;; learn a simple relation between a and a+.5
(defun test1 (num-hidden)
  (let ((w (loop for i below 3 collect (loop for j below num-hidden collect (1- (random 2.0))))))
    (loop for i below 1000 do
	 (let* ((a (random .5))
		(v (list a (+ .5 a) 1)))
	   (format t "~%i:~A v:~A~%" i v)
	   (setf w (num+ w (learn v w .05)))
	   (format t "new w:~A~%h:~A~%" w (h-from-v v w))
	   (format t "v-recall:~A~%" (recall v w))))))
  

(defparameter v '(0.5 1 1))

(let ((w '((-1.5 -.4) (-.3 .1) (-2.0 1.1))))
  (loop for i below 1000 do
       (format t "~%i:~A~%" i)
       (setf w (num+ w (learn v w .05)))
       (format t "new w:~A~%h:~A~%" w (h-from-v v w))
       (format t "v-recall:~A~%" (recall v w))))

;;(let ((w '((-1.5 -.4 .2) (-.3 .1 -2.2) (-2.0 1.1 .5))))
(let ((w (loop for i below 3 collect (loop for j below 3 collect (1- (random 2.0))))))
  (loop for i below 1000 do
       (let* ((a (random .5))
	      (v (list a (+ .5 a) 1)))
	 (format t "~%i:~A v:~A~%" i v)
	 (setf w (num+ w (learn v w .05)))
	 (format t "new w:~A~%h:~A~%" w (h-from-v v w))
	 (format t "v-recall:~A~%" (recall v w)))))

(let ((w '((0.61336917 -0.87789977 0.10563295 0.108813316 -1.0656557
        -0.14073084 -0.71745485 0.08811118 -0.898514 -0.75391614 0.6452821
        -0.880524 0.98574054 0.026347551 0.32547185 0.91539073 -0.9214095
        -0.28599972 -0.16070473 -0.8744107 -0.65713847 -0.83152556
        0.9450839 0.4008578 0.45750025 -0.6717649 -1.062213 -0.2902688
        0.30618614 0.6396431 -0.007923858 0.39384183 -0.3331783 0.1697953
        0.62041825 0.18130209 -0.3652584 -0.61548156 0.4417567 0.13190964
        -0.18594131 -0.82465225 0.91777277 0.8256128 0.30499038
        -0.64885134 -0.8260492 0.5652199 -1.0221136 -0.5210891)
       (0.123640746 -1.0991547 0.8257825 -0.6773345 -0.44339636
        -0.59915906 -0.9420719 -0.17498662 -0.6426829 -0.3476663
        0.16052774 0.44924057 0.4589913 -0.8162435 0.49449313 0.26592124
        0.29615963 0.20463501 0.48759156 -0.13148634 1.0299048e-4
        -0.17958458 0.5699613 -0.771378 0.62940395 -0.14486916 0.5233946
        -0.081126615 0.84262145 -1.1710441 -1.1018877 0.6783397 -0.4131744
        0.63944435 0.35797864 0.4017728 0.26525897 -0.19415595 -0.8150256
        0.63009846 0.264528 -0.7353737 -0.40663537 -0.29356733
        -0.107356735 0.51135135 -0.7070773 -1.0214218 0.57366973
        0.5727504)
       (0.65444666 0.81611556 -0.85355663 -0.17414212 0.65510595
        0.28344753 -0.11460361 0.727842 1.0101643 -0.47758725 0.27214402
        0.6342863 -0.16444565 -0.85028917 0.7287216 0.8259994 0.23069003
        -0.20637375 0.6675672 0.29848567 -0.11644666 -0.73341966
        0.42420742 0.9716775 0.3900459 -0.9327895 0.40578544 0.2137381
        -0.5372908 0.925518 0.3201437 0.15297723 -0.23414783 0.57303727
        0.6763206 -0.44792926 0.776379 -0.19283253 0.85654205 -0.2789275
        -0.8926282 0.011856664 0.11657309 -0.72564214 -0.7304891 0.934093
        0.39207947 0.86143875 0.5613059 0.45791808)))
      (v '(.1 0 1.0)))
  (loop for i below 10 do
       (setf v (recall v w))
       (format t "i:~A v-recall:~A~%" i v)))
