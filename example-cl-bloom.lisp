(load "~/quicklisp/setup.lisp")
(ql:quickload 'cl-bloom)

(defun recovered (capacity false-drop-rate fill-size check-size)
  "(* FALSE-DROP-RATE CHECK-SIZE) is the expected number of false positives when (= CAPACITY FILL-SIZE)."
  (let ((bf (bloom:make-filter :capacity capacity :false-drop-rate false-drop-rate)))
    (print "adding")
    (loop for i below fill-size do
	 (bloom:add bf i))
    (print "checking")
    (print (loop for i below check-size sum
		(if (bloom:memberp bf i) 1 0)))))

(defun recovered-set (fill-size check-size)
  (print "making ")
  (let ((cbf (bloom:make-set-filter (loop for i below fill-size collect i))))
    (print "checking")
    (print (loop for i below check-size sum
		(if (bloom:memberp cbf i) 1 0)))))
