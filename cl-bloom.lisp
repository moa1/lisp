(load "~/quicklisp/setup.lisp")
(ql:quickload 'cl-bloom)

(defun recovered (capacity false-drop-rate fill-size check-size)
  (let ((bf (bloom:make-filter :capacity capacity :false-drop-rate false-drop-rate)))
    (loop for i below fill-size do
	 (bloom:add bf i))
    (loop for i below check-size sum (if (bloom:memberp bf i) 1 0))))
