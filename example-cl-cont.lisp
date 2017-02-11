(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-cont)
(use-package :cl-cont)

;; in scheme call/cc doesn't abort the function, but continues after the call/cc call.
;; (define the-continuation #f)
;; (define (cc-test)
;;   (let ((i 0))
;;     (call/cc (lambda (k) (set! the-continuation k)))
;;     (set! i (1+ i))
;;     i))


(defparameter the-continuation nil)

(defun/cc test ()
  (let ((i 0))
    (call/cc (lambda (k) (setf the-continuation k) (funcall k)))
    (incf i)
    i))

