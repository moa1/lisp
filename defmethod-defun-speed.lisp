(load "~/quicklisp/setup.lisp")
(ql:quickload 'utils)

(defun test-1 (a)
  a)

(defmethod test-2 ((a number))
  a)

(defmethod test-3 ((a symbol))
  a)

(defmethod test-3 ((a float))
  a)

(defmethod test-3 ((a integer))
  a)

(defun time-them ()
  (timediff (test-1 5) (test-2 5) :showtimes t)
  (timediff (test-1 5) (test-3 5) :showtimes t)
  (timediff (test-2 5) (test-3 5) :showtimes t))
