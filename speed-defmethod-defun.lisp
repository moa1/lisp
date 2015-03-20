(load "~/quicklisp/setup.lisp")
(ql:quickload :utils)

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
  (utils:timediff (test-1 5) (test-2 5) :showtimes t)
  (utils:timediff (test-1 5) (test-3 5) :showtimes t)
  (utils:timediff (test-2 5) (test-3 5) :showtimes t))

;; test whether calling a generic with many defined methods takes longer than one with few.

(defmethod test-many ((a number))
  a)

(defmethod test-many ((a symbol))
  a)

(defmethod test-many ((a list))
  a)

(defmethod test-many ((a cons))
  a)

(defmethod test-many ((a standard-object))
  a)

(defmethod test-few ((a number))
  a)

(defun test-many-few-methods ()
  (utils:timediff (test-many 5) (test-few 5) :showtimes t))

;; with many types

(loop for i below 100 do
     (let ((name (intern (format nil "STRUCT-~A" i))))
       (eval `(progn
		(defstruct ,name a b c)
		(defmethod test-verymany ((a ,name)) a)))))

(defmethod test-veryfew ((a struct-0))
  a)

(defun test-verymany-veryfew-methods ()
  (let ((s (make-struct-0 :a 1 :b 2 :c 3)))
    (utils:timediff (test-verymany s) (test-veryfew s) :showtimes t)))

(defun test-verymany-with-without-declared-type ()
  (let ((a (make-struct-0 :a 1 :b 2 :c 3))
	(b (make-struct-0 :a 1 :b 2 :c 3)))
    (declare (type struct-0 a))
    (utils:timediff (test-verymany a) (test-verymany b) :showtimes t)))
