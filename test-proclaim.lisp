(defparameter x 5.1)

(defun bla ()
  (+ x 0.5))

(defun foo1 ()
  (values x (bla)))

(defun foo2 ()
  (proclaim '(type fixnum x))
  (values x (bla)))

(defun foo3 ()
  (declaim (type fixnum x))
  (values x (bla)))

(defun foo4 ()
  (declare (type fixnum x))
  (values x (bla)))
