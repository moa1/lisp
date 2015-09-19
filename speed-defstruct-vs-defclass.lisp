(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :utils)

(defstruct (v3s
	     (:constructor make-v3s (x y z)))
  x y z)

(defclass v3c (standard-object)
  ((x :initarg :x :accessor v3c-x)
   (y :initarg :y :accessor v3c-y)
   (z :initarg :z :accessor v3c-z)))

(defun make-v3c (x y z)
  (make-instance 'v3c :x x :y y :z z))

(defun speed-make-v3s-vs-v3c ()
  (format t "Body1 is structure, Body2 is class.~%")
  (utils:timediff
   (make-v3s 0 0 0)
   (make-v3c 0 0 0)
   :showtimes t
   :maxtime 2))

(defun speed-access-v3s-vs-v3c ()
  (format t "Body1 is structure, Body2 is class.~%")
  (let ((s (make-v3s 1 2 3))
	(c (make-v3c 1 2 3)))
    (utils:timediff
     (+ (v3s-x s) (v3s-y s) (v3s-z s))
     (+ (v3c-x c) (v3c-y c) (v3c-z c))
     :showtimes t
     :maxtime 2)))
