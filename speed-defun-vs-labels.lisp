(load "~/quicklisp/setup.lisp")
(ql:quickload 'utils)

(defun fun-1 (a b)
  (declare (optimize (speed 3) (safety 0)))
  (if (<= a 0)
      b
      (fun-2 (1- a) (1+ b))))

(defun fun-2 (a b)
  (declare (optimize (speed 3) (safety 0)))
  (if (<= a 0)
      b
      (fun-1 (1- a) (1+ b))))

(defun fun-defun (a)
  (declare (optimize (speed 3) (safety 0)))
  (fun-1 a 0))

(defun fun-labels (a)
  (declare (optimize (speed 3) (safety 0)))
  (labels ((fun-1 (a b)
	     (if (<= a 0)
		 b
		 (fun-2 (1- a) (1+ b))))
	   (fun-2 (a b)
	     (if (<= a 0)
		 b
		 (fun-1 (1- a) (1+ b)))))
    ;;(declare (inline fun-1 fun-2)) ;; note: *INLINE-EXPANSION-LIMIT* (200) was exceeded, probably trying to inline a recursive function.
    (fun-1 a 0)))

(defun timediff-defun-vs-labels (a)
  (timediff (fun-defun a) (fun-labels a) :showtimes t))
