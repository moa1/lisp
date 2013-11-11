(defun fun-a (a b)
  (+ a b))

(defun read-value ()
  (format t "enter a value:~%")
  (list (read)))

(defun fun-b (a b)
  (restart-case (+ a b)
    (t-instead ()
      t)
    (use-value (v)
      :interactive read-value
      v)))

(defun fun-c (a b)
  (declare (ignore a b))
  (signal 'simple-type-error :format-control "simple-type-error"))

(defun fun-d (a b)
  (declare (ignore a b))
  (signal "undefined-error"))

(defun case-a (fun)
  (handler-case (funcall fun 1 'a)
    (simple-type-error (c)
      (format t "case-a c:~A~%" c)
      nil)))

(defun case-b (fun)
  (handler-case (case-a fun)
    (simple-type-error (c)
      (format t "case-b c:~A~%" c)
      nil)))

(defun bind-a (fun)
  (handler-bind
      ((simple-type-error (lambda (c) (format t "bind-a c:~A~%" c) nil)))
    (funcall fun 1 2)
    (funcall fun 1 'a)))

(defun bind-b (fun)
  (handler-bind
      ((simple-type-error (lambda (c) (format t "bind-b c:~A~%" c) nil)))
    (bind-a fun)))

(defun bind-handle-a (fun)
  (handler-bind
      ((simple-type-error (lambda (c)
			    (format t "bind-handle-a c:~A~%" c)
			    (invoke-restart 'use-value 'val))))
    (bind-a fun)))

;; example: (case-a #'fun-a)
;; example: (bind-a #'fun-b)
;; example: (bind-handle-a #'fun-c)
