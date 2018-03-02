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

(defun tagbody-restart (n)
  ;; NOTE: this function signals a simple-control-error, since the restart is not active anymore.
  (labels ((rec-signal (a i)
	     (restart-case
		 (if (< i 0)
		     (error 'simple-condition :format-control "simple-condition")
		     (rec-signal a (1- i)))
	       (continue ()
		 a))))
    (let ((restarts nil))
      (tagbody
	 (dotimes (i n)
	   (handler-bind
	       ((simple-condition (lambda (c)
				    (push (find-restart 'continue c) restarts)
				    (go after))))
		  (rec-signal i 10))
	   after
	   (print (list "restarts" restarts))))
      (print (list "HERE restarts" restarts))
      (loop for restart in restarts collect
	   (invoke-restart restart)))))

(defun catcher (n)
  ;; NOTE: this function signals a SIMPLE-CONTROL-ERROR due to attempting to throw a tag that does not exist.
  (let ((tags (loop for i below n collect (gensym))))
    (labels ((rec (i res)
	       (let ((tag (elt tags i)))
		 (catch tag
		   (if (>= i (1- n))
		       res
		       (rec (1+ i) (cons tag res))))
		 (list i))))
      (let ((tags2 (rec 0 nil)))
	(print (list "tags2" tags2))
	(loop for tag in tags2 for i below n collect
	     (when (= 0 (mod i 2))
	       (throw tag i)))))))

;;; WARNINGS

(define-condition mywarning (warning)
  ())

(defun warn-demo ()
  (flet ((warner ()
	   (format t "before #'WARN.~%")
	   (restart-case (warn (make-condition 'mywarning))
	     (mufflewarning ()
	       t))
	   (format t "after #'WARN.~%")))
    (handler-bind ((mywarning #'(lambda (warning)
				  (format t "warning ~S has been signaled~%" warning)
				  (muffle-warning warning)
				  ;;(invoke-restart 'mufflewarning)
				  )))
      (warner))))

(defun warn-demo2-does-not-work ()
  "Apparently HANDLER-CASE cannot do the same thing as HANDLER-BIND, since it unwinds the stack and cannot invoke restarts that were not established before it (i.e. the restart must be in a form enclosing the HANDLER-CASE)."
  (flet ((warner ()
	   (restart-case (warn "Foo.")
	     (my-restart () 7))))
    (handler-case (warner)
      (warning (c)
	(declare (ignore c))
	(invoke-restart 'my-restart)))))

