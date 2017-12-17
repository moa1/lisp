;; specificity

(declaim (optimize (debug 3)))

(defclass obj ()
  ())

(defmethod fun ((a obj))
  (format t "obj~%"))

(defmethod fun (a)
  (format t "fallback~%"))

(defun test-specificity ()
  (fun (make-instance 'obj))
  (fun 1))

;; around

(defmethod met :around ((a obj))
  (format t "around obj before a:~S~%" a)
  (call-next-method a)
  (format t "around obj after~%")
  '(around obj))

(defmethod met :around (a)
  (format t "around fallback before a:~S~%" a)
  (call-next-method a)
  (format t "around fallback after~%")
  '(around fallback))

(defmethod met ((a obj))
  (format t "primary obj a:~S~%" a)
  '(primary obj))

(defmethod met (a)
  (format t "primary fallback a:~S~%" a)
  '(primary fallback))

(defun test-around ()
  (met (make-instance 'obj)))
