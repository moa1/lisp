(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :simple-finalizer)

(defun alloc (s)
  (let ((new (make-instance 'simfin:foreign-object :fp (cffi:foreign-alloc :char :count s))))
    (loop for i below s do
	 (setf (cffi:mem-aref (simfin:fp new) :char i) 22))
    new))

;; On pc1400, (test 3875) is killed, but (test 3812) completes.
(defun test (size &optional (bytes 1000000000))
  (let* ((f (/ bytes size 10)))
    (format t "f:~A~%" f)
    (loop for j below 10 do
	 (format t "j:~A~%" j)
	 (loop for i below f do
	      (alloc size)
	  ))))
