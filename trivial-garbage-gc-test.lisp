;; trivial garbage automatic garbage collection test

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :trivial-garbage)
(ql:quickload :cffi)

(defclass jit-state (standard-object)
  ((ptr :initarg :ptr :reader jit-state-ptr))
  (:documentation "jit-state-t object"))

(defparameter *free-number* 0)

(defun allocate-and-use (bytes)
  (declare (optimize (speed 3) (debug 0)))
  #+(or sbcl clisp) (let* ((ptr (cffi:foreign-alloc :char :count bytes)))
		      (loop for i fixnum below bytes do
			   (setf (cffi:mem-aref ptr :char i) 1))
		      ptr)
  #+null (cffi:foreign-alloc :char :initial-element 1 :count bytes))
;;(utils:timesec (lambda () (free-state-ptr (allocate-and-use 10000))))

(defun free-state-ptr (state-ptr)
  (incf *free-number*)
  (cffi:foreign-free state-ptr))

(defun new-state (bytes)
  "Create a new JIT state that is automatically garbage-collected.
Note that you may NOT call #'FN-JIT-CLEAR-STATE or #'FN-JIT-DESTROY-STATE on PTR."
  (let* ((ptr (allocate-and-use bytes)) ;use the allocated region to make sure it is in physical memory.
	 (new (make-instance 'jit-state :ptr ptr)))
    (tg:finalize new (lambda () (free-state-ptr ptr)))
    new))

;; on pc1400, CLISP is killed when (test 300000 :rep 100 :explicit-gc nil) arrives at: "free has been called 856 times" "65/100 (allocating 33 times 300000 bytes)"
;; on pc1400, SBCL is killed for (test 2721 :rep 100 :explicit-gc nil), but (test 2720 :rep 100 :explicit-gc nil) works.


(defun test (bytes &key (mb 10) (rep 10) (explicit-gc nil))
  "Allocate a number of chunks with BYTES length so that MB Megabytes are allocated in total. Repeat this REP times."
  (setf *free-number* 0)
  (let* ((reptotal (* mb 1000000))
	 (count (floor (/ reptotal bytes))))
    (format t "(test ~A :mb ~A :rep ~A)~%" bytes mb rep)
    (format t "If automatic garbage collection is not triggerd, we will use ~A MB in total.~%" (* mb rep))
    (finish-output)
    (tg:gc :full t)
    ;; In the loop, I'm not calling #'TG:GC on purpose to test automatic garbage collection.
    (loop for r below rep do
	 (when explicit-gc
	   (tg:gc))
	 (format t "~A/~A (allocating ~A times ~A bytes)~%" r rep count bytes)
	 (finish-output)
	 (loop for i below count collect (new-state bytes))
	 (format t "free has been called ~A times~%" *free-number*)
	 (finish-output))))
