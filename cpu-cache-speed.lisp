;;;; measure the different cache sizes of the CPU.

(load "~/quicklisp/setup.lisp")
(ql:quickload 'utils)
(use-package 'utils)

(defun vector-speed-read (size)
  "Returns the time in seconds needed to sum up a vector of size SIZE."
  (flet ((add-vector (v)
	   (loop for e across v sum e)))
    (let ((v (make-array size :element-type 'integer :initial-element 0)))
;;      (/ 1.0 (nth-value 2 (timecps (10 :stats t :time 2.0) (add-vector v)))))))
      (timesec (lambda () (add-vector v))))))

(defun vector-speed-write (size)
  "Returns the time in seconds needed to fill a vector of size SIZE."
  (flet ((fill-vector (v size)
	   (loop for i below size do (setf (svref v i) 0))))
    (let ((v (make-array size :element-type 'integer :initial-element 0)))
;;      (/ 1.0 (nth-value 2 (timecps (10 :stats t :time 2.0) (fill-vector v size)))))))
      (timesec (lambda () (fill-vector v size))))))

(defun tabulate-speed-per-element-by-array-size (sizes &optional (time-funcs (list #'vector-speed-read #'vector-speed-write)))
  "Return, for a list of vector sizes SIZES, the time needed to operate over such a vector divided by the vector size."
  (loop for size in sizes collect
       (progn
	 (print (list "size" size))
	 (cons size
	       (loop for time-func in time-funcs collect
		    (let ((tpe (coerce (/ (funcall time-func size) size) 'long-float)))
		      (print (list "time-func" time-func "time-per-element" tpe))
		      tpe))))))

;; the following results are from measuring with timecps, and they are not reproducible with timesec.
;;(tabulate-speed-per-element-by-array-size '(10 100 1000 10000 100000 1000000 10000000)) === '((10 9.560539e-9 1.8632809e-8) (100 5.8476446e-9 1.46406265e-8) (1000 5.8213936e-9 1.8672667e-8) (10000 6.6687504e-9 1.415625e-8) (100000 6.7202794e-9 1.4248001e-8) (1000000 6.6354837e-9 1.422308e-8) (10000000 6.666667e-9 1.426e-8)). This shows that there is a jump in reading speed between vector sizes of 1000 and 10000, but the speed of writing the array is constant for different matrix sizes.

;; the following results are from measuring with timecps, and they are not reproducible with timesec.
;;(tabulate-speed-per-element-by-array-size '(2140 2141 2142 2143 2144 2145 2146 2147 2148 2149 2150) (list #'vector-speed-read)) === '((2140 5.729424e-9) (2141 5.5931806e-9) (2142 5.5505107e-9) (2143 6.3484484e-9) (2144 6.379523e-9) (2145 6.372029e-9) (2146 6.3810575e-9) (2147 6.382452e-9) (2148 6.3809353e-9) (2149 6.583731e-9) (2150 6.615553e-9)). This might indicate that L1 cache is of size 2142 integers == 8568 bytes.

(defun vector-sum (v)
  (loop for e across v sum e))

(quote (let ((v (make-array 1000 :element-type 'integer :initial-element 0)))
	 (timesec (lambda () (vector-sum v)))))
