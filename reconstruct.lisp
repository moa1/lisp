(load "/home/toni/lisp/rbm.lisp")

(defun rbm-reconstruction-error (data rbm &key (neg-data (rbm-v-from-h (rbm-h-from-v data rbm) rbm)))
  (when (not (equal (array-element-type data) 'single-float))
    (setf data (copy-array data :array-element-type 'single-float)))
  (when (not (equal (array-element-type neg-data) 'single-float))
    (setf neg-data (copy-array neg-data :array-element-type 'single-float)))
  (let* ((n-cases (array-dimension data 0))
	 (n-v (rbm-n-v rbm))
	 (err-array (make-array (list n-cases n-v) :element-type 'single-float)))
    (array-array-fun data neg-data (lambda (a b) (declare (type single-float a b)) (expt (- a b) 2)) err-array)
    (aref (the (simple-array single-float) (array-project (the (simple-array single-float) (array-project err-array #'+)) #'+)))))

(defun num-errors (data tries &key (rbm-learn-function #'rbm-learn-minibatch))
  (let ((errors nil))
    (loop for i below tries do
	 (let* ((rbm (funcall rbm-learn-function *data* (new-rbm 6 3 :v-binary 6 :h-softmax '(3)) .01 0 .002 10000))
		(err (rbm-reconstruction-error data rbm)))
	   (push err errors)))
    errors))

;;(defparameter *errors-4b978c8* (num-errors *data* 10 :rbm-learn-function #'rbm-learn))
;;(0.035471544 0.037396763 0.033422336 0.037201185 0.03520447 0.03634293 2.0073388 0.037223883 0.03491285 0.034689706)

;;(defparameter *errors-ea2e566* (num-errors *data* 10))
;;(0.042258292 0.034710754 0.03378935 0.03479761 0.03765295 2.004737 0.03518253 2.002512 0.037795715 0.033678286)
