;; C Output:
;; double time:10.202179
;; float time:10.183021
;; int time:2.262741

(defun speed-add-double ()
  "Takes 5.393 seconds, which is faster than the C counterpart (see speed_double_vs_int.c)"
  (let ((a 0.0)
	(b 1.5)
	(start (get-internal-real-time)))
    (loop for i below 1000000000 do
	 (incf a b))
    (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

(defun speed-add-integer ()
  "Takes 91.684 seconds. Actually this is an unfair comparison, since LISP uses bignums."
  (let ((a 0)
	(b 15)
	(start (get-internal-real-time)))
    (loop for i below 1000000000 do
	 (incf a b))
    (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

(defun run-tests ()
  (format t "double time: ~A~%" (speed-add-double))
  (format t "integer time: ~A~%" (speed-add-integer)))
