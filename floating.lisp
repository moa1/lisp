
(defun floating (stream number colonp atp 
                 &optional (*print-base* 10) (num-digits 10)
                 &rest args)
  "Taken from [https://stackoverflow.com/questions/21103985/floating-point-formatted-to-base-16] by Joshua Taylor."
  (declare (ignore colonp args))
  ;; If the number is negative, print the #\- and invert the number.
  ;; Otherwise, the number is non-negative, and if an @ was provided
  ;; we print a leading #\+.
  (cond
    ((minusp number)
     (write-char #\- stream)
     (setq number (- number)))
    (atp
     (write-char #\+ stream)))
  ;; Print number, which is now guaranteed to be positive.  Begin by
  ;; taking its integer part and printing it, followed by a point.
  ;; Then, pull individual places and write them.  This continues,
  ;; updating quotient and remainder by multiplying the remainder by
  ;; the base and taking the floor again until either the remainder
  ;; becomes zero, or we've reached the maximum number of digits.
  (multiple-value-bind (quotient remainder) (floor number 1.0)
    (write quotient :stream stream)
    (write-char #\. stream)
    (do ((num-digits num-digits (1- num-digits)))
        ((or (zerop remainder) (zerop num-digits)))
      (multiple-value-setq (quotient remainder)
        (floor (* *print-base* remainder)))
      (write quotient :stream stream))))

(defun test ()
  (loop for f in '(-1024.0 -4.0 -3.0 -2.0 -1.0 -0.4 -0.3 -0.2 -0.1 0.0 0.1 0.2 0.3 0.4 1.0 2.0 3.0 4.0 1024.0) do
       (format t "~A == ~16/floating/ ~A~%" f f (multiple-value-list (integer-decode-float f)))))
