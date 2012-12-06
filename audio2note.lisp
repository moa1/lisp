(defparameter nn (expt 2 (/ 1 12)))

(defun print-octave (pitch)
  (dotimes (i 12)
    (format t "~A~%" (* pitch (expt nn i)))))

(defun note-diff (pitch1 pitch2)
  ;; nn**d = p2/p1
  ;; d*log(nn) = log(p2/p1)
  ;; d = log(p2/p1)/log(nn)
  (/ (log (/ pitch2 pitch1)) (log nn)))

