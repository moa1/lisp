(asdf:oos 'asdf:load-op 'utils)


(defun estimate-mean (max-sd min-samples next-sample)
  "Get (number MIN-SAMPLES) samples by calling NEXT-SAMPLE this often. If the
standard deviation is greater than MAX-SD, get another sample and repeat,
otherwise return the mean of all samples."
  (warn "theoretic foundation lacking: sample need not be normally distributed")
  ;; re-implement this function:
  ;; if the samples come from a normal distribution, then use the t-distribution
  ;; to determine the confidence interval.
  ;; like here: http://wiki.stat.ucla.edu/socr/index.php/AP_Statistics_Curriculum_2007_Estim_S_Mean
  ;; (described at "Interval Estimation of a Population Mean")
  ;; if the sample comes from another distribution, then:
  ;; - find out what the distribution might be? (and construct a point estimator
  ;;   for this distribution)
  ;; - another method?
  (labels ((rec (samples)
	     (multiple-value-bind (mean sd n)
		 (statistics:mean-sd-n samples)
	       (format t "mean:~8F sd:~F n:~A max-sd:~F~%" mean sd n max-sd)
	       (if (<= (statistics:sd samples) max-sd)
		   (values mean sd n)
		   (rec (cons (funcall next-sample) samples))))))
    (let ((samples (loop repeat min-samples collect (funcall next-sample))))
      (rec samples))))

