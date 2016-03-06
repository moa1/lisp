;;;; see https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test how to calculate the U statistic.
;;;; see http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Nonparametric/mobile_pages/BS704_Nonparametric4.html for a table with critical U values for sample sizes in [3;20] (two- and one-sided) and an example of calculating the U statistic.
;;;; http://elegans.som.vcu.edu/~leon/stats/utable.pl calculates the U-statistic table exactly and via Monte-Carlo simulation.

(defparameter *a* '(7 5 6 4 12))
(defparameter *b* '(3 6 4 2 1))

(defun ranks (s1 s2)
  "Return the (1-based) ranks of S1 and S2 (in the list (append S1 S2)).
For example, (ranks '(2 3 1) '(0 2)) == ((5 7/2 2) (7/2 1))."
  ;; TODO: generalize to any number of input sequences, e.g. (ranks s1 s2 s3).
  (let ((s (nconc (loop for i in s1 collect (cons i 1))
		  (loop for i in s2 collect (cons i 2)))))
    (setf s (sort s #'< :key #'car))
    (let ((n-same 1) ;number of same values
	  (r-same-sum 1) ;sum of same ranks
	  (v-same (caar s)) ;most recent same value
	  (n-same-1 (if (= (cdar s) 1) 1 0)) ;number of same values in s1
	  (n-same-2 (if (= (cdar s) 1) 0 1)) ;number of same values in s2
	  (r1 nil)
	  (r2 nil))
      (loop for (v . group) in (cdr s)
	 for r from 2 ;index 1 was left out
	 do
	   (if (= v v-same)
	       (progn
		 (incf n-same)
		 (incf r-same-sum r)
		 (if (= group 1)
		     (incf n-same-1)
		     (incf n-same-2)))
	       (let ((r-avg (/ r-same-sum n-same)))
		 (loop for i below n-same-1 do (push r-avg r1))
		 (loop for i below n-same-2 do (push r-avg r2))
		 (setf n-same 1)
		 (setf r-same-sum r)
		 (if (= group 1)
		     (setf n-same-1 1 n-same-2 0)
		     (setf n-same-1 0 n-same-2 1))
		 (setf v-same v))))
      (let ((r-avg (/ r-same-sum n-same)))
	(loop for i below n-same-1 do (push r-avg r1))
	(loop for i below n-same-2 do (push r-avg r2)))
      (list r1 r2))))

(defun u-stat (s1 s2)
  (let* ((n1 (length s1))
	 (n2 (length s2))
	 (n (+ n1 n2))
	 (ranks (ranks s1 s2))
	 (r1 (apply #'+ (car ranks)))
	 (total-ranks (/ (* n (1+ n)) 2))
	 (r2 (- total-ranks r1))
	 (n-p (* n1 n2)))
    (flet ((u (r n)
	     (+ n-p (/ (* n (1+ n)) 2) (- r))))
      (let ((u1 (u r1 n1))
	    (u2 (u r2 n2)))
	;;(print (list r1 r2 n1 n2 n-p total-ranks u1 u2))
	(min u1 u2)))))
