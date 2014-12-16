(load "~/quicklisp/setup.lisp")
(ql:quickload 'ieee-floats)
(use-package 'ieee-floats)

;;Wikipedia "vocal range" says: In terms of frequency, human voices are roughly in the range of 80 Hz to 1100 Hz (that is, E2 to C6) for normal male and female voices together.
;; Whistling goes from about 500 to 2500 Hz.

(defun read-float32-file (filename)
  (with-open-file (stream filename :element-type '(integer 0 #.(1- (ash 1 32))))
    (let* ((l (file-length stream))
	   (f (make-array l :element-type 'float :initial-element 0.0)))
      (dotimes (i l)
	(setf (aref f i) (decode-float32 (read-byte stream))))
      f)))

(defun write-float32-file (filename sequence)
  "Write the sequence SEQUENCE containing numbers to file name FILENAME."
  (with-open-file (stream filename :direction :output :element-type '(integer 0 #.(1- (ash 1 32))) :if-exists :supersede :if-does-not-exist :create)
    (map nil
	 (lambda (x)
	   (write-byte (encode-float32 x) stream))
	 sequence)))

(defun samplestofreq (samples samplerate &key (offset 0) (min-note-ampl 0.0035))
  "MIN-NOTE-AMPL is the minimum amplitude of a wave to be recorded."
  (let ((i-min nil)
	(i-max nil)
	(a-min nil)
	(a-max nil)
	(dir nil)
	(res nil))
    (setf dir (if (> (aref samples 1) (aref samples 0)) '+ '-))
    (loop for i from 2 below (length samples) do
	 (let ((last (aref samples (1- i)))
	       (a (aref samples i)))
	   ;;(print (list "dir" dir "last" last "a" a))
	   (labels ((emitfreq (i-last-minmax a-last-minmax)
		      (let* ((half-wave (- (1- i) i-last-minmax))
			     (freq (float (/ samplerate (* 2 half-wave))))
			     (ampl (abs (- a-last-minmax last))))
			(when (> ampl min-note-ampl)
			  (push (list (+ offset (1- i)) freq ampl) res))))
		    (watch-minmax ()
		      (if (> a last)
			  (when (eq dir '-)
			    (when (or (not i-max) (emitfreq i-max a-max))
			      (setf dir '+)
			      (setf i-min (1- i))
			      (setf a-min last)))
			  (when (eq dir '+)
			    (when (or (not i-min) (emitfreq i-min a-min))
			      (setf dir '-)
			      (setf i-max (1- i))
			      (setf a-max last))))))
	     (watch-minmax))))
    (nreverse res)))

(defun samplestofreq-range (samples samplerate range-start range-stop &key (min-note-ampl 0.00538))
  (when (null range-stop)
    (setf range-stop (length samples)))
  (let* ((l (- range-stop range-start))
	 (samples-range (make-array l :displaced-to samples :displaced-index-offset range-start)))
    (samplestofreq samples-range samplerate :offset range-start :min-note-ampl min-note-ampl)))

;;;; For the band-pass filters, see Wikipedia articles "Low-pass filter", "High-pass filter", "Band-pass filter".

(defun make-low-pass-rc-filter (sample-rate cutoff)
  "A low-pass RC filter with cutoff frequency CUTOFF for the sample rate SAMPLE-RATE."
  (let* ((rc (/ 1 (* 2 pi cutoff)))
	 (dt (/ 1 sample-rate))
	 (a (coerce (/ dt (+ rc dt)) 'single-float))
	 (1-a (- 1 a)))
    (print (list rc dt a))
    (let ((lasty 0))
      (lambda (x)
	"Return the filtered value X."
	(let* ((y (+ (* a x) (* 1-a lasty))))
	  (setf lasty y))))))

(defun make-high-pass-rc-filter (sample-rate cutoff)
  "A high-pass RC filter with cutoff frequency CUTOFF for the sample rate SAMPLE-RATE."
  (let* ((rc (/ 1 (* 2 pi cutoff)))
	 (dt (/ 1 sample-rate))
	 (a (coerce (/ rc (+ rc dt)) 'single-float)))
    (print (list rc dt a))
    (let ((lastx 0)
	  (lasty 0))
      (lambda (x)
	"Return the filtered value X."
	(let* ((y (+ (* a (+ lasty x (- lastx))))))
	  (setf lastx x)
	  (setf lasty y))))))

(defun make-filter-chain (filter-chain)
  "Return a function of one parameter, which applies the parameter to (car CHAIN), then this result to (cadr CHAIN), and so on.
Returns the final result."
  (labels ((f1 (x filters-rest)
	     (if (null filters-rest)
		 x
		 (f1 (funcall (car filters-rest) x) (cdr filters-rest)))))
    (lambda (x)
      (f1 x filter-chain))))

(defun make-chained-low-pass-rc-filter (chains sample-rate cutoff)
  (make-filter-chain (loop for i below chains collect
			  (make-low-pass-rc-filter sample-rate cutoff))))

(defun make-chained-high-pass-rc-filter (chains sample-rate cutoff)
  (make-filter-chain (loop for i below chains collect
			  (make-high-pass-rc-filter sample-rate cutoff))))

(defun make-phase-locked-loop (sample-rate input-filter loop-filter control-gain pll-freq output-filter)
  "Make a phase-locked loop circuit which tracks the loudest frequency in a specific frequency band."
  ;; See http://www.arachnoid.com/phase_locked_loop/index.html.
  (let ((control 0)
	(integral 0)
	(ref 0)
	(hz (/ 1 sample-rate))
	(time 0)
	(pll-freq-2-pi (coerce (* 2 pi pll-freq) 'single-float)))
    (lambda (x)
      "Return the phase-locked-loop-filtered value X."
      (let* ((x (funcall input-filter x))
	     (c (funcall loop-filter (* x ref control-gain)))
	     (i (+ integral (/ c sample-rate)))
	     (r (cos (* pll-freq-2-pi (+ time i)))))
	(setf control c)
	(setf integral i) ;TODO: wrap integral (at multiples of 1?)
	(setf ref r)
	(incf time hz)
	(funcall output-filter c)))))

(defun pll-sweep-response (seconds sample-rate sweep-from sweep-to noise-level input-filter loop-filter control-gain pll-freq output-filter)
  (let ((output nil)
	(dur seconds)
	(modi 0)
	(cf (/ (+ sweep-from sweep-to) 2))
	(pll (make-phase-locked-loop sample-rate input-filter loop-filter control-gain pll-freq output-filter)))
    (loop
       for n below (* sample-rate dur)
       for time = (/ n sample-rate)
       for freq = (+ sweep-from (* time (- sweep-to sweep-from) (/ 1 dur)))
       do
	 (incf modi (/ (- freq cf) (* cf sample-rate)))
	 (print (list "time" (float time)))
	 (let* ((x1 (cos (coerce (* 2 pi cf (+ time modi)) 'single-float)))
		(x2 (cos (coerce (* 2 pi 1423 time) 'single-float)))
		(noise (1- (random 2.0)))
		(x (+ (* x1 0.4) (* x2 0.1) 0.25 (* noise noise-level)))
		(y (funcall pll x)))
	   (push y output)))
    (print "loop done")
    (nreverse output)))

(defun write-table (filename separator &rest columns)
  "Write to file name FILENAME the sequences COLUMNS (which have to be of equal length) separated by string SEPARATOR."
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (apply #'map
	   nil
	   (lambda (&rest rest)
	     (format stream "~A" (car rest))
	     (dolist (r (cdr rest))
	       (format stream "~A~A" separator r))
	     (format stream "~%"))
	   columns)))

(defun pll-test (output-filename &key (seconds 4))
  (let* ((sample-rate 48000)
	 (input-filter (make-chained-high-pass-rc-filter 2 sample-rate 100))
	 (loop-filter (lambda (x) x))
	 (output-filter (make-chained-low-pass-rc-filter 2 sample-rate 10))
	 (control-gain 64)
	 (pll-freq 440)
	 (sweep-from 0)
	 (sweep-to 2750)
	 (noise-level 0.1)
	 (output (pll-sweep-response seconds sample-rate sweep-from sweep-to noise-level input-filter loop-filter control-gain pll-freq output-filter))
	 (freq-inc (coerce (/ (- sweep-to sweep-from) (length output)) 'single-float))
	 (freqs (do ((n 0 (1+ n)) (f sweep-from (+ f freq-inc)) (out nil)) ((>= n (length output)) (nreverse out)) (when (= 0 (mod n 1000)) (print n)) (push f out))))
    (print (list (length freqs) (length output)))
    (write-table output-filename " " freqs output)))

;;(pll-test "/tmp/test.dat")

(defun phase-locked-loop-on-sequence (input-sequence sample-rate input-filter-cutoff control-gain pll-freq output-filter-cutoff)
  (let* ((input-filter (make-chained-high-pass-rc-filter 2 sample-rate input-filter-cutoff))
	 (loop-filter (lambda (x) x))
	 (output-filter (make-chained-low-pass-rc-filter 2 sample-rate output-filter-cutoff))
	 (pll (make-phase-locked-loop sample-rate input-filter loop-filter control-gain pll-freq output-filter))
	 (out nil)
	 (n 0))
    (map nil
	 (lambda (x)
	   (when (= 0 (mod n 1000))
	     (format t "~A/~A~%" n (length input-sequence)))
	   (incf n)
	   (push (funcall pll x) out))
	 input-sequence)
    (nreverse out)))


(defparameter *a* (read-float32-file "~/mic2midi/whistle2_48kHz_32bit_float.raw"))
(defparameter *shaky* (make-array 1400 :displaced-to *a* :displaced-index-offset 359000))

;;(write-float32-file "/tmp/test.dat" (phase-locked-loop-on-sequence *shaky* 48000 100 64 440 10))
;;(write-float32-file "/tmp/testwhistle.dat" (phase-locked-loop-on-sequence *a* 48000 100 64 440 10))
