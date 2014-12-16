;; see example of binary-types, and practical common lisp:parsing binary files

(asdf:oos 'asdf:load-op 'utils)
(asdf:oos 'asdf:load-op 'binary-types)

(binary-types:define-binary-class one-long ()
  ((mean :binary-type 'binary-types:s32)))

(defparameter binary-file "/mnt/mehr/platte/tmp/microarrays/GSM14942.CEL.gz.raw")

(defparameter the-cel-names (mapcar
			     (lambda (x)
			       (concatenate 'string 
					    "/mnt/mehr/platte/tmp/microarrays/"
					    x))
			     (list "GSM14941.CEL.gz.raw"
				   "GSM14942.CEL.gz.raw"
				   "GSM14943.CEL.gz.raw"
				   "GSM14944.CEL.gz.raw"
				   "GSM14945.CEL.gz.raw"
				   "GSM14946.CEL.gz.raw"
				   "GSM14947.CEL.gz.raw"
				   "GSM14948.CEL.gz.raw"
				   "GSM3921.CEL.gz.raw"
				   "GSM3923.CEL.gz.raw"
				   "GSM3925.CEL.gz.raw"
				   "GSM3927.CEL.gz.raw"
				   "GSM3929.CEL.gz.raw"
				   "GSM3931.CEL.gz.raw"
				   "GSM3933.CEL.gz.raw"
				   "GSM3935.CEL.gz.raw"
				   "GSM3937.CEL.gz.raw"
				   "GSM3939.CEL.gz.raw"
				   "GSM9514.CEL.gz.raw"
				   "GSM9515.CEL.gz.raw"
				   "GSM9516.CEL.gz.raw"
				   "GSM9517.CEL.gz.raw"
				   "GSM9522.CEL.gz.raw"
				   "GSM9524.CEL.gz.raw"
				   "GSM9525.CEL.gz.raw"
				   "GSM9526.CEL.gz.raw"
				   "GSM9852.CEL.gz.raw"
				   "GSM9861.CEL.gz.raw"
				   "GSM9862.CEL.gz.raw"
				   "GSM9863.CEL.gz.raw"
				   "GSM9864.CEL.gz.raw"
				   "GSM9932.CEL.gz.raw"
				   "GSM9933.CEL.gz.raw"
				   "GSM9934.CEL.gz.raw")))

(defun read-raw-cel-slow (filename)
;;   (let ((binary-types:*endian* :little-endian))
;;     (with-open-file (stream filename :element-type '(unsigned-byte 8))
;;       (let ((val1 (binary-types:read-binary 'one-long stream))
;; 	    (val2 (binary-types:read-binary 'one-long stream)))
;; 	(format t "~A ~A~%" (slot-value val1 'mean) (slot-value val2 'mean)))))
;;  
  (let ((binary-types:*endian* :little-endian)
	(means (make-array (* 712 712) :element-type 'integer)))
    (with-open-file (stream filename :element-type '(unsigned-byte 8))
      (loop for i from 0 below (* 712 712)
	 do
	   (let ((val1 (binary-types:read-binary 'one-long stream)))
	     (setf (aref means i) (slot-value val1 'mean)))))
    means))

(defun read-raw-cel (filename)
  (let ((means (make-array (* 712 712) :element-type 'integer)))
    (with-open-file (stream filename :element-type '(unsigned-byte 8))
      (loop for i from 0 below (* 712 712)
	 do
	   (let* ((v1 (read-byte stream))
		 (v2 (read-byte stream))
		 (v3 (read-byte stream))
		 (v4 (read-byte stream))
		 (result (+ (* 256 256 256 v4) (* 256 256 v3)
			    (* 256 v2) v1)))
	     (setf (aref means i) result))))
    means))


;; (timeit (1) (mapcar (lambda (x) (read-raw-cel x)) the-cel-names))
(defparameter the-cels
  (mapcar (lambda (x) (read-raw-cel x)) the-cel-names))

(defun pair-fraction-diff (p1 p2 ca cb)
  (let ((v1a (aref ca p1))
	(v2a (aref ca p2))
	(v1b (aref cb p1))
	(v2b (aref cb p2)))
    (if (= 0 v2a)
	(if (= 0 v2b)
	    0
	    nil)
	(if (= 0 v2b)
	    nil
	    (abs (- (/ v1a v2a) (/ v1b v2b) 0.0))))))

(defun sample (c1 c2)
  (let ((arraylength (if (= (length c1) (length c2))
			 (length c1)
			 (error "lengths!="))))
    (labels ((random-pair ()
	       (let ((p1 (random arraylength)))
		 (labels ((next ()
			    (let ((p2 (random arraylength)))
			      (if (= p1 p2)
				  (next)
				  p2))))
		   (values p1 (next)))))
	     (iterate (diffsum n)
	       (if (> n 100000)
		   (/ diffsum n)
		   (multiple-value-bind (p1 p2)
		       (random-pair)
		     (let ((diff (pair-fraction-diff p1 p2 c1 c2)))
		       ;;(prind diff)
		       (if (null diff)
			   (iterate diffsum n)
			   (iterate (+ diffsum diff) (1+ n))))))))
      (iterate 0 0))))

(defun rma-normalize (cels)
  (assert (apply #'all-eq-lengths cels) (cels)
	  "cels must have equal lengths" cels)
  ;; 1: Given n arrays of length p, form X of dimension p × n where each array
  ;; is a column.
  ;; 2: Sort each column of X to give Xsort .
  ;; 3: Take the means across rows of Xsort and assign this mean to each
  ;; element in the row to get quantile equalized Xsort .
  ;; 4: Get Xnormalized by rearranging each column of Xsort to have the same
  ;; ordering as original X.
  (let* ((len (length (car cels)))
	 (x (mapcar (lambda (x) (declare (ignore x)) (make-array len)) cels))
	 (xsum (make-array len :initial-element 0)))
    (loop for c in cels for xc in x for count from 0 do
	 (prind count)
	 (loop for i below len do
	      (setf (aref xc i) (cons i (aref c i))))
	 (sort xc #'< :key #'cdr)
	 (loop for i below len do
	      (setf (aref xsum i) (+ (aref xsum i) (cdr (aref xc i))))
	      (setf (aref xc i) (car (aref xc i)))))
    (loop for i below len do
	 (setf (aref xsum i) (/ (aref xsum i) (length cels) 1.0)))
    (loop for xc in x for count2 from 0 do
	 (prind count2)
	 (loop for i below len do
	      (setf (elt xc i) (cons (elt xc i) (elt xsum i))))
	 (sort xc #'< :key #'car)
	 (loop for i below len do
	      (setf (elt xc i) (cdr (elt xc i)))))
    x))

;;(defparameter the-norm-cels (rma-normalize the-cels))




;;(defun mindiff-fraction (c1 c2)
;;  (let* ((min-i (elt c1 0))
;;	 (min-j (elt c2 0))
;;	 (min-diff ()))
;;    (loop for i from 0 below (* 712 712) do
;;	 (loop for j from (1+ i) below (* 712 712) do
;;	      (let ((diff
