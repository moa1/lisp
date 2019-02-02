;;;; Fixed-point implementation.

;;;; Type definition

(defmacro prind (&rest args)
  "Print args"
  (let ((i (gensym)))
    `(progn
       ,@(loop for a in args collect
	      (if (eq a T)
		  `(format t "~%")
		  `(progn
		     (format t "~A:" ,(format nil "~A" a))
		     (dolist (,i (multiple-value-list ,a))
		       (prin1 ,i)
		       (princ " ")))))
       (format t "~%"))))

(defclass fixed (standard-object)
  ((integer-bits :initarg :integer-bits :reader fixed-integer-bits :type (and fixnum unsigned-byte) :documentation "The (read-only) number of bits of the number before the point.")
   (fraction-bits :initarg :fraction-bits :reader fixed-fraction-bits :type (and fixnum unsigned-byte) :documentation "The (read-only number) of bits of the number after the point.")
   (number :initarg :number :accessor fixed-number :type integer :documentation "The number represented as INTEGER-BITS bits of the integer value, followed by FRACTION-BITS of the fractional value."))
  (:documentation "A fixed-point number, represented by an integer number."))

;; These declaims are to speed up SBCL that ignores the :type declarations in defclass.
(declaim (ftype (function (fixed) (and fixnum unsigned-byte)) fixed-integer-bits))
(declaim (ftype (function (fixed) (and fixnum unsigned-byte)) fixed-fraction-bits))
(declaim (ftype (function (fixed) integer) fixed-number))
(declaim (ftype (function (fixed integer)) (setf fixed-number)))

;;;; Helper functions

(defun signed-integer-range (bits)
  "Return as first/second value the lowest/highest number representable by a signed integer number with BITS bits."
  (declare (type (and unsigned-byte fixnum) bits))
  (if (= 0 bits)
      (values 0 0)
      (let ((bits-1 (1- bits)))
	(values (- (expt 2 bits-1))
		(1- (expt 2 bits-1))))))

(defun unsigned-integer-range (bits)
  "Return as first/second value the lowest/highest number representable by a unsigned integer number with BITS bits."
  (declare (type (and unsigned-byte fixnum) bits))
  (values 0 (1- (expt 2 bits))))

(defun to-rational (fixed)
  "Convert fixed-point number FIXED to a rational."
  (/ (fixed-number fixed) (ash 1 (fixed-fraction-bits fixed))))

(defun to-float (fixed)
  "Convert fixed-point number FIXED to a float."
  (float (to-rational fixed)))

(defconstant +print-radix-symbols+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun print-integer (integer &optional stream)
  (declare (type integer integer))
  (when (= integer 0)
    (princ "0" stream)
    (return-from print-integer))
  (when (< integer 0)
    (princ "-" stream))
  (do* ((i (abs integer)) (res nil)) ((= i 0) (loop for c in res do (princ (aref +print-radix-symbols+ c) stream)))
    (multiple-value-bind (a b) (floor i *print-base*)
      (setf i a)
      (push b res))))

(defvar *print-precision* 15 "NIL or an integer. An integer means the maximal number of digits to print when a fraction is printed. NIL means that an infinite number of digits is printed.")

(defun print-fraction (fraction &optional stream)
  (declare (type (or rational float) fraction))
  (assert (and (<= 0 fraction) (< fraction 1)))
  (princ "." stream)
  ;; TODO: add a variable *print-rounding* that controls how the last digit of the fraction is rounded, with possible values :truncate (truncate after *print-precision*), :truncate-indicate-rest (truncate after *print-precision* and print "..." if the rest is not 0) or :round (round last digit to the nearest possible digit).
  (do* ((i (* fraction *print-base*)) (n 0 (1+ n))) ((or (= i 0) (and (not (null *print-precision*)) (>= n *print-precision*))))
    (multiple-value-bind (a b) (floor (* i *print-base*) *print-base*)
      (princ (aref +print-radix-symbols+ a) stream)
      (setf i b))))

(defun print-number (number &optional stream)
  (declare (type (or rational float) number))
  (when (< number 0)
    (princ "-" stream))
  (multiple-value-bind (integer fraction) (truncate number)
    (setf integer (abs integer))
    (setf fraction (abs fraction))
    (print-integer integer stream)
    (print-fraction fraction stream)))

(defmethod describe-object ((f fixed) stream)
  (let* ((integer-bits (fixed-integer-bits f))
	 (fraction-bits (fixed-fraction-bits f))
	 (number (fixed-number f))
	 (denom (expt 2 fraction-bits)))
    (format stream "fixed-point Q~A.~A number " integer-bits fraction-bits)
    (multiple-value-bind (integer fraction) (truncate number denom)
      (print-number (+ integer (/ fraction denom)) stream))
    (format stream " (~A)~%" number)))

(defmethod print-object ((f fixed) stream)
  (let* (;(integer-bits (fixed-integer-bits f))
	 (fraction-bits (fixed-fraction-bits f))
	 (number (fixed-number f))
	 (denom (expt 2 fraction-bits)))
    (multiple-value-bind (integer fraction) (truncate number denom)
      (print-number (+ integer (/ fraction denom)) stream))))

(defun to-fixed (number &optional (integer-bits 16) (fraction-bits 16) (fraction-rounding #'round))
  "Convert NUMBER to a fixed-point number with INTEGER-BITS bits before the point, and FRACTION-BITS bits after the point and return it.
FRACTION-ROUNDING is a function that rounds the fractional number to the nearest representable number (for example, one of #'round, #'floor, #'ceiling).
Return as 2nd value the rest that was not representable."
  (declare (type (and fixnum unsigned-byte) integer-bits fraction-bits))
  ;; This function has to check every input, because #'make-instance doesn't check the inputs.
  (etypecase number
    ((or integer float rational)
     ;;(prind number)
     (multiple-value-bind (integer fraction) (floor number)
       ;;(prind integer fraction)
       (assert (multiple-value-bind (low high) (signed-integer-range integer-bits) (<= low integer high)))
       (let* ((denom (expt 2 fraction-bits)))
	 ;;(prind denom)
	 (multiple-value-bind (fraction rest) (funcall fraction-rounding (* fraction denom))
	   ;;(prind fraction rest)
	   (when (= fraction denom)
	     (incf integer 1)
	     (decf fraction denom)
	     (assert (multiple-value-bind (low high) (signed-integer-range integer-bits) (<= low integer high))))
	   (assert (multiple-value-bind (low high) (unsigned-integer-range fraction-bits) (<= low fraction high)))
	   (values
	    (let ((number (+ (ash integer fraction-bits) fraction)))
	      (make-instance 'fixed :integer-bits integer-bits :fraction-bits fraction-bits :number number))
	    (/ rest denom))))))))

;; negative numbers
(assert (multiple-value-bind (fixed rest) (to-fixed -345/100 3 1 #'floor)
	  (and (= (to-rational fixed) -350/100) (= rest 5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -345/100 3 1 #'round)
	  (and (= (to-rational fixed) -350/100) (= rest 5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -345/100 3 1 #'ceiling)
	  (and (= (to-rational fixed) -300/100) (= rest -45/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -315/100 3 1 #'floor)
	  (and (= (to-rational fixed) -350/100) (= rest 35/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -315/100 3 1 #'round)
	  (and (= (to-rational fixed) -300/100) (= rest -15/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed -315/100 3 1 #'ceiling)
	  (and (= (to-rational fixed) -300/100) (= rest -15/100))))
;; positive-numbers
(assert (multiple-value-bind (fixed rest) (to-fixed 345/100 3 1 #'floor)
	  (and (= (to-rational fixed) 300/100) (= rest 45/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 345/100 3 1 #'round)
	  (and (= (to-rational fixed) 350/100) (= rest -5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 345/100 3 1 #'ceiling)
	  (and (= (to-rational fixed) 350/100) (= rest -5/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 315/100 3 1 #'floor)
	  (and (= (to-rational fixed) 300/100) (= rest 15/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 315/100 3 1 #'round)
	  (and (= (to-rational fixed) 300/100) (= rest 15/100))))
(assert (multiple-value-bind (fixed rest) (to-fixed 315/100 3 1 #'ceiling)
	  (and (= (to-rational fixed) 350/100) (= rest -35/100))))

(defun test-to-fixed (&key (n 1000))
  "Test #'TO-FIXED and #'TO-RATIONAL by creating N random numbers converting them to and from FIXED."
  (loop for i below n do
       (let* ((integer-bits (random 10))
	      (fraction-bits (random 10))
	      (integer (if (> integer-bits 0) (- (random (expt 2 integer-bits)) (expt 2 (1- integer-bits))) 0))
	      (fraction (/ (random (expt 2 fraction-bits)) (expt 2 fraction-bits)))
	      (number (+ integer fraction)) ;NUMBER is exactly representable with INTEGER-BITS and FRACTION-BITS.
	      (fixed0 (to-fixed number integer-bits fraction-bits #'ceiling))
	      (fixed1 (to-fixed number integer-bits fraction-bits #'floor))
	      (rational0 (to-rational fixed0))
	      (rational1 (to-rational fixed1)))
	 (assert (= rational0 rational1 number) nil "integer-bits:~A fraction-bits:~A number:~A fixed0:~A fixed1:~A rational0:~A rational1:~A" integer-bits fraction-bits number fixed0 fixed1 rational0 rational1))))
(test-to-fixed)

(defun most-positive-fixed (integer-bits fraction-bits)
  "The largest positive fixed-point number with INTEGER-BITS bits for the integer part of the number and FRATION-BITS bits for the fractional part."
  (nth-value
   0
   (to-fixed
    (+ (nth-value 1 (signed-integer-range integer-bits))
       (/ (nth-value 1 (unsigned-integer-range fraction-bits)) (expt 2 fraction-bits)))
    integer-bits
    fraction-bits)))

(defun least-positive-fixed (integer-bits fraction-bits)
  "The smallest positive fixed-point number with INTEGER-BITS bits for the integer part of the number and FRATION-BITS bits for the fractional part."
  (nth-value
   0
   (to-fixed
    (+ 0
       (/ 1 (expt 2 fraction-bits)))
    integer-bits
    fraction-bits)))

(defun least-negative-fixed (integer-bits fraction-bits)
  "The largets negative fixed-point number with INTEGER-BITS bits for the integer part of the number and FRATION-BITS bits for the fractional part."
  (nth-value
   0
   (to-fixed
    (- 0
       (/ 1 (expt 2 fraction-bits)))
    integer-bits
    fraction-bits)))

(defun most-negative-fixed (integer-bits fraction-bits)
  "The largest positive fixed-point number with INTEGER-BITS bits for the integer part of the number and FRATION-BITS bits for the fractional part."
  ;;TODO check this is correct, for example: CL-USER> (describe (most-negative-fixed 3 2)) gives fixed-point Q3:2 number -4.75 (-19)
  (nth-value
   0
   (to-fixed
    (- (nth-value 0 (signed-integer-range integer-bits))
       0)
    integer-bits
    fraction-bits)))

;;;; Arithmetic
;;; The idea of the following functions is that you can compute the resulting number by first dividing the fixed numbers by their denominators, then performing the 2-ary operation on the 2 numbers, and then multiplying the result with the denominator of the result.
;; (defun f* (a b &optional ri rf (rounder #'round))
;;   (with-slots ((an number) (af fraction-bits)) a
;;     (with-slots ((bn number) (bf fraction-bits)) b
;;       (let* ((adenom (expt 2 af))
;; 	     (bdenom (expt 2 bf))
;; 	     (rdenom (expt 2 rf))
;; 	     ;;(rn (* (* (/ an adenom) (/ bn bdenom)) rdenom)))
;; 	     (rn (/ (* an bn rdenom) (* adenom bdenom))))


;;; Addition

(defun f2+ (asym bsym an bn af bf ri rf)
  `((+ ,an ,bn)
    ))

(defun f2- (asym bsym an bn af bf ri rf)
  `((- ,an ,bn)
    ))

(defun f2* (asym bsym an bn af bf ri rf)
  `((* ,an ,bn)))

(defun f* (a b &optional ri rf (rounder #'round))
  (with-slots ((an number) (af fraction-bits)) a
    (with-slots ((bn number) (bf fraction-bits)) b
      (let* ((adenom (expt 2 af))
	     (bdenom (expt 2 bf))
	     (rdenom (expt 2 rf))
	     ;;(rn (* (* (/ an adenom) (/ bn bdenom)) rdenom)))
	     (rn (/ (* an bn rdenom) (* adenom bdenom))))
	(multiple-value-bind (number rest) (funcall rounder rn)
	  (values
	   (make-instance 'fixed :integer-bits ri :fraction-bits rf :number (funcall rounder number))
	   (/ rest rdenom)))))))

(defun f/ (a b &optional ri rf (rounder #'round))
  (with-slots ((an number) (af fraction-bits)) a
    (with-slots ((bn number) (bf fraction-bits)) b
      (let* ((adenom (expt 2 af))
	     (bdenom (expt 2 bf))
	     (rdenom (expt 2 rf))
	     ;;  (an/adenom)/(bn/bdenom)*rdenom
	     ;;(rn (* (/ (/ an adenom) (/ bn bdenom)) rdenom)))
	     (rn (/ (* an bdenom rdenom) (* adenom bn))))
	(multiple-value-bind (number rest) (funcall rounder rn)
	  (values
	   (make-instance 'fixed :integer-bits ri :fraction-bits rf :number (funcall rounder number))
	   (/ rest rdenom)))))))

;; TODO: let the simple arithmetic functions be of two arguments, and let them return the code to do the function on two fixed-point numbers of equal precision (maybe also return a version where the operation is done on two fixed-point numbers with unequal precision, and allow specifying the integer-bits and fraction-bits of the target).
;; TODO: write a function that defines many functions f+,f-,... which all accept between 0(for +,*) or 1(for -,/) and infinity arguments in two versions: those which perform type-checking and type-conversion (maybe also allow specifying the resuling precision), and those who don't.
