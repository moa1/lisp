(defun ins- (bits a b)
  (let ((max-1 (1- (ash 1 bits))))
    (logand (- a b) max-1)))

(defclass sic ()
  ((bits :initarg :bits :documentation "The number of bits that one instruction works on.")
   (mem :initarg :mem :documentation "The memory of the SIC.")
   (ins :initarg :ins :initform #'ins- :documentation "The instruction that the SIC uses.")))

(defmethod print-object ((o sic) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A" (slot-value o 'mem))))

(defun make-sic (bits size)
  (let ((max-1 (1- (ash 1 bits))))
    (make-instance 'sic
		   :bits bits
		   :mem  (make-array size :element-type (list 'unsigned-byte max-1) :initial-element 0 :adjustable nil :fill-pointer nil))))

;;(defun eval (sic)
;;  "Use #'INS- to evaluate SIC"
