(defclass group ()
  (;; the identity element
   (identity :accessor group-identity
	     :initarg :identity)
   ;; for one element of the set, return the next
   (set-next :type (function (t) t)
	     :accessor group-set-next
	     :initarg :set-next)
   ;; for two elements of the set, return a third
   (operation :type (function (t t) t)
	      :accessor group-operation
	      :initarg :operation)
   ;; return the inverse of an element
   (inverse :type (function (t) t)
	    :accessor group-inverse
	    :initarg :inverse)
   ;; self-documentating superflous nonsense ;-)
   (randomreadonlyclassvalue :reader group-randomreadonlyclassvalue
			     :initform (random 10)
			     :allocation :class)))

;;(defmethod operation (el1 el2)
;;  (funcall (group-operation el1) el1 el2))

(make-instance 'group
	       :identity 0
	       :set-next (lambda (x) (cond ((zerop x) (1+ x))
					   ((plusp x) (- x))
					   ((minusp x) (1+ (- x)))))
	       :operation #'+
	       :inverse #'-)

;;(defmethod ((g group))
;;  (prind "group" g))
;;
;;(defgeneric a+ (&rest args)
;;  (:documentation "Return the sum of the ARGS as the most specific type."))


(defun a+ (&rest args)
  