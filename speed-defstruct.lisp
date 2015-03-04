(defstruct (dcons
	     (:constructor make-dcons-default)
	     (:constructor make-dcons (slot1 slot2 slot3)))
  slot1
  slot2
  slot3)

(defstruct (dcons-vector
	     (:constructor make-dcons-vector-default)
	     (:constructor make-dcons-vector (slot1 slot2 slot3))
	     (:type (vector t)))
  slot1
  slot2
  slot3)

(time (loop for i below 100000000 do (make-dcons-default :slot1 nil :slot2 nil :slot3 nil)))
(time (loop for i below 100000000 do (make-dcons nil nil nil)))
(time (loop for i below 100000000 do (make-dcons-vector-default :slot1 nil :slot2 nil :slot3 nil)))
(time (loop for i below 100000000 do (make-dcons-vector nil nil nil)))

