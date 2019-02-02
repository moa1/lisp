(defstruct str
  (a nil))

(defun fill-ht ()
  (let ((ht (make-hash-table :weakness :key)))
    (loop for i from 0 do
	 (setf (gethash (make-weak-pointer (random 100000000)) ht) nil)
	 (when (= 0 (mod i 10000)) (print (hash-table-count ht))))))

