(defmacro prind (&rest args)
  "Print args"
  ;; TODO: modify the pretty print dispatch table so that it prints representations readable by #'READ. (especially modify the table so that printing a float respects *print-base*.)
  (let ((i (gensym "I")))
    `(let ((*print-pretty* t)
	   (*print-right-margin* most-positive-fixnum))
       ,@(loop for a in args collect
	      (if (eq a T)
		  `(format t "~%")
		  `(progn
		     (format t "~A:" ,(format nil "~A" a))
		     (dolist (,i (multiple-value-list ,a))
		       (prin1 ,i)
		       (princ " ")))))
       (format t "~%"))))

(defclass sm ()
  ((states :initform 0 :initarg :states :type (and unsigned-byte integer) :accessor sm-states :documentation "The maximal number of states.")
   (trans :initform (make-array 0 :element-type '(and unsigned-byte integer) :initial-element 0) :initarg :trans :type (array (and unsigned-byte integer)) :accessor sm-trans :documentation "The state transition array")))

(defun make-sm (num-states &key (initial-trans (loop for i below num-states collect i)))
  (make-instance 'sm
		 :states num-states
		 :trans (make-array num-states :element-type '(and unsigned-byte integer) :initial-contents initial-trans)))

(defmethod print-object ((sm sm) stream)
  (print-unreadable-object (sm stream :type t :identity t)
    (format stream "~A ~A" (sm-states sm) (sm-trans sm))))

(defun sm-next (sm state)
  (aref (sm-trans sm) state))

(defun sm-circle (sm state &key (visited (make-array (sm-states sm) :initial-element nil)))
  (let ((order nil)
	(first state))
    (loop do
	 (push state order)
	 (let* ((next (sm-next sm state))
		(nextorder (aref visited next)))
	   (prind order)
	   (when nextorder
	     (setf (aref visited next) order)
	     (setf (aref visited first) order)
	     (return (values (nreverse order) visited)))
	   (loop for e in (reverse nextorder) do (push e order))
	   (setf (aref visited next) order)
	   (setf state next)))))

(defun sm-circles (sm)
  (let ((circles nil)
	(visited (make-array (sm-states sm) #|:element-type '(or (and unsigned-byte integer) (values nil))|# :initial-element nil)))
    (loop do
	 (let ((first (loop for i from 0 for e across visited do (when (null e) (return i)))))
	   (when (null first)
	     (return circles))
	   (multiple-value-bind (order next-visited)
	       (sm-circle sm first :visited visited)
	     (prind visited order)
	     (setf visited next-visited)
	     (push order circles))))))
