(load "~/quicklisp/setup.lisp")
(ql:quickload :utils)
(use-package :utils)

;;;; Test whether functional usage of an alist is faster than functional usage of a hash-table. Functional usage means being able to return a modified container without modifying the old container.

(defun make-empty-closed-alist ()
  nil)

(defun bind-closed-alist (var val closed &key (test #'equal))
  "Return an object representing the variables and their values bound in CLOSED, and the variable named VAR bound to VAL.
Return 'FAIL if the variable VAR is already bound to another value than VAL.
Return the unmodified CLOSED if VAR is bound to a value equal to VAL (under equality test TEST)."
  (declare (type symbol var)
	   (type list closed))
  ;; NOTE: this function must not modify CLOSED, since OPEN-VAR depends on an unmodified CLOSED.
  (let ((acons (assoc var closed)))
    (if (null acons)
	(acons var val closed)
	(if (funcall test val (cdr acons))
	    closed
	    'fail))))

(defun make-empty-closed-hash-table ()
  (make-hash-table :test 'eq))

(defun bind-closed-hash-table (var val closed &key (test #'equal))
  (declare (type symbol var)
	   (type hash-table closed))
  ;; NOTE: this function must not modify CLOSED, since OPEN-VAR depends on an unmodified CLOSED.
  (multiple-value-bind (bound present) (gethash var closed)
    (if present
	(if (funcall test val bound)
	    closed
	    'fail)
	(let ((new-closed (make-hash-table :test 'eq)))
	  (loop for k being the hash-key in closed using (hash-value v) do
	       (setf (gethash k new-closed) v))
	  (setf (gethash var new-closed) val)
	  new-closed))))

(defun make-key-value-list (n n-keys)
  (let ((keys (loop for i below n-keys collect (gensym "KEY"))))
    (loop for i below n collect (cons (elt keys (random n-keys)) (random 10)))))

(defun bind-list-alist (key-value-list)
  (let ((closed (make-empty-closed-alist)))
    (loop for (k . v) in key-value-list do
	 (let ((new-closed (bind-closed-alist k v closed)))
	   (when (not (eq 'fail new-closed))
	     (setf closed new-closed))))
    closed))

(defun bind-list-hash-table (key-value-list)
  (let ((closed (make-empty-closed-hash-table)))
    (loop for (k . v) in key-value-list do
	 (let ((new-closed (bind-closed-hash-table k v closed)))
	   (when (not (eq 'fail new-closed))
	     (setf closed new-closed))))
    closed))

(defun time-alist-vs-hash-table (n-list n-keys)
  (print "generating key-value-list")
  (let* ((key-value-list (make-key-value-list n-list n-keys))
	 (f-alist (lambda () (bind-list-alist key-value-list)))
	 (f-hash-table (lambda () (bind-list-hash-table key-value-list))))
    (print "timing functions")
    (values (float (timesec f-alist))
	    (float (timesec f-hash-table)))))
;; Using hash-table in a functional manner takes always longer than using an alist in a functional manner. (As expected, when thinking about the time needed to copy an existing hash-table to a new hash-table.)
