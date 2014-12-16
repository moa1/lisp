(asdf:oos 'asdf:load-op 'utils)

(defun my-search (item list &key key (test #'eql))
  (labels ((helper (list)
	     (cond ((null list)
		    nil)
		   ((funcall test
			     item
			     (if key (funcall key (car list)) (car list)))
		    (car list))
		   (t (helper (cdr list))))))
    (helper list)))

(defun my-search2 (item list &key key (test #'eql))
  (specializing-labels ((helper-with-key ((akey key)))
			(helper-no-key ((akey nil))))
      (rec (list)
       (cond ((null list)
	      nil)
	     ((funcall test
		       item
		       (if akey (funcall akey (car list)) (car list)))
	      list)
	     (t (rec (cdr list)))))
    (if key
	(helper-with-key list)
	(helper-no-key list))))

(defun my-search3 (item list &key key (test #'eql))
  (specializing-labels ((helper+key+test ((akey key) (atest test)))
			(helper-key+test ((akey nil) (atest test)))
			(helper+key-test ((akey key) (atest nil)))
			(helper-key-test ((akey nil) (atest nil))))
      (rec (list)
       (cond ((null list)
	      nil)
	     ((if atest
		  (funcall test
			   item
			   (if akey (funcall akey (car list)) (car list)))
		  (eql item (if akey (funcall akey (car list)) (car list))))
	      (car list))
	     (t (rec (cdr list)))))
    (if key
	(if (eq test #'eql)
	    (helper+key-test list)
	    (helper+key+test list))
	(if (eq test #'eql)
	    (helper-key-test list)
	    (helper-key+test list)))))

(defmacro print-run-time (&body body)
  (prind body)
  `(progn
     (let ((l (multiple-value-list
;;	       (timeit (10 :stats t) (progn-repeat (1000000) ,@body)))))
	       (timecps (10 :stats t :time 1) ,@body))))
       (format t "~A:~A~%"
	       l
	       ,(format nil "~A" body))
       )))

(defun test-my-searches ()
  (let ((list '((5) (2) (5) (1 . 5))))
    (print-run-time (my-search   5 list))
    (print-run-time (my-search2  5 list))
    (print-run-time (my-search3  5 list))
    (print-run-time (member-tree 5 list))
    (print-run-time (find        5 list))
    (print-run-time (my-search   5 list :key #'null))
    (print-run-time (my-search2  5 list :key #'null))
    (print-run-time (my-search3  5 list :key #'null))
    (print-run-time (member-tree 5 list :key #'null))
    (print-run-time (find        5 list :key #'null))))

(defun test-searches ()
  (let ((list '((5) (2) (5) (1 . 5) 5)))
    (print-run-time (position   5 list))
    (print-run-time (find       5 list))
    (print-run-time (remove     5 list))
    ))
