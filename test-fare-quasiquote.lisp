(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :fare-quasiquote)

;; from fare-quasiquote's quasiquote-test.lisp
(defun rprinc (x)
  "hand-made princ that allows to see inside quasiquotes (results are implementation-dependent)"
  (labels
      ((rprinc-list (x)
	 (princ "(")
	 (rprinc-list-contents x)
	 (princ ")"))
       (rprinc-list-contents (x)
	 (rprinc (car x))
	 (rprinc-cdr (cdr x)))
       (rprinc-cdr (x)
	 (if x (if (consp x)
		   (progn
		     (princ " ")
		     (rprinc-list-contents x))
		   (progn
		     (princ " . ")
		     (rprinc x))))))
    (cond
      ((consp x) (rprinc-list x))
      (t (princ x)))
    x))

;; from fare-quasiquote's quasiquote-test.lisp
(defmacro with-qq-syntax ((&key) &body body)
  `(call-with-qq-syntax #'(lambda () ,@body)))

;; from fare-quasiquote's quasiquote-test.lisp
(defun call-with-qq-syntax (thunk)
  (with-standard-io-syntax
    (let (;;(*package* (find-package :fare-quasiquote/test))
	  (*readtable* fare-quasiquote:*fq-readtable*)
	  (*print-pprint-dispatch* fare-quasiquote:*fq-pprint-dispatch*)
	  (*print-pretty* t)
	  (*print-readably* nil)
	  (*print-case* :downcase))
      (funcall thunk))))

(defun test-fare-quasiquote ()
  (labels ((rq (s) (with-qq-syntax () (read-from-string s))));; from fare-quasiquote's quasiquote-test.lisp
    (values
     (rq "a")
     (rq "`,a")
     (rq "(a b)")
     (rq "`(a b)")
     (rq "`(,a b)")
     (rq "`(,@a b)"))))
