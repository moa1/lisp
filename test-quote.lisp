(defun dissect (l)
  (cond
    ((null l)
     "NIL")
    ((consp l)
     (concatenate 'string "(CONS " (dissect (car l)) " " (dissect (cdr l)) ")"))
    ;; TODO: implement ((typep l 'sb-impl::comma) ...): see #'TEST-SBCL below.
    (t
     (format nil "~S" l))))

(defun dissect-string (string)
  (dissect (read-from-string string)))

(defun print-dissect (string &optional (stream t))
  (format stream "~S:~% ~S~%" string (dissect-string string)))

(defun test1 ()
  (print-dissect "a")
  (print-dissect "`,a")
  (print-dissect "(a b)")
  (print-dissect "`(a b)")
  (print-dissect "`(,a b)")
  (print-dissect "`(,@a b)"))
(test1)

(defun test-sbcl ()
  (labels ((dissect-comma (a)
	     "A must be of type SB-IMPL::COMMA"
	     (values (sb-impl::comma-constructor a)
		     (sb-impl::comma-expr a)
		     (sb-impl::comma-kind a)
		     (sb-impl::comma-p a)
		     (sb-impl::comma-splicing-p a)))
	   (print-dissect-comma (a &optional (stream t))
	     (format stream "~S:~% ~S~%" a (multiple-value-list (dissect-comma a)))))
    (let* ((l (read-from-string "`(,a)"))
	   (a (caadr l)))
      (print-dissect-comma a))
    (let* ((l (read-from-string "`(,@a)"))
	   (a (caadr l)))
      (print-dissect-comma a))))

;; ECL 15.3.7:
;; CL-USER> (test1)
;; "a":
;;  "A"
;; "`,a":
;;  "(CONS SI:QUASIQUOTE (CONS (CONS SI:UNQUOTE (CONS A NIL)) NIL))"
;; "(a b)":
;;  "(CONS A (CONS B NIL))"
;; "`(a b)":
;;  "(CONS SI:QUASIQUOTE (CONS (CONS A (CONS B NIL)) NIL))"
;; "`(,a b)":
;;  "(CONS SI:QUASIQUOTE (CONS (CONS (CONS SI:UNQUOTE (CONS A NIL)) (CONS B NIL)) NIL))"
;; "`(,@a b)":
;;  "(CONS SI:QUASIQUOTE (CONS (CONS (CONS SI:UNQUOTE-SPLICE (CONS A NIL)) (CONS B NIL)) NIL))"

;; GNU CLISP 2.49:
;; CL-USER> (test1)
;; "a":
;;  "A"
;; "`,a":
;;  "(CONS SYSTEM::BACKQUOTE (CONS (CONS SYSTEM::UNQUOTE (CONS A NIL)) NIL))"
;; "(a b)":
;;  "(CONS A (CONS B NIL))"
;; "`(a b)":
;;  "(CONS SYSTEM::BACKQUOTE (CONS (CONS A (CONS B NIL)) NIL))"
;; "`(,a b)":
;;  "(CONS SYSTEM::BACKQUOTE (CONS (CONS (CONS SYSTEM::UNQUOTE (CONS A NIL)) (CONS B NIL)) NIL))"
;; "`(,@a b)":
;;  "(CONS SYSTEM::BACKQUOTE (CONS (CONS (CONS SYSTEM::SPLICE (CONS A NIL)) (CONS B NIL)) NIL))"

;; SBCL 1.3.1
;; CL-USER> (test1)
;; "a":
;;  "A"
;; "`,a":
;;  "(CONS SB-INT:QUASIQUOTE (CONS ,A NIL))"
;; "(a b)":
;;  "(CONS A (CONS B NIL))"
;; "`(a b)":
;;  "(CONS SB-INT:QUASIQUOTE (CONS (CONS A (CONS B NIL)) NIL))"
;; "`(,a b)":
;;  "(CONS SB-INT:QUASIQUOTE (CONS (CONS ,A (CONS B NIL)) NIL))"
;; "`(,@a b)":
;;  "(CONS SB-INT:QUASIQUOTE (CONS (CONS ,@A (CONS B NIL)) NIL))"

(defun test-clisp-wrong-reconstruction ()
  (print `(a `b))
  (print `(a `(,b))))
