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

#|
;; trying to get the inner representation after the reader of SBCL parsed a string...
(defun showexpr (expr &optional (sep " "))
  (etypecase expr
    (cons (format t "(")
	  (showexpr (car expr) "")
	  (dolist (sub (cdr expr)) (showexpr sub))
	  (format t ")"))
    (t (let ((type (type-of expr)))
	 (case type
	   (sb-impl::comma
	    (let ((expr1 (sb-int:comma-expr expr)))
	      (format t "!!~S!!" (eql expr1 'a))
	      (format t "~A ~S ~S" sep type expr1)))
	   (t (format t "~A ~A ~A~%" sep type expr)))))))

;;(showexpr '`(1+ ,a))

;;(with-input-from-string (stream "`,a")
;;  (read stream))

;; doesn't work... I can't get the token after a COMMA. So I'll have to write a #'READ myself. No, I found the function in SBCL, that gets at the symbol which is commaed, it's called #'SB-INT:COMMA-EXPR.

(defmacro 1plusm (a)
  `(1+ ,a))

(defun 1plusf (a)
  `(1+ ,a))

(defun repexpr (expr n)
  (let ((res '(progn)))
    (dotimes (x n)
      (push expr res))
    (nreverse res)))
|#

(defun macro-function-eval (fun &rest args)
  (apply fun args))

(defstruct setf-expander
  access-fn
  lambda-list
  temporary
  value
  store
  storing
  accessing)

;;(defmacro my-define-setf-expander (access-fn lambda-list)

(define-modify-macro nreversef () nreverse "Reverse SEQUENCE in-place.")

(defun setf-expand (pairs env psetfp)
  (let (valforms temps values stores storings accessings)
    (do* ((pairs1 pairs (cddr pairs1))
	  (place (car pairs1) (car pairs1))
	  (valform (cadr pairs1) (cadr pairs1)))
	 ((progn
	    (when (and pairs1 (null (cdr pairs1))) (error "missing value"))
	    (null pairs1)))
      (push valform valforms)
      (multiple-value-bind (temp value store storing accessing)
	  (get-setf-expansion place env)
	(push temp temps)
	(push value values)
	(push store stores) ;;multiple STORES in the case of e.g. (SETF (VALUES A B) (VALUES 1 2)).
	(push storing storings)
	(push accessing accessings)))
    (nreversef valforms) (nreversef temps) (nreversef values) (nreversef stores) (nreversef storings) (nreversef accessings)
    (if psetfp
	(labels ((make-form (stores valforms temps values innermost-form)
		   "Takes care that the subforms are evaluated in left-to-right order."
		   (if (null stores)
		       innermost-form
		       `(let (,@(mapcar #'list (car temps) (car values)))
			  (multiple-value-bind (,@(car stores)) ,(car valforms)
			    ,(make-form (cdr stores) (cdr valforms) (cdr temps) (cdr values)
					innermost-form))))))
	  (make-form stores valforms temps values `(progn ,@storings nil)))
	(labels ((make-setf-form (temp value store valform storing is-last?)
		   "Takes care that the subforms are evaluated in left-to-right order."
		   `(let (,@(mapcar #'list temp value))
		      (multiple-value-bind (,@store) ,valform
			,storing
			,@(when is-last?
			    `((values ,@store)))))))
	  (cons 'progn
		(mapcar #'make-setf-form temps values stores valforms storings
			(append (mapcar (constantly nil) (cdr temps)) '(t))))))))

(defmacro my-psetf (&rest pairs)
  (setf-expand pairs nil t))

(defmacro my-setf (&rest pairs)
  (setf-expand pairs nil nil))

;; TODO: macros SHIFTF ROTATEF
