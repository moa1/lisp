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

;;; functions from ALEXANDRIA

;; taken from quicklisp's ALEXANDRIA package.
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

;;; misc functions

(defun last1 (list)
  (car (last list)))

(defun (setf last1) (list new)
  (setf (car (last list)) new))

;;; LLET

;; TODO: support all features of quicklisp packages:
;;cletris-20151031-git                formlets-20161204-git         map-bind-20120811-git
;;cl-singleton-mixin-20150505-git     let-over-lambda-20150923-git  metabang-bind-20171130-git
;;cl-string-complete-20120107-hg      let-plus-20171130-git         shuffletron-20150608-git
;;enhanced-multiple-value-bind-1.0.1  letrec-20131111-hg            x.let-star-20150709-git

(defparameter *llet-binders* (make-hash-table) "The defined LLET binders.")

(defmacro define-llet-binder (keyword lambda-list &body body)
  (check-type keyword keyword)
  `(setf (gethash ,keyword *llet-binders*)
	 (lambda ,lambda-list ,@body)))

(define-llet-binder :gensym (name)
  (check-type name symbol)
  `(,name (gensym ,(format nil "~S" name))))

(defun expand-llet (bindings body)
  (check-type bindings list)
  (labels ((bindings (binding rest-bindings forms)
	     (check-type binding list)
	     (let* ((length (length binding))
		    (first (car binding))
		    (rest (cdr binding))
		    (result-form
		     (cond
		       ((null binding)
			forms)
		       ((listp first)
			(assert (null (cdr binding)) () "Not a single form: ~S" binding)
			(cons (car binding) forms))
		       ((keywordp first)
			(let ((binder-fn (gethash first *llet-binders*)))
			  (assert binder-fn () "Unknown LLET binder ~S" first)
			  `((let (,(apply binder-fn rest)) ,@forms))))
		       ((= length 2)
			`((let ((,first ,@rest))
			    ,@forms)))
		       (t
			(mapc (lambda (var) (check-type var symbol)) (butlast binding))
			`((multiple-value-bind (,@(butlast binding)) ,(last1 binding)
			    ,@forms))))))
	       (if (null binding)
		   result-form
		   (bindings (car rest-bindings) (cdr rest-bindings) result-form)))))
    (let ((reverse-bindings (reverse bindings)))
      (bindings (car reverse-bindings) (cdr reverse-bindings) body))))

(defmacro llet (bindings &body body)
  "LLET is like LET*, but besides the normal (NAME VALUE)-bindings, it can handle multiple-value-bindings (VARS... FORM), intermediate expressions that are not bound to a variable (EXPRESSION), and user-definable llet-macros which expand to a let-binding.
An example is:
(llet ((normal-assignment 'as-in-LET-or-LET*)
       (whole remainder (truncate 10 7)) ;multiple-value-bind
       ((format t \"This is an intermediate form without assignment\"))
       (:gensym name) ;this can be defined like (DEFINE-LLET-BINDER :GENSYM (NAME) `(,NAME (GENSYM)))
       )
  (values normal-assignment whole remainder name))
== 'as-in-LET-or-LET* 1 3 #:NAME505."
  (let ((forms (expand-llet bindings body)))
    (if (null (cdr forms))
	(car forms)
	forms)))

(defun test-llet ()
  (let ((x 0))
    (llet ((a (incf x))
	   (i r (truncate (+ x 9) 7))
	   (b (incf x))
	   ((setf x 0))
	   (c x)
	   (:gensym name))
      (assert (= a 1))
      (assert (and (= i 1) (= r 3)))
      (assert (= b 2))
      (assert (= c 0))
      (check-type name symbol))))

;;;

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

(defparameter *setf-expander-functions* (make-hash-table) "The defined setf-expander functions.")

(defmacro my-define-setf-expander (access-fn lambda-list &body body)
  (check-type access-fn symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',access-fn *setf-expander-functions*)
	   (lambda ,lambda-list (block ,access-fn ,@body)))
     ',access-fn))

(my-define-setf-expander car (list)
  (llet ((:gensym slist)
	 (:gensym store))
    (values `(,slist)
	    `(,list)
	    `(store)
	    `(rplaca ,slist ,store)
	    `(car ,slist))))

(defun my-get-setf-expansion (form &optional environment)
  (cond
    ((atom form)
     (llet ((:gensym store))
       (values nil
	       nil
	       `(,store)
	       `(setq ,form ,store)
	       form)))
    (t
     (let ((access-fn (car form)))
       (check-type access-fn symbol)
       (let ((expander (gethash access-fn *setf-expander-functions*)))
	 (assert (not (null expander)) () "Undefined setf-expansion for ~S" form)
	 ;; TODO: FIXME: We have to pass ENVIRONMENT iff ACCESS-FN accepts it!
	 (apply access-fn (cdr form)))))))

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

;; TODO: macros SHIFTF ROTATEF INCF DECF DEFINE-MODIFY-MACRO DEFSETF DEFINE-SETF-METHOD
