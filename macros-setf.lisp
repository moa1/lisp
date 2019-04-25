(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)

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

;;; misc functions

(defun last1 (list)
  (car (last list)))

(defun (setf last1) (list new)
  (setf (car (last list)) new))

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

(defun is-comma-eql (comma-x y)
  (and (typep comma-x 'sb-impl::comma)
       (eql (sb-int:comma-expr comma-x) y)))

(defun make-comma (expr)
  ;; is there a standardized way to convert EXPR to ",EXPR"?
  ;; no, because there doesn't need to be: `(A ,B) is just another way of saying (LIST 'A B).
  (SB-IMPL::%DEFAULT-COMMA-CONSTRUCTOR :expr expr :kind 0))

(defun macro-function-eval (fun &rest args)
  (apply fun args))

(defparameter *setf-expander-functions* (make-hash-table) "The defined setf-expander functions.")

(defmacro my-define-setf-expander (access-fn lambda-list &body body)
  "DEFINE-SETF-EXPANDER"
  (check-type access-fn symbol)
  (let ((has-environment-arg (find '&environment lambda-list)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',access-fn *setf-expander-functions*)
	     (cons (lambda ,lambda-list (block ,access-fn ,@body))
		   ,has-environment-arg))
       ',access-fn)))

;; adopted from package alexandria-20170830-git/macros.lisp, function #'PARSE-ORDINARY-LAMBDA-LIST.
(defun parse-defsetf-lambda-list (lambda-list &key (normalize t)
                                  allow-specializers
                                  (normalize-optional normalize)
                                  (normalize-keyword normalize))
  "Parses a defsetf lambda-list, returning as multiple values:
1. Required parameters.
2. Optional parameter specifications, normalized into form:
   (name init suppliedp)
3. Name of the rest parameter, or NIL.
4. Keyword parameter specifications, normalized into form:
   ((keyword-name name) init suppliedp)
5. Boolean indicating &ALLOW-OTHER-KEYS presence.
6. Name of the environment parameter, or NIL.
7. Existence of &KEY in the lambda-list.
Signals a PROGRAM-ERROR if the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (envp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil)
        (env nil))
    (labels ((fail (elt)
               (alexandria:simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
						elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (alexandria:simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
						  what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt))
           (setf keyp t))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&environment
           (cond ((eq state '&rest)
                  (fail elt))
                 (envp
                  (alexandria:simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
						   elt lambda-list))
                 (t
                  (setf envp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &environment)))
             (alexandria:simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
              (check-variable elt "required parameter")
              (push elt required))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             ((and normalize-optional tail)
                              (setf elt (append elt '(nil))))
                             (normalize-optional
                              (setf elt (append elt '(nil nil)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil nil))))))
              (push (alexandria:ensure-list elt) optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (symbolp keyword)
                                  (alexandria:simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
								   keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (when normalize-keyword
                                (setf var-or-kv (list (alexandria:make-keyword var-or-kv) var-or-kv)))))
                       (cond ((cdr tail)
                              (check-spec tail "keyword-supplied-p parameter"))
                             ((and normalize-keyword tail)
                              (setf tail (append tail '(nil))))
                             (normalize-keyword
                              (setf tail '(nil nil))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (alexandria:make-keyword elt) elt) nil nil)
                                   elt))))
              (push elt keys))
             (&environment
              (check-variable elt "&environment parameter")
              (setf env elt))
             (t
              (alexandria:simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys env keyp)))

(defun make-gensym-list* (symbols)
  "Return a list of new symbols named like the symbols in SYMBOLS."
  (loop for s in symbols collect (gensym (format nil "~S" s))))

(defmacro my-defsetf (access-fn &body rest)
  "Long and short forms of #'DEFSETF."
  (cond
    ((listp (car rest)) ;long form
     (assert (>= (length rest) 2) () "The long form of #'DEFSETF has at least the three arguments~%(ACCESS-FN LAMBDA-LIST (STORE-VARIABLE*), but has only ~S~%" (cons access-fn rest))
     (let ((lambda-list (car rest))
	   (store-vars (cadr rest))
	   (body (cddr rest)))
       (multiple-value-bind (required optional rest keys allow-other-keys env keyp)
	   (parse-defsetf-lambda-list lambda-list)
	 (multiple-value-bind (forms declarations doc-string)
	     (alexandria:parse-body body :documentation t :whole "forms")
	   (let* ((required-local (make-gensym-list* required))
		  (optional-local (make-gensym-list* optional))
		  (rest-local (when rest (gensym "REST")))
		  (keys-local (make-gensym-list* (mapcar #'cadar keys)))
		  (keys-present-local (make-gensym-list* (mapcar #'third keys)))
		  (lambda-vars-local (append required-local optional-local (when rest-local (list rest-local)) keys-local keys-present-local))
		  (store-vars-local (make-gensym-list* store-vars))
		  (all-vars (append required (mapcar #'first optional) (mapcar #'third optional) (when rest (list rest)) (mapcar #'cadar keys) (mapcar #'third keys) (when env (list env))))
		  ;;(all-vars (append lambda-vars store-vars))
		  ;;(all-vars-local (append lambda-vars-local store-vars-local))
		  (all-vars-local lambda-vars-local))
	     (flet ((subst-vars (news olds tree)
		      (let ((stree (copy-tree tree)))
			(loop for n in news for o in olds do
			     (setf stree (nsubst-if n (lambda (x) (or (eql x o) (is-comma-eql x o))) stree)))
			stree)))
	       `(my-define-setf-expander ,access-fn ,lambda-list
		  ;;(let (,@(loop for avn in all-vars-local for av in all-vars collect (list avn av)))
		  (values (list ,@(mapcar (lambda (x) `(quote ,x)) all-vars-local))
			  (list ,@all-vars)
			  (list ,@(mapcar (lambda (x) `(quote ,x)) store-vars-local))
			  ,@(subst-vars (append all-vars-local store-vars-local)
					(append all-vars store-vars)
					forms)
			  ,(list 'quote
				 (cons access-fn
				       (append required-local
					       optional-local
					       (when keyp `(&keys ,@(loop for ((kn n) &rest r) in keys-local collect `(kn n))))
					       (when allow-other-keys `(allow-other-keys)))))))))))))
    (t ;short form
     'bla)))

(my-defsetf car (list) (store)
  `(progn (rplaca ,list ,store) ,store))
;; check that this produces something equivalent to
;; (my-define-setf-expander car (list)
;;   (alexandria:with-gensyms (slist store)
;;     (values `(,slist)
;; 	    `(,list)
;; 	    `(,store)
;; 	    `(progn (rplaca ,slist ,store) ,store)
;; 	    `(car ,slist))))
;; by evaluating this:
;; (macroexpand-1 '(my-defsetf car (list) (store)
;;                   `(progn (rplaca ,list ,store) ,store)))

#|
(my-define-setf-expander values (&rest values &environment env)
  (let* ((exps (mapcar (lambda (value) (multiple-value-list (my-get-setf-expansion value env))) values))
	 (temps (mapcar #'first exps))
	 (values (mapcar #'second exps))
	 (setters (mapcar #'fourth exps))
	 (stores (mapcar (lambda (x) (declare (ignore x)) (gensym "STORE")) exps)))
    `(values (,@temps)
	     (,@values)
	     (,@stores)
	     (values ,@(mapcar
|#

(defun my-get-setf-expansion (form &optional environment)
  (cond
    ((atom form)
     (alexandria:with-gensyms (store)
       (values nil
	       nil
	       `(,store)
	       `(setq ,form ,store)
	       form)))
    (t
     (let ((access-fn (car form)))
       (check-type access-fn symbol)
       (let* ((lookup (gethash access-fn *setf-expander-functions*))
	      (expander (car lookup))
	      (has-environment-arg (cdr lookup)))
	 (assert (not (null expander)) () "Undefined setf-expansion for ~S" form)
	 ;; TODO: FIXME: We have to pass ENVIRONMENT iff ACCESS-FN accepts it!
	 (apply expander (if has-environment-arg
			     (append (cdr form) (list environment))
			     (cdr form))))))))

(define-modify-macro nreversef () nreverse "Reverse SEQUENCE in-place.")

(defun setf-expand (pairs env psetfp)
  (let (valforms temps values stores setters getters)
    (do* ((pairs1 pairs (cddr pairs1))
	  (place (car pairs1) (car pairs1))
	  (valform (cadr pairs1) (cadr pairs1)))
	 ((progn
	    (when (and pairs1 (null (cdr pairs1))) (error "missing value"))
	    (null pairs1)))
      (push valform valforms)
      (multiple-value-bind (temp value store setter getter)
	  (get-setf-expansion place env)
	(push temp temps)
	(push value values)
	(push store stores) ;;multiple STORES in the case of e.g. (SETF (VALUES A B) (VALUES 1 2)).
	(push setter setters)
	(push getter getters)))
    (nreversef valforms) (nreversef temps) (nreversef values) (nreversef stores) (nreversef setters) (nreversef getters)
    (if psetfp
	(labels ((make-form (stores valforms temps values innermost-form)
		   "Takes care that the subforms are evaluated in left-to-right order."
		   (if (null stores)
		       innermost-form
		       `(let (,@(mapcar #'list (car temps) (car values)))
			  (multiple-value-bind (,@(car stores)) ,(car valforms)
			    ,(make-form (cdr stores) (cdr valforms) (cdr temps) (cdr values)
					innermost-form))))))
	  (make-form stores valforms temps values `(progn ,@setters nil)))
	(labels ((make-setf-form (temp value store valform setter is-last?)
		   "Takes care that the subforms are evaluated in left-to-right order."
		   `(let (,@(mapcar #'list temp value))
		      (multiple-value-bind (,@store) ,valform
			,setter
			,@(when is-last?
			    `((values ,@store)))))))
	  (cons 'progn
		(mapcar #'make-setf-form temps values stores valforms setters
			(append (mapcar (constantly nil) (cdr temps)) '(t))))))))

(defmacro my-psetf (&rest pairs)
  "PSETF"
  (setf-expand pairs nil t))

(defmacro my-setf (&rest pairs)
  "SETF"
  (setf-expand pairs nil nil))

;; TODO: macros SHIFTF ROTATEF INCF DECF DEFINE-MODIFY-MACRO DEFSETF DEFINE-SETF-METHOD
