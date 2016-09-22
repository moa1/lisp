;;;; Lisp to C Compiler

(load "~/quicklisp/setup.lisp")
(ql:quickload :walker)

;;;; Package

(defpackage :lispccompiler
  (:use :cl))
(in-package :lispccompiler)

;;;; Helpers

(eval-when (:compile-toplevel :load-toplevel :execute)
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
	 (format t "~%")))))

;; Taken from ALEXANDRIA
(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
	       (when subtree
		 (if (consp subtree)
		     (progn
		       (traverse (car subtree))
		       (traverse (cdr subtree)))
		     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun join-strings (stringlist separator)
  (apply #'concatenate 'string
	 (car stringlist)
	 (let ((rest nil))
	   (loop for arg in (cdr stringlist) do
		(push separator rest)
		(push arg rest))
	   (nreverse rest))))

;;;; The backend stuff implementing the special operators of Lisp and some macros.

(defclass multiple-value-bind-form (walker:form walker:body-form)
  ((vars :initarg :vars :accessor walker:form-vars :type list :documentation "list of VARs")
   (values :initarg :values :accessor walker:form-values :type generalform)
   (declspecs :initarg :declspecs :accessor walker:form-declspecs :type list)))
(defclass values-form (walker:form walker:body-form)
  ())
(defclass nth-value-form (walker:form) ;TODO: implement NTH-VALUE-form, but I think this will not be easy since there is no way in LISP to specify multiple value types for a form. Probably implementing this will need an automatic type inferencer, like NIMBLE.
  ((value :initarg :value :accessor walker:form-value :type generalform)
   (values :initarg :values :accessor walker:form-values :type generalform)))
(defclass defun-form (walker:fun-binding)
  ())
(defclass declaim-form (walker:form)
  ((declspecs :initarg :declspecs :accessor walker:form-declspecs :type list)))
(defclass funcall-form (walker:form)
  ((var :initarg :sym :accessor walker:form-var :type walker:var)
   (arguments :initarg :arguments :accessor walker:form-arguments :type list :documentation "list of GENERALFORMs")))
(defclass prind-form (walker:form walker:body-form)
  ((lexicalnamespace :initarg :lexicalnamespace :accessor walker:form-lexicalnamespace)
   (freenamespace :initarg :freenamespace :accessor walker:form-freenamespace)))

(defmethod print-object ((object multiple-value-bind-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-vars object) (walker:form-values object) (walker:format-body object t nil))))
(defmethod print-object ((object values-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (walker:format-body object nil nil))))
(defmethod print-object ((object nth-value-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (walker:form-value object) (walker:form-values object))))
(defmethod print-object ((object defun-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-sym object) (walker:form-llist object) (walker:format-body object t t))))
(defmethod print-object ((object declaim-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (walker:form-declspecs object))))
(defmethod print-object ((object funcall-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-var object))
    (loop for arg in (walker:form-arguments object) do
	 (format stream " ~S" arg))))

(defun parse-some-macros-p (form lexical-namespace free-namespace parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (declare (ignore customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function lexical-namespace free-namespace parent))
  (and (listp form)
       (let ((head (car form)))
	 (find head '(multiple-value-bind values nth-value defun declaim funcall)))))

(defun parse-some-macros (form lexical-namespace free-namespace parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (declare (optimize (debug 3)))
  (labels ((reparse (form parent &key (lexical-namespace lexical-namespace))
	     (walker:parse form lexical-namespace free-namespace parent
			   :customparsep-function customparsep-function
			   :customparse-function customparse-function
			   :customparsedeclspecp-function customparsedeclspecp-function
			   :customparsedeclspec-function customparsedeclspec-function))
	   (parse-body (body current &key (lexical-namespace lexical-namespace))
	     (assert (walker:proper-list-p body) () "Body is not a proper list: ~S" body)
	     (loop for form in body collect (reparse form current :lexical-namespace lexical-namespace))))
    (let ((head (car form))
	  (rest (cdr form)))
      (cond
	((eq head 'multiple-value-bind)
	 (assert (and (consp rest) (listp (car rest)) (listp (cadr rest))) () "Cannot parse MULTIPLE-VALUE-BIND-form ~S" form)
	 (let* ((vars-form (let ((vars-form (car rest))) (loop for var in vars-form do (assert (symbolp var) () "VARs in MULTIPLE-VALUE-BIND-form must be symbols, not ~S" var)) vars-form))
		(values-form (cadr rest))
		(body (cddr rest))
		(current (make-instance 'multiple-value-bind-form :parent parent))
		(parsed-vars (loop for var-form in vars-form collect (reparse var-form current)))
		(parsed-values (reparse values-form current)))
	   (multiple-value-bind (body parsed-declspecs)
	       (walker:parse-declaration-in-body body lexical-namespace free-namespace current :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
	     (setf (walker:form-vars current) parsed-vars)
	     (setf (walker:form-values current) parsed-values)
	     (setf (walker:form-declspecs current) parsed-declspecs)
	     (setf (walker:form-body current) (parse-body body current)))
	   current))
	((eq head 'values)
	 (let* ((objects-form rest)
		(current (make-instance 'values-form :parent parent))
		(parsed-objects (loop for object-form in objects-form collect (reparse object-form current))))
	   (setf (walker:form-body current) parsed-objects)
	   current))
	((eq head 'nth-value)
	 (assert (and (consp rest) (consp (cdr rest)) (null (cddr rest))) () "Cannot parse NTH-VALUE-form ~S" form)
	 (let* ((value-form (car rest))
		(values-form (cadr rest))
		(current (make-instance 'nth-value-form :parent parent))
		(parsed-value (reparse value-form current))
		(parsed-values (reparse values-form current)))
	   (setf (walker:form-value current) parsed-value)
	   (setf (walker:form-values current) parsed-values)
	   current))
	((eq head 'defun)
	 (multiple-value-bind (fun-type name) (walker:valid-function-name-p (car rest))
	   (let* ((lambda-list-and-body (cdr rest))
		  (block-name (ecase fun-type ((walker:fun) name) ((walker:setf-fun) (cadr name)))) ;CLHS Glossary "function block name" defines "If the function name is a list whose car is setf and whose cadr is a symbol, its function block name is the symbol that is the cadr of the function name."
		  (blo (make-instance 'walker:blo :name block-name :freep nil))
		  (current (make-instance 'defun-form :parent parent :blo blo))
		  (sym (walker:namespace-lookup/create 'walker:fun name lexical-namespace free-namespace)))
	     (walker:parse-and-set-functiondef lambda-list-and-body #'walker:parse-ordinary-lambda-list (walker:augment-lexical-namespace blo lexical-namespace) free-namespace current :customparsep-function customparsep-function :customparse-function customparse-function :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)
	     (setf (walker:nso-definition blo) current)
	     (setf (walker:form-sym current) sym)
	     current)))
	((eq head 'declaim)
	 (let* ((declspecs rest)
		(current (make-instance 'declaim-form :parent parent))
		(parsed-declspecs (walker:parse-declspecs declspecs lexical-namespace free-namespace current :customparsedeclspecp-function customparsedeclspecp-function :customparsedeclspec-function customparsedeclspec-function)))
	   (setf (walker:form-declspecs current) parsed-declspecs)
	   current))
	((eq head 'funcall)
	 (assert (symbolp (car rest)) () "Cannot parse FUNCALL-form ~S" form)
	 (let* ((fun-sym (car rest))
		(arg-forms (cdr rest))
		(sym (walker:namespace-lookup/create 'walker:var fun-sym lexical-namespace free-namespace))
		(current (make-instance 'funcall-form :parent parent :sym sym))
		(parsed-arguments nil))
	   (loop do
		(when (null arg-forms) (return))
		(assert (and (consp arg-forms) (listp (cdr arg-forms))) () "Invalid argument rest ~S in function or macro application" arg-forms)
		(push (reparse (car arg-forms) current) parsed-arguments)
		(setf arg-forms (cdr arg-forms)))
	   (setf (walker:form-arguments current) (nreverse parsed-arguments))
	   current))
	((eq head 'prind)
	 (let* ((current (make-instance 'prind-form :parent parent :lexicalnamespace lexical-namespace :freenamespace free-namespace)))
	   (setf (walker:form-body current) (parse-body rest current))
	   current))
	))))

(defun deparse-multiple-value-bind-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'multiple-value-bind
	 (mapcar (lambda (var) (funcall recurse-function var ast)) (walker:form-vars ast))
	 (funcall recurse-function (walker:form-values ast) ast)
	 (walker:deparse-body ast recurse-function t nil)))
(defun deparse-values-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'values (walker:deparse-body ast recurse-function nil nil)))
(defun deparse-nth-value-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'nth-value
	 (funcall recurse-function (walker:form-value ast) ast)
	 (funcall recurse-function (walker:form-values ast) ast)))
(defun deparse-defun-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'defun
	 (funcall recurse-function (walker:form-sym ast) ast)
	 (walker:deparse-body ast recurse-function t t)))
(defun deparse-declaim-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'declaim
	 (funcall recurse-function (walker:form-declspecs ast) ast)))
(defun deparse-funcall-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'funcall
	 (funcall recurse-function (walker:form-var ast) ast)
	 (mapcar (lambda (arg) (funcall recurse-function arg ast)) (walker:form-arguments ast))))
(defun deparse-prind-form (ast parent recurse-function)
  (declare (ignore parent))
  (list* 'prind
	 (walker:deparse-body ast recurse-function nil nil)))

(defun deparse-typecase (ast parent &key &allow-other-keys)
  (declare (ignore parent))
  (typecase ast
    (multiple-value-bind-form #'deparse-multiple-value-bind-form)
    (values-form #'deparse-values-form)
    (nth-value-form #'deparse-nth-value-form)
    (defun-form #'deparse-defun-form)
    (declaim-form #'deparse-declaim-form)
    (funcall-form #'deparse-funcall-form)
    (prind-form #'deparse-prind-form)
    (t nil)))

(defun deparse-some-macros (ast parent)
  (declare (optimize (debug 3)))
  (if (deparse-typecase ast parent)
      (funcall (deparse-typecase ast parent) ast parent #'deparse-some-macros)
      (walker:deparse ast parent :customdeparsep-function #'deparse-typecase :customdeparse-function #'deparse-some-macros)))

(defun walker-parse (form parent &key lexical-namespace free-namespace)
  (let ((lexical-namespace (if lexical-namespace lexical-namespace (walker:make-empty-lexical-namespace)))
	(free-namespace (if free-namespace free-namespace (walker:make-default-free-common-lisp-namespace))))
    (walker:parse form lexical-namespace free-namespace parent :customparsep-function #'parse-some-macros-p :customparse-function #'parse-some-macros)))

(defun walker-deparse (ast parent)
  (deparse-some-macros ast parent))

;; Types

(defun sym-declspec-type (sym)
  (declare (optimize (debug 3)))
  (let* ((type-declspecs (remove-if (lambda (declspec) (not (or (subtypep (type-of declspec) 'walker:declspec-type)
								(subtypep (type-of declspec) 'walker:declspec-ftype))))
				    (walker:nso-declspecs sym))))
    (assert (not (null type-declspecs)) () "Lisp symbol ~A has unknown type" sym)
    (let ((first-declspec-type (walker:declspec-type (car type-declspecs))))
      ;;(prind type-declspecs first-declspec-type)
      (assert (loop for d in (cdr type-declspecs) always (eq (walker:declspec-type d) first-declspec-type)) () "Conflicting type declarations in ~S" type-declspecs)
      ;; TODO: for compatible type-declarations, find the most specific type and return it. (e.g. for NUMBER and FLOAT return FLOAT.)
      first-declspec-type)))

(defun parse-ftype-declspec (type)
  "Return two values: the list of argument types and the list of value types (without VALUES as CAR)."
  (assert (and (consp type) (= 3 (length type)) (eq 'function (car type)) (listp (cadr type))))
  (let ((arguments (cadr type))
	(results (let ((results (caddr type)))
		   (assert (or (symbolp results) (and (listp results) (eq (car results) 'values))))
		   (if (listp results)
		       (cdr results)
		       (list results)))))
    (values arguments results)))

;;;; The C backend.

(defclass nso ()
  ((name :initarg :name :accessor nso-name :type string)
   (freep :initarg :freep :accessor nso-freep :type boolean :documentation "Whether the NSO is globally visible.")))

(defclass sym (nso)
  ((type :initarg :type :accessor nso-type :type (or symbol list) :documentation "The C type of the SYM.")))

(defclass var (sym)
  ())

(defclass fun (sym)
  ((values :initarg :values :accessor nso-values :type list :documentation "the arguments passed to the function that deliver back the computed values of the function. Is a list of VARs.")))

(defclass tag (nso)
  ())

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A"  (nso-type object) (nso-name object))))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A"  (nso-name object))))

(defun make-var (name type)
  (make-instance 'var :name name :type type))

(defun make-fun (name type)
  (make-instance 'fun :name name :type type))

(defun make-tag (name)
  (make-instance 'tag :name name))

(defclass namespace ()
  ;; the NSOs after (:GLOBAL . NIL) are considered to be part of the global environment.
  ((var :initform (list '(:global . nil)) :initarg :var :accessor namespace-var)
   (fun :initform (list '(:global . nil)) :initarg :fun :accessor namespace-fun)
   (tag :initform (list '(:global . nil)) :initarg :tag :accessor namespace-tag)
   (lispnsos :initform (list '(:global . nil)) :initarg :lispnsos :accessor namespace-lispnsos :documentation "A mapping from WALKER:NSO to NSO.")))

(defmethod print-object ((object namespace) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "VARs:~S FUNs:~S TAGs:~S LISPNSOS:~S" (namespace-var object) (namespace-fun object) (namespace-tag object) (namespace-lispnsos object))))

(defun make-empty-namespace ()
  (make-instance 'namespace))

(defun shallow-copy-namespace (namespace)
  (make-instance 'namespace
		 :var (namespace-var namespace)
		 :fun (namespace-fun namespace)
		 :tag (namespace-tag namespace)
		 :lispnsos (namespace-lispnsos namespace)))

(defun augment-namespace (name nso namespace &key (slot (type-of nso)) set-globally)
  (let* ((namespace (shallow-copy-namespace namespace))
	 (alist (slot-value namespace slot)))
    (cond
      (set-globally
       (let* ((global-env-cons (member :global alist :key #'car))
	      (global-env (cdr global-env-cons)))
	 (assert (not (null global-env-cons)))
	 (setf (cdr global-env-cons) (acons name nso global-env))))
      (t
       (setf (slot-value namespace slot) (acons name nso alist))))
    namespace))

(defun augment-namespace-with-lispsym (lispsym nso namespace &key set-globally)
  (let* ((namespace (shallow-copy-namespace namespace))
	 (namespace (augment-namespace (nso-name nso) nso namespace :set-globally set-globally))
	 (namespace (augment-namespace lispsym nso namespace :slot 'lispnsos :set-globally set-globally)))
    namespace))

(defun find-new-name (namespace nso-type &optional (prefix "tmp"))
  "Return a C name prefixed with PREFIX and suffixed with a number so that the name is unique in the known names of type NSO-TYPE in NAMESPACE."
  (let* ((alist (slot-value namespace nso-type))
	 (counter 0))
    (loop do
	 (let* ((name (format nil "~A~A" prefix counter))
		(cell (assoc name alist :test 'equal)))
	   (when (null cell)
	     (return-from find-new-name name)))
	 (incf counter))))

(defun augment-nso (namespace nso &key lispsym insist-on-name set-globally)
  "Augment NAMESPACE with NSO, which has its name set to a unique string, prefixed by (NSO-NAME NSO).
If LISPSYM is non-NIL, NAMESPACE records a mapping from LISPSYM to NSO.
Return the augmented NAMESPACE."
  (declare (optimize (debug 3)))
  (let ((name (if insist-on-name (nso-name nso) (find-new-name namespace (type-of nso) (nso-name nso)))))
    (let* ((nso-type (type-of nso))
	   (alist (slot-value namespace nso-type))
	   (cell (assoc name alist :test 'equal)))
      (assert (null cell) () "Insisted on name ~S in namespace, but it exists already." (nso-name nso)))
    (setf (nso-name nso) name)
    (if lispsym
	(augment-namespace-with-lispsym lispsym nso namespace :set-globally set-globally)
	(augment-namespace (nso-name nso) nso namespace :set-globally set-globally))))

(defun find-var-for-lisp-var (namespace lisp-sym)
  (let* ((c-nso-assoc (assoc lisp-sym (namespace-lispnsos namespace)))
	 (c-nso (cdr c-nso-assoc)))
    (assert c-nso-assoc () "Lisp variable ~A was not augmented to C-namespace~%~A" lisp-sym namespace)
    c-nso))

(defun find-fun-for-lisp-fun (namespace lisp-fun lisp-arguments)
  "LISP-ARGUMENTS must be a list of Lisp-types, or WALKER:VAR, WALKER:SELFEVALOBJECT, or WALKER:THE-FORM instances."
  (declare (optimize (debug 3)))
  (let* ((lispnsos (namespace-lispnsos namespace)))
    (flet ((found-it (possible)
	     (let* ((c-fun-assoc possible)
		    (c-fun (cdr c-fun-assoc)))
	       (assert c-fun-assoc () "Lisp function ~A was not augmented to C-namespace" lisp-fun)
	       (return-from find-fun-for-lisp-fun c-fun))))
      (let* ((lisp-name (walker:nso-name lisp-fun))
	     (possible-by-name (remove-if (lambda (x) (or (eq (car x) :global) (not (eq (walker:nso-name (car x)) lisp-name)))) lispnsos)))
	(assert (not (null possible-by-name)) () "Lisp function ~A was not augmented to C-namespace" lisp-fun)
	(if (= 1 (length possible-by-name))
	    (found-it (car possible-by-name))
	    (let* ((lisp-argument-types (loop for arg in lisp-arguments collect
					     (typecase arg
					       (symbol (convert-type-from-lisp-to-cffi arg))
					       (list (convert-type-from-lisp-to-cffi arg))
					       (walker:var (convert-type-from-lisp-to-cffi (sym-declspec-type arg)))
					       (walker:selfevalobject (convert-type-from-lisp-to-cffi (type-of (walker:selfevalobject-object arg))))
					       (walker:the-form (convert-type-from-lisp-to-cffi (walker:form-type arg)))
					       (t (error "Cannot determine type of argument ~A in a call to:~%~A"
							 (walker-deparse arg nil)
							 (cons (walker-deparse lisp-fun nil)
							       (loop for arg in lisp-arguments collect (walker-deparse arg nil))))))))
		   (possible-by-name-and-types (remove-if (lambda (x) (not (equal lisp-argument-types (cadr (nso-type (cdr x)))))) possible-by-name)))
	      ;;(prind lisp-argument-types) (prind possible-by-name) (prind possible-by-name-and-types)
	      (assert (not (null possible-by-name-and-types)) () "Lisp function ~A was augmented to C-namespace,~%but argument types do not match requested types ~A" lisp-fun lisp-argument-types)
	      (assert (= 1 (length possible-by-name-and-types)) () "Lisp function ~A has multiple C-counterparts:~%~A" lisp-fun possible-by-name-and-types)
	      (found-it (car possible-by-name-and-types))))))))

(defun convert-type-from-cffi-to-c (type name)
  (declare (optimize (debug 3)) (type (or symbol list) type) (type string name))
  ;;(prind "convert-type-from-cffi-to-c" type name)
  (labels ((basetype (type)
	     (case type
	       ((:int) "int")
	       ((:int8) "int8_t")
	       ((:int16) "int16_t")
	       ((:int32) "int32_t")
	       ((:int64) "int64_t")
	       ((:char) "char")
	       ((:float) "float")
	       ((:double) "double")
	       ((:void) "void")
	       ((:string) "char*")
	       ((:v4sf) "v4sf")
	       (t nil)))
	   (count-pointers (type n-pointers)
	     ;;(prind "count-pointers" type n-pointers)
	     (if (listp type)
		 (cond
		   ((eq (car type) :pointer)
		    (assert (null (cddr type))) ;it should be (:pointer (:pointer :int)), not (:pointer (:pointer :int) :bla)
		    (count-pointers (cadr type) (1+ n-pointers)))
		   (t (values n-pointers type)))
		 (values n-pointers type)))
	   (functiondecl (type name n-pointers)
	     (assert (and (listp type) (eq (car type) :function)))
	     ;; currently all compiled functions return nothing. TODO: maybe implement a variable number of return-values by returning an int, the number of values specified in the VALUES-form.
	     (let* ((arguments (cadr type))
		    (values (let ((v (caddr type)))
			      (if (symbolp v)
				  (list v)
				  v)))
		    (values (mapcar (lambda (type) (list :pointer type)) values))
		    (args-and-values (append arguments values)))
	       (format nil
		       "void (*~A~A)(~A)"
		       (join-strings (loop for i below n-pointers collect "*") "")
		       name
		       (join-strings (mapcar #'convert-type-from-cffi-to-c
					     args-and-values
					     (loop for i below (length args-and-values) collect (format nil "a~A" i)))
				     ", ")))))
    (multiple-value-bind (n-pointers type) (count-pointers type 0)
      (let ((basetype (basetype type)))
	;;(prind n-pointers type basetype)
	(cond
	  (basetype (format nil "~A~A ~A" basetype (join-strings (loop for i below n-pointers collect "*") "") name))
	  (t (functiondecl type name n-pointers)))))))

(defun convert-type-from-lisp-to-cffi (type)
  (declare (optimize (debug 3)))
  ;;(when (symbolp type) (prind type (symbol-package type)))
  (cond
    ((and (listp type) (eq (car type) 'function)) ;this is not in CFFI
     (multiple-value-bind (arguments values) (parse-ftype-declspec type)
       (list :function
	     (loop for type in arguments collect (convert-type-from-lisp-to-cffi type))
	     (loop for type in values collect (convert-type-from-lisp-to-cffi type)))))
    ((subtypep type 'integer) :int)
    ((subtypep type 'symbol) :int)
    ((subtypep type 'single-float) :float)
    ((subtypep type 'double-float) :double)
    ((subtypep type nil) :void)
    ((eq type 'string) :string)
    ((eq type 'v4s-float) :v4sf)
    (t (error "unknown LISP type ~S" type))))
    
(defun augment-namespace-with-builtin-function (symbol c-name argument-types values-types namespace)
  (let* ((ast (walker:parse-with-empty-namespaces `(flet ((,symbol ()))
						     (declare (ftype (function (,@argument-types) (values ,@values-types)) ,symbol))
						     (,symbol))))
	 (fun (walker:form-fun (car (walker:form-body ast))))
	 (c-type (convert-type-from-lisp-to-cffi (sym-declspec-type fun)))
	 (c-fun (make-fun c-name c-type)))
    (augment-nso namespace c-fun :lispsym fun :insist-on-name t)))

(defun convert-name (name)
  "Return NAME converted to a C variable or function name."
  (let* ((name-string (string (walker:nso-name name)))
	 (name-string (if (find (elt name-string 0) "0123456789")
			  (concatenate 'string "_" name-string)
			  name-string)))
    (substitute #\e #\= (substitute #\l #\< (substitute #\g #\> (substitute #\d #\/ (substitute #\p #\+ (substitute #\_ #\- name-string))))))))

(defun c-code (&rest l)
  (flatten l))

(defun c-scope (&rest rest)
  (declare (optimize (debug 3)))
  (concatenate 'list
	       (list (format nil "{"))
	       (mapcar (lambda (s) (concatenate 'string "  " s)) (flatten rest))
	       (list (format nil "}"))))

(defun c-declaration (var &optional initial-value)
  (declare (optimize (debug 3)))
  (let ((prefix (format nil "~A" (convert-type-from-cffi-to-c (nso-type var) (nso-name var))))
	(suffix (if initial-value (format nil " = ~A;" initial-value) (format nil ";"))))
    (format nil "~A~A" prefix suffix)))

(defun c-assign (l-name r-value)
  (declare (optimize (debug 3)))
  (let* ((deref-p (let ((type (nso-type l-name))) (and (listp type) (eq (car type) :pointer))))
	 (deref (if deref-p "*" ""))
	 (assign-code (format nil "~A~A = ~A;" deref (nso-name l-name) r-value)))
    assign-code))

(defun emit-c (form &key (values-types '((:pointer :int))))
  (declare (optimize (debug 3)))
  (let ((values (loop for i from 0 for type in values-types collect (make-var (format nil "value~A" i) type)))
	(namespace (make-empty-namespace))
	(builtin-functions '(;; arithmetic: int int
			     (+ "plus_int_int" (integer integer) (integer))
			     (* "multiply_int_int" (integer integer) (integer))
			     (- "minus_int_int" (integer integer) (integer))
			     (/ "divide_int_int" (integer integer) (integer))
			     (max "max_int_int" (integer integer) (integer))
			     (min "min_int_int" (integer integer) (integer))
			     ;; arithmetic float float
			     (+ "plus_float_float" (single-float single-float) (single-float))
			     (* "multiply_float_float" (single-float single-float) (single-float))
			     (- "minus_float_float" (single-float single-float) (single-float))
			     (/ "divide_float_float" (single-float single-float) (single-float))
			     (max "max_float_float" (single-float single-float) (single-float))
			     (min "min_float_float" (single-float single-float) (single-float))
			     ;; arithmetic float int
			     (+ "plus_float_int" (single-float integer) (single-float))
			     (* "multiply_float_int" (single-float integer) (single-float))
			     (- "minus_float_int" (single-float integer) (single-float))
			     (/ "divide_float_int" (single-float integer) (single-float))
			     (max "max_float_int" (single-float integer) (single-float))
			     (min "min_float_int" (single-float integer) (single-float))
			     ;; arithmetic int float
			     (+ "plus_int_float" (integer single-float) (single-float))
			     (* "multiply_int_float" (integer single-float) (single-float))
			     (- "minus_int_float" (integer single-float) (single-float))
			     (/ "divide_int_float" (integer single-float) (single-float))
			     (max "max_int_float" (integer single-float) (single-float))
			     (min "min_int_float" (integer single-float) (single-float))
			     ;; arithmetic: int
			     (1+ "plusone_int" (integer) (integer))
			     (1- "minusone_int" (integer) (integer))
			     (abs "abs_int" (integer) (integer))
			     (signum "signum_int" (integer) (integer))
			     ;; arithmetic: float
			     (1+ "plusone_float" (single-float) (single-float))
			     (1- "minusone_float" (single-float) (single-float))
			     (abs "abs_float" (single-float) (single-float))
			     (signum "signum_float" (single-float) (single-float))
			     ;; misc
			     (eq "eq_int_int" (symbol symbol) (integer))
			     ;; comparison: int int
			     (< "less_int_int" (integer integer) (integer))
			     (<= "lessequal_int_int" (integer integer) (integer))
			     (> "greater_int_int" (integer integer) (integer))
			     (>= "greaterequal_int_int" (integer integer) (integer))
			     (= "equal_int_int" (integer integer) (integer))
			     (/= "notequal_int_int" (integer integer) (integer))
			     ;; comparison: float float
			     (< "less_float_float" (single-float single-float) (integer))
			     (<= "lessequal_float_float" (single-float single-float) (integer))
			     (> "greater_float_float" (single-float single-float) (integer))
			     (>= "greaterequal_float_float" (single-float single-float) (integer))
			     (= "equal_float_float" (single-float single-float) (integer))
			     (/= "notequal_float_float" (single-float single-float) (integer))
			     ;; comparison: float int
			     (< "less_float_int" (single-float integer) (integer))
			     (<= "lessequal_float_int" (single-float integer) (integer))
			     (> "greater_float_int" (single-float integer) (integer))
			     (>= "greaterequal_float_int" (single-float integer) (integer))
			     (= "equal_float_int" (single-float integer) (integer))
			     (/= "notequal_float_int" (single-float integer) (integer))
			     ;; comparison: int float
			     (< "less_int_float_int" (integer single-float) (integer))
			     (<= "lessequal_int_float" (integer single-float) (integer))
			     (> "greater_int_float" (integer single-float) (integer))
			     (>= "greaterequal_int_float" (integer single-float) (integer))
			     (= "equal_int_float" (integer single-float) (integer))
			     (/= "notequal_int_float" (integer single-float) (integer))
			     ;; logical
			     (and-f "and_f_int_int" (integer integer) (integer))
			     (or-f "or_f_int_int" (integer integer) (integer))
			     (not "not_int" (integer) (integer))
			     (fail-with-message "fail_with_message_str" (string) ())
			     ;; type conversion
			     (float "float_int" (integer) (single-float))
			     (floor "floor_float" (single-float) (integer single-float))
			     ;; voxelneu-specific functions
			     (v4-x "v4_x_v4sf" (v4s-float) (single-float))
			     (v4-y "v4_y_v4sf" (v4s-float) (single-float))
			     (v4-z "v4_z_v4sf" (v4s-float) (single-float))
			     (v4-w "v4_w_v4sf" (v4s-float) (single-float))
			     (v4-replace-x "v4_replace_x_v4sf" (v4s-float single-float) (v4s-float))
			     (v4-replace-y "v4_replace_y_v4sf" (v4s-float single-float) (v4s-float))
			     (v4-replace-z "v4_replace_z_v4sf" (v4s-float single-float) (v4s-float))
			     (v4-replace-w "v4_replace_w_v4sf" (v4s-float single-float) (v4s-float))
			     (v4+ "v4_plus_v4sf" (v4s-float v4s-float) (v4s-float))
			     (v4- "v4_minus_v4sf" (v4s-float v4s-float) (v4s-float))
			     (v4.* "v4_scale_v4sf" (v4s-float single-float) (v4s-float))
			     (make-v4 "make_v4_float_float_float_float" (single-float single-float single-float single-float) (v4s-float))
			     (fractional-part "fractional_part_float" (single-float) (single-float))
			     (prind-helper "prind_helper_string_int" (string integer) ())
			     (prind-helper "prind_helper_string_float" (string single-float) ())
			     (prind-helper "prind_helper_string_v4sf" (string v4s-float) ())
			     (prind-helper "prind_helper_string_string" (string string) ())
			     (prind-helper-format "prind_helper_format_string" (string) ())
			     ;;(advance-ray-3d "ADVANCE_RAY_3D0" (v4s-float v4s-float single-float single-float single-float single-float single-float single-float integer) (v4s-float single-float symbol))
			     ;;(intersect-border "INTERSECT_BORDER0" (v4s-float v4s-float v4s-float single-float single-float single-float single-float single-float single-float single-float) (v4s-float single-float))
			     ;;(outside-border-p "OUTSIDE_BORDER0" (v4s-float single-float single-float single-float) (boolean))
			     ;;(scan-line-continuous "SCAN_LINE_CONTINUOUS0" (v4s-float v4s-float single-float (function (v4s-float) boolean) single-float single-float single-float single-float single-float single-float) (v4s-float v4s-float boolean single-float))
			     ))
	(global-variables '((*automatic-stepsize* "AUTOMATIC_STEPSIZE" boolean)
			    (*exact-exit* "EXACT_EXIT" boolean)
			    (*mouse-info* "MOUSE_INFO" boolean)
			    (*step-size* "STEP_SIZE" single-float)
			    (*integer-steps* "INTEGER_STEPS" boolean)
			    (*skip-near-z* "SKIP_NEAR_Z" boolean)
			    (*near-z* "NEAR_Z" single-float)
			    (*scan-line-type* "SCAN_LINE_TYPE" symbol)
			    )))
    (loop for (lisp-sym c-name args-types values-types) in builtin-functions do
	 (setf namespace (augment-namespace-with-builtin-function lisp-sym c-name args-types values-types namespace)))
    (let ((free-namespace (walker:make-empty-free-namespace))
	  (macros
	   '(and or when cond assert error loop setf incf prind)))
	   ;;'(when cond assert error loop setf incf prind)))
      (loop for macro in macros do
	   (walker:augment-free-namespace (make-instance 'walker:fun :name macro :freep t :declspecs nil :macrop t) free-namespace))
      (loop for (lisp-sym c-name type) in global-variables do
	   (let ((walker-sym (walker:namespace-lookup/create 'walker:var lisp-sym (walker:make-empty-lexical-namespace) free-namespace)))
	     (walker:parse-declspecs `((type ,type ,lisp-sym)) (walker:make-empty-lexical-namespace) free-namespace nil)
	     (setf namespace (augment-nso namespace (make-var c-name (convert-type-from-lisp-to-cffi type)) :lispsym walker-sym :insist-on-name t))))
      (let* ((ast (walker-parse form nil :free-namespace free-namespace))
	     (c-lines (emitc ast namespace values))
	     (c-code (join-strings c-lines (format nil "~%"))))
	(format nil "~A" c-code)))))

;; Example: (lispccompiler::emit-c '(let ((a 1.0) (b 1.0) (c 1.0)) (declare (type single-float a b c)) a b (+ (the single-float (+ a b)) c)) :values-types '((:pointer :float)))

(defgeneric emitc (generalform namespace values-vars)
  (declare (optimize (debug 3)))
  (:documentation "The emitc methods return three values: the generated code to compute the variable, the variable name of the value of the AST, and the namespace augmented with the variable."))

(defmethod emitc-body (body (namespace namespace) values)
  (declare (optimize (debug 3)))
  (c-code (loop for body in (butlast body) collect (c-code (emitc body namespace nil) ";"))
	  (emitc (car (last body)) namespace values)))

(defmethod emitc ((obj walker:selfevalobject) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let ((obj (walker:selfevalobject-object obj)))
    (cond
      ((or (null obj) (eq obj t))
       (unless (null values)
	 (c-assign (car values) (if obj 1 0))))
      ((or (subtypep (type-of obj) 'integer) (subtypep (type-of obj) 'float))
       (unless (null values)
	 (c-assign (car values) obj)))
      ((subtypep (type-of obj) 'string)
       (unless (null values)
	 (c-assign (car values) (format nil "\"~A\"" obj))))
      (t
       (error "unknown type of selfevalobject ~S" obj)))))

(defmethod emitc ((ast walker:quote-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((obj (walker:form-object ast))
	 (c-obj (format nil "~A /*~A*/"
			(etypecase obj
			  (integer obj)
			  (single-float obj)
			  (symbol (sxhash obj)))
			obj)))
    (unless (null values)
      (c-assign (car values) c-obj))))

(defmethod emitc ((nso walker:sym) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (cond
    ((keywordp (walker:nso-name nso))
     (unless (null values)
       (c-assign (car values) (sxhash (walker:nso-name nso)))))
    (t
     (let* ((c-nso (find-var-for-lisp-var namespace nso)))
       (if (null values)
	   (nso-name c-nso)
	   (c-assign (car values) (nso-name c-nso)))))))

(defun c-defun-head (fun parameters values)
  (declare (optimize (debug 3)))
  (let* ((c-fun-type (let ((type (nso-type fun))) (assert (eq (car type) :function)) type))
	 (parameters-type (cadr c-fun-type))
	 (values-type (caddr c-fun-type)))
    (loop for p1 in parameters-type for p2 in parameters do (assert (equal p1 (nso-type p2))))
    (loop for v1 in values-type for v2 in values do (assert (equal v1 (cadr (nso-type v2)))))
    (format nil "void ~A (~A)"
	    (nso-name fun)
	    (join-strings (append
			   (mapcar (lambda (x) (format nil "~A" (convert-type-from-cffi-to-c (nso-type x) (nso-name x)))) parameters)
			   (mapcar (lambda (x) (format nil "~A" (convert-type-from-cffi-to-c (nso-type x) (nso-name x)))) values))
			  ", "))))

(defun emitc-function (ast c-sym declspec-type namespace values)
  (declare (optimize (debug 3))
	   (ignore values))
  (assert (not (null declspec-type)) () "Unknown function type for function ~S" c-sym)
  (let* ((parameters (walker:llist-required (walker:form-llist ast)))
	 (c-parameters (loop for par in parameters collect
			    (let* ((c-par (walker:argument-var par))
				   (declspec-type0 (sym-declspec-type c-par))
				   (declspec-type (convert-type-from-lisp-to-cffi declspec-type0))
				   (sym (make-var (convert-name c-par) declspec-type)))
			      (assert (not (null declspec-type0)) () "unknown parameter type")
			      (setf namespace (augment-nso namespace sym :lispsym c-par))
			      sym)))
	 (c-values (loop for type in (caddr (nso-type c-sym)) for i from 0 collect
			(let* ((sym (make-var (format nil "value~A" i) (list :pointer type))))
			  (assert (not (null type)) () "unknown value type")
			  (setf namespace (augment-nso namespace sym))
			  sym))))
    (setf (nso-values c-sym) c-values) ;store VALUES for RETURN-FROM.
    (let ((body-code (emitc-body (walker:form-body ast) namespace c-values)))
      (c-code (list (format nil "~A" (c-defun-head c-sym c-parameters c-values)))
	      (c-scope body-code)))))

(defun emitc-let-let*-flet-labels (ast namespace values)
  (declare (optimize (debug 3)))
  (assert (or (subtypep (type-of ast) 'walker:let-form) (subtypep (type-of ast) 'walker:let*-form) (subtypep (type-of ast) 'walker:flet-form) (subtypep (type-of ast) 'walker:labels-form)))
  (let* ((bindings-namespace namespace) ;the namespace augmented with all binding variables
	 (bindings-syms (loop for binding in (walker:form-bindings ast) collect
			     (let* ((walker-sym (walker:form-sym binding))
				    (declspec-type0 (sym-declspec-type walker-sym))
				    (declspec-type (convert-type-from-lisp-to-cffi declspec-type0))
				    (make-sym-function (etypecase ast (walker:let-form #'make-var) (walker:let*-form #'make-var) (walker:flet-form #'make-fun) (walker:labels-form #'make-fun)))
				    (binding-sym (funcall make-sym-function (convert-name walker-sym) declspec-type))
				    (pbinding-sym (funcall make-sym-function (format nil "p~A" (convert-name walker-sym)) (list :pointer declspec-type))))
			       (assert (not (null declspec-type0)) () "unknown binding type")
			       (setf bindings-namespace (augment-nso bindings-namespace binding-sym :lispsym walker-sym))
			       (setf bindings-namespace (augment-nso bindings-namespace pbinding-sym))
			       (list binding-sym pbinding-sym))))
	 (bindings-code (loop for binding in (walker:form-bindings ast) for (binding-sym pbinding-sym) in bindings-syms collect
			     (cond
			       ((or (subtypep (type-of ast) 'walker:let-form) (subtypep (type-of ast) 'walker:let*-form))
				(let* ((value-namespace (etypecase ast (walker:let-form namespace) (walker:let*-form bindings-namespace)))
				       (value-code (emitc (walker:form-value binding) value-namespace (list pbinding-sym))))
				  (c-code (c-declaration binding-sym)
					  (c-declaration pbinding-sym (format nil "&~A" (nso-name binding-sym)))
					  (c-scope value-code))))
			       ((or (subtypep (type-of ast) 'walker:flet-form) (subtypep (type-of ast) 'walker:labels-form))
				(let ((namespace (etypecase ast (walker:flet-form namespace) (walker:labels-form bindings-namespace))))
				  (emitc-function binding binding-sym (nso-type binding-sym) namespace values))))))
	 (body-code (emitc-body (walker:form-body ast) bindings-namespace values)))
    (c-code (c-scope bindings-code
		     body-code))))

(defmethod emitc ((ast walker:let-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (emitc-let-let*-flet-labels ast namespace values))

(defmethod emitc ((ast walker:let*-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (emitc-let-let*-flet-labels ast namespace values))

(defmethod emitc ((ast walker:flet-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (emitc-let-let*-flet-labels ast namespace values))

(defmethod emitc ((ast walker:labels-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (emitc-let-let*-flet-labels ast namespace values))

(defun emitc-function-application-funcall (ast namespace values)
  (declare (optimize (debug 3)))
  (prind (walker-deparse ast nil))
  (let* ((fun (etypecase ast (walker:application-form (walker:form-fun ast)) (funcall-form (walker:form-var ast))))
	 (arguments (walker:form-arguments ast))
	 (c-fun (etypecase ast (walker:application-form (find-fun-for-lisp-fun namespace fun arguments)) (funcall-form (find-var-for-lisp-var namespace fun))))
	 (fun-type (nso-type c-fun))
	 (fun-arguments-type (cadr fun-type))
	 (fun-values-type (caddr fun-type))
	 (arguments-namespace namespace)
	 (arguments-syms (loop for i from 0 for arg in arguments for arg-type in fun-arguments-type collect
			      (let ((arg-sym (make-var (format nil "arg~A" i) arg-type))
				    (parg-sym (make-var (format nil "parg~A" i) (list :pointer arg-type))))
				(setf arguments-namespace (augment-nso arguments-namespace arg-sym))
				(setf arguments-namespace (augment-nso arguments-namespace parg-sym))
				(list arg-sym parg-sym))))
	 (arguments-code (loop for (arg-sym parg-sym) in arguments-syms for arg in arguments collect
				(c-code (c-scope
					 (emitc arg arguments-namespace (list parg-sym))))))
	 (all-values (loop for value-type in fun-values-type for i from 0 collect
			  (if (< i (length values))
			      (elt values i)
			      (let ((value-sym (make-var (format nil "value~A" i) (list :pointer value-type))))
				(setf arguments-namespace (augment-nso arguments-namespace value-sym))
				value-sym)))))
    (loop for value in values for value-type in fun-values-type do
	 (assert (eq (cadr (nso-type value)) value-type) () "Wrong type for ~S, expected (:POINTER ~S), in application form:~%~S" value value-type (walker-deparse ast nil)))
    (c-code (c-scope (loop for (arg-sym parg-sym) in arguments-syms collect
			  (c-code
			   (c-declaration arg-sym)
			   (c-declaration parg-sym (format nil "&~A" (nso-name arg-sym)))))
		     (loop for sym in all-values for i from 0 collect
			  (unless (< i (length values))
			    (c-code (c-declaration sym))))
		     arguments-code
		     (format nil "~A(~A);"
			     (etypecase ast (walker:application-form (nso-name c-fun)) (funcall-form (format nil "(*~A)" (nso-name c-fun))))
			     (join-strings
			      (concatenate 'list
					   (mapcar #'nso-name (mapcar #'car arguments-syms))
					   (mapcar (lambda (v) (format nil "~A" (nso-name v))) all-values))
			      ", "))))))
  
(defmethod emitc ((ast walker:application-form) (namespace namespace) values)
  (emitc-function-application-funcall ast namespace values))

(defmethod emitc ((ast walker:function-form) (namespace namespace) values)
  (let* ((fun (walker:form-object ast))
	 (lisp-arguments (parse-ftype-declspec (sym-declspec-type fun)))
	 (c-fun (find-fun-for-lisp-fun namespace fun lisp-arguments)))
    (c-code (c-assign (car values) (nso-name c-fun)))))

(defmethod emitc ((ast funcall-form) (namespace namespace) values)
  (emitc-function-application-funcall ast namespace values))
  
(defmethod emitc ((ast walker:if-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((test-var (make-var "if_test" :int))
	 (ptest-var (make-var "pif_test" '(:pointer :int)))
	 (namespace (augment-nso namespace test-var))
	 (namespace (augment-nso namespace ptest-var))
	 (test-code (emitc (walker:form-test ast) namespace (list ptest-var)))
	 (then-code (emitc (walker:form-then ast) namespace values))
	 (else-code (when (walker:form-else ast) (emitc (walker:form-else ast) namespace values))))
    (c-code (c-scope (c-declaration test-var)
		     (c-declaration ptest-var (format nil "&~A" (nso-name test-var)))
		     test-code
		     (format nil "if (~A)" (nso-name test-var))
		     (c-scope then-code)
		     (when else-code
		       (c-code (format nil "else")
			       (c-scope else-code)))))))

(defmethod emitc ((ast values-form) (namespace namespace) values)  
  (declare (optimize (debug 3)))
  (let* ((body (walker:form-body ast)))
    (assert (<= (length body) (length values)))
    (c-code (c-scope
	     (loop for object in body for value in values collect
		  (emitc object namespace (list value)))))))

(defmethod emitc ((ast multiple-value-bind-form) (namespace namespace) values)  
  (declare (optimize (debug 3)))
  (let* ((vars-namespace namespace) ;the namespace augmented with all binding variables
	 (vars-syms (loop for walker-sym in (walker:form-vars ast) collect
			 (let* ((declspec-type0 (sym-declspec-type walker-sym))
				(declspec-type (convert-type-from-lisp-to-cffi declspec-type0))
				(container-sym (make-var (convert-name walker-sym) declspec-type))
				(var-sym (make-var (format nil "value_~A" (convert-name walker-sym)) (list :pointer declspec-type))))
			   (assert (not (null declspec-type0)) () "unknown binding type")
			   (setf vars-namespace (augment-nso vars-namespace container-sym :lispsym walker-sym))
			   (setf vars-namespace (augment-nso vars-namespace var-sym))
			   (list container-sym var-sym))))
	 (body-code (emitc-body (walker:form-body ast) vars-namespace values)))
    (c-code (c-scope (loop for (container-sym var-sym) in vars-syms collect
			  (list (c-declaration container-sym)
				(c-declaration var-sym (format nil "&~A" (nso-name container-sym)))))
		     (emitc (walker:form-values ast) namespace (mapcar #'cadr vars-syms))
		     body-code))))

(defmethod emitc ((ast walker:the-form) (namespace namespace) values)  
  (declare (optimize (debug 3)))
  (c-code (emitc (walker:form-value ast) namespace values)))

(defmethod emitc ((ast walker:setq-form) (namespace namespace) values)  
  (declare (optimize (debug 3)))
  (c-code (loop for var in (walker:form-vars ast) for form-value in (walker:form-values ast) collect
	       (let* ((c-var (find-var-for-lisp-var namespace var))
		      (pvar-sym (make-var (format nil "p~A" (nso-name c-var)) (list :pointer (nso-type c-var))))
		      (namespace (augment-nso namespace pvar-sym)))
		 (c-scope (c-declaration pvar-sym (format nil "&~A" (nso-name c-var)))
			  (emitc form-value namespace (list pvar-sym)))))))

(defmethod emitc ((ast defun-form) (namespace namespace) values)
  (let* ((defun-namespace namespace)
	 (walker-sym (walker:form-sym ast))
	 (declspec-type0 (sym-declspec-type walker-sym))
	 (declspec-type (convert-type-from-lisp-to-cffi declspec-type0))
	 (c-sym (let ((c-sym (make-fun (convert-name walker-sym) declspec-type)))
		  (setf defun-namespace (augment-nso defun-namespace c-sym :lispsym walker-sym :insist-on-name t :set-globally t))
		  c-sym)))
    (assert (not (null declspec-type0)) () "Unknown function type for function ~S" walker-sym)
    (emitc-function ast c-sym declspec-type defun-namespace values)))
  
(defmethod emitc ((ast walker:progn-form) (namespace namespace) values)
  (c-code (c-scope (emitc-body (walker:form-body ast) namespace values))))

(defmethod emitc ((ast declaim-form) (namespace namespace) values)  
  nil)

(defmethod emitc ((ast walker:tagbody-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  ;; do not use #'EMITC-BODY, it returns the value of the last body-form, but TAGBODY returns NIL.
  (c-code (loop for body in (walker:form-body ast) collect (c-code (emitc body namespace nil) ";"))))

(defmethod emitc ((ast walker:tag) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (prind ast)
  (c-code (format nil "~A:;" (convert-name ast))))

(defmethod emitc ((ast walker:go-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (c-code (format nil "goto ~A;" (convert-name (walker:form-tag ast)))))

(defmethod emitc ((ast walker:return-from-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  ;; Check that the (RETURN-FROM NAME) is in the body of NAME, not another function inside NAME.
  (let ((blo (walker:form-blo ast))
	(current ast))
    (assert
     (loop do
	  (setf current (walker:form-parent current))
	  (cond
	    ((null current)
	     (return nil))
	    ((subtypep (type-of current) 'walker:functiondef)
	     ;;(prind (walker:form-blo current) blo)
	     (when (eq blo (walker:form-blo current))
	       (return t)))))
     () "RETURN-FROM-form is not in the body of ~A, but in a sub-function of it" blo))
  (let* ((blo (walker:form-blo ast))
	 (fun (walker:form-sym (walker:nso-definition blo)))
	 (lisp-arguments (parse-ftype-declspec (sym-declspec-type fun)))
	 (c-fun (find-fun-for-lisp-fun namespace fun lisp-arguments))
	 (c-fun-values (nso-values c-fun)))
    (prind "RETURN-FROM-form" blo c-fun-values)
    (c-code (emitc (walker:form-value ast) namespace c-fun-values)
	    ;;(format nil "return ~A;" (length c-fun-values)))))
	    (format nil "return;"))))

(defmethod emitc ((ast walker:macroapplication-form) (namespace namespace) values)
  "This method rewrites some macro-applications to known special forms and calls #'EMITC on them."
  (declare (optimize (debug 3)))
  (prind (walker-deparse ast nil))
  (let* ((function-symbol (walker:nso-name (walker:form-fun ast)))
	 (arguments (walker:form-arguments ast))
	 (lexical-namespace (walker:form-lexicalnamespace ast))
	 (free-namespace (walker:form-freenamespace ast))
	 (form
	  (ecase function-symbol
	    ((and)
	     (labels ((helper (arguments)
			(if (null arguments)
			    t
			    `(if ,(car arguments)
				 ,(helper (cdr arguments))
				 nil))))
	       (helper arguments)))
	    ((or)
	     (labels ((helper (arguments)
			(if (null arguments)
			    nil
			    `(if ,(car arguments)
				 t
				 ,(helper (cdr arguments))))))
	       (helper arguments)))
	    ((when)
	     (labels ((helper (arguments)
			`(if ,(car arguments)
			     ,(list* 'progn (cdr arguments)))))
	       (helper arguments)))
	    ((cond)
	     (labels ((helper (arguments)
			(if (null arguments)
			    nil
			    (let ((test (caar arguments)))
			      (if (eq test t)
				  (list* 'progn (cdar arguments))
				  `(if ,test
				       ,(list* 'progn (cdar arguments))
				       ,(helper (cdr arguments))))))))
	       (helper arguments)))
	    ((assert)
	     (labels ((helper (test-form &optional place-form datum-form &rest argument-form)
			(declare (ignore place-form argument-form))
			(let* ((error-message (substitute #\Space #\Newline (format nil "failed assertion: ~S datum: ~A" test-form datum-form))))
			  ;; TODO: implement printing ARGUMENT-FORMs
			  `(if (not ,test-form)
			       (fail-with-message ,error-message)))))
	       (apply #'helper arguments)))
	    ((error)
	     (labels ((helper (datum-form &rest arguments-form)
			`(assert nil () ,datum-form ,@arguments-form)))
	       (apply #'helper arguments)))
	    ((loop)
	     (labels ((helper (arguments)
			(cond
			  ((eq (car arguments) 'do)
			   (let ((start (gensym "START")))
			     `(tagbody
				 ,start
				 ,@(cdr arguments)
				 (go ,start))))
			  (t (error "LOOP feature not implemented")))))
	       (helper arguments)))
	    ((setf)
	     (labels ((helper (arguments)
			`(setq ,@arguments)))
	       (helper arguments)))
	    ((incf)
	     (labels ((helper (arguments)
			(let ((inc (if (null (cdr arguments)) 1 (cadr arguments))))
			  `(setq ,(car arguments) (+ ,(car arguments) ,inc)))))
	       (helper arguments)))
	    ((prind)
	     (labels ((helper (arguments)
			(let ((ins (loop for arg in arguments collect
					(let* ((arg-ast (walker-parse arg nil :lexical-namespace lexical-namespace :free-namespace free-namespace))
					       (arg-type (cond
							   ((and (typep arg-ast 'walker:selfevalobject) (stringp (walker:selfevalobject-object arg-ast))) 'string)
							   ((typep arg-ast 'walker:the-form) (walker:form-type arg-ast))
							   (t (sym-declspec-type arg-ast)))))
					  `(prind-helper (the string ,(format nil "~A:" arg)) (the ,arg-type ,arg))))))
			  `(progn
			     ,@(apply #'append (loop for in in (butlast ins) collect `(,in (prind-helper-format " "))))
			     ,@(unless (null ins) `(,(car (last ins)) (prind-helper-format "\\n")))))))
	       (helper arguments)))
	    )))
    (prind form)
    (let* ((ast (walker-parse form (walker:form-parent ast) :lexical-namespace lexical-namespace :free-namespace free-namespace)))
      (emitc ast namespace values))))

(defun compile-voxelneu ()
  (let ((code 
	 (with-open-file (stream "~/voxelneu/lisp/all-code.lisp")
	   (read stream))))
    (defparameter *all-code* code)
    (let ((c-code (emit-c code)))
      (with-open-file (stream "~/voxelneu/lisp/all-code.c" :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format stream "~A" (subseq c-code 1 (1- (length c-code))))))))
      
