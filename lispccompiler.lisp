;;;; Lisp to C Compiler

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :walker)

;;;; Package

;; Example: (lispccompiler::emit-c '(let ((a 1.0) (b 1.0) (c 1.0)) (declare (type single-float a b c)) a b (+ (the single-float (+ a b)) c)) :values-types '((:pointer :float)))


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
(defclass nth-value-form (walker:form)
  ((value :initarg :value :accessor walker:form-value :type generalform)
   (values :initarg :values :accessor walker:form-values :type generalform)))

(defmethod print-object ((object multiple-value-bind-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~A" (walker:form-vars object) (walker:form-values object) (walker:format-body object t nil))))
(defmethod print-object ((object values-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (walker:format-body object nil nil))))
(defmethod print-object ((object nth-value-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (walker:form-value object) (walker:form-values object))))

(defun parse-some-macros-p (form lexical-namespace free-namespace parent &key customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function)
  (declare (ignore customparsep-function customparse-function customparsedeclspecp-function customparsedeclspec-function lexical-namespace free-namespace parent))
  (and (listp form)
       (let ((head (car form)))
	 (find head '(multiple-value-bind values nth-value)))))

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
	))))

;;;; The C backend.

(defclass nso ()
  ((name :initarg :name :accessor nso-name :type string)
   (scope :initarg :scope :accessor nso-scope :type list :documentation "The scope the NSO is defined or declared in.")))

(defclass sym (nso)
  ((type :initarg :type :accessor nso-type :type (or symbol list))))

(defclass var (sym)
  ())

(defclass fun (sym)
  ())

(defclass tag (nso)
  ())

(defmethod print-object ((object sym) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~A"  (nso-type object) (nso-name object))))

(defclass namespace ()
  ((var :initform nil :initarg :var :accessor namespace-var)
   (fun :initform nil :initarg :fun :accessor namespace-fun)
   (tag :initform nil :initarg :tag :accessor namespace-tag)
   (lispnsos :initform nil :initarg :lispnsos :accessor namespace-lispnsos :documentation "A mapping from WALKER:NSO to NSO.")))

(defmethod print-object ((object namespace) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "VARs:~S FUNs:~S TAGs:~S LISPNSOS:~S" (namespace-var object) (namespace-fun object) (namespace-tag object) (namespace-lispnsos object))))

(defun make-empty-namespace ()
  (make-instance 'namespace))

(defmethod shallow-copy-namespace ((namespace namespace))
  (make-instance 'namespace
		 :var (namespace-var namespace)
		 :fun (namespace-fun namespace)
		 :tag (namespace-tag namespace)
		 :lispnsos (namespace-lispnsos namespace)))

(defmethod augment-namespace ((nso nso) (namespace namespace))
  (let ((namespace (shallow-copy-namespace namespace))
	(slot (cond
		((subtypep (type-of nso) 'var) 'var)
		((subtypep (type-of nso) 'fun) 'fun)
		((subtypep (type-of nso) 'tag) 'tag)
		(t (error "unknown NSO type ~A" (type-of nso))))))
    (setf (slot-value namespace slot)
	  (acons (nso-name nso) nso (slot-value namespace slot)))
    namespace))

(defmethod augment-namespace-with-lispsym ((nso nso) (namespace namespace) lispsym)
  (let* ((namespace (shallow-copy-namespace namespace))
	 (namespace (augment-namespace nso namespace)))
    (setf (namespace-lispnsos namespace) (acons lispsym nso (namespace-lispnsos namespace)))
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

(defun make-var (name type)
  (make-instance 'var :name name :type type))

(defun make-fun (name type)
  (make-instance 'fun :name name :type type))

(defun make-tag (name)
  (make-instance 'tag :name name))

(defun augment-nso (namespace nso &key (lispsym nil) (insist-on-name nil))
  "Augment NAMESPACE with NSO, which has its name set to a unique string, prefixed by (NSO-NAME NSO).
If LISPSYM is non-NIL, NAMESPACE records a mapping from LISPSYM to NSO.
Return the augmented NAMESPACE."
  (declare (optimize (debug 3)))
  (let* ((name (if insist-on-name (nso-name nso) (find-new-name namespace (type-of nso) (nso-name nso)))))
    (let* ((nso-type (type-of nso))
	   (alist (slot-value namespace nso-type))
	   (cell (assoc name alist :test 'equal)))
      (assert (null cell) () "Insisted on name ~S in namespace, but it exists already." (nso-name nso)))
    (setf (nso-name nso) name)
    (if lispsym
	(augment-namespace-with-lispsym nso namespace lispsym)
	(augment-namespace nso namespace))))

(defun find-var-for-lisp-var (namespace lisp-sym)
  (let* ((c-nso-assoc (assoc lisp-sym (namespace-lispnsos namespace)))
	 (c-nso (cdr c-nso-assoc)))
    (assert c-nso-assoc () "Lisp variable ~A was not augmented to C-namespace ~A" lisp-sym namespace)
    c-nso))

(defun find-fun-for-lisp-fun (namespace lisp-fun lisp-arguments)
  (declare (optimize (debug 3)))
  (let* ((lisp-argument-types (loop for arg in lisp-arguments collect
				   (etypecase arg
				     (walker:var (convert-type-from-lisp-to-cffi (sym-declspec-type arg)))
				     (walker:selfevalobject (convert-type-from-lisp-to-cffi (type-of (walker:selfevalobject-object arg))))
				     (walker:the-form (convert-type-from-lisp-to-cffi (walker:form-type arg))))))
	 (lispnsos (namespace-lispnsos namespace)))
    (let* ((possible-by-name (remove-if-not (lambda (x) (and (eq (walker:nso-name lisp-fun) (walker:nso-name (car x))))) lispnsos))
	   (possible-by-name-and-types (remove-if-not (lambda (x) (equal lisp-argument-types (cadr (nso-type (cdr x))))) possible-by-name)))
      ;;(prind lisp-argument-types) (prind possible-by-name) (prind possible-by-name-and-types)
      (assert (not (null possible-by-name)) () "Lisp function ~A was not augmented to C-namespace ~A" lisp-fun namespace)
      (assert (not (null possible-by-name-and-types)) () "Lisp function ~A was augmented to C-namespace, but argument types do not match requested types ~A" lisp-fun lisp-argument-types)
      (let* ((c-fun-assoc (car possible-by-name-and-types))
	     (c-fun (cdr c-fun-assoc)))
	(assert c-fun-assoc () "Lisp function ~A was not augmented to C-namespace ~A" lisp-fun namespace)
	c-fun))))

(defun convert-type-from-cffi-to-c (type)
  ;; TODO: FIXME: this function sucks and in addition does no error checking... look at the type conversion code in CFFI maybe.
  (labels ((rec (type)
	     (ecase (car type)
	       ((nil) "")
	       ((:int) (assert (null (cdr type))) "int")
	       ((:int8) (assert (null (cdr type))) "int8_t")
	       ((:int16) (assert (null (cdr type))) "int16_t")
	       ((:int32) (assert (null (cdr type))) "int32_t")
	       ((:int64) (assert (null (cdr type))) "int64_t")
	       ((:char) (assert (null (cdr type))) "char")
	       ((:float) (assert (null (cdr type))) "float")
	       ((:double) (assert (null (cdr type))) "double")
	       ((:void) (assert (null (cdr type))) "void")
	       ((:pointer) (format nil "~A*" (convert-type-from-cffi-to-c (cdr type)))))))
    (assert (not (null type)))
    (if (listp type)
	(rec type)
	(rec (list type)))))

(defun convert-type-from-lisp-to-cffi (type)
  (cond
    ((subtypep type 'integer) :int)
    ((subtypep type 'symbol) :int)
    ((subtypep type 'single-float) :float)
    ((subtypep type 'double-float) :double)
    ((subtypep type nil) :void)
    ((and (listp type) (eq (car type) 'function)) ;this is not in CFFI
     (assert (and (= (length type) 3) (listp (cadr type))))
     (list :function
	   (loop for arg in (cadr type) collect (convert-type-from-lisp-to-cffi arg))
	   (let ((results (caddr type)))
	     (assert (or (symbolp results) (and (listp results) (eq (car results) 'values))))
	     (if (listp results)
		 (cons 'values (loop for res in (cdr results) collect (convert-type-from-lisp-to-cffi res)))
		 (list 'values (convert-type-from-lisp-to-cffi results))))))
    (t (error "unknown LISP type ~S" type))))
    
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

(defun augment-namespace-with-builtin-function (symbol c-name argument-types values-types namespace)
  (let* ((ast (walker:parse-with-empty-namespaces `(flet ((,symbol ()))
						     (declare (ftype (function (,@argument-types) (values ,@values-types)) ,symbol))
						     (,symbol))))
	 (fun (walker:form-fun (car (walker:form-body ast))))
	 (c-type (convert-type-from-lisp-to-cffi (sym-declspec-type fun)))
	 (c-fun (make-fun c-name c-type)))
    (augment-nso namespace c-fun :lispsym fun :insist-on-name t)))

(defun convert-name (name)
  "Return NAME converted to a C variable or function name. This means - is converted to _."
  (let ((name-string (string (walker:nso-name name))))
    (substitute #\_ #\- name-string)))

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
  (let ((prefix (format nil "~A ~A" (convert-type-from-cffi-to-c (nso-type var)) (nso-name var)))
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
  (let ((ast (walker:parse-with-empty-namespaces form :customparsep-function #'parse-some-macros-p :customparse-function #'parse-some-macros))
	(values (loop for i from 0 for type in values-types collect (make-var (format nil "value~A" i) type)))
	(namespace (make-empty-namespace))
	(builtin-functions '((+ "plus_int_int" (integer integer) (integer))
			     (+ "plus_float_float" (single-float single-float) (single-float))
			     (* "multiply_int_int" (integer integer) (integer))
			     (* "multiply_float_float" (single-float single-float) (single-float))
			     (- "minus_int_int" (integer integer) (integer))
			     (- "minus_float_float" (single-float single-float) (single-float))
			     (1+ "plusone_int" (integer) (integer))
			     (1+ "plusone_float" (single-float) (single-float))
			     (1- "minusone_int" (integer) (integer))
			     (1- "minusone_float" (single-float) (single-float))
			     (print "print_int" (integer) ())
			     (< "less_int_int" (integer integer) (integer))
			     (<= "lessequal_int_int" (integer integer) (integer))
			     (> "greater_int_int" (integer integer) (integer))
			     (>= "greaterequal_int_int" (integer integer) (integer))
			     (= "equal_int_int" (integer integer) (integer))
			     )))
    (loop for bf in builtin-functions do
	 (setf namespace (augment-namespace-with-builtin-function (car bf) (cadr bf) (caddr bf) (cadddr bf) namespace)))
    (let ((c-lines (emitc ast namespace values)))
      (join-strings c-lines (format nil "~%")))))

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
      ((or (subtypep (type-of obj) 'integer) (subtypep (type-of obj) 'float))
       (unless (null values)
	 (c-assign (car values) obj)))
      (t
       (error "unknown type of selfevalobject ~S" obj)))))

(defmethod emitc ((ast walker:quote-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((obj (walker:form-object ast))
	 (c-obj (format nil "~A /*~A*/"
			(etypecase obj
			  (integer obj)
			  (float obj)
			  (symbol (sxhash obj)))
			obj)))
    (unless (null values)
      (c-assign (car values) c-obj))))

(defmethod emitc ((nso walker:sym) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((c-nso (find-var-for-lisp-var namespace nso)))
    (if (null values)
	(nso-name c-nso)
	(c-assign (car values) (nso-name c-nso)))))

(defun c-defun-head (fun parameters values)
  (declare (optimize (debug 3)))
  (let* ((c-fun-type (let ((type (nso-type fun))) (assert (eq (car type) :function)) type))
	 (parameters-type (cadr c-fun-type))
	 (values-type (cdaddr c-fun-type)))
    (loop for p1 in parameters-type for p2 in parameters do (assert (eq p1 (nso-type p2))))
    (loop for v1 in values-type for v2 in values do (assert (eq v1 (cadr (nso-type v2)))))
    (format nil "int ~A (~A)"
	    (nso-name fun)
	    (join-strings (append
			   (mapcar (lambda (x) (format nil "~A ~A" (convert-type-from-cffi-to-c (nso-type x)) (nso-name x))) parameters)
			   (mapcar (lambda (x) (format nil "~A ~A" (convert-type-from-cffi-to-c (nso-type x)) (nso-name x))) values))
			  ", "))))

(defun emitc-let-flet (ast namespace values)
  (declare (optimize (debug 3)))
  (assert (or (subtypep (type-of ast) 'walker:let-form) (subtypep (type-of ast) 'walker:flet-form)))
  (assert (not (null (walker:form-body ast))))
  ;;(prind ast)
  (let* ((bindings-namespace namespace) ;the namespace augmented with all binding variables
	 (bindings-syms (loop for binding in (walker:form-bindings ast) collect
			     (let* ((walker-sym (walker:binding-sym binding))
				    (declspec-type0 (sym-declspec-type walker-sym))
				    (declspec-type (convert-type-from-lisp-to-cffi declspec-type0))
				    (make-sym-function (cond ((subtypep (type-of ast) 'walker:let-form) #'make-var) ((subtypep (type-of ast) 'walker:flet-form) #'make-fun)))
				    (binding-sym (funcall make-sym-function (convert-name walker-sym) declspec-type)))
			       (assert (not (null declspec-type0)) () "unknown binding type")
			       (setf bindings-namespace (augment-nso bindings-namespace binding-sym :lispsym walker-sym))
			       binding-sym)))
	 (bindings-code (loop for binding in (walker:form-bindings ast) for binding-sym in bindings-syms collect
			     (cond
			       ((subtypep (type-of ast) 'walker:let-form)
				(let ((value-code (emitc (walker:binding-value binding) bindings-namespace (list binding-sym))))
				  (c-code (c-declaration binding-sym)
					  (c-scope value-code))))
			       ((subtypep (type-of ast) 'walker:flet-form)
				(let* ((parameters (walker:llist-required (walker:form-llist binding)))
				       (parameters-namespace bindings-namespace)
				       (c-parameters (loop for par in parameters collect
							  (let* ((c-par (walker:argument-var par))
								 (declspec-type0 (sym-declspec-type c-par))
								 (declspec-type (convert-type-from-lisp-to-cffi declspec-type0))
								 (sym (make-var (convert-name c-par) declspec-type)))
							    (assert (not (null declspec-type0)) () "unknown parameter type")
							    (setf parameters-namespace (augment-nso parameters-namespace sym :lispsym c-par))
							    sym)))
				       (c-values (loop for type in (cdaddr (nso-type binding-sym)) for i from 0 collect
						      (let* ((sym (make-var (format nil "value~A" i) (list :pointer type))))
							(assert (not (null type)) () "unknown value type")
							(setf parameters-namespace (augment-nso parameters-namespace sym))
							sym)))
				       (body-code (emitc-body (walker:form-body binding) parameters-namespace c-values)))
				  (c-code (list (format nil "~A" (c-defun-head binding-sym c-parameters c-values)))
					  (c-scope body-code)))))))
	 (body-code (emitc-body (walker:form-body ast) bindings-namespace values)))
    (c-code (c-scope bindings-code
		     body-code))))

(defmethod emitc ((ast walker:let-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (emitc-let-flet ast namespace values))

(defmethod emitc ((ast walker:flet-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (emitc-let-flet ast namespace values))

(defmethod emitc ((ast walker:application-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((fun (walker:form-fun ast))
	 (arguments (walker:form-arguments ast))
	 (c-fun (find-fun-for-lisp-fun namespace fun arguments))
	 (fun-type (nso-type c-fun))
	 (fun-arguments-type (cadr fun-type))
	 (fun-values-type (cdaddr fun-type))
	 (arguments-namespace namespace)
	 (arguments-syms (loop for i from 0 for arg in arguments for arg-type in fun-arguments-type collect
			      (let ((arg-sym (make-var (format nil "arg~A" i) arg-type))
				    (parg-sym (make-var (format nil "parg~A" i) (list :pointer arg-type))))
				(setf arguments-namespace (augment-nso arguments-namespace arg-sym))
				(setf arguments-namespace (augment-nso arguments-namespace parg-sym))
				(list arg-sym parg-sym))))
	 (arguments-code (loop for (arg-sym parg-sym) in arguments-syms for arg in arguments collect
				(c-code (c-scope
					 (emitc arg arguments-namespace (list parg-sym)))))))
    (prind ast values fun-values-type)
    (loop for value in values for value-type in fun-values-type do (assert (eq (cadr (nso-type value)) value-type)))
    (c-code (c-scope (loop for (arg-sym parg-sym) in arguments-syms collect
			  (c-code
			   (c-declaration arg-sym)
			   (c-declaration parg-sym (format nil "&~A" (nso-name arg-sym)))))
		     arguments-code
		     (format nil "~A(~A);" (nso-name c-fun)
			     (join-strings
			      (concatenate 'list
					   (mapcar #'nso-name (mapcar #'car arguments-syms))
					   (mapcar (lambda (v type) (declare (ignore type)) (format nil "~A" (nso-name v))) values fun-values-type))
			      ", "))))))

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
