;;;; Lisp to C Compiler

(load "/home/toni/quicklisp/setup.lisp")
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

;;;; The backend stuff implementing the special operators of Lisp and some macros.

(defclass backend ()
  ;;((nso ))
  ()
  (:documentation "The global backend variables."))

;;;; The C backend.

;;(deftype ctype () `(or :int :char :int8 :int16 :int32 :int64 :float :double :pointer :void))

(defclass nso ()
  ((name :initarg :name :accessor nso-name :type string)))

(defclass sym (nso)
  ((type :initarg :type :accessor nso-type :type (or symbol list))))

(defclass var (sym)
  ())

(defclass fun (sym)
  ())

(defclass tag (nso)
  ())

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
  (make-instance 'var :name name :type type))

(defun make-tag (name)
  (make-instance 'tag :name name))

(defun augment-nso (namespace nso &optional (lispsym nil))
  "Augment NAMESPACE with NSO, which has its name set to a unique string, prefixed by (NSO-NAME NSO).
If LISPSYM is non-NIL, NAMESPACE records a mapping from LISPSYM to NSO.
Return the augmented NAMESPACE."
  (declare (optimize (debug 3)))
  (let* ((name (find-new-name namespace (type-of nso) (nso-name nso))))
    (setf (nso-name nso) name)
    (if lispsym
	(augment-namespace-with-lispsym nso namespace lispsym)
	(augment-namespace nso namespace))))
  
(defclass c-backend (backend)
  ((stream :initarg :stream :accessor backend-stream :type stream :documentation "The output stream that the C file is written to."))
  (:documentation "The global C backend variables."))

(defun convert-type (type)
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
	       ((:pointer) (format nil "~A*" (convert-type (cdr type)))))))
    (assert (not (null type)))
    (if (listp type)
	(rec type)
	(rec (list type)))))

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

(defun c-assign (l-name r-value)
  (format nil "~A = ~A;" l-name r-value))

(defgeneric emitc (generalform namespace)
  (declare (optimize (debug 3)))
  (:documentation "The emitc methods return three values: the generated code to compute the variable, the variable name of the value of the AST, and the namespace augmented with the variable."))

(defun emit-declaration (var &optional initial-value)
  (declare (optimize (debug 3)))
  (let ((prefix (format nil "~A ~A" (convert-type (nso-type var)) (nso-name var)))
	(suffix (if initial-value (format nil " = ~A;" initial-value) (format nil ";"))))
    (format nil "~A~A" prefix suffix)))

(defmethod emitc ((obj walker:selfevalobject) (namespace namespace))
  (let ((obj (walker:selfevalobject-object obj)))
    (cond
      ((subtypep (type-of obj) 'integer)
       (let* ((value (make-var "i" :int))
	      (namespace (augment-nso namespace value)))
	 (values (emit-declaration value obj)
		 value
		 namespace)))
      ((subtypep (type-of obj) 'single-float)
       (let* ((value (make-var "f" :float))
	      (namespace (augment-nso namespace value)))
	 (values (emit-declaration value obj)
		 value
		 namespace)))
      ((subtypep (type-of obj) 'double-float)
       (let* ((value (make-var "d" :double))
	      (namespace (augment-nso namespace value)))
	 (values (emit-declaration value obj)
		 value
		 namespace)))
      (t
       (error "unknown type of selfevalobject ~S" obj)))))

(defmethod emitc ((ast walker:if-form) (namespace namespace))
  (declare (optimize (debug 3)))
  (let* ((if-var (make-var "if_var" nil))
	 (if-namespace (augment-nso namespace if-var)))
    (multiple-value-bind (test-code test-var test-namespace) (emitc (walker:form-test ast) if-namespace)
      (multiple-value-bind (then-code then-var then-namespace) (emitc (walker:form-then ast) test-namespace)
	(declare (ignore then-namespace))
	(multiple-value-bind (else-code else-var else-namespace) (when (walker:form-else ast) (emitc (walker:form-else ast) test-namespace))
	  (declare (ignore else-namespace))
	  (unless (null else-code) (assert (eq (nso-type else-var) (nso-type then-var))))
	  (setf (nso-type if-var) (nso-type then-var))
	  (let* ((if-declaration (emit-declaration if-var)))
	    (values (c-code if-declaration
			    (c-scope test-code
				     (format nil "if (~A)" (nso-name test-var))
				     (c-scope then-code (c-assign (nso-name if-var) (nso-name then-var)))
				     (when else-code
				       (c-code (format nil "else")
					       (c-scope else-code (c-assign (nso-name if-var) (nso-name else-var)))))))
		    if-var
		    if-namespace)))))))

(defun sym-declspec-type (sym)
  (let* ((type-declspecs (remove-if (lambda (declspec) (not (subtypep (type-of declspec) 'walker:declspec-type)))
				    (walker:nso-declspecs sym)))
	 (first-declspec-type (car type-declspecs)))
    ;;(prind type-declspecs)
    (assert (loop for d in (cdr type-declspecs) always (eq (walker:declspec-type d) first-declspec-type)) () "Conflicting type declarations in ~S" type-declspecs)
    ;; TODO: for compatible type-declarations, find the most specific type and return it. (e.g. for NUMBER and FLOAT return FLOAT.)
    first-declspec-type))

(defmethod emitc ((ast walker:let-form) (namespace namespace))
  (declare (optimize (debug 3)))
  (assert (not (null (walker:form-body ast))))
  ;;(prind ast)
  (prind namespace)
  (let* ((let-var (make-var "let_value" nil))
	 (let-namespace (augment-nso namespace let-var))
	 (bindings-namespace let-namespace) ;the namespace augmented with all binding variables
	 (bindings-var (loop for binding in (walker:form-bindings ast) collect
			    (let* ((walker-var (walker:binding-sym binding))
				   (binding-var (make-var (convert-name walker-var) nil)))
			      (prind "before" walker-var bindings-namespace)
			      (setf bindings-namespace (augment-nso bindings-namespace binding-var walker-var))
			      (prind "after" binding-var bindings-namespace)
			      binding-var)))
	 (bindings-code (loop
			   for binding in (walker:form-bindings ast)
			   for binding-var in bindings-var
			   collect
			     (multiple-value-bind (value-code value-var value-namespace)
				 (emitc (walker:binding-value binding) bindings-namespace)
			       (declare (ignore value-namespace))
			       (setf (nso-type binding-var) (nso-type value-var))
			       (c-scope value-code (c-assign (nso-name binding-var) (nso-name value-var))))))
	 (body-code (loop for body in (butlast (walker:form-body ast)) collect
			 (multiple-value-bind (body-code body-var body-namespace) (emitc body bindings-namespace)
			   (declare (ignore body-var body-namespace))
			   (c-scope body-code)))))
    (multiple-value-bind (last-body-code last-body-var last-body-namespace) (emitc (car (last (walker:form-body ast))) bindings-namespace)
      (declare (ignore last-body-namespace))
      (setf (nso-type let-var) (nso-type last-body-var))
      (let* ((let-declaration (emit-declaration let-var))
	     (bindings-declarations (loop for var in bindings-var collect (emit-declaration var))))
	(values (c-code
		  let-declaration
		  (c-scope bindings-declarations
			   bindings-code
			   body-code
			   (c-scope last-body-code)
			   (c-assign (nso-name let-var) (nso-name last-body-var))))
		let-var
		let-namespace)))))

(defmethod emitc ((nso walker:sym) (namespace namespace))
  (declare (optimize (debug 3)))
  (let* ((c-nso-assoc (assoc nso (namespace-lispnsos namespace)))
	 (c-nso (cdr c-nso-assoc)))
    (assert c-nso-assoc () "Lisp nso ~A was not augmented to c-namespace ~A" nso namespace)
    (values (format nil "~A;" (nso-name c-nso)) c-nso namespace)))

(defmethod emitc ((nso walker:tag) (namespace namespace))
  (declare (optimize (debug 3)))
  (let* ((c-nso-assoc (assoc nso (namespace-lispnsos namespace)))
	 (c-nso (cdr c-nso-assoc)))
    (assert c-nso-assoc () "Lisp nso ~A was not augmented to c-namespace ~A" nso namespace)
    (values (format nil "~A:;" (nso-name c-nso)) c-nso namespace)))

(defun emit-c (form)
  (declare (optimize (debug 3)))
  (let ((ast (walker:parse-with-empty-namespaces form)))
    (multiple-value-bind (c-lines var namespace) (emitc ast (make-empty-namespace))
      (declare (ignore var namespace))
      (apply #'concatenate 'string (mapcar (lambda (s) (format nil "~A~%" s)) c-lines)))))
