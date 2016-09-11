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

(defun join-strings (stringlist separator)
  (apply #'concatenate 'string
	 (car stringlist)
	 (let ((rest nil))
	   (loop for arg in (cdr stringlist) do
		(push separator rest)
		(push arg rest))
	   (nreverse rest))))

;;;; The backend stuff implementing the special operators of Lisp and some macros.

(defclass backend ()
  ;;((nso ))
  ()
  (:documentation "The global backend variables."))

;;;; The C backend.

;;(deftype ctype () `(or :int :char :int8 :int16 :int32 :int64 :float :double :pointer :void))

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
    (t (error "unknown type ~S" type))))
    
(defun convert-name (name)
  "Return NAME converted to a C variable or function name. This means - is converted to _."
  (let ((name-string (string (walker:nso-name name))))
    (substitute #\_ #\- name-string)))

(defparameter *c-scope* nil "The current C scope.")

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
  (let ((assign-code (format nil "~A = ~A;" (nso-name l-name) r-value)))
    assign-code))

(defun emit-c (form)
  (declare (optimize (debug 3)))
  (let ((ast (walker:parse-with-empty-namespaces form))
	(values (loop for i below 5 collect (make-var (format nil "value~A" i) nil)))
	(namespace (make-empty-namespace))
	(builtin-functions '((+ "plus_integer_integer" (integer integer) (integer)))))
    (loop for bf in builtin-functions do
	 (setf namespace (augment-namespace-with-builtin-function (car bf) (cadr bf) (caddr bf) (cadddr bf) namespace)))
    (let ((c-lines (emitc ast namespace values)))
      (join-strings c-lines (format nil "~%")))))

(defgeneric emitc (generalform namespace values-vars)
  (declare (optimize (debug 3)))
  (:documentation "The emitc methods return three values: the generated code to compute the variable, the variable name of the value of the AST, and the namespace augmented with the variable."))

(defun sym-declspec-type (sym)
  (declare (optimize (debug 3)))
  (let* ((type-declspecs (remove-if (lambda (declspec) (not (or (subtypep (type-of declspec) 'walker:declspec-type)
								(subtypep (type-of declspec) 'walker:declspec-ftype))))
				    (walker:nso-declspecs sym)))
	 (first-declspec-type (walker:declspec-type (car type-declspecs))))
    ;;(prind type-declspecs first-declspec-type)
    (assert (loop for d in (cdr type-declspecs) always (eq (walker:declspec-type d) first-declspec-type)) () "Conflicting type declarations in ~S" type-declspecs)
    ;; TODO: for compatible type-declarations, find the most specific type and return it. (e.g. for NUMBER and FLOAT return FLOAT.)
    first-declspec-type))

(defmethod emitc-body (body (namespace namespace) values)
  (declare (optimize (debug 3)))
  (c-code (loop for body in (butlast body) collect (c-scope (emitc body namespace nil)))
	  (c-scope (emitc (car (last body)) namespace values))))

(defmethod emitc ((obj walker:selfevalobject) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let ((obj (walker:selfevalobject-object obj)))
    (cond
      ((subtypep (type-of obj) 'integer)
       (unless (null values)
	 (c-assign (car values) obj)))
      (t
       (error "unknown type of selfevalobject ~S" obj)))))

(defmethod emitc ((nso walker:sym) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((c-nso-assoc (assoc nso (namespace-lispnsos namespace)))
	 (c-nso (cdr c-nso-assoc)))
    (assert c-nso-assoc () "Lisp nso ~A was not augmented to c-namespace ~A" nso namespace)
    (if (null values)
	(nso-name c-nso)
	(c-assign (car values) (nso-name c-nso)))))

(defmethod emitc ((ast walker:let-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (assert (not (null (walker:form-body ast))))
  ;;(prind ast)
  (let* ((bindings-namespace namespace) ;the namespace augmented with all binding variables
	 (bindings-syms (loop for binding in (walker:form-bindings ast) collect
			     (let* ((walker-sym (walker:binding-sym binding))
				    (declspec-type (convert-type-from-lisp-to-cffi (sym-declspec-type walker-sym)))
				    (binding-sym (make-var (convert-name walker-sym) declspec-type)))
			       (assert (not (null declspec-type)))
			       (setf bindings-namespace (augment-nso bindings-namespace binding-sym walker-sym))
			       binding-sym)))
	 (bindings-code (loop for binding in (walker:form-bindings ast) for binding-sym in bindings-syms collect
			     (let ((value-code (emitc (walker:binding-value binding) bindings-namespace (list binding-sym))))
			       (c-code (c-declaration binding-sym)
				       (c-scope value-code)))))
	 (body-code (emitc-body (walker:form-body ast) bindings-namespace values)))
    (c-code (c-scope bindings-code
		     body-code))))

(defun augment-namespace-with-builtin-function (symbol c-name argument-types values-types namespace)
  (let* ((ast (walker:parse-with-empty-namespaces `(flet ((,symbol ()))
						     (declare (ftype (function (,@argument-types) (values ,@values-types)) ,symbol))
						     (,symbol))))
	 (fun (walker:form-fun (car (walker:form-body ast))))
	 (debug1 (prind fun (walker:nso-declspecs fun)))
	 (c-type (convert-type-from-lisp-to-cffi (sym-declspec-type fun)))
	 (c-fun (make-fun c-name c-type)))
    (augment-nso namespace c-fun fun)))

(defmethod emitc ((ast walker:application-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((fun (walker:form-fun ast))
	 (c-fun-assoc (let ((res (assoc (walker:nso-name fun) (namespace-lispnsos namespace) :key #'walker:nso-name))) (assert res () "Lisp fun ~A was not augmented to c-namespace ~A" fun namespace) res))
	 (c-fun (cdr c-fun-assoc))
	 (fun-type (nso-type c-fun))
	 (fun-arguments-type (cadr fun-type))
	 (fun-values-type (cdaddr fun-type))
	 (arguments (walker:form-arguments ast))
	 (arguments-namespace namespace)
	 (arguments-syms (loop for i from 0 for arg in arguments for arg-type in fun-arguments-type collect
			      (let ((arg-sym (make-var (format nil "arg~A" i) arg-type)))
				(setf arguments-namespace (augment-nso arguments-namespace arg-sym))
				arg-sym)))
	 (arguments-code (loop for arg-sym in arguments-syms for arg in arguments collect
				(c-code
				 (emitc arg arguments-namespace (list arg-sym))))))
    ;; TODO: FIXME: uncomment the following when I can declare value types in #'EMIT-C.
    ;;(loop for value in values for value-type in fun-values-type do (assert (eq (nso-type value) value-type)))
    (c-code (c-scope (loop for arg-sym in arguments-syms collect (c-declaration arg-sym))
		     arguments-code
		     (format nil "~A(~A, ~A);" (nso-name c-fun)
			     (join-strings (mapcar #'nso-name arguments-syms) ", ")
			     (join-strings (mapcar (lambda (v) (format nil "&~A" (nso-name v))) values) ", "))))))

(defmethod emitc ((ast walker:if-form) (namespace namespace) values)
  (declare (optimize (debug 3)))
  (let* ((test-var (make-var "if_test" :int))
	 (namespace (augment-nso namespace test-var))
	 (test-code (emitc (walker:form-test ast) namespace (list test-var)))
	 (then-code (emitc (walker:form-then ast) namespace values))
	 (else-code (when (walker:form-else ast)
		      (emitc (walker:form-else ast) namespace values))))
    (c-code (c-scope (c-declaration test-var)
		     test-code
		     (format nil "if (~A)" (nso-name test-var))
		     (c-scope then-code)
		     (when else-code
		       (c-code (format nil "else")
			       (c-scope else-code)))))))
