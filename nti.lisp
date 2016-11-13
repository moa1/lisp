;; TODO: think about how ASSERT can be included. The NTI paper says: """In the classical non-deterministic manner, the flow of control is terminated only by branches of the computation that fail in the sense that there are no legitimate values for variables. In this setting, predicates are modelled by partial functions whose results are ignored. One particularly valuable partial function of this kind is "assert(p)" which is defined only for the argument "true".""" One way to include ASSERT would be to model it as an IF-FORM, which has as its (only) THEN-FORM the code following the ASSERT. What about: (LET ((A 1)) (PROGN (ASSERT (INTEGERP A))) (LET ((B 1)) B))? Should the THEN-FORM include the second LET? SBCL compiles the following without a warning, although it would be possible to infer that the assertion always fails: (DEFUN TEST (X Y) (ASSERT (AND (> X Y) (<= X Y)))). Neither does SBCL complain for (DEFUN TEST (X) (ASSERT (AND (INTEGERP X) (TYPEP X 'SINGLE-FLOAT)))) or for (DEFUN TEST (X) (DECLARE (TYPE SINGLE-FLOAT X)) (ASSERT (INTEGERP X))).


(load "~/quicklisp/setup.lisp")
(ql:quickload :walker)
(ql:quickload :equals)

(defpackage :nimble-type-inferencer
  (:documentation "Nimble type inferencer for ANSI Lisp, see the paper \"The Nimble Type Inferencer for Common Lisp-84\" by Henry G. Baker.")
  (:use :cl :equals)
  (:export
   ;; for classes: export the class and _all_ accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   ))
(in-package :nimble-type-inferencer)

(defparameter +builtin-functions+
  '(;; arithmetic: int int
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
    (height-function "height_function_v4sf_float_heightmap" (v4s-float single-float heightmap) (single-float))
    ;;(advance-ray-3d "ADVANCE_RAY_3D0" (v4s-float v4s-float single-float single-float single-float single-float single-float single-float integer) (v4s-float single-float symbol))
    ;;(intersect-border "INTERSECT_BORDER0" (v4s-float v4s-float v4s-float single-float single-float single-float single-float single-float single-float single-float) (v4s-float single-float))
    ;;(outside-border-p "OUTSIDE_BORDER0" (v4s-float single-float single-float single-float) (boolean))
    ;;(scan-line-continuous "SCAN_LINE_CONTINUOUS0" (v4s-float v4s-float single-float (function (v4s-float) boolean) single-float single-float single-float single-float single-float single-float) (v4s-float v4s-float boolean single-float))
    (make-array-single-float "make_array_float_uint_uint" (unsigned-byte unsigned-byte) ((array single-float)))
    (make-array-integer "make_array_int_uint_uint" (unsigned-byte unsigned-byte) ((array integer)))
    (aref "aref_array_float_uint" ((array single-float) unsigned-byte) (single-float))
    (aref "aref_array_integer_uint" ((array integer) unsigned-byte) (integer))
    ))

;; transform the program representation as parsed by WALKER to a program which only uses the following constructs: 1. selfevalobjects, (self-evaluating) variable and function symbols, let-form, setq-form, (function) application-form.


(defun make-empty-namespace ()
  nil)

(defun augment-namespace (sym type namespace)
  (acons sym type namespace))

(defun namespace-lookup (sym namespace)
  (let* ((cons (assoc sym namespace :test #'equals)))
    (assert (not (null cons)) () "SYM ~A not bound in ~S" sym namespace)
    (cdr cons)))

(defun (setf namespace-lookup) (value sym namespace)
  (let* ((cons (assoc sym namespace :test #'equals)))
    (assert (not (null cons)) () "SYM ~A not bound in ~S" sym namespace)
    (setf (cdr cons) value)))

(defun copy-namespace (namespace)
  (copy-tree namespace))

(defun map-namespace (function &rest namespaces)
  "Note that one can assign to the CAR or CONS of the CELLS."
  (declare (type (function (&rest list) t) function))
  (apply #'mapc
	 (lambda (&rest cells)
	   (assert (let ((var (caar cells))) (loop for cell in (cdr cells) always (equals (car cell) var))) () "Variable names are not equal: ~S" (mapcar #'car cells))
	   (unless (typep (caar cells) 'walker:tag)
	     (apply function cells)))
	 (mapcar #'reverse namespaces)))

(load "nti-subtypep.lisp")

(defstruct v4s-float)
(defparameter +builtin-types+
  '(nil t null ;these are always needed: NIL and T as bottom and top element of the type lattice, and NULL as result of TAGBODY.
    number integer unsigned-byte single-float
    symbol
    v4s-float
    array (array integer) (array single-float)))

(defparameter +builtin-typehash+ (make-typegraph +builtin-types+))

(defun join (type1 type2)
  (join-type type1 type2 +builtin-typehash+))

(defun meet (type1 type2)
  (meet-type type1 type2 +builtin-typehash+))

(defstruct (results (:constructor make-results*))
  "The type of multiple values: NVALUES is the number of finite values; FINITE is of type (LIST TYPE); and INFINITE is of type TYPE."
  (nvalues -1 :type integer)
  (finite nil :type list)
  (infinite nil :type (or symbol list)))

(defun make-results (&rest results)
  (make-results* :nvalues (length results) :finite results :infinite nil))

(defun result1 (results)
  (if (< (results-nvalues results) 1)
      (results-infinite results)
      (car (results-finite results))))

(defun (setf result1) (value results)
  (cond
    ((<= (results-nvalues results) 0)
     (setf (results-nvalues results) 1)
     (setf (results-finite results) (list value)))
    (t
     (setf (car (results-finite results)) value))))

(defun process-results (results1 results2 function)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Apply FUNCTION to them."
  (declare (optimize (debug 3)))
  (let* ((nv1 (results-nvalues results1))
	 (nv2 (results-nvalues results2))
	 (min-nv (max 0 (min nv1 nv2)))
	 (max-nv (max nv1 nv2))
	 (finite (append (loop for i below min-nv for t1 in (results-finite results1) for t2 in (results-finite results2) collect
			      (funcall function t1 t2))
			 (loop for i from min-nv below max-nv collect
			      (let ((t1 (if (>= i nv1) (results-infinite results1) (elt (results-finite results1) i)))
				    (t2 (if (>= i nv2) (results-infinite results2) (elt (results-finite results2) i))))
				(funcall function t1 t2)))))
	 (infinite (funcall function (results-infinite results1) (results-infinite results2)))
	 (finite-cropped (let* ((last 0))
			   (loop for i from (1- (length finite)) downto 0 do
				(when (not (equal (elt finite i) infinite)) (setf last (1+ i)) (return)))
			   (subseq finite 0 last))))
    (make-results* :nvalues (length finite-cropped) :finite finite-cropped :infinite infinite)))    

(defun meet-results (results1 results2)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Meet them."
  (process-results results1 results2 #'meet))

(defun join-results (results1 results2)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Join them."
  (process-results results1 results2 #'join))

(defmethod print-object ((object results) stream)
  (print-unreadable-object (object stream :type t)
    (loop for type in (append (results-finite object) (list (results-infinite object))) do
	 (format stream "~A " type))
    (format stream "...")))

(defun meet-arguments (args1 args2)
  "ARGS1 and ARGS2 are each a list of TYPES, one for each NTH-VALUE. Meet the types of same N."
  (assert (= (length args1) (length args2)))
  (loop for i below (length args1) collect
       (meet-type (elt args1 i) (elt args2 i) +builtin-typehash+)))

(defun var-declared-type (var)
  (let* ((declspecs (walker:nso-declspecs var))
	 (types (mapcar #'walker:declspec-type (remove-if-not (lambda (x) (typep x 'walker:declspec-type)) declspecs)))
	 (type (reduce #'meet types :initial-value t)))
    (assert (not (null type)) () "Impossible type declarations for variable ~S: ~S" var types)
    type))

(defun fun-result-lookup (fun arg-types)
  "ARG-TYPES is the list of types of (arg1-type arg2-type ...)"
  (let ((fun (if (typep fun 'walker:fun)
		 (walker:nso-name fun)
		 fun))
	(possible nil))
    (loop for (lisp-name c-name fun-arg-types fun-results-types) in +builtin-functions+ do
	 (when (and (eq fun lisp-name)
		    ;;(equal arg-types fun-arg-types)
		    (loop for arg-type in arg-types for fun-arg-type in fun-arg-types always
			 (is-subtypep fun-arg-type arg-type +builtin-typehash+)))
	   (push fun-results-types possible)))
    (when (null possible)
      (return-from fun-result-lookup (make-results* :nvalues 0 :infinite nil))) ;TODO: return multiple values representation number
    ;;(assert (not (null possible)) () "Unknown function ~S with types ~S" fun arg-types)
    ;; POSSIBLE is now a ((TYPE-RESULT1 TYPE-RESULT2 ...) (TYPE-RESULT1 TYPE-RESULT2 ...) ...). Meet the types TYPE-RESULT1, then TYPE-RESULT2, ..., to get a list of TYPE-RESULTs.
    (assert (apply #'= (mapcar #'length possible)))
    (let ((length (length (car possible))))
      (apply #'make-results
	     (loop for l below length collect
		  (join-typelist (loop for poss in possible collect (elt poss l)) +builtin-typehash+))))))

(defun fun-arguments-lookup (fun results)
  "RESULTS-TYPES is the list of types of (result1-type result2-type ...)"
  (let ((fun (if (typep fun 'walker:fun)
		 (walker:nso-name fun)
		 fun))
	(possible nil))
    ;;(prind fun results-types)
    (loop for (lisp-name c-name fun-arg-types fun-results-types) in +builtin-functions+ do
	 (when (and (eq fun lisp-name)
		    ;;(equal arg-types fun-arg-types)
		    (block always
		      (process-results (apply #'make-results fun-results-types) results
				       (lambda (t1 t2) (unless (is-subtypep t1 t2 +builtin-typehash+) (return-from always nil)) nil))
		      t))
	   (push fun-arg-types possible)))
    ;;(prind possible)
    (when (null possible)
      (return-from fun-arguments-lookup '(nil))) ;TODO: return the correct number of arguments
    (assert (not (null possible)))
    ;; POSSIBLE is now a ((TYPE-ARG1 TYPE-ARG2 ...) (TYPE-ARG1 TYPE-ARG2 ...) ...). Meet the types TYPE-ARG1, then TYPE-ARG2, ..., to get a list of TYPE-ARGs.
    (assert (apply #'= (mapcar #'length possible)))
    (let ((length (length (car possible))))
      (loop for l below length collect
	   (join-typelist (loop for poss in possible collect (elt poss l)) +builtin-typehash+)))))

;;; CLASSES

(defclass flowstate-prev ()
  ((prev-upper :initarg :prev-upper :accessor form-prev-upper)
   (prev-lower :initarg :prev-lower :accessor form-prev-lower)))

(defclass flowstate-next ()
  ((next-upper :initarg :next-upper :accessor form-next-upper)
   (next-lower :initarg :next-lower :accessor form-next-lower)))

(defclass flowstate (flowstate-prev flowstate-next)
  ()
  (:documentation "The namespaces before and after the form are distinct objects, because there may be a SETQ of a lexically visible variable inside the LET."))

(defclass flowstate-prevs ()
  ((prevs-upper :initarg :prevs-upper :accessor form-prevs-upper)
   (prevs-lower :initarg :prevs-lower :accessor form-prevs-lower)))

(defclass flowstate-nexts ()
  ((nexts-upper :initarg :nexts-upper :accessor form-nexts-upper)
   (nexts-lower :initarg :nexts-lower :accessor form-nexts-lower)))

(defclass formvalue ()
  ((form-upper :initarg :form-upper :accessor form-upper)
   (form-lower :initarg :form-lower :accessor form-lower))
  (:documentation "This is the type of the value of a form."))

(defclass selfevalobject (walker:selfevalobject flowstate formvalue)
  ())

(defclass var (flowstate formvalue)
  ((var :initarg :var :accessor walker:form-var :type walker:var)))

(defclass fun (flowstate formvalue)
  ((fun :initarg :fun :accessor walker:form-fun :type walker:fun)))

(defclass tag (flowstate-prevs flowstate-next formvalue) ;no parent FLOWSTATE, because a TAG may have multiple PREV states. FORMVALUE is needed by #'DEDUCE-FORWARD and #'DEDUCE-BACKWARD.
  ((tag :initarg :tag :accessor walker:form-tag :type walker:tag)))

(defclass blo (flowstate-prev flowstate-nexts)
  ((blo :initarg :blo :accessor walker:form-blo :type walker:blo)
   (definition :initarg :definition :accessor walker:nso-definition)))

(defmethod equals ((x walker:sym) (y walker:sym) &rest keys &key recursive &allow-other-keys)
  (declare (ignore recursive))
  (and (apply #'equals (walker:nso-name x) (walker:nso-name y) keys)
       (apply #'equals (walker:nso-freep x) (walker:nso-freep y) keys)))

(defclass var-binding (walker:var-binding)
  ())

(defclass let-form (walker:let-form flowstate formvalue)
  ())

(defclass application-form (walker:application-form flowstate formvalue)
  ())

(defclass setq-form (walker:special-form flowstate formvalue) ;note that this is not a specialization of WALKER:SETQ-FORM
  ((var :initarg :var :accessor walker:form-var :type var)
   (value :initarg :value :accessor walker:form-value :type walker:generalform)))

(defclass alt-form (walker:special-form flowstate formvalue)
  ((branches :initarg :branches :accessor form-branches :documentation "A list of branches that may be taken by the IF- or COND-form.")))

(defclass tagbody-form (walker:tagbody-form flowstate formvalue)
  ())

(defclass go-form (walker:go-form flowstate formvalue) ;FORMVALUE is needed by #'DEDUCE-FORWARD and #'DEDUCE-BACKWARD.
  ())

(defclass block-form (walker:block-form flowstate formvalue)
  ())

(defclass return-from-form (walker:return-from-form flowstate formvalue) ;FORMVALUE is needed by #'DEDUCE-FORWARD and #'DEDUCE-BACKWARD.
  ())

(defmethod print-object ((object var) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S VAR:~S " (form-upper object) (form-lower object) (walker:form-var object))))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-tag object))))

(defmethod print-object ((object blo) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (walker:form-blo object))))

(defmethod print-object ((object let-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S BINDINGS:~A ~A" (form-upper object) (form-lower object)
	    (let ((first-form (car (walker:form-body object))))
	      (loop for binding in (walker:form-bindings object) collect
		   (let ((var (walker:form-sym binding))
			 (value (walker:form-value binding)))
		     (format nil "(UPPER:~S LOWER:~S ~S ~S)" (namespace-lookup var (form-prev-upper first-form)) (namespace-lookup var (form-prev-lower first-form)) var value))))
	    (walker:format-body object t nil))))

(defmethod print-object ((object application-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S" (form-upper object) (form-lower object) (walker:form-fun object))
    (loop for arg in (walker:form-arguments object) do
	 (format stream " ~S" arg))))

(defmethod print-object ((object setq-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S ~S" (form-upper object) (form-lower object) (walker:form-var object) (walker:form-value object))))

(defmethod print-object ((object alt-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S" (form-upper object) (form-lower object) (form-branches object))))

(defmethod print-object ((object tagbody-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~A" (form-upper object) (form-lower object) (walker:format-body object nil nil))))

(defmethod print-object ((object block-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S ~A" (form-upper object) (form-lower object) (walker:form-blo object) (walker:format-body object nil nil))))

(defmethod print-object ((object return-from-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((value (walker:form-value object)))
      (format stream "UPPER:~S LOWER:~S ~S" (form-upper value) (form-lower value) (walker:form-blo object)))))

;;; PREPARE-AST

(defmethod prepare-ast ((ast walker:selfevalobject) prev-upper prev-lower)
  (let ((type (etypecase (walker:selfevalobject-object ast)
		(integer 'integer)
		(float 'single-float)
		(null 'null)
		(symbol 'symbol)
		(t t))))
    (make-instance 'selfevalobject :object (walker:selfevalobject-object ast)
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		   :form-upper (make-results type) :form-lower (make-results nil))))

(defmethod prepare-ast ((ast walker:var) prev-upper prev-lower)
  (make-instance 'var :var ast
		 :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		 :form-upper (make-results t) :form-lower (make-results nil)))

(defmethod prepare-ast ((ast walker:fun) prev-upper prev-lower)
  (make-instance 'fun :fun ast
		 :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		 :form-upper (make-results 'function) :form-lower (make-results nil)))

(defmethod prepare-ast ((ast walker:var-binding) prev-upper prev-lower)
  (let ((value (if (null (walker:form-value ast))
		   (prepare-ast (make-instance 'walker:selfevalobject :object nil) prev-upper prev-lower)
		   (prepare-ast (walker:form-value ast) prev-upper prev-lower))))
    (make-instance 'var-binding :sym (walker:form-sym ast) :value value)))

(defmethod prepare-ast ((ast walker:let-form) prev-upper prev-lower)
  (let* ((bindings (loop for binding in (walker:form-bindings ast) collect (prepare-ast binding prev-upper prev-lower)))
	 (body-prev-upper (let ((namespace prev-upper))
			    (loop for binding in bindings do
				 (let* ((type (var-declared-type (walker:form-sym binding))))
				   (setf namespace (augment-namespace (walker:form-sym binding) type namespace))))
			    namespace))
	 (body-prev-lower (let ((namespace prev-lower))
			    (loop for binding in bindings do
				 (setf namespace (augment-namespace (walker:form-sym binding) nil namespace)))
			    namespace))
	 (body-next-upper body-prev-upper)
	 (body-next-lower body-prev-lower)
	 (body-forms (if (null (walker:form-body ast))
			 (make-instance 'walker:selfevalobject :object nil)
			 (walker:form-body ast)))
	 (body (loop for form in body-forms collect
		    (let ((ast (prepare-ast form body-next-upper body-next-lower)))
		      (setf body-next-upper (form-next-upper ast))
		      (setf body-next-lower (form-next-lower ast))
		      ast))))
    (make-instance 'let-form :bindings bindings :declspecs (walker:form-declspecs ast) :body body
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace prev-lower)
		   :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))

(defmethod prepare-ast ((ast walker:application-form) prev-upper prev-lower)
  (declare (optimize (debug 3)))
  ;; construct a LET-form that holds all the arguments
  (let* ((arguments (loop for argument in (walker:form-arguments ast) for i from 0 collect
			 (make-instance 'walker:var :name (gensym (format nil "TEMP~A-" i)) :freep nil :declspecs nil :macrop nil)))
	 (bindings (loop for argument in arguments for value in (walker:form-arguments ast) collect
			(let ((binding (make-instance 'var-binding :sym argument :value (prepare-ast value prev-upper prev-lower))))
			  (setf (walker:nso-definition argument) binding)
			  binding)))
	 (next-upper (let ((namespace prev-upper))
		       (loop for binding in bindings do
			    (setf namespace (augment-namespace (walker:form-sym binding) t namespace)))
		       namespace))
	 (next-lower (let ((namespace prev-lower))
		       (loop for binding in bindings do
			    (setf namespace (augment-namespace (walker:form-sym binding) nil namespace)))
		       namespace))
	 (body (list (make-instance 'application-form :fun (walker:form-fun ast) :arguments arguments
				    :prev-upper next-upper :prev-lower next-lower :next-upper next-upper :next-lower next-lower
				    :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))
	 (let-form (make-instance 'let-form :bindings bindings :body body :declspecs nil
				  :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace prev-lower) ;the values could contain SETQ, therefore the #'COPY-NAMESPACE.
				  :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))
    let-form))

(defmethod prepare-ast ((ast walker:setq-form) prev-upper prev-lower)
  ;; translate the SETQ having multiple variable-value pairs into a sequence of SETQ-forms with a single variable-value pair.
  (let* ((next-upper prev-upper)
	 (next-lower prev-lower)
	 (body nil))
    (loop for var in (walker:form-vars ast) for value in (walker:form-values ast) do
	 (let ((prev-upper next-upper)
	       (prev-lower next-lower)
	       (value2 (prepare-ast value next-upper next-lower)))
	   (setf next-upper (copy-namespace next-upper) next-lower (copy-namespace next-lower))
	   (push (make-instance 'setq-form :var var :value value2
				:prev-upper prev-upper :prev-lower prev-lower :next-upper next-upper :next-lower next-lower
				:form-upper (make-results t) :form-lower (make-results nil))
		 body)))
    ;; wrap the multiple SETQ-FORMs in a substitute for a PROGN-FORM.
    (make-instance 'let-form :bindings nil :declspecs nil :body (nreverse body)
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper next-upper :next-lower next-lower
		   :form-upper (make-results t) :form-lower (make-results nil))))

(defmethod prepare-ast ((ast walker:if-form) prev-upper prev-lower)
  (let* ((next-upper-inside (copy-namespace prev-upper))
	 (next-lower-inside (copy-namespace prev-lower))
	 (then-branch (prepare-ast (walker:form-then ast) next-upper-inside next-lower-inside))
	 (else-branch (if (walker:form-else ast)
			  (prepare-ast (walker:form-else ast) next-upper-inside next-lower-inside)
			  (prepare-ast (make-instance 'walker:selfevalobject :object nil) next-upper-inside next-lower-inside)))
	 (branches (list then-branch else-branch))
	 (next-upper-outside (copy-namespace prev-upper)) ;variables local to a branch are not visible after the IF, but the IF could contain a SETQ, whose change we need to carry.
	 (next-lower-outside (copy-namespace prev-lower)))
    (make-instance 'alt-form :branches branches
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper next-upper-outside :next-lower next-lower-outside
		   :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))

(defmethod prepare-ast ((ast walker:tagbody-form) prev-upper prev-lower)
  (let ((tags-upper prev-upper)
	(tags-lower prev-lower)
	(tags (make-hash-table :test #'equal)))
    (loop for form in (walker:form-body ast) do
	 (when (typep form 'walker:tag)
	   (let ((ast (make-instance 'tag :tag form :prevs-upper nil :prevs-lower nil :form-upper (make-results 'null) :form-lower (make-results 'null)))) ;NULL because a TAGBODY returns NIL, and a TAG might be the last element in the body of the TAGBDOY.
	     (when (gethash form tags)
	       (error "Tag ~S appears more than once in TAGBODY ~S" (walker:nso-name form) ast))
	     (setf (gethash form tags) ast)
	     (setf tags-upper (augment-namespace form ast tags-upper))
	     (setf tags-lower (augment-namespace form ast tags-lower)))))
    (let* ((next-upper tags-upper)
	   (next-lower tags-lower)
	   (body-forms (if (null (walker:form-body ast))
			   (make-instance 'walker:selfevalobject :object nil)
			   (walker:form-body ast)))
	   (body (loop for form in body-forms collect
		      (cond
			((typep form 'walker:tag)
			 (let ((ast (gethash form tags nil)))
			   (push (copy-namespace next-upper) (form-prevs-upper ast))
			   (push (copy-namespace next-lower) (form-prevs-lower ast))
			   (setf (form-next-upper ast) next-upper)
			   (setf (form-next-lower ast) next-lower)
			   ast))
			(t
			 (let ((ast (prepare-ast form next-upper next-lower)))
			   (setf next-upper (form-next-upper ast))
			   (setf next-lower (form-next-lower ast))
			   ast))))))
      (make-instance 'tagbody-form :body body
		     :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace prev-lower) ;NEXT must be a separate instance, because there may be a SETQ in TAGBODY.
		     :form-upper (make-results 'null) :form-lower (make-results 'null)))))

(defmethod prepare-ast ((ast walker:go-form) prev-upper prev-lower)
  (let ((tag (namespace-lookup (walker:form-tag ast) prev-upper)))
    (unless (find prev-upper (form-prevs-upper tag))
      (push prev-upper (form-prevs-upper tag))
      (push prev-lower (form-prevs-lower tag)))
    (make-instance 'go-form :tag (walker:form-tag ast)
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		   :form-upper (make-results 'null) :form-lower (make-results 'null)))) ;NULL because a TAGBODY returns NIL.

(defmethod prepare-ast ((ast walker:block-form) prev-upper prev-lower)
  (let* ((blo (make-instance 'blo :blo (walker:form-blo ast) :prev-upper prev-upper :prev-lower prev-lower :nexts-upper nil :nexts-lower nil))
	 (current (make-instance 'block-form :blo (walker:form-blo ast)
				 :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace prev-lower)
				 :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil)))
	 (next-upper (augment-namespace (walker:form-blo ast) blo prev-upper))
	 (next-lower (augment-namespace (walker:form-blo ast) blo prev-lower))
	 (body-forms (if (null (walker:form-body ast))
			 (make-instance 'walker:selfevalobject :object nil)
			 (walker:form-body ast)))
	 (body (loop for form in body-forms collect
		    (let ((ast (prepare-ast form next-upper next-lower)))
		      (setf next-upper (form-next-upper ast))
		      (setf next-lower (form-next-lower ast))
		      ast))))
    (push next-upper (form-nexts-upper blo))
    (push next-lower (form-nexts-lower blo))
    (setf (walker:nso-definition blo) current)
    (setf (walker:form-body current) body)
    current))

(defmethod prepare-ast ((ast walker:return-from-form) prev-upper prev-lower)
  (let* ((blo (namespace-lookup (walker:form-blo ast) prev-upper))
	 (value (prepare-ast (walker:form-value ast) prev-upper prev-lower))
	 (value-next-upper (form-next-upper value))
	 (value-next-lower (form-next-lower value)))
    (push value-next-upper (form-nexts-upper blo))
    (push value-next-lower (form-nexts-lower blo))
    (make-instance 'return-from-form :blo (walker:form-blo ast) :value value
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper value-next-upper :next-lower value-next-lower
		   :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))

;;; DEDUCE-FORWARD

(defun carry-prev-to-next (prev-upper prev-lower next-upper next-lower)
  "Carry over changes from PREV to NEXT.
  Derived from forward propagation rule for when A splits into B and C, but there is no C.
upper(B) = upper(A) meet upper(B)
upper(C) = upper(A) meet upper(C)
lower(B) = lower(A) meet upper(B)
lower(C) = lower(A) meet upper(C)"
  (map-namespace (lambda (a-upper a-lower b-upper b-lower)
		   (setf (cdr b-upper) (meet (cdr a-upper) (cdr b-upper)))
		   (setf (cdr b-lower) (meet (cdr a-lower) (cdr b-upper))))
		 prev-upper prev-lower next-upper next-lower))

(defun merge-branches (prevs-upper prevs-lower next-upper next-lower)
  "Backward propagation rule for split of A into B and C:
upper(A) = (upper(B) join upper(C)) meet upper(A)
lower(A) = (lower(B) join lower(C)) meet upper(A)"
  (let ((after-upper (lambda (a-upper &rest branches-upper)
		       (setf (cdr a-upper) (meet (join-typelist (mapcar #'cdr branches-upper) +builtin-typehash+) (cdr a-upper)))))
	(after-lower (lambda (a-upper a-lower &rest branches-lower)
		       (setf (cdr a-lower) (meet (join-typelist (mapcar #'cdr branches-lower) +builtin-typehash+) (cdr a-upper))))))
    (apply #'map-namespace after-upper next-upper prevs-upper)
    (apply #'map-namespace after-lower next-upper next-lower prevs-lower)))

(defmethod deduce-forward ((ast selfevalobject))
  ;; nothing to do, #'PREPARE-AST already initialized FORM-UPPER and FORM-LOWER.
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object, since there cannot be a type change inside a SELFEVALOBJECT form.
  nil)

(defmethod deduce-forward ((ast var))
  (declare (optimize (debug 3)))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  (let* ((upper (namespace-lookup (walker:form-var ast) (form-prev-upper ast)))
	 (lower (namespace-lookup (walker:form-var ast) (form-prev-lower ast)))
	 ;; TODO: FIXME: do we have to MEET here, or can we just set the result to the type of the variable?
	 (new-upper (meet upper (result1 (form-upper ast))))
	 (new-lower (meet lower (result1 (form-upper ast)))))
    (setf (form-upper ast) (make-results new-upper))
    (setf (form-lower ast) (make-results new-lower))))

(defmethod deduce-forward ((ast let-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast)))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form)))
    (carry-prev-to-next (form-prev-upper ast) (form-prev-lower ast) body-upper body-lower)
    (loop for binding in (walker:form-bindings ast) do
	 (let ((var (walker:form-sym binding))
	       (value (walker:form-value binding)))
	   (deduce-forward value)
	   (let* ((value-upper (result1 (form-upper value)))
		  (value-lower (result1 (form-lower value)))
		  (var-upper (namespace-lookup var body-upper))
		  ;; TODO: FIXME: are the following MEETs correct?
		  (new-upper (meet value-upper var-upper))
		  (new-lower (meet value-lower var-upper)))
	     (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var value-upper var-upper)
	     (setf (namespace-lookup var body-upper) new-upper)
	     (setf (namespace-lookup var body-lower) new-lower))))
    (loop for form in (butlast (walker:form-body ast)) do
	 (deduce-forward form))
    (let ((last-form (car (last (walker:form-body ast)))))
      (deduce-forward last-form)
      ;; TODO: FIXME: are the following MEETs correct?
      (setf (form-upper ast) (meet-results (form-upper last-form) (form-upper ast)))
      (setf (form-lower ast) (meet-results (form-lower last-form) (form-upper ast)))
      (carry-prev-to-next (form-next-upper last-form) (form-next-lower last-form) (form-next-upper ast) (form-next-lower ast)))))


(defmethod deduce-forward ((ast application-form))
  "upper(z) = t-function(f,0,upper(x),upper(y)) meet upper(z)
lower(z) = t-function(f,0,lower(x),lower(y)) meet upper(z))"
  (declare (optimize (debug 3)))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  (let* ((prev-upper (form-prev-upper ast))
	 (prev-lower (form-prev-lower ast))
	 (fun (walker:form-fun ast))
	 (args (walker:form-arguments ast))
	 (arg-types-upper (loop for arg in args collect (namespace-lookup arg prev-upper)))
	 (arg-types-lower (loop for arg in args collect (namespace-lookup arg prev-lower)))
	 (fun-result-upper (fun-result-lookup fun arg-types-upper))
	 (fun-result-lower (fun-result-lookup fun arg-types-lower)))
    (when (null (result1 fun-result-upper))
      (error "Function application ~S yields impossible type ~S." ast fun-result-upper))
    (setf (form-upper ast) (meet-results fun-result-upper (form-upper ast)))
    (setf (form-lower ast) (meet-results fun-result-lower (form-upper ast)))
    (when (null (form-upper ast))
      (error "Meet in function application ~S between types ~S and ~S yields impossible type." ast fun-result-upper (form-upper ast)))))

(defmethod deduce-forward ((ast setq-form))
  (declare (optimize (debug 3)))
  ;; before changing the namespace, carry over changes made earlier.
  (carry-prev-to-next (form-prev-upper ast) (form-prev-lower ast) (form-next-upper ast) (form-next-lower ast))
  (let* ((next-upper (form-next-upper ast))
	 (next-lower (form-next-lower ast))
	 (var (walker:form-var ast))
	 (value (walker:form-value ast))
	 (decl (var-declared-type var)))
    (deduce-forward value)
    (let ((new-upper (meet (result1 (form-upper value)) decl))
	  (new-lower (meet (result1 (form-lower value)) decl)))
      (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var (result1 (form-upper value)) decl)
      (setf (namespace-lookup var next-upper) new-upper)
      (setf (namespace-lookup var next-lower) new-lower)
      (setf (form-upper ast) (make-results new-upper))
      (setf (form-lower ast) (make-results new-lower)))))

(defmethod deduce-forward ((ast alt-form))
  (declare (optimize (debug 3)))
  (let ((branches (form-branches ast)))
    (loop for branch in branches do
	 (carry-prev-to-next (form-prev-upper ast) (form-prev-lower ast) (form-prev-upper branch) (form-prev-lower branch)))
    (loop for branch in branches do
	 (deduce-forward branch))
    (merge-branches (mapcar #'form-next-upper branches) (mapcar #'form-next-lower branches) (form-next-upper ast) (form-next-lower ast))
    (setf (form-upper ast) (meet-results (reduce #'join-results (mapcar #'form-upper branches) :initial-value (form-upper (car branches))) (form-upper ast)))
    (setf (form-lower ast) (meet-results (reduce #'join-results (mapcar #'form-lower branches) :initial-value (form-lower (car branches))) (form-upper ast)))))

(defmethod deduce-forward ((ast tagbody-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast)))
	 (last-form (car (last (walker:form-body ast)))))
    (let ((body-upper (if (typep first-form 'tag) (car (last (form-prevs-upper first-form))) (form-prev-upper first-form)))
	  (body-lower (if (typep first-form 'tag) (car (last (form-prevs-upper first-form))) (form-prev-lower first-form))))
      (unless (null first-form)
	(carry-prev-to-next (form-prev-upper ast) (form-prev-lower ast) body-upper body-lower)))
    (loop for form in (walker:form-body ast) do
	 (deduce-forward form))
    (unless (null last-form)
      (carry-prev-to-next (form-next-upper last-form) (form-next-lower last-form) (form-next-upper ast) (form-next-lower ast)))
    ;; no need to update FORM-UPPER or FORM-LOWER, since TAGBODY always returns NIL.
    ))

(defmethod deduce-forward ((ast tag))
  (declare (optimize (debug 3)))
  (merge-branches (form-prevs-upper ast) (form-prevs-lower ast) (form-next-upper ast) (form-next-lower ast)))

(defmethod deduce-forward ((ast go-form))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  )

(defmethod deduce-forward ((ast block-form))
  (loop for form in (walker:form-body ast) do
       (deduce-forward form))
  (let ((last-form (car (last (walker:form-body ast)))))
    ;; TODO: FIXME: are the following MEETs correct?
    (setf (form-upper ast) (meet-results (form-upper last-form) (form-upper ast)))
    (setf (form-lower ast) (meet-results (form-lower last-form) (form-upper ast)))
    (carry-prev-to-next (form-next-upper last-form) (form-next-lower last-form) (form-next-upper ast) (form-next-lower ast))))

(defmethod deduce-forward ((ast return-from-form))
  (let* ((blo (namespace-lookup (walker:form-blo ast) (form-prev-upper ast)))
	 (block-form (walker:nso-definition blo))
	 (value (walker:form-value ast)))
    (deduce-forward value)
    (setf (form-upper block-form) (meet-results (form-upper value) (form-upper block-form)))
    (setf (form-lower block-form) (meet-results (form-lower value) (form-upper block-form)))
    (carry-prev-to-next (form-next-upper block-form) (form-next-lower block-form) (form-next-upper value) (form-next-upper value))))

(labels ((prepare (form)
	   (let* ((ast (walker:parse-with-empty-namespaces form :free-common-lisp-namespace t))
		  (ast (prepare-ast ast nil nil)))
	     ast))
	 (assert-result (form upper)
	   (let* ((ast (prepare form)))
	     (deduce-forward ast)
	     (let* ((results (form-upper ast))
		    (finite (results-finite results))
		    (results (loop for i below (length upper) collect
				  (if (< i (length finite)) (elt finite i) (results-infinite results)))))
	       (assert (equal results upper) () "failed assertion for form ~S: results=~S upper:~S" form results upper)))))
  (macrolet ((assert-error (form)
	       (let ((ast-sym (gensym "AST")) (form-sym (gensym "FORM")))
		 `(let* ((,form-sym ,form)
			 (,ast-sym (prepare ,form-sym)))
		    (handler-case (progn (deduce-forward ,ast-sym) (assert nil () "failed (ASSERT-ERROR ~S)" ,form-sym))
		      ;; TODO: replace T with something like TYPE-ERROR.
		      (t () t))))))
    (assert-result '1 '(integer))
    (assert-result '(+ 1 2) '(integer))
    (assert-result '(+ 1 2.0) '(single-float))
    (assert-result '(let ((a 1) (b 2)) (+ a b)) '(integer))
    (assert-error '(let ((a 1) (b 2)) (declare (type single-float a)) (+ a b)))
    (assert-result '(let ((a 1) (b 2)) (declare (type number a)) (+ a b)) '(integer))
    (assert-result '(let ((a 1.0)) (setq a 1) (+ a 2)) '(integer))
    (assert-error '(let ((a 1.0)) (declare (type single-float a)) (setq a 1) (+ a 2)))
    (assert-result '(if 1 2 3) '(integer))
    (assert-result '(if 1 2 nil) '(t))
    (assert-result '(if 1 2) '(t))
    (assert-result '(let ((a 1)) (if 1 (setq a 2)) a) '(integer))
    (assert-result '(let ((a nil)) (if 1 (setq a 1) (setq a 2)) a) '(integer))
    (assert-result '(let ((a nil)) (if 1 (setq a 1) (setq a nil)) a) '(t))
    (assert-result '(let ((a nil)) (let () (setq a 1)) a) '(integer))
    (assert-result '(tagbody a (go b) b) '(null))
    (assert-result '(let ((a 10)) (tagbody s (setq a (1- a)) (if (<= a 0) (go e)) (go s) e) a) '(integer))
    (assert-result '(let ((a nil)) (tagbody (if (null a) (setq a 10)) s (setq a (1- a)) (if (<= a 0) (go e)) (go s) e) a) '(number))
    ))

;;; DEDUCE-BACKWARD

(defmethod deduce-backward ((ast selfevalobject))
  ;; nothing to do, PREPARE-AST already initialized FORM-UPPER and FORM-LOWER.
  ;; no need to carry over NEXT to PREV; they are the same object, since there cannot be a type change inside a SELFEVALOBJECT form.
  nil)

(defmethod deduce-backward ((ast var))
  (declare (optimize (debug 3)))
  ;; no need to carry over NEXT to PREV; they are the same object, since there cannot be a type change inside a SELFEVALOBJECT form.
  (let* ((var (walker:form-var ast))
	 (prev-upper (form-prev-upper ast))
	 (prev-lower (form-prev-lower ast))
	 (old-upper (namespace-lookup var prev-upper)))
    ;; do we have to MEET here, or can we just set the result to the type of the variable?
    (setf (namespace-lookup var prev-upper) (meet (result1 (form-upper ast)) old-upper))
    (setf (namespace-lookup var prev-lower) (meet (result1 (form-lower ast)) old-upper))))

(defmethod deduce-backward ((ast let-form))
  (declare (optimize (debug 3)))
  (let ((last-form (car (last (walker:form-body ast)))))
    (carry-prev-to-next (form-next-upper ast) (form-next-lower ast) (form-next-upper last-form) (form-next-lower last-form))
    (setf (form-upper last-form) (meet-results (form-upper ast) (form-upper last-form)))
    (setf (form-lower last-form) (meet-results (form-lower ast) (form-upper last-form)))
    (deduce-backward last-form))
  (loop for form in (reverse (butlast (walker:form-body ast))) do
       (deduce-backward form))
  (let* ((first-form (car (walker:form-body ast)))
	 (body-upper (form-prev-upper first-form)))
    (loop for binding in (reverse (walker:form-bindings ast)) do
	 (let* ((var (walker:form-sym binding))
		(value (walker:form-value binding))
		(value-upper (result1 (form-upper value)))
		(value-lower (result1 (form-lower value)))
		(var-upper (namespace-lookup var body-upper))
		;; TODO: FIXME: are the following MEETs correct?
		(new-upper (meet value-upper var-upper))
		(new-lower (meet value-lower var-upper)))
	   (setf (result1 (form-upper value)) new-upper)
	   (setf (result1 (form-lower value)) new-lower)
	   (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var var-upper value-upper))
	 (deduce-backward (walker:form-value binding)))
    (carry-prev-to-next (form-prev-upper first-form) (form-prev-lower first-form) (form-prev-upper ast) (form-prev-lower ast))))

(defmethod deduce-backward ((ast application-form))
  (declare (optimize (debug 3)))
  ;; no need to carry over NEXT to PREV; they are the same object, since there cannot be a type change inside a APPLICATION-FORM.
  (let* ((prev-upper (form-prev-upper ast))
	 (prev-lower (form-prev-lower ast))
	 (fun (walker:form-fun ast))
	 (results-upper (form-upper ast))
	 (results-lower (form-lower ast))
	 (args (walker:form-arguments ast))
	 (arg-types-upper (loop for arg in args collect (namespace-lookup arg prev-upper)))
	 (arg-types-lower (loop for arg in args collect (namespace-lookup arg prev-lower)))
	 (fun-args-upper (fun-arguments-lookup fun results-upper))
	 (fun-args-lower (fun-arguments-lookup fun results-lower))
	 (met-arguments (meet-arguments fun-args-upper arg-types-upper)))
    (declare (ignore arg-types-lower fun-args-lower))
    (loop for arg in args for type in met-arguments do
	 (setf (namespace-lookup arg prev-upper) type))
    ;; "upper(z) = upper(zbefore)" ? What does it mean?
    ))

(defmethod deduce-backward ((ast setq-form))
  (declare (optimize (debug 3)))
  (deduce-backward (walker:form-value ast))
  (let* ((var (walker:form-var ast))
	 (value (walker:form-value ast))
	 (old-upper (result1 (form-upper value)))
	 (old-lower (result1 (form-lower value)))
	 (var-upper (namespace-lookup var (form-next-upper ast)))
	 (var-lower (namespace-lookup var (form-next-lower ast)))
	 ;; TODO: FIXME: are the following MEETs correct?
	 (new-upper (meet old-upper var-upper))
	 (new-lower (meet old-lower var-upper)))
    (declare (ignore var-lower))
    (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var var-upper old-upper)
    (setf (result1 (form-upper value)) new-upper)
    (setf (result1 (form-lower value)) new-lower)
    (setf (namespace-lookup var (form-next-upper ast)) new-upper)
    (setf (namespace-lookup var (form-next-lower ast)) new-lower))
  ;; at last, carry over changes made earlier to the previous form.
  (carry-prev-to-next (form-next-upper ast) (form-next-lower ast) (form-prev-upper ast) (form-prev-lower ast)))

(defmethod deduce-backward ((ast alt-form))
  (declare (optimize (debug 3)))
  (let ((branches (form-branches ast)))
    (loop for branch in branches do
	 (carry-prev-to-next (form-prev-upper ast) (form-prev-lower ast) (form-prev-upper branch) (form-prev-lower branch)))
    (loop for branch in branches do
	 (setf (form-upper branch) (meet-results (form-upper ast) (form-upper branch)))
	 (setf (form-lower branch) (meet-results (form-lower ast) (form-upper branch)))
	 (deduce-backward branch))
    (merge-branches (mapcar #'form-next-upper branches) (mapcar #'form-next-lower branches) (form-next-upper ast) (form-next-lower ast))))

(defmethod deduce-backward ((ast tagbody-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast)))
	 (last-form (car (last (walker:form-body ast)))))
    ;; no need to update FORM-UPPER or FORM-LOWER, since TAGBODY always returns NIL.
    (unless (null last-form)
      (carry-prev-to-next (form-next-upper ast) (form-next-lower ast) (form-next-upper last-form) (form-next-lower last-form)))
    (loop for form in (walker:form-body ast) do
	 (deduce-backward form))
    (let ((body-upper (if (typep first-form 'tag) (car (last (form-prevs-upper first-form))) (form-prev-upper first-form)))
	  (body-lower (if (typep first-form 'tag) (car (last (form-prevs-upper first-form))) (form-prev-lower first-form))))
      (unless (null first-form)
	(carry-prev-to-next body-upper body-lower (form-prev-upper ast) (form-prev-lower ast))))))

(defmethod deduce-backward ((ast tag))
  (declare (optimize (debug 3)))
  (let ((prevs-upper (form-prevs-upper ast))
	(prevs-lower (form-prevs-lower ast)))
    (loop for prev-upper in prevs-upper for prev-lower in prevs-lower do
	 (carry-prev-to-next (form-next-upper ast) (form-next-lower ast) prev-upper prev-lower))))

(defmethod deduce-backward ((ast go-form))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  )

(defmethod deduce-backward ((ast block-form))
  (let* ((last-form (car (last (walker:form-body ast))))
	 (blo (namespace-lookup (walker:form-blo ast) (form-prev-upper last-form))))
    ;; TODO: FIXME: are the following MEETs correct?
    (loop for return-next-upper in (form-nexts-upper blo) for return-next-lower in (form-nexts-lower blo) do
	 (carry-prev-to-next (form-next-upper ast) (form-next-lower ast) return-next-upper return-next-lower)))
  (loop for form in (walker:form-body ast) do
       (deduce-backward form)))

(defmethod deduce-backward ((ast return-from-form))
  (let* ((blo (namespace-lookup (walker:form-blo ast) (form-prev-upper ast)))
	 (block-form (walker:nso-definition blo))
	 (value (walker:form-value ast)))
    (carry-prev-to-next (form-next-upper value) (form-next-upper value) (form-next-upper block-form) (form-next-lower block-form))
    (setf (form-upper value) (meet-results (form-upper block-form) (form-upper value)))
    (setf (form-lower value) (meet-results (form-lower block-form) (form-upper value)))
    (deduce-forward value)))

(let* ((form '(aref (make-array-single-float 10 20) 2))
       (ast (walker:parse-with-empty-namespaces form :free-common-lisp-namespace t))
       (ast (prepare-ast ast nil nil)))
  (deduce-backward ast)
  (assert (eq 'array (namespace-lookup (walker:form-sym (car (walker:form-bindings ast))) (form-prev-upper (car (walker:form-body ast)))))))
(let* ((form '(1+ (block bl (return-from bl 1))))
       (ast (walker:parse-with-empty-namespaces form :free-common-lisp-namespace t))
       (ast (prepare-ast ast nil nil)))
  (deduce-backward ast)
  (assert (eq 'number (result1 (form-upper (walker:form-value (car (walker:form-bindings ast))))))))
