;; TODO: think about how ASSERT can be included. The NTI paper says: """In the classical non-deterministic manner, the flow of control is terminated only by branches of the computation that fail in the sense that there are no legitimate values for variables. In this setting, predicates are modelled by partial functions whose results are ignored. One particularly valuable partial function of this kind is "assert(p)" which is defined only for the argument "true".""" One way to include ASSERT would be to model it as an IF-FORM, which has as its (only) THEN-FORM the code following the ASSERT. What about: (LET ((A 1)) (PROGN (ASSERT (INTEGERP A))) (LET ((B 1)) B))? Should the THEN-FORM include the second LET? SBCL compiles the following without a warning, although it would be possible to infer that the assertion always fails: (DEFUN TEST (X Y) (ASSERT (AND (> X Y) (<= X Y)))). Neither does SBCL complain for (DEFUN TEST (X) (ASSERT (AND (INTEGERP X) (TYPEP X 'SINGLE-FLOAT)))) or for (DEFUN TEST (X) (DECLARE (TYPE SINGLE-FLOAT X)) (ASSERT (INTEGERP X))).


(load "~/quicklisp/setup.lisp")
(ql:quickload '(:walker :walker-plus))
(ql:quickload :equals)

(defpackage :nimble-type-inferencer
  (:documentation "Nimble type inferencer for ANSI Lisp, see the paper \"The Nimble Type Inferencer for Common Lisp-84\" by Henry G. Baker.")
  (:use :cl :equals)
  (:export
   ;; for classes: export the class and _all_ accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   ))
(in-package :nimble-type-inferencer)

(defparameter +builtin-functions+
  '(;; functions needed for NTI.
    (null "null_t" (t) (boolean))
    (null "null_null" (null) (boolean))
    (null "null_boolean" (boolean) (boolean))
    (null "null_list" (list) (boolean))
    (null "null_number" (number) (boolean))
    (null "null_fixnum" (fixnum) (boolean))
    (null "null_int" (integer) (boolean))
    (null "null_uint" (unsigned-byte) (boolean))
    (null "null_float" (single-float) (boolean))
    (null "null_symbol" (symbol) (boolean))
    (null "null_v4sf" (v4s-float) (boolean))
    (null "null_array" (array) (boolean))
    (null "null_array_fixnum" ((array fixnum)) (boolean))
    (null "null_array_float" ((array single-float)) (boolean))
    ;; arithmetic: int int
    (+ "plus_int_int" (fixnum fixnum) (fixnum))
    (* "multiply_int_int" (fixnum fixnum) (fixnum))
    (- "minus_int_int" (fixnum fixnum) (fixnum))
    (/ "divide_int_int" (fixnum fixnum) (fixnum))
    (max "max_int_int" (fixnum fixnum) (fixnum))
    (min "min_int_int" (fixnum fixnum) (fixnum))
    ;; arithmetic float float
    (+ "plus_float_float" (single-float single-float) (single-float))
    (* "multiply_float_float" (single-float single-float) (single-float))
    (- "minus_float_float" (single-float single-float) (single-float))
    (/ "divide_float_float" (single-float single-float) (single-float))
    (max "max_float_float" (single-float single-float) (single-float))
    (min "min_float_float" (single-float single-float) (single-float))
    ;; arithmetic float int
    (+ "plus_float_int" (single-float fixnum) (single-float))
    (* "multiply_float_int" (single-float fixnum) (single-float))
    (- "minus_float_int" (single-float fixnum) (single-float))
    (/ "divide_float_int" (single-float fixnum) (single-float))
    (max "max_float_int" (single-float fixnum) (single-float))
    (min "min_float_int" (single-float fixnum) (single-float))
    ;; arithmetic int float
    (+ "plus_int_float" (fixnum single-float) (single-float))
    (* "multiply_int_float" (fixnum single-float) (single-float))
    (- "minus_int_float" (fixnum single-float) (single-float))
    (/ "divide_int_float" (fixnum single-float) (single-float))
    (max "max_int_float" (fixnum single-float) (single-float))
    (min "min_int_float" (fixnum single-float) (single-float))
    ;; arithmetic: int
    (1+ "plusone_int" (fixnum) (fixnum))
    (1- "minusone_int" (fixnum) (fixnum))
    (abs "abs_int" (fixnum) (fixnum))
    (signum "signum_int" (fixnum) (fixnum))
    ;; arithmetic: float
    (1+ "plusone_float" (single-float) (single-float))
    (1- "minusone_float" (single-float) (single-float))
    (abs "abs_float" (single-float) (single-float))
    (signum "signum_float" (single-float) (single-float))
    ;; misc
    (eq "eq_int_int" (symbol symbol) (fixnum))
    ;; comparison: int int
    (< "less_int_int" (fixnum fixnum) (boolean))
    (<= "lessequal_int_int" (fixnum fixnum) (boolean))
    (> "greater_int_int" (fixnum fixnum) (boolean))
    (>= "greaterequal_int_int" (fixnum fixnum) (boolean))
    (= "equal_int_int" (fixnum fixnum) (boolean))
    (/= "notequal_int_int" (fixnum fixnum) (boolean))
    ;; comparison: float float
    (< "less_float_float" (single-float single-float) (boolean))
    (<= "lessequal_float_float" (single-float single-float) (boolean))
    (> "greater_float_float" (single-float single-float) (boolean))
    (>= "greaterequal_float_float" (single-float single-float) (boolean))
    (= "equal_float_float" (single-float single-float) (boolean))
    (/= "notequal_float_float" (single-float single-float) (boolean))
    ;; comparison: float int
    (< "less_float_int" (single-float fixnum) (boolean))
    (<= "lessequal_float_int" (single-float fixnum) (boolean))
    (> "greater_float_int" (single-float fixnum) (boolean))
    (>= "greaterequal_float_int" (single-float fixnum) (boolean))
    (= "equal_float_int" (single-float fixnum) (boolean))
    (/= "notequal_float_int" (single-float fixnum) (boolean))
    ;; comparison: int float
    (< "less_int_float_int" (fixnum single-float) (boolean))
    (<= "lessequal_int_float" (fixnum single-float) (boolean))
    (> "greater_int_float" (fixnum single-float) (boolean))
    (>= "greaterequal_int_float" (fixnum single-float) (boolean))
    (= "equal_int_float" (fixnum single-float) (boolean))
    (/= "notequal_int_float" (fixnum single-float) (boolean))
    ;; logical
    (and-f "and_f_int_int" (fixnum fixnum) (fixnum))
    (or-f "or_f_int_int" (fixnum fixnum) (fixnum))
    (not "not_int" (fixnum) (fixnum))
    (not "not_bool" (boolean) (boolean))
    (fail-with-message "fail_with_message_str" (string) ())
    ;; type conversion
    (float "float_int" (fixnum) (single-float))
    (floor "floor_float" (single-float) (fixnum single-float))
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
    (outside-border-p "outside_border_p_v4sf_float_float_float" (v4s-float single-float single-float single-float) (boolean))
    (computelod "computelod_float_float" (single-float single-float) (fixnum))
    (prind-helper "prind_helper_string_int" (string fixnum) ())
    (prind-helper "prind_helper_string_float" (string single-float) ())
    (prind-helper "prind_helper_string_v4sf" (string v4s-float) ())
    (prind-helper "prind_helper_string_string" (string string) ())
    (prind-helper-format "prind_helper_format_string" (string) ())
    (height-function "height_function_v4sf_float_heightmap" (v4s-float single-float heightmap) (single-float))
    (computestepsize "computestepsize_float" (single-float) (single-float))
    (computestepsize "computestepsize_int" (fixnum) (single-float))
    (is-above-heightmap "is_above_heightmap" (v4s-float single-float t) (boolean))
    ;;(advance-ray-3d "ADVANCE_RAY_3D0" (v4s-float v4s-float single-float single-float single-float single-float single-float single-float fixnum) (v4s-float single-float symbol))
    (intersect-border "INTERSECT_BORDER0" (v4s-float v4s-float v4s-float single-float single-float single-float single-float single-float single-float) (v4s-float single-float))
    ;;(outside-border-p "OUTSIDE_BORDER0" (v4s-float single-float single-float single-float) (boolean))
    ;;(scan-line-continuous "SCAN_LINE_CONTINUOUS0" (v4s-float v4s-float single-float (function (v4s-float) boolean) single-float single-float single-float single-float single-float single-float) (v4s-float v4s-float boolean single-float))
    (make-array-single-float "make_array_float_uint_uint" (unsigned-byte unsigned-byte) ((array single-float)))
    (make-array-fixnum "make_array_int_uint_uint" (unsigned-byte unsigned-byte) ((array fixnum)))
    (aref "aref_array_float_uint" ((array single-float) unsigned-byte) (single-float))
    (aref "aref_array_fixnum_uint" ((array fixnum) unsigned-byte) (fixnum))
    ))

;; transform the program representation as parsed by WALKER to a program which only uses the following constructs: 1. selfevalobjects, (self-evaluating) variable and function symbols, let-form, setq-form, (function) application-form.


(defun make-empty-namespace ()
  nil)

(defun augment-namespace (sym type namespace)
  (acons sym type namespace))

(defun namespace-boundp (sym namespace)
  (let* ((cons (assoc sym namespace :test #'equals)))
    (not (null cons))))

(defun namespace-lookup (sym namespace)
  (let* ((cons (assoc sym namespace :test #'equals)))
    (assert (not (null cons)) () "SYM ~A not bound in ~S" sym namespace)
    (cdr cons)))

(defmacro debug-namespace (sym namespace)
  (let ((sym-sym (gensym "SYM"))
	(ns-sym (gensym "NS"))
	(cons-sym (gensym "CONS")))
    `(let ((,sym-sym ,sym)
	   (,ns-sym ,namespace))
       (loop for ,cons-sym in ,ns-sym do
	    (when (eq (walker:nso-name (car ,cons-sym)) ,sym-sym)
	      (format t "sym:~A has type ~A in namespace ~S~%" ,sym-sym (cdr ,cons-sym) ',namespace))))))

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

(defun namespace-vars (namespace)
  "For debugging."
  (let ((vars nil))
    (loop for cons in namespace do
	 (when (typep (car cons) 'walker:var)
	   (push cons vars)))
    (nreverse vars)))

(load "nti-subtypep.lisp")

(defstruct v4s-float)
(defparameter +builtin-types+
  '(nil t null boolean list ;these are always needed: NIL and T as bottom and top element of the type lattice, NULL as result of TAGBODY, BOOLEAN as type of &OPTIONAL and &KEY suppliedp arguments, and LIST for &REST arguments.
    number integer fixnum unsigned-byte single-float (and unsigned-byte fixnum)
    symbol
    v4s-float
    array (array fixnum) (array single-float)))

(defparameter +builtin-typehash+ (make-typegraph +builtin-types+))

(defun join (type1 type2)
  (join-type type1 type2 +builtin-typehash+))

(defun meet (type1 type2)
  (meet-type type1 type2 +builtin-typehash+))

(defstruct (results (:constructor make-results*))
  "The type of multiple values: NVALUES is the number of possible finite values exponentiated by 2 and LOGIOR'd together; FINITE is of type (LIST TYPE); and INFINITE is of type TYPE."
  (nvalues -1 :type integer :read-only t) ;the number of values of the result, -1 means any number of values
  (finite nil :type list) ;the beginning of the values list of the result
  (infinite nil :type (or symbol list))) ;the infinite part of the values list of the result

(defun make-results (&rest results)
  (make-results* :nvalues (expt 2 (length results)) :finite (copy-list results) :infinite 'null))

(defun most-significant-bit (n)
  "Returns the number of the most significant bit, which is set in N. N must be an unsigned number. Returns -1 if N is 0."
  (declare (type unsigned-byte n))
  (if (= 0 n)
      -1
      (let ((e 1))
	(loop for i from 0 do
	     (when (/= 0 (logior n e))
	       (setf n (logandc2 n e)))
	     (when (= n 0)
	       (return i))
	     (setf e (* e 2))))))

(defun results-most (results)
  (most-significant-bit (results-nvalues results)))

(defun resultn (results n)
  (declare (optimize (debug 3)))
  (assert (or (= (results-nvalues results) -1) (< n (results-most results))) () "Trying to read ~Sth element of at most ~S values" n (results-most results))
  (cond
    ((< n (length (results-finite results)))
     (elt (results-finite results) n))
    (t
     (results-infinite results))))

(defun (setf resultn) (value results n)
  (let ((nf (length (results-finite results))))
    (assert (or (= (results-nvalues results) -1) (< n (results-most results))) () "Trying to set ~Sth element of at most ~S values" n (results-most results))
    (cond
      ((< n nf)
       (setf (elt (results-finite results) n) value))
      (t
       (setf (results-finite results) (append (results-finite results)
					      (loop for i below (- n nf) collect
						   (results-infinite results))
					      (list value))))))
  ;; remove FINITE tail that is equal to INFINITE.
  (let ((l (loop for i from (length (results-finite results)) downto 0 for e in (reverse (cons nil (results-finite results))) do
		(when (or (= i 0) (not (equal e (results-infinite results))))
		  (return i)))))
    (setf (results-finite results) (subseq (results-finite results) 0 l))))

(defun result1 (results)
  (resultn results 0))

(defun (setf result1) (value results)
  (setf (resultn results 0) value))

(defmethod print-object ((object results) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~S]" (results-nvalues object))
    (loop for type in (append (results-finite object) (list (results-infinite object))) do
	 (format stream " ~A" type))
    (format stream "...")))

(let ((r (make-results* :nvalues -1 :finite nil :infinite nil)))
  (setf (resultn r 0) 1)
  (assert (= (resultn r 0) 1))
  (setf (result1 r) 2)
  (assert (= (resultn r 0) (result1 r) 2))
  (setf (result1 r) nil)
  (assert (and (= (results-nvalues r) -1) (null (results-finite r)) (null (results-infinite r))))
  (setf (resultn r 1) 1)
  (assert (and (= (results-nvalues r) -1) (equal (results-finite r) '(nil 1)) (null (results-infinite r)))))
(let ((r (make-results* :nvalues 4 :finite nil :infinite nil)))
  (setf (resultn r 0) 1)
  (assert (and (= (result1 r) (resultn r 0) 1) (= (results-nvalues r) 4)))
  (setf (result1 r) nil)
  (assert (and (null (resultn r 0)) (null (result1 r)) (= (results-nvalues r) 4) (null (results-finite r)) (null (results-infinite r))))
  (setf (resultn r 1) 1)
  (assert (and (= (results-nvalues r) 4) (equal (results-finite r) '(nil 1)) (null (results-infinite r))))
  (setf (resultn r 1) nil)
  (assert (and (= (results-nvalues r) 4) (null (results-finite r)) (null (results-infinite r)))))

(defun process-results (results1 results2 function nvalues-function)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Apply FUNCTION to them."
  (declare (optimize (debug 3)))
  (let* ((nf1 (length (results-finite results1)))
	 (nf2 (length (results-finite results2)))
	 (min-nf (max 0 (min nf1 nf2)))
	 (max-nf (max nf1 nf2))
	 (finite (append (loop for i below min-nf for t1 in (results-finite results1) for t2 in (results-finite results2) collect
			      (funcall function t1 t2))
			 (loop for i from min-nf below max-nf collect
			      (let ((t1 (if (>= i nf1) (results-infinite results1) (elt (results-finite results1) i)))
				    (t2 (if (>= i nf2) (results-infinite results2) (elt (results-finite results2) i))))
				(funcall function t1 t2)))))
	 (infinite (funcall function (results-infinite results1) (results-infinite results2)))
	 (finite-cropped (let* ((last 0))
			   (loop for i from (1- (length finite)) downto 0 do
				(when (not (equal (elt finite i) infinite)) (setf last (1+ i)) (return)))
			   (subseq finite 0 last)))
	 (nv1 (results-nvalues results1))
	 (nv2 (results-nvalues results2))
	 (nv (funcall nvalues-function nv1 nv2)))
    (make-results* :nvalues nv :finite finite-cropped :infinite infinite)))    

(defun meet-results (results1 results2)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Meet them and return the new RESULTS."
  (process-results results1 results2 #'meet #'logand))

(defun join-results (results1 results2)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Join them and return the new RESULTS."
  (process-results results1 results2 #'join #'logior))

(defun meet-arguments (args1 args2)
  "ARGS1 and ARGS2 are each a list of TYPES, one for each NTH-VALUE. Meet the types of same N."
  (assert (= (length args1) (length args2)))
  (loop for i below (length args1) collect
       (meet-type (elt args1 i) (elt args2 i) +builtin-typehash+)))

(defun fun-result-lookup-upper (fun arg-types)
  "ARG-TYPES is the list of types of (arg1-type arg2-type ...)"
  (let ((fun (if (typep fun 'walker:fun)
		 (walker:nso-name fun)
		 fun))
	(name-found nil)
	(possible nil))
    (loop for (lisp-name c-name fun-arg-types fun-results-types) in +builtin-functions+ do
	 ;;(when (eq fun lisp-name) (prind fun arg-types fun-arg-types))
	 (when (and (eq fun lisp-name)
		    (setf name-found t)
		    ;;(equal arg-types fun-arg-types)
		    (loop for arg-type in arg-types for fun-arg-type in fun-arg-types always
			 (is-subtypep fun-arg-type arg-type +builtin-typehash+)))
	   (push fun-results-types possible)))
    (assert name-found () "Unknown function ~S" fun)
    (assert (not (null possible)) () "Called function ~S with unknown argument types ~S" fun arg-types)
    ;; POSSIBLE is now a ((TYPE-RESULT1 TYPE-RESULT2 ...) (TYPE-RESULT1 TYPE-RESULT2 ...) ...). Meet the types TYPE-RESULT1, then TYPE-RESULT2, ..., to get a list of TYPE-RESULTs.
    (assert (apply #'= (mapcar #'length possible)))
    (let ((length (length (car possible))))
      (apply #'make-results
	     (loop for l below length collect
		  (join-typelist (loop for poss in possible collect (elt poss l)) +builtin-typehash+))))))

(defun fun-result-lookup-lower (fun arg-types)
  "ARG-TYPES is the list of types of (arg1-type arg2-type ...)"
  (declare (ignore fun arg-types))
  (make-results* :nvalues -1 :infinite nil)) ;TODO: FIXME: store in lookup table and lookup the lower bound.

(defun fun-arguments-lookup (fun results)
  "RESULTS-TYPES is the list of types of (result1-type result2-type ...)"
  (let ((fun (if (typep fun 'walker:fun)
		 (walker:nso-name fun)
		 fun))
	(name-found nil)
	(possible nil))
    (loop for (lisp-name c-name fun-arg-types fun-results-types) in +builtin-functions+ do
	 (when (and (eq fun lisp-name)
		    (setf name-found t)
		    ;;(equal arg-types fun-arg-types)
		    (block always
		      (process-results (apply #'make-results fun-results-types) results
				       (lambda (t1 t2) (unless (is-subtypep t1 t2 +builtin-typehash+) (return-from always nil)) nil)
				       (constantly 0)) ;the value doesn't matter
		      t))
	   (push fun-arg-types possible)))
    (assert name-found () "Unknown function ~S" fun)
    (when (null possible)
      (return-from fun-arguments-lookup '(nil))) ;TODO: return the correct number of arguments
    (assert (not (null possible)))
    ;; POSSIBLE is now a ((TYPE-ARG1 TYPE-ARG2 ...) (TYPE-ARG1 TYPE-ARG2 ...) ...). Meet the types TYPE-ARG1, then TYPE-ARG2, ..., to get a list of TYPE-ARGs.
    (assert (apply #'= (mapcar #'length possible)))
    (let ((length (length (car possible))))
      (loop for l below length collect
	   (join-typelist (loop for poss in possible collect (elt poss l)) +builtin-typehash+)))))

(defclass function-type ()
  ((required :initform nil :initarg :required :accessor type-required :type list)
   (optional :initform nil :initarg :optional :accessor type-optional :type list)
   (rest :initarg :rest :accessor type-rest)
   (key :initform nil :initarg :key :accessor type-key :type list)
   (values :initform nil :initarg :values :accessor type-values :type list)))

(defun parse-function-declaration (decl)
  (multiple-value-bind (args vals) (walker:parse-function-declaration decl)
    (if (eq args 'function)
	'function
	(make-instance 'function-type
		       :required (cdr (assoc '&required args))
		       :optional (cdr (assoc '&optional args))
		       :rest (cadr (assoc '&rest args))
		       :key (cdr (assoc '&key args))
		       :values (apply #'make-results vals)))))

(defun meet-function (result1 result2)
  (cond
    ((null result1)
     nil)
    ((null result2)
     nil)
    ((eq result1 'function)
     result2)
    ((eq result2 'funcion)
     result1)
    (t
     ;; compare parameter and return value types and meet them. TODO: implement meeting &REST with &REQUIRED and &OPTIONAL arguments.
     (flet ((meet-list (r1 r2)
	      (if (= (length r1) (length r2))
		  (loop for type1 in r1 for type2 in r2 collect (let ((type (meet type1 type2))) (assert (not (null type))) type))
		  (return-from meet-function nil))))
       (let ((required (meet-list (type-required result1) (type-required result2)))
	     (optional (meet-list (type-optional result1) (type-optional result2)))
	     (rest (let ((type (meet (type-rest result1) (type-rest result2)))) (assert (not (null type))) type))
	     (key (meet-list (type-key result1) (type-key result2)))
	     (values (meet-results (type-values result1) (type-values result2))))
	 (assert (loop for type in values always (not (null type))) () "Impossible type ~S for values of function" values)
	 (make-instance 'function-type :required required :optional optional :rest rest :key key :values values))))))

(defun sym-declared-type (sym)
  (let* ((declspecs (walker:nso-declspecs sym))
	 (types (mapcar #'walker:declspec-type (remove-if-not (lambda (x) (typep x 'walker:declspec-type)) declspecs)))
	 (type (etypecase sym
		 (walker:var
		  (reduce #'meet types :initial-value t))
		 (walker:fun
		  (reduce #'meet-function types :initial-value 'function)))))
    (assert (not (null type)) () "Impossible type declarations for variable ~S: ~S" sym types)
    type))

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
  ((form-upper :initarg :form-upper :accessor form-upper :type results)
   (form-lower :initarg :form-lower :accessor form-lower :type results))
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
   (definition :initarg :definition :accessor walker:nso-definition)
   (branches :initarg :branches :accessor form-branches)))

(defmethod equals ((x walker:sym) (y walker:sym) &rest keys &key recursive &allow-other-keys)
  (declare (ignore recursive))
  (and (apply #'equals (walker:nso-name x) (walker:nso-name y) keys)
       (apply #'equals (walker:nso-freep x) (walker:nso-freep y) keys)))

(defclass var-binding (walker:var-binding)
  ())

(defclass fun-binding (walker:fun-binding flowstate-prevs) ;really, a function is not only called from multiple locations (FLOWSTATE-PREFS), but also merges into those multiple locations as well (FLOWSTATE-NEXTS), but they are the same, so we only use FLOWSTATE-PREVS here.
  ())

(defclass let-form (walker:let-form flowstate formvalue)
  ())

(defclass flet-form (walker:flet-form flowstate formvalue)
  ())

(defclass application-form (walker:application-form flowstate formvalue)
  ())

(defclass setq-form (walker:setq-form flowstate formvalue)
  ((varnamespaces-upper :initarg :varnamespaces-upper :accessor form-varnamespaces-upper)
   (varnamespaces-lower :initarg :varnamespaces-lower :accessor form-varnamespaces-lower)))

(defclass alt-form (walker:special-form flowstate formvalue)
  ((test :initarg :test :accessor walker:form-test)
   (branches :initarg :branches :accessor form-branches :documentation "A list of branches that may be taken by the IF- or COND-form.")))

(defclass tagbody-form (walker:tagbody-form flowstate formvalue)
  ())

(defclass go-form (walker:go-form flowstate formvalue) ;FORMVALUE is needed by #'DEDUCE-FORWARD and #'DEDUCE-BACKWARD.
  ())

(defclass block-form (walker:block-form flowstate formvalue)
  ())

(defclass return-from-form (walker:return-from-form flowstate formvalue) ;FORMVALUE is needed by #'DEDUCE-FORWARD and #'DEDUCE-BACKWARD.
  ())

(defclass values-form (walker-plus:values-form flowstate formvalue)
  ())

(defclass multiple-value-bind-form (walker-plus:multiple-value-bind-form flowstate formvalue)
  ())

(defmethod print-object ((object selfevalobject) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S VAR:~S " (form-upper object) (form-lower object) (walker:selfevalobject-object object))))

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

(defun llist-types-string (binding)
  (let* ((llist (walker:form-llist binding))
	 (first-form (car (walker:form-body binding)))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form))
	 (arguments (append (walker:llist-required llist) (walker:llist-optional llist) (walker:llist-optional llist) (when (walker:llist-rest llist) (walker:llist-rest llist)) (walker:llist-key llist) (when (walker:llist-allow-other-keys llist) (list '&allow-other-keys)) (walker:llist-aux llist))))
    (with-output-to-string (stream)
      (loop for arg in arguments do
	   (print-unreadable-object (arg stream :type t :identity t)
	     (format stream "UPPER:~S LOWER:~S VAR:~S"
		     (namespace-lookup (walker:argument-var arg) body-upper)
		     (namespace-lookup (walker:argument-var arg) body-lower)
		     (walker:argument-var arg)))))))

(defmethod print-object ((object flet-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S BINDINGS:~A ~A" (form-upper object) (form-lower object)
	    (loop for binding in (walker:form-bindings object) collect
		 (format nil "(~S LLIST:(~A) ~A)" (walker:form-sym binding) (llist-types-string binding) (walker:format-body binding t nil)))
	    (walker:format-body object t nil))))

(defmethod print-object ((object application-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S" (form-upper object) (form-lower object) (walker:form-fun object))
    (loop for arg in (walker:form-arguments object) do
	 (format stream " ~S" arg))))

(defmethod print-object ((object setq-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S " (form-upper object) (form-lower object))
    (loop for var in (walker:form-vars object) for value in (walker:form-values object) do
	 (format stream " ~S ~S" var value))))

(defmethod print-object ((object alt-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S ~S" (form-upper object) (form-lower object) (walker:form-test object) (form-branches object))))

(defmethod print-object ((object tagbody-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~A" (form-upper object) (form-lower object) (walker:format-body object nil nil))))

(defmethod print-object ((object block-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~S ~A" (form-upper object) (form-lower object) (walker:form-blo object) (walker:format-body object nil nil))))

(defmethod print-object ((object return-from-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((value (walker:form-value object)))
      (format stream "UPPER:~S LOWER:~S ~S ~S" (form-upper value) (form-lower value) (walker:form-blo object) value))))

(defmethod print-object ((object values-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~A" (form-upper object) (form-lower object) (walker:format-body object nil nil))))

(defmethod print-object ((object multiple-value-bind-form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "UPPER:~S LOWER:~S ~A ~A ~A" (form-upper object) (form-lower object) (walker:form-vars object) (walker:form-values object) (walker:format-body object nil nil))))

;;; PREPARE-AST

(defun meet-form! (ast upper-results lower-results)
  "Meet FORM-UPPER and FORM-LOWER of AST with new RESULTS UPPER-RESULTS LOWER-RESULTS. Modifies AST."
  (let ((upper (form-upper ast)))
    ;; TODO: FIXME: are the following MEETs correct?
    (let ((upper-new (meet-results upper-results upper)))
      (assert (or (and (= (results-nvalues upper-new) -1) (not (null (result1 upper-new))))
		  (and (not (< (results-most upper-new) 0)) (loop for i below (results-most upper-new) always (not (null (resultn upper-new i)))))) () "Impossible result type ~S from meet of ~S and ~S for form~%~S" upper-new upper-results upper (annotate ast))
      (setf (form-upper ast) upper-new)
      (setf (form-lower ast) (meet-results lower-results upper)))))

(defmethod prepare-ast ((ast walker:selfevalobject) prev-upper prev-lower)
  (let ((type (etypecase (walker:selfevalobject-object ast)
		(fixnum 'fixnum)
		(float 'single-float)
		(null 'null)
		(boolean 'boolean) ;must be after NULL
		(symbol 'symbol)
		(t t))))
    (make-instance 'selfevalobject :object (walker:selfevalobject-object ast)
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		   :form-upper (make-results type) :form-lower (make-results type))))

(defun the-form (ast form)
  (list 'the
	(let* ((results (form-upper ast)))
	  (cond ((= (results-nvalues results) -1)
		 t)
		(t (let ((values (loop for i below (results-most results) collect (resultn results i))))
		     (assert (loop for type in values always (not (null type))))
		     (if (= (length values) 1)
			 (car values)
			 (cons 'values values))))))
	form))
	  
(defmethod annotate ((ast selfevalobject))
  (the-form ast (walker:selfevalobject-object ast)))

(defmethod prepare-ast ((ast walker:var) prev-upper prev-lower)
  (make-instance 'var :var ast
		 :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		 :form-upper (make-results (sym-declared-type ast)) :form-lower (make-results nil)))

(defmethod annotate ((ast var))
  (the-form ast (walker:nso-name (walker:form-var ast))))

(defmethod prepare-ast ((ast walker:fun) prev-upper prev-lower)
  (make-instance 'fun :fun ast
		 :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		 :form-upper (make-results 'function) :form-lower (make-results nil)))

(defmethod prepare-ast ((ast walker:var-binding) prev-upper prev-lower)
  (let ((value (if (null (walker:form-value ast))
		   (prepare-ast (make-instance 'walker:selfevalobject :object nil) prev-upper prev-lower)
		   (prepare-ast (walker:form-value ast) prev-upper prev-lower))))
    (make-instance 'var-binding :sym (walker:form-sym ast) :value value)))

(defmethod annotate ((ast var-binding))
  (list (walker:nso-name (walker:form-sym ast)) (annotate (walker:form-value ast))))

(defmethod prepare-ast ((ast walker:fun-binding) prev-upper prev-lower)
  ;; augment namespaces with the parameters of the ORDINARY-LLIST.
  (declare (optimize (debug 3)))
  (let ((body-upper prev-upper)
	(body-lower prev-lower)
	(llist (walker:form-llist ast))
	(ftype (sym-declared-type (walker:form-sym ast))))
    (loop for arg in (walker:llist-required llist) for i from 0 do
	 (let* ((var (walker:argument-var arg))
		(type (if (eq ftype 'function) t (elt (type-required ftype) i)))
		(new-arg (meet (sym-declared-type var) type)))
	   (assert (not (null new-arg)) () "Cannot meet types ~S and ~S for argument ~S in function ~S" (sym-declared-type var) type var (walker:form-sym ast))
	   (setf body-upper (augment-namespace var new-arg body-upper))
	   (setf body-lower (augment-namespace var nil body-lower))))
    (loop for arg in (walker:llist-optional llist) for i from 0 do
	 (let* ((var (walker:argument-var arg))
		(type (if (eq ftype 'function) t (elt (type-optional ftype) i)))
		(suppliedp (walker:argument-suppliedp arg))
		(new-arg (meet (sym-declared-type var) type)))
	   (assert (not (null new-arg)) () "Cannot meet types ~S and ~S for argument ~S in function ~S" (sym-declared-type var) type var (walker:form-sym ast))
	   (setf body-upper (augment-namespace var new-arg body-upper))
	   (setf body-lower (augment-namespace var nil body-lower))
	   (setf body-upper (augment-namespace suppliedp 'boolean body-upper))
	   (setf body-lower (augment-namespace suppliedp nil body-lower))))
    (when (walker:llist-rest llist)
      (let* ((var (walker:argument-var (walker:llist-rest llist)))
	     (type (if (eq ftype 'function) t (type-rest ftype)))
	     (new-arg (meet 'list (meet (sym-declared-type var) type))))
	(assert (not (null new-arg)) () "Cannot meet types ~S, ~S, and ~S for argument ~S in function ~S" 'list (sym-declared-type var) type var (walker:form-sym ast))
	(setf body-upper (augment-namespace var new-arg body-upper)) ;TODO: FIXME: probably the type should be LIST.
	(setf body-lower (augment-namespace var nil body-lower))))
    (loop for arg in (walker:llist-key llist) for i from 0 do
	 (let* ((var (walker:argument-var arg))
		(type (if (eq ftype 'function) t (elt (type-key ftype) i)))
		(suppliedp (walker:argument-suppliedp arg))
		(new-arg (meet (sym-declared-type var) type)))
	   (assert (not (null new-arg)) () "Cannot meet types ~S and ~S for argument ~S in function ~S" (sym-declared-type var) type var (walker:form-sym ast))
	   (setf body-upper (augment-namespace var new-arg body-upper))
	   (setf body-lower (augment-namespace var nil body-lower))
	   (setf body-upper (augment-namespace suppliedp 'boolean body-upper))
	   (setf body-lower (augment-namespace suppliedp nil body-lower))))
    (loop for arg in (walker:llist-aux llist) do
	 (let ((var (walker:argument-var arg)))
	   (setf body-upper (augment-namespace var (sym-declared-type var) body-upper))
	   (setf body-lower (augment-namespace var nil body-lower))))
    ;; the first form in the body must be a BLOCK-FORM, so that RETURN-FROM works.
    (let* ((body (if (null (walker:form-body ast))
		     (make-instance 'walker:selfevalobject :object nil)
		     (make-instance 'walker:block-form :blo (walker:form-blo ast) :body (walker:form-body ast))))
	   (parsed-body (prepare-ast body body-upper body-lower)))
      ;; meet values type with declared values type
      (meet-form! parsed-body
		  (if (eq ftype 'function) (make-results* :infinite t) (type-values ftype))
		  (if (eq ftype 'function) (make-results* :infinite nil) (type-values ftype)))
      (make-instance 'fun-binding :sym (walker:form-sym ast) :llist llist :declspecs (walker:form-declspecs ast) :documentation (walker:form-documentation ast) :body (list parsed-body)
		     :prevs-upper nil :prevs-lower nil))))

(defun fun-declaration (fun-binding)
  (let ((upper (form-prev-upper (car (walker:form-body fun-binding)))))
    (list 'function
	  (loop for arg in (walker:llist-required (walker:form-llist fun-binding)) collect
	       (let ((type (namespace-lookup (walker:argument-var arg) upper)))
		 (assert (not (null type)) () "Impossible type ~S for function argument ~S" type (walker:argument-var arg))
		 type))
	  (cadr (the-form (car (walker:form-body fun-binding)) nil)))))

(defun annotate-declaration (syms upper)
  (warn "#'ANNOTATE-DECLARATION is incorrect") ;;since it should take the #'JOIN over all NAMESPACES of a symbol, not just the first namespace"
  (let ((types (make-hash-table :test #'equal)))
    (loop for sym in syms do
	 (let ((type (namespace-lookup sym upper)))
	   (assert (not (null type)) () "Impossible type ~S for variable ~S" type sym)
	   (when (typep type 'fun-binding)
	     (setf type (fun-declaration type)))
	   (push sym (gethash type types nil))))
    (list* 'declare
	   (loop for type being the hash-key of types using (hash-value syms) collect
		(list* (cond
			 ((or (eq type 'function) (and (listp type) (eq (car type) 'function)))
			  'ftype)
			 (t
			  'type))
		       type
		       (loop for sym in syms collect (walker:nso-name sym)))))))

(defmethod annotate ((arg walker:required-argument))
  (walker:nso-name (walker:argument-var arg)))

(defmethod annotate ((llist walker:ordinary-llist))
  (loop for arg in (walker:llist-required llist) collect (annotate arg)))

(defmethod annotate ((ast fun-binding))
  (list* (walker:nso-name (walker:form-sym ast))
	 (annotate (walker:form-llist ast))
	 ;;(annotate-declaration (loop for arg in (walker:llist-required (walker:form-llist ast)) collect (walker:argument-var arg)) (form-prev-upper (car (walker:form-body ast))))
	 (loop for form in (walker:form-body ast) collect
	      (annotate form))))

(defun prepare-bindings-ast (ast prev-upper prev-lower)
  (declare (optimize (debug 3)))
  (flet ((make-bindings-parallel ()
	   (let* ((bindings (loop for binding in (walker:form-bindings ast) collect (prepare-ast binding prev-upper prev-lower)))
		  (body-prev-upper (let ((namespace prev-upper))
				     (loop for binding in bindings do
					  (let* ((type (sym-declared-type (walker:form-sym binding))))
					    (setf namespace (augment-namespace (walker:form-sym binding) (etypecase ast (walker:let-form type) (walker:flet-form binding)) namespace))))
				     namespace))
		  (body-prev-lower (let ((namespace prev-lower))
				     (loop for binding in bindings do
					  (setf namespace (augment-namespace (walker:form-sym binding) (etypecase ast (walker:let-form nil) (walker:flet-form binding)) namespace)))
				     namespace)))
	     (values bindings body-prev-upper body-prev-lower)))
	 (make-bindings-sequential ()
	   (let* ((body-prev-upper prev-upper)
		  (body-prev-lower prev-lower)
		  (bindings 
		   (loop for binding in (walker:form-bindings ast) collect
		;; this code sucks, because there is a circular dependency: we need the namespace augmented with the parsed binding, and for parsing the binding we need the augmented namespace.
		  (etypecase ast
		    (walker:labels-form
		     (let ((fake-binding (make-instance 'fun-binding :sym (walker:form-sym binding) :prevs-upper nil :prevs-lower nil)))
		       (setf body-prev-upper (augment-namespace (walker:form-sym binding) (etypecase ast (walker:labels-form fake-binding)) body-prev-upper))
		       (setf body-prev-lower (augment-namespace (walker:form-sym binding) (etypecase ast (walker:labels-form fake-binding)) body-prev-lower))
		       ;; transfer the parsed binding slots to the fake binding
		       (let ((prepared-binding (prepare-ast binding body-prev-upper body-prev-lower)))
			 (setf (walker:form-llist fake-binding) (walker:form-llist prepared-binding)
			       (walker:form-declspecs fake-binding) (walker:form-declspecs prepared-binding)
			       (walker:form-documentation fake-binding) (walker:form-documentation prepared-binding)
			       (walker:form-body fake-binding) (walker:form-body prepared-binding)))
		       fake-binding))
		    (walker:let*-form
		     (let ((bind (prepare-ast binding body-prev-upper body-prev-lower)))
		       (setf body-prev-upper (augment-namespace (walker:form-sym binding) (sym-declared-type (walker:form-sym binding)) body-prev-upper))
		       (setf body-prev-lower (augment-namespace (walker:form-sym binding) nil body-prev-lower))
		       bind))))))
	     (values bindings body-prev-upper body-prev-lower))))
    (multiple-value-bind (bindings body-prev-upper body-prev-lower)
	(etypecase ast
	  (walker:let-form (make-bindings-parallel))
	  (walker:let*-form (make-bindings-sequential))
	  (walker:flet-form (make-bindings-parallel))
	  (walker:labels-form (make-bindings-sequential)))
      (let* ((body-next-upper body-prev-upper)
	     (body-next-lower body-prev-lower)
	     (body-forms (if (null (walker:form-body ast))
			     (list (make-instance 'walker:selfevalobject :object nil))
			     (walker:form-body ast)))
	     (body (loop for form in body-forms collect
			(let ((ast (prepare-ast form body-next-upper body-next-lower)))
			  (setf body-next-upper (form-next-upper ast))
			  (setf body-next-lower (form-next-lower ast))
			  ast))))
	(make-instance (etypecase ast (walker:let-form 'let-form) (walker:let*-form 'let-form) (walker:flet-form 'flet-form) (walker:labels-form 'flet-form)) :bindings bindings :declspecs (walker:form-declspecs ast) :body body :parent ast ;parent is needed for #'ANNOTATE
		       :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace prev-lower)
		       :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))))

(defun annotate-bindings (ast)
  (the-form ast (list* (etypecase (walker:form-parent ast) (walker:let-form 'let) (walker:let*-form 'let*) (walker:flet-form 'flet) (walker:labels-form 'labels))
		       (loop for binding in (walker:form-bindings ast) collect (annotate binding))
		       ;;(annotate-declaration (loop for binding in (walker:form-bindings ast) collect (walker:form-sym binding)) (form-prev-upper (car (walker:form-body ast))))
		       (loop for form in (walker:form-body ast) collect (annotate form)))))

(defmethod prepare-ast ((ast walker:let-form) prev-upper prev-lower)
  (prepare-bindings-ast ast prev-upper prev-lower))

(defmethod prepare-ast ((ast walker:let*-form) prev-upper prev-lower)
  (prepare-bindings-ast ast prev-upper prev-lower))

(defmethod annotate ((ast let-form))
  (annotate-bindings ast))

(defmethod prepare-ast ((ast walker:flet-form) prev-upper prev-lower)
  (prepare-bindings-ast ast prev-upper prev-lower))

(defmethod prepare-ast ((ast walker:labels-form) prev-upper prev-lower)
  (prepare-bindings-ast ast prev-upper prev-lower))

(defmethod annotate ((ast flet-form))
  (annotate-bindings ast))

(defmethod prepare-ast ((ast walker:application-form) prev-upper prev-lower)
  (declare (optimize (debug 3)))
  ;; construct a LET-form that holds all the arguments
  (let* ((next-upper prev-upper)
	 (next-lower prev-lower)
	 (arguments (loop for arg in (walker:form-arguments ast) collect
			 (let ((ast (prepare-ast arg next-upper next-lower)))
			   (setf next-upper (form-next-upper ast))
			   (setf next-lower (form-next-lower ast))
			   ast)))
	 (form (make-instance 'application-form :fun (walker:form-fun ast) :arguments arguments
			      :prev-upper prev-upper :prev-lower prev-lower :next-upper next-upper :next-lower next-lower
			      :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))
    (when (namespace-boundp (walker:form-fun ast) prev-upper)
      (let* ((fun (walker:form-fun ast))
	     (binding (namespace-lookup fun prev-upper)))
	(push form (form-prevs-upper binding))))
    form))

(defmethod annotate ((ast application-form))
  (the-form ast (list* (walker:nso-name (walker:form-fun ast))
		       (loop for arg in (walker:form-arguments ast) collect
			    (annotate arg)))))

(defmethod prepare-ast ((ast walker:setq-form) prev-upper prev-lower)
  ;; translate the SETQ having multiple variable-value pairs into a sequence of SETQ-forms with a single variable-value pair.
  (let* ((next-upper prev-upper)
	 (next-lower prev-lower)
	 (values (walker:form-values ast))
	 (varnamespaces-upper nil)
	 (varnamespaces-lower nil)
	 (values (loop for var in (walker:form-vars ast) for value in values collect
		      (let ((value2 (prepare-ast value next-upper next-lower)))
			(setf next-upper (copy-namespace next-upper) next-lower (copy-namespace next-lower))
			(push next-upper varnamespaces-upper)
			(push next-lower varnamespaces-lower)
			value2))))
    (setf varnamespaces-upper (nreverse varnamespaces-upper))
    (setf varnamespaces-lower (nreverse varnamespaces-lower))
    (make-instance 'setq-form :vars (walker:form-vars ast) :values values
		   :varnamespaces-upper varnamespaces-upper :varnamespaces-lower varnamespaces-lower
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper (car (last varnamespaces-upper)) :next-lower (car (last varnamespaces-lower))
		   :form-upper (make-results t) :form-lower (make-results nil))))

(defmethod annotate ((ast setq-form))
  (let ((l nil))
    (loop for var in (walker:form-vars ast) for value in (walker:form-values ast) do
	 (push (walker:nso-name var) l)
	 (push (annotate value) l))
    (the-form ast (cons 'setq (nreverse l)))))

(defmethod prepare-ast ((ast walker:if-form) prev-upper prev-lower)
  (let* ((test-form (prepare-ast (walker:form-test ast) prev-upper prev-lower))
	 (upper (form-next-upper test-form))
	 (lower (form-next-lower test-form))
	 (then-branch (prepare-ast (walker:form-then ast) (copy-namespace upper) (copy-namespace lower))) ;the branches need to have isolated namespaces.
	 (else-branch (if (walker:form-else ast)
			  (prepare-ast (walker:form-else ast) (copy-namespace upper) (copy-namespace lower))
			  (prepare-ast (make-instance 'walker:selfevalobject :object nil) (copy-namespace upper) (copy-namespace lower))))
	 (branches (list then-branch else-branch)))
    (make-instance 'alt-form :branches branches :test test-form
		   :prev-upper upper :prev-lower lower :next-upper (copy-namespace upper) :next-lower (copy-namespace lower) ;variables local to a branch are not visible after the IF, but the branches could contain a SETQ, whose change we need to carry.
		   :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))

(defmethod annotate ((ast alt-form))
  (assert (= (length (form-branches ast)) 2))
  (the-form ast (list* 'if (annotate (walker:form-test ast)) (loop for branch in (form-branches ast) collect (annotate branch)))))

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
		     :form-upper (make-results 'null) :form-lower (make-results 'null))))) ;TODO: FIXME: implement flow control analysis to determine whether a TAGBODY can exit via its last form, and in this case, set the result type of the TAGBODY to (VALUES NULL), but unless we can be sure that the TAGBODY exits we may not set the result type to NULL, because the TAGBODY can be the last form of a BLOCK form, and the BLOCK form will meet its result types determined from RETURN-FROM-forms with the NULL result type of its last form, which will likely result in a NULL type.

(defmethod annotate ((ast tag))
  (walker:nso-name (walker:form-tag ast)))

(defmethod annotate ((ast tagbody-form))
  (the-form ast (list* 'tagbody
		       (loop for form in (walker:form-body ast) collect (annotate form)))))

(defmethod prepare-ast ((ast walker:go-form) prev-upper prev-lower)
  (let ((tag (namespace-lookup (walker:form-tag ast) prev-upper)))
    (unless (find prev-upper (form-prevs-upper tag))
      (push prev-upper (form-prevs-upper tag))
      (push prev-lower (form-prevs-lower tag)))
    (make-instance 'go-form :tag (walker:form-tag ast)
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		   :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))

(defmethod annotate ((ast go-form))
  (the-form ast (list 'go (walker:nso-name (walker:form-tag ast)))))

(defmethod prepare-ast ((ast walker:block-form) prev-upper prev-lower)
  (let* ((blo (make-instance 'blo :blo (walker:form-blo ast) :prev-upper prev-upper :prev-lower prev-lower :branches nil :nexts-upper nil :nexts-lower nil))
	 (next-upper (augment-namespace (walker:form-blo ast) blo prev-upper))
	 (next-lower (augment-namespace (walker:form-blo ast) blo prev-lower))
	 (body-forms (if (null (walker:form-body ast))
			 (make-instance 'walker:selfevalobject :object nil)
			 (walker:form-body ast)))
	 (body (loop for form in body-forms collect
		    (let ((ast (prepare-ast form next-upper next-lower)))
		      (setf next-upper (form-next-upper ast))
		      (setf next-lower (form-next-lower ast))
		      ast)))
	 (current (make-instance 'block-form :blo (walker:form-blo ast) :body body
				 :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace prev-lower) ;cannot use namespace of last body form because it is annotated with blo
				 :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))
    (setf (walker:nso-definition blo) current)
    (push (car (last body)) (form-branches blo))
    (push next-upper (form-nexts-upper blo))
    (push next-lower (form-nexts-lower blo))
    current))

(defmethod annotate ((ast block-form))
  (the-form ast (list* 'block (walker:nso-name (walker:form-blo ast))
		       (loop for form in (walker:form-body ast) collect (annotate form)))))

(defmethod prepare-ast ((ast walker:return-from-form) prev-upper prev-lower)
  (let* ((blo (namespace-lookup (walker:form-blo ast) prev-upper))
	 (value (prepare-ast (walker:form-value ast) prev-upper prev-lower))
	 (value-next-upper (form-next-upper value))
	 (value-next-lower (form-next-lower value))
	 (current (make-instance 'return-from-form :blo (walker:form-blo ast) :value value
				 :prev-upper prev-upper :prev-lower prev-lower :next-upper value-next-upper :next-lower value-next-lower
				 :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))
    (push current (form-branches blo))
    (push value-next-upper (form-nexts-upper blo))
    (push value-next-lower (form-nexts-lower blo))
    current))

(defmethod annotate ((ast return-from-form))
  (the-form ast (list 'return-from (walker:nso-name (walker:form-blo ast)) (annotate (walker:form-value ast)))))

(defmethod prepare-ast ((ast walker-plus:values-form) prev-upper prev-lower)
  (let* ((body (walker:form-body ast))
	 (next-upper prev-upper)
	 (next-lower prev-lower)
	 (parsed-body (loop for form in body collect
			   (let ((ast (prepare-ast form next-upper next-lower)))
			     (setf next-upper (form-next-upper ast))
			     (setf next-lower (form-next-lower ast))
			     ast))))
    (make-instance 'values-form :body parsed-body
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper next-upper :next-lower next-lower
		   :form-upper (apply #'make-results (loop repeat (length body) collect t)) :form-lower (apply #'make-results (loop repeat (length body) collect nil)))))

(defmethod annotate ((ast values-form))
  (the-form ast (list* 'values
		       (loop for value in (walker:form-body ast) collect (annotate value)))))

(defmethod prepare-ast ((ast walker-plus:multiple-value-bind-form) prev-upper prev-lower)
  (let* ((next-upper prev-upper)
	 (next-lower prev-lower)
	 (vars (loop for var in (walker:form-vars ast) collect
		    (progn
		      (setf next-upper (augment-namespace var (sym-declared-type var) next-upper))
		      (setf next-lower (augment-namespace var nil next-lower))
		      var)))
	 (values (prepare-ast (walker:form-values ast) prev-upper prev-lower))
	 (body-forms (if (null (walker:form-body ast))
			 (list (make-instance 'walker:selfevalobject :object nil))
			 (walker:form-body ast)))
	 (body (loop for form in body-forms collect
		    (let ((ast (prepare-ast form next-upper next-lower)))
		      (setf next-upper (form-next-upper ast))
		      (setf next-lower (form-next-lower ast))
		      ast))))
    (make-instance 'multiple-value-bind-form :vars vars :values values :body body
		   :prev-upper prev-upper :prev-lower prev-lower :next-upper (copy-namespace prev-upper) :next-lower (copy-namespace next-lower)
		   :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil))))

(defmethod annotate ((ast multiple-value-bind-form))
  (the-form ast
	    (list* 'multiple-value-bind
		   (loop for var in (walker:form-vars ast) collect (walker:nso-name var))
		   (annotate (walker:form-values ast))
		   ;;(annotate-declaration (walker:form-vars ast) (car (walker:form-body ast)))
		   (loop for form in (walker:form-body ast) collect (annotate form)))))

(defclass macroapplication-form (walker:macroapplication-form flowstate formvalue)
  ())

;; TODO: remove this when assert is implemented, and/or when macro-to-normal-code functions are implemented.
(defmethod prepare-ast ((ast walker:macroapplication-form) prev-upper prev-lower)
  (ecase (walker:nso-name (walker:form-fun ast))
    ((prind assert)
     (make-instance 'macroapplication-form :fun (walker:form-fun ast) :arguments (walker:form-arguments ast) :lexicalnamespace nil :freenamespace nil
		    :prev-upper prev-upper :prev-lower prev-lower :next-upper prev-upper :next-lower prev-lower
		    :form-upper (make-results* :infinite t) :form-lower (make-results* :infinite nil)))))

(defmethod annotate ((ast macroapplication-form))
  (list* (walker:nso-name (walker:form-fun ast)) (walker:form-arguments ast)))

(defmethod deduce-forward ((ast macroapplication-form))
  )

(defmethod deduce-backward ((ast macroapplication-form))
  )

;;; DEDUCE-FORWARD

(defun meet-namespaces! (a-upper a-lower b-upper b-lower)
  "Carry over changes from B to A, and thus modifies A."
  (declare (optimize (debug 3)))
  (map-namespace (lambda (a-upper a-lower b-upper b-lower)
		   (assert (and (not (eq a-upper a-lower)) (not (eq b-upper b-lower))))
		   (assert (and (equal (car a-upper) (car b-upper)) (equal (car a-lower) (car b-lower)) (equal (car a-upper) (car a-lower))) () "Mismatching variable names ~S, ~S, ~S, ~S" (car a-upper) (car a-lower) (car b-upper) (car b-lower))
		   (unless (or (typep (cdr a-upper) 'fun-binding) (typep (cdr a-upper) 'blo))
		     (let* ((upper (cdr a-upper)))
		       (let ((new-upper (meet (cdr b-upper) upper)))
			 (assert (not (null new-upper)) () "Cannot meet types ~S and ~S for variable ~S" (cdr b-upper) upper (car a-upper))
			 (setf (cdr a-upper) new-upper))
		       (setf (cdr a-lower) (meet (cdr b-lower) upper)))))
		 a-upper a-lower b-upper b-lower))

(defun meet-namespace-prev! (form-new form-old)
  (declare (optimize (debug 3)))
  (meet-namespaces! (form-prev-upper form-new) (form-prev-lower form-new) (form-prev-upper form-old) (form-prev-lower form-old)))

(defun meet-namespace-next! (form-new form-old)
  (declare (optimize (debug 3)))
  (meet-namespaces! (form-next-upper form-new) (form-next-lower form-new) (form-next-upper form-old) (form-next-lower form-old)))

(defun meet-namespace-prev-next! (form-new form-old)
  (declare (optimize (debug 3)))
  (meet-namespaces! (form-prev-upper form-new) (form-prev-lower form-new) (form-next-upper form-old) (form-next-lower form-old)))

(defun meet-namespace-next-prev! (form-new form-old)
  (declare (optimize (debug 3)))
  (meet-namespaces! (form-next-upper form-new) (form-next-lower form-new) (form-prev-upper form-old) (form-prev-lower form-old)))

(defun join-namespaces! (a-upper a-lower bs-upper bs-lower)
  "Backward propagation rule for split of A into B and C:
upper(A) = (upper(B) join upper(C)) meet upper(A)
lower(A) = (lower(B) join lower(C)) meet upper(A).
Modifies A."
  (declare (optimize (debug 3)))
  (let ((after-upper (lambda (a-upper &rest branches-upper)
		       (unless (or (typep (cdr a-upper) 'fun-binding) (typep (cdr a-upper) 'blo))
			 (let ((new-upper (meet (join-typelist (mapcar #'cdr branches-upper) +builtin-typehash+) (cdr a-upper))))
			   (assert (not (null new-upper)) () "Cannot meet types from branches ~S and ~S for variable ~S" (mapcar #'cdr branches-upper) (cdr a-upper) (car a-upper))
			   (setf (cdr a-upper) new-upper)))))
	(after-lower (lambda (a-upper a-lower &rest branches-lower)
		       (unless (or (typep (cdr a-lower) 'fun-binding) (typep (cdr a-upper) 'blo))
			 (setf (cdr a-lower) (meet (join-typelist (mapcar #'cdr branches-lower) +builtin-typehash+) (cdr a-upper)))))))
    (apply #'map-namespace after-upper a-upper bs-upper)
    (apply #'map-namespace after-lower a-upper a-lower bs-lower)))

(defun join-forms! (ast branches)
  "Modifies AST."
  (meet-form! ast
	      (reduce #'join-results (mapcar #'form-upper (cdr branches)) :initial-value (form-upper (car branches)))
	      (reduce #'join-results (mapcar #'form-lower (cdr branches)) :initial-value (form-lower (car branches)))))

(defmethod deduce-forward ((ast selfevalobject))
  ;; nothing to do, #'PREPARE-AST already initialized FORM-UPPER and FORM-LOWER.
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object, since there cannot be a type change inside a SELFEVALOBJECT form.
  )

(defmethod deduce-forward ((ast var))
  (declare (optimize (debug 3)))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  (let* ((upper (namespace-lookup (walker:form-var ast) (form-prev-upper ast)))
	 (lower (namespace-lookup (walker:form-var ast) (form-prev-lower ast))))
    (assert (not (null upper)) () "Impossible type ~S for variable ~S" upper ast)
    (setf (form-upper ast) (make-results upper))
    (setf (form-lower ast) (make-results lower))))

(defmethod deduce-forward ((ast let-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast)))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form)))
    (meet-namespace-prev! first-form ast)
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
      (meet-form! ast (form-upper last-form) (form-lower last-form))
      (meet-namespace-next! ast last-form))))

(defmethod deduce-forward ((ast fun-binding))
  (declare (optimize (debug 3)))
  (let* ((llist (walker:form-llist ast))
	 (first-form (car (walker:form-body ast)))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form))
	 ;; TODO: implement &OPTIONAL, &REST, and &KEY arguments
	 (calls-arguments-upper (mapcar (lambda (application-form)
					  (mapcar (lambda (arg) (result1 (form-upper arg)))
						  (walker:form-arguments application-form)))
					(form-prevs-upper ast)))
	 (calls-arguments-lower (mapcar (lambda (application-form)
					  (mapcar (lambda (arg) (result1 (form-lower arg)))
						  (walker:form-arguments application-form)))
					(form-prevs-upper ast)))) ;note that here, (FORM-PREVS-LOWER AST)==(FORM-PREVS-UPPER AST)
    ;; CALLS-ARGUMENTS is now a list (with calls) of a list (with arguments) of types
    (loop for arg in (walker:llist-required llist) for i from 0 do
	 (let* ((calls-argument-upper (loop for types in calls-arguments-upper collect (elt types i)))
		(calls-argument-lower (loop for types in calls-arguments-lower collect (elt types i)))
		(old-upper (namespace-lookup (walker:argument-var arg) body-upper))
		(argument-upper (if (null calls-argument-upper) t (join-typelist calls-argument-upper +builtin-typehash+)))
		(argument-lower (if (null calls-argument-upper) nil (join-typelist calls-argument-lower +builtin-typehash+)))
		(new-upper (meet argument-upper old-upper)))
	   (assert (not (null new-upper)) () "Impossible type for argument ~S: cannot meet types ~S and ~S" arg argument-upper old-upper)
	   (setf (namespace-lookup (walker:argument-var arg) body-upper) new-upper)
	   (setf (namespace-lookup (walker:argument-var arg) body-lower) (meet argument-lower old-upper))))
    (loop for form in (walker:form-body ast) do
	 (deduce-forward form))
    ;; push result types to call sites
    (let ((last-form (car (last (walker:form-body ast)))))
      (loop for application-form in (form-prevs-upper ast) do
	   (meet-form! application-form (form-upper last-form) (form-lower last-form))))))

(defmethod deduce-forward ((ast flet-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast))))
    (meet-namespace-prev! first-form ast)
    (flet ((process-body ()
	     (loop for form in (butlast (walker:form-body ast)) do
		  (deduce-forward form))
	     (let ((last-form (car (last (walker:form-body ast)))))
	       (deduce-forward last-form)
	       (meet-form! ast (form-upper last-form) (form-lower last-form))
	       (meet-namespace-next! ast last-form))))
      ;; deduce-forward the body, to determine argument types to functions.
      (process-body)
      ;; deduce-forward the functions.
      (loop for binding in (walker:form-bindings ast) do
	   (deduce-forward binding))
      ;; deduce-forward the body again, to propagate the updated result types of the functions.
      (process-body))))

(defmethod deduce-forward ((ast application-form))
  "upper(z) = t-function(f,0,upper(x),upper(y)) meet upper(z)
lower(z) = t-function(f,0,lower(x),lower(y)) meet upper(z))"
  (declare (optimize (debug 3)))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  (loop for arg in (walker:form-arguments ast) do
       (deduce-forward arg))
  (let* ((fun (walker:form-fun ast))
	 (args (walker:form-arguments ast))
	 (arg-types-upper (loop for arg in args collect (result1 (form-upper arg))))
	 (arg-types-lower (loop for arg in args collect (result1 (form-lower arg)))))
    (flet ((determine-fun-result ()
	     (if (namespace-boundp fun (form-prev-upper ast))
		 (values (form-upper ast) (form-lower ast)) ;the computation is done by #'DEDUCE-FORWARD on FUN-BINDING.
		 (let ((fun-result-upper (fun-result-lookup-upper fun arg-types-upper))
		       (fun-result-lower (fun-result-lookup-lower fun arg-types-lower)))
		   (values fun-result-upper fun-result-lower)))))
      (multiple-value-bind (fun-result-upper fun-result-lower) (determine-fun-result)
	(when (null (result1 fun-result-upper))
	  (error "Function application ~S yields impossible type ~S." ast fun-result-upper))
	(meet-form! ast fun-result-upper fun-result-lower)
	(when (null (form-upper ast))
	  (error "Meet in function application ~S between types ~S and ~S yields impossible type." ast fun-result-upper (form-upper ast)))))))

(defmethod deduce-forward ((ast setq-form))
  (declare (optimize (debug 3)))
  (let ((old-form (car (walker:form-values ast)))
	(form-upper 'null)
	(form-lower 'null))
    (loop for var in (walker:form-vars ast) for value in (walker:form-values ast) for varnamespace-upper in (form-varnamespaces-upper ast) for varnamespace-lower in (form-varnamespaces-lower ast) do
	 (meet-namespaces! varnamespace-upper varnamespace-lower (form-next-upper old-form) (form-next-lower old-form))
	 (deduce-forward value)
	 (let* ((decl (sym-declared-type var))
		(new-upper (meet (result1 (form-upper value)) decl))
		(new-lower (meet (result1 (form-lower value)) decl)))
	   (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var (result1 (form-upper value)) decl)
	   (setf old-form value)
	   (setf (namespace-lookup var varnamespace-upper) new-upper)
	   (setf (namespace-lookup var varnamespace-lower) new-lower)
	   (setf form-upper new-upper form-lower new-lower)))
    (setf (form-upper ast) (make-results form-upper) (form-lower ast) (make-results form-lower))))

(defmethod deduce-forward ((ast alt-form))
  (declare (optimize (debug 3)))
  (let ((test (walker:form-test ast))
	(branches (form-branches ast)))
    (deduce-forward test)
    (loop for branch in branches do
	 (meet-namespace-prev-next! branch test)
	 (deduce-forward branch))
    (join-namespaces! (form-next-upper ast) (form-next-lower ast) (mapcar #'form-next-upper branches) (mapcar #'form-next-lower branches))
    (join-forms! ast branches)))

(defmethod deduce-forward ((ast tagbody-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast)))
	 (last-form (car (last (walker:form-body ast)))))
    (let ((body-upper (if (typep first-form 'tag) (car (last (form-prevs-upper first-form))) (form-prev-upper first-form)))
	  (body-lower (if (typep first-form 'tag) (car (last (form-prevs-lower first-form))) (form-prev-lower first-form))))
      (meet-namespaces! body-upper body-lower (form-prev-upper ast) (form-prev-lower ast)))
    (loop for form in (walker:form-body ast) do
	 (deduce-forward form))
    (unless (null last-form)
      (meet-namespace-next! ast last-form))
    ;; no need to update FORM-UPPER or FORM-LOWER, since TAGBODY always returns NIL.
    ))

(defmethod deduce-forward ((ast tag))
  (declare (optimize (debug 3)))
  (join-namespaces! (form-next-upper ast) (form-next-lower ast) (form-prevs-upper ast) (form-prevs-lower ast)))

(defmethod deduce-forward ((ast go-form))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  )

(defmethod deduce-forward ((ast block-form))
  (declare (optimize (debug 3)))
  (loop for form in (walker:form-body ast) do
       (deduce-forward form))
  (meet-namespace-next! ast (car (last (walker:form-body ast))))
  (let* ((last-form (car (last (walker:form-body ast))))
	 (blo (namespace-lookup (walker:form-blo ast) (form-prev-upper last-form))))
    (join-forms! ast (form-branches blo))
    (join-namespaces! (form-next-upper ast) (form-next-lower ast) (form-nexts-upper blo) (form-nexts-lower blo))))

(defmethod deduce-forward ((ast return-from-form))
  (let* (;;(blo (namespace-lookup (walker:form-blo ast) (form-prev-upper ast)))
	 (value (walker:form-value ast)))
    (deduce-forward value)
    (meet-form! ast (form-upper value) (form-lower value))))


(defmethod deduce-forward ((ast values-form))
  (let ((body (walker:form-body ast)))
    (loop for i from 0 for form in body do
	 (deduce-forward form))
    (let ((new-upper (apply #'make-results (loop for form in body collect (result1 (form-upper form)))))
	  (new-lower (apply #'make-results (loop for form in body collect (result1 (form-lower form))))))
      (meet-form! ast new-upper new-lower))))

(defmethod deduce-forward ((ast multiple-value-bind-form))
  (declare (optimize (debug 3)))
  (let* ((body (walker:form-body ast))
	 (first-form (car body))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form))
	 (values (walker:form-values ast)))
    (deduce-forward values)
    (meet-namespace-prev! first-form ast)
    (loop for i from 0 for var in (walker:form-vars ast) do
	 (let* ((upper (namespace-lookup var body-upper))
		(val-upper (if (< i (results-most (form-upper values)))
			       (resultn (form-upper values) i)
			       'null))
		(val-lower (if (< i (results-most (form-upper values)))
			       (resultn (form-lower values) i)
			       'null))
		(new-upper (meet val-upper upper)))
	   (assert (not (null new-upper)) () "Cannot meet types ~S and ~S for variable ~S" val-upper upper var)
	   (setf (namespace-lookup var body-upper) new-upper)
	   (setf (namespace-lookup var body-lower) (meet val-lower upper))))
    (loop for form in (butlast body) do
	 (deduce-forward form))
    (let ((last-form (car (last body))))
      (deduce-forward last-form)
      (meet-form! ast (form-upper last-form) (form-lower last-form))
      (meet-namespace-next! ast last-form))))

(defun test-forward ()
  (labels ((prepare (form)
	     (let* ((ast (walker:parse-with-namespace form :free-namespace (walker:make-free-namespace) :parser (walker:make-parser (list (cons #'walker-plus:parse-p #'walker-plus:parse) (cons #'walker:parse-p #'walker:parse)))))
		    (ast (prepare-ast ast nil nil)))
	       ast))
	   (assert-result (form upper)
	     (declare (optimize (debug 3)))
	     (let* ((ast (prepare form)))
	       (deduce-forward ast)
	       (let* ((results (form-upper ast))
		      (finite (results-finite results))
		      (results (loop for i below (length upper) collect
				    (if (< i (length finite)) (elt finite i) (results-infinite results)))))
		 (assert (equal results upper) () "failed assertion for form ~S:~%results:~S wanted:~S" form results upper)))))
    (macrolet ((assert-error (form)
		 (let ((ast-sym (gensym "AST")) (form-sym (gensym "FORM")))
		   `(let* ((,form-sym ,form)
			   (,ast-sym (prepare ,form-sym)))
		      (handler-case (progn (deduce-forward ,ast-sym) (assert nil () "failed (ASSERT-ERROR ~S)" ,form-sym))
			;; TODO: replace T with something like TYPE-ERROR.
			(t () t))))))
      (assert-result '1 '(fixnum))
      (assert-result '(+ 1 2) '(fixnum))
      (assert-result '(+ 1 2.0) '(single-float))
      (assert-result '(let ((a 1) (b 2)) (+ a b)) '(fixnum))
      (assert-error '(let ((a 1) (b 2)) (declare (type single-float a)) (+ a b)))
      (assert-result '(let ((a 1) (b 2)) (declare (type number a)) (+ a b)) '(fixnum))
      (assert-result '(let ((a 1.0)) (setq a 1) a) '(fixnum))
      (assert-result '(let ((a 1.0)) (setq a 1) (+ a 2)) '(fixnum))
      (assert-result '(let ((a 1) (b 2)) (setq a 1.0 b a) b) '(single-float))
      (assert-error '(let ((a 1.0)) (declare (type single-float a)) (setq a 1) (+ a 2)))
      (assert-result '(if 1 2 3) '(fixnum))
      (assert-result '(if 1 2 nil) '(t))
      (assert-result '(if 1 2) '(t))
      (assert-result '(let ((a 1)) (if 1 (setq a 2)) a) '(fixnum))
      (assert-result '(let ((a nil)) (if 1 (setq a 1) (setq a 2.0)) a) '(number))
      (assert-result '(let ((a nil)) (if 1 (setq a 1) (setq a nil)) a) '(t))
      (assert-result '(let ((a nil)) (let () (setq a 1)) a) '(fixnum))
      (assert-result '(tagbody a (go b) b) '(null))
      (assert-result '(let ((a 10)) (tagbody s (setq a (1- a)) (if (<= a 0) (go e)) (go s) e) a) '(fixnum))
      (assert-result '(let ((a nil)) (tagbody (if (null a) (setq a 10)) s (setq a (1- a)) (if (<= a 0) (go e)) (go s) e) a) '(number))
      (assert-result '(block bla (let ((a 1)) (return-from bla (setq a nil)) a)) '(null))
      (assert-result '(let ((a 1)) (block f1 (return-from f1 a))) '(fixnum))
      (assert-result '(block b (if 1 (return-from b 1)) (if 2 (return-from b 1.0) 0)) '(number))
      (assert-result '(flet ((f1 (a) (let ((b a)) b))) nil) '(null))
      (assert-result '(flet ((f1 (a) a)) (f1 1)) '(fixnum))
      (assert-result '(flet ((f1 (a) a)) (f1 1) (f1 1.1)) '(number))
      (assert-result '(flet ((f1 (a) (return-from f1 a))) (f1 1)) '(fixnum))
      (assert-result '(flet ((f1 (a) (return-from f1 a))) (f1 1) (f1 1.1)) '(number))
      ;;(assert-result '(labels ((f1 (a) (if (= 0 a) (return-from f1 0) (f1 (1- a))))) (f1 1)) '(fixnum))
      ;;(assert-result '(labels ((f1 (a) (if (= 0 a) 0 (f1 (1- a))))) (f1 1)) '(fixnum))
      (assert-result '(values 1 1.0) '(fixnum single-float))
      (assert-result '(multiple-value-bind (a b) (values 1 1.0) b) '(single-float))
      (assert-result '(multiple-value-bind (a b) 1 (values a b)) '(fixnum null))
      )))
(test-forward)

;;; DEDUCE-BACKWARD

(defmethod deduce-backward ((ast selfevalobject))
  ;; nothing to do, PREPARE-AST already initialized FORM-UPPER and FORM-LOWER.
  ;; no need to carry over NEXT to PREV; they are the same object, since there cannot be a type change inside a SELFEVALOBJECT form.
  )

(defmethod deduce-backward ((ast var))
  (declare (optimize (debug 3)))
  ;; no need to carry over NEXT to PREV; they are the same object, since there cannot be a type change inside a SELFEVALOBJECT form.
  (let* ((var (walker:form-var ast))
	 (prev-upper (form-prev-upper ast))
	 (prev-lower (form-prev-lower ast))
	 (var-upper (namespace-lookup var prev-upper))
	 (new-upper (meet (result1 (form-upper ast)) var-upper)))
    (assert (not (null new-upper)) () "Cannot meet types ~S and ~S for variable ~S" (result1 (form-upper ast)) var-upper var)
    (setf (namespace-lookup var prev-upper) new-upper)
    (setf (namespace-lookup var prev-lower) (meet (result1 (form-lower ast)) var-upper))))

(defmethod deduce-backward ((ast let-form))
  (declare (optimize (debug 3)))
  (let ((last-form (car (last (walker:form-body ast)))))
    (meet-namespace-next! last-form ast)
    (meet-form! last-form (form-upper ast) (form-lower ast))
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
	   (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var var-upper value-upper)
	   (setf (result1 (form-upper value)) new-upper)
	   (setf (result1 (form-lower value)) new-lower))
	 (deduce-backward (walker:form-value binding)))
    (meet-namespace-prev! ast first-form)))

(defmethod deduce-backward ((ast fun-binding))
  (declare (optimize (debug 3)))
  (let* ((llist (walker:form-llist ast))
	 (first-form (car (walker:form-body ast)))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form)))
    ;; pull result types from call sites
    (let* ((last-form (car (last (walker:form-body ast))))
	   (application-forms (form-prevs-upper ast)))
      (unless (null application-forms)
	(join-forms! last-form application-forms)))
    (loop for form in (reverse (walker:form-body ast)) do
	 (deduce-backward form))
    ;; TODO: implement &OPTIONAL, &REST, and &KEY arguments
    (loop for application-form in (form-prevs-upper ast) do
	 (loop for arg in (walker:llist-required llist) for apparg in (walker:form-arguments application-form) do
	      (let* ((var (walker:argument-var arg))
		     (var-upper (namespace-lookup var body-upper))
		     (arg-upper (result1 (form-upper apparg)))
		     (type (meet var-upper arg-upper)))
		(assert (not (null type)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" arg var-upper arg-upper)
		(setf (result1 (form-upper apparg)) type)
		(setf (result1 (form-lower apparg)) (meet (namespace-lookup var body-lower) var-upper)))))))

(defmethod deduce-backward ((ast flet-form))
  (declare (optimize (debug 3)))
  (flet ((process-body ()
	   (let ((last-form (car (last (walker:form-body ast)))))
	     (meet-namespace-next! last-form ast)
	     (meet-form! last-form (form-upper ast) (form-lower ast))
	     (deduce-backward last-form))
	   (loop for form in (reverse (butlast (walker:form-body ast))) do
		(deduce-backward form))))
    ;; deduce-backward the body, to determine result types of functions.
    (process-body)
    ;; deduce-backward the functions.
    (loop for binding in (walker:form-bindings ast) do
	 (deduce-backward binding))
    ;; deduce-backward the body again, to propagate the updated argument types of the functions.
    (process-body))
  (meet-namespace-prev! ast (car (walker:form-body ast))))

(defmethod deduce-backward ((ast application-form))
  (declare (optimize (debug 3)))
  (let* ((fun (walker:form-fun ast))
	 (results-upper (form-upper ast))
	 (args (walker:form-arguments ast))
	 (arg-types-upper (loop for arg in args collect (result1 (form-upper arg))))
	 )
    (flet ((determine-fun-arguments ()
	     (if (namespace-boundp fun (form-prev-upper ast))
		 arg-types-upper ;the computation is done by #'DEDUCE-BACKWARD on FUN-APPLICATION.
		 (let* ((fun-args-upper (fun-arguments-lookup fun results-upper))
			;;(fun-args-lower (fun-arguments-lookup fun results-lower))
			(met-arguments (meet-arguments fun-args-upper arg-types-upper)))
		   met-arguments))))
      (loop for arg in args for type in (determine-fun-arguments) do
	   (setf (result1 (form-upper arg)) type))
      ;; "upper(z) = upper(zbefore)" ? What does it mean?
      )
    (loop for arg in (reverse args) do (deduce-backward arg))))

(defmethod deduce-backward ((ast setq-form))
  (declare (optimize (debug 3)))
  (let ((old-form ast))
    (let* ((last-value (car (last (walker:form-values ast)))))
      (meet-form! last-value (form-upper ast) (form-lower ast)))
    (loop for var in (reverse (walker:form-vars ast)) for value in (reverse (walker:form-values ast)) for varnamespace-upper in (reverse (form-varnamespaces-upper ast)) for varnamespace-lower in (reverse (form-varnamespaces-lower ast)) do
	 (meet-namespaces! (form-next-upper old-form) (form-next-lower old-form) varnamespace-upper varnamespace-lower)
	 (deduce-backward value)
	 (let* ((decl (sym-declared-type var))
		(new-upper (meet (result1 (form-upper value)) decl))
		(new-lower (meet (result1 (form-lower value)) decl)))
	   (assert (not (null new-upper)) () "Impossible type for variable ~S: cannot meet types ~S and ~S" var (result1 (form-upper value)) decl)
	   (setf (namespace-lookup var varnamespace-upper) new-upper) ;we use the NEXT namespace to store new values
	   (setf (namespace-lookup var varnamespace-lower) new-lower)
	   (setf old-form value)))))

(defmethod deduce-backward ((ast alt-form))
  (declare (optimize (debug 3)))
  (let ((test (walker:form-test ast))
	(branches (form-branches ast)))
    (loop for branch in branches do
	 (meet-namespace-next! branch ast)
	 (meet-form! branch (form-upper ast) (form-lower ast))
	 (deduce-backward branch))
    (join-namespaces! (form-next-upper test) (form-next-lower test) (mapcar #'form-prev-upper branches) (mapcar #'form-prev-lower branches))
    (deduce-backward test)))

(defmethod deduce-backward ((ast tagbody-form))
  (declare (optimize (debug 3)))
  (let* ((first-form (car (walker:form-body ast)))
	 (last-form (car (last (walker:form-body ast)))))
    ;; no need to update FORM-UPPER or FORM-LOWER, since TAGBODY always returns NIL.
    (unless (null last-form)
      (meet-namespace-next! last-form ast))
    (loop for form in (walker:form-body ast) do
	 (deduce-backward form))
    (let ((body-upper (if (typep first-form 'tag) (car (last (form-prevs-upper first-form))) (form-prev-upper first-form)))
	  (body-lower (if (typep first-form 'tag) (car (last (form-prevs-lower first-form))) (form-prev-lower first-form))))
      (unless (null first-form)
	(meet-namespaces! (form-prev-upper ast) (form-prev-lower ast) body-upper body-lower)))))

(defmethod deduce-backward ((ast tag))
  (declare (optimize (debug 3)))
  (let ((prevs-upper (form-prevs-upper ast))
	(prevs-lower (form-prevs-lower ast)))
    (loop for prev-upper in prevs-upper for prev-lower in prevs-lower do
	 (meet-namespaces! prev-upper prev-lower (form-next-upper ast) (form-next-lower ast)))))

(defmethod deduce-backward ((ast go-form))
  ;; no need to carry over PREV to NEXT; #'PREPARE-AST defined them the same object.
  )

(defmethod deduce-backward ((ast block-form))
  (declare (optimize (debug 3)))
  (let* ((last-form (car (last (walker:form-body ast))))
	 (blo (namespace-lookup (walker:form-blo ast) (form-prev-upper last-form))))
    (loop for return-next-upper in (form-nexts-upper blo) for return-next-lower in (form-nexts-lower blo) do
	 (meet-namespaces! return-next-upper return-next-lower (form-next-upper ast) (form-next-lower ast)))
    (loop for return-form in (form-branches blo) do
	 (meet-form! return-form (form-upper ast) (form-lower ast))))
  (meet-namespace-next! (car (last (walker:form-body ast))) ast)
  (loop for form in (walker:form-body ast) do
       (deduce-backward form)))

(defmethod deduce-backward ((ast return-from-form))
  (declare (optimize (debug 3)))
  (let* (;;(blo (namespace-lookup (walker:form-blo ast) (form-prev-upper ast)))
	 ;;(block-form (walker:nso-definition blo))
	 (value (walker:form-value ast)))
    (meet-form! value (form-upper ast) (form-lower ast))
    (deduce-backward value)))

(defmethod deduce-backward ((ast values-form))
  (let ((body (reverse (walker:form-body ast))))
    (loop for form in body for i from (1- (length body)) downto 0 do
	 (let ((new-upper (make-results (resultn (form-upper ast) i)))
	       (new-lower (make-results (resultn (form-lower ast) i))))
	   (meet-form! form new-upper new-lower))
	 (deduce-backward form))))

(defmethod deduce-backward ((ast multiple-value-bind-form))
  (declare (optimize (debug 3)))
  (let* ((body (walker:form-body ast))
	 (first-form (car body))
	 (body-upper (form-prev-upper first-form))
	 (body-lower (form-prev-lower first-form))
	 (values (walker:form-values ast)))
    (let ((last-form (car (last body))))
      (meet-namespace-next! last-form ast)
      (meet-form! last-form (form-upper ast) (form-lower ast))
      (deduce-backward last-form))
    (loop for form in (reverse (butlast body)) do
	 (deduce-backward form))
    (loop for i from 0 for var in (walker:form-vars ast) do
	 (let* ((upper (resultn (form-upper values) i))
		(var-upper (namespace-lookup var body-upper))
		(var-lower (namespace-lookup var body-lower))
		(new-upper (meet var-upper upper)))
	   (assert (not (null new-upper)) () "Cannot meet types ~S and ~S for variable ~S" var-upper upper var)
	   (setf (resultn (form-upper values) i) new-upper)
	   (setf (resultn (form-lower values) i) (meet var-lower upper))))
    (meet-namespace-prev! ast first-form)
    (deduce-backward values)))

(defun test-backward ()
  (let* ((form '(aref (make-array-single-float 10 20) 2))
	 (ast (walker:parse-with-namespace form :free-namespace (walker:make-free-namespace)))
	 (ast (prepare-ast ast nil nil)))
    (deduce-backward ast)
    (assert (eq 'array (result1 (form-upper (car (walker:form-arguments ast)))))))
  (let* ((form '(1+ (block bl (return-from bl 1))))
	 (ast (walker:parse-with-namespace form :free-namespace (walker:make-free-namespace)))
	 (ast (prepare-ast ast nil nil)))
    (deduce-backward ast)
    (assert (eq 'number (result1 (form-upper (car (walker:form-arguments ast)))))))
  (let* ((form '(flet ((f1 (a) a)) (1+ (f1 1))))
	 (ast (walker:parse-with-namespace form :free-namespace (walker:make-free-namespace)))
	 (ast (prepare-ast ast nil nil)))
    (deduce-backward ast)
    (assert (eq 'number (result1 (form-upper (car (walker:form-body (car (walker:form-bindings ast))))))))))
(test-backward)


(unless (boundp '+scan-line-stepped-from-below-code+)
  (defparameter +scan-line-stepped-from-below-code+ nil))
(defun deduce-voxelneu-code (&optional (form +scan-line-stepped-from-below-code+))
  (let* ((variables '(*mipmap-factor* *automatic-stepsize* *step-size* *exact-exit* *mouse-info*))
	 (free-namespace (walker:make-free-namespace :variables variables :macros (append '(prind) walker:+common-lisp-macros+)))
	 (ast (walker:parse-with-namespace form :parser (walker:make-parser (list (cons #'walker-plus:parse-p #'walker-plus:parse) (cons #'walker:parse-p #'walker:parse))) :free-namespace free-namespace))
	 (upper nil)
	 (lower nil))
    (loop for var in variables do
	 (setf upper (augment-namespace (walker:namespace-lookup 'walker:var var free-namespace) t upper))
	 (setf lower (augment-namespace (walker:namespace-lookup 'walker:var var free-namespace) t lower)))
    (let ((ast (prepare-ast ast upper lower)))
      (let ((old nil)
	    (new t))
	(loop until (equal old new) for i from 0 do
	     (format t "deduce loop ~A~%" i)
	     (setf old new)
	     (deduce-forward ast)
	     (deduce-backward ast)
	     (setf new (annotate ast)))
	new))))
