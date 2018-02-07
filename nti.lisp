;; TODO: Think about how to make the NTI code (and maybe the WALKER package more functional). Thought 1: Make the code functional by defining a hash-table that has as key a sub-tree of a lisp form, and as value another hash-table with keys :WALKER, :ENVIRONMENT, :TYPE. When the second hash-table has key :WALKER, then the value of the hash-table is the parsed WALKER expression, when the key is :ENVIRONMENT, it is an ALIST with (symbol,parsed WALKER:SYM)-entries (i.e. the WALKER:NAMESPACE (free and lexical namespaces) at the start of the sub-tree of the lisp form), and when it is :TYPE, it is an ALIST with (parsed WALKER:SYM,type)-entries (i.e. the NTI:NAMESPACE). This should allow parsing functions twice. For example, when the second hash-table has key :

;; TODO: think about how ASSERT can be included. The NTI paper says: """In the classical non-deterministic manner, the flow of control is terminated only by branches of the computation that fail in the sense that there are no legitimate values for variables. In this setting, predicates are modelled by partial functions whose results are ignored. One particularly valuable partial function of this kind is "assert(p)" which is defined only for the argument "true".""" One way to include ASSERT would be to model it as an IF-FORM, which has as its (only) THEN-FORM the code following the ASSERT. What about: (LET ((A 1)) (PROGN (ASSERT (INTEGERP A))) (LET ((B 1)) B))? Should the THEN-FORM include the second LET? SBCL compiles the following without a warning, although it would be possible to infer that the assertion always fails: (DEFUN TEST (X Y) (ASSERT (AND (> X Y) (<= X Y)))). Neither does SBCL complain for (DEFUN TEST (X) (ASSERT (AND (INTEGERP X) (TYPEP X 'SINGLE-FLOAT)))) or for (DEFUN TEST (X) (DECLARE (TYPE SINGLE-FLOAT X)) (ASSERT (INTEGERP X))).

#|
TODO: Implement Flow Control Analysis. What about the following program:
(LET ((A 1))
  (TAGBODY
   L
     (NULL A)
     (IF (TYPEP A 'INTEGER)
	 (PROGN (SETQ A 1.0) (GO L))
	 (SETQ A 1))
     (NULL A))
  A)
The IF-form has two exits: tag L and the second (NULL A) form. After the first #'DEDUCE-FORWARD, in the first (NULL A), A has type FIXNUM. After the second #'DEDUCE-FORWARD, in the first (NULL A), A has type NUMBER. In the second (NULL A), A is INTEGER after every iteration, because the only entrypoint to the second (NULL A) is the second branch (SETQ A 1) of the IF-form.
I could implement the non-standard exit of the first branch by setting FROM-UPPER and FORM-LOWER of the PROGN-form to (MAKE-RESULTS* :NVALUES 0). The IF-form could detect this and use only the namespace during the second branch in its merging of branch namespaces. Consider the following program that extends the first example, and shows how to evaluate the body of a form:
(PROGN
  (BLOCK B
    (LET ((A 1))
      (TAGBODY
       L
	 (IF (TYPEP A 'INTEGER)
	     (PROGN (SETQ A 1.0) (GO L) (SETQ A NIL)))
	 (RETURN-FROM B A))
      NIL)))
The evaluation of the body of PROGN would have to detect that GO doesn't return, and skip the evaluation of the rest of its body, i.e. (SETQ A NIL), then assign (MAKE-RESULTS* :NVALUES 0) to the whole PROGN-form, so that the IF-form ignores the namespace of its first branch in determining the merge of namespaces and form-results. The IF-form would receive a MAKE-RESULTS different from (MAKE-RESULTS* :NVALUES 0) (because it has two branches, one of which does exit normally), but the (RETURN-FROM B A) would receive a (MAKE-RESULTS* :NVALUES 0) again. Then, the TAGBODY-form and the LET-form would receive (MAKE-RESULTS* :NVALUES 0), because the evaluation of their BODYs shows that their last forms return (MAKE-RESULTS* :NVALUES 0), which propagates. The normal last form of BLOCK would receive a (MAKE-RESULTS* :NVALUES 0), but because the BLOCK-form does a merge at its exit, it would merge this (MAKE-RESULTS* :NVALUES 0) with the FORM-UPPER from the (RETURN-FROM B A), which is (MAKE-RESULTS 'NUMBER), resulting in (MAKE-RESULTS 'NUMBER) for the whole BLOCK-form. This then propagates to the outermost PROGN-form.
What about GOs out of functions? (TAGBODY (FLET ((F1 () (GO L))) (F1)) L) We know that the GO-form must leave the FLET-form, because a TAGBODY can only have labels in its top-level body (i.e. not in nested forms inside the TAGBODY). This means that if the body of a function returns a (MAKE-RESULTS :NVALUES 0), we know that its application form (F1) won't return either.
|#

(declaim (optimize (debug 3)))

(load "~/quicklisp/setup.lisp")
(ql:quickload '(:walker :walker-plus))

(defpackage :nimble-type-inferencer
  (:documentation "Nimble type inferencer for ANSI Lisp, see the paper \"The Nimble Type Inferencer for Common Lisp-84\" by Henry G. Baker.")
  (:use :cl)
  (:export
   ;; for classes: export the class and _all_ accessors on one line so that deleting a class doesn't have to consider all exports of other classes
   ))
(in-package :nimble-type-inferencer)

(defun id-of (x)
  "This function returns a string that identifies object X. This can be useful for debugging."
  (let* ((s (with-output-to-string (stream)
	      (print-unreadable-object (x stream :type nil :identity t))))
	 (id (subseq s (1+ (position #\< s)) (position #\> s :from-end t))))
    (subseq id (position #\Space id :test-not #'char-equal) (1+ (position #\Space id :test-not #'char-equal :from-end t)))))

(defun copy-hash-table (ht)
  (let ((new (make-hash-table :test (hash-table-test ht) :size (hash-table-size ht) :rehash-size (hash-table-rehash-size ht) :rehash-threshold (hash-table-rehash-threshold ht))))
    (maphash (lambda (k v) (setf (gethash k new) v)) ht)
    new))

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

(defun find-builtin-function (fun-name)
  "Return the definition of FUN-NAME as defined in +BUILTIN-FUNCTIONS+, or NIL if it's not defined there."
  (find fun-name +builtin-functions+ :key #'car))

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

(defun type-of-object (object)
  (etypecase object
    (fixnum 'fixnum)
    (float 'single-float)
    (null 'null)
    (boolean 'boolean) ;must be after NULL
    (symbol 'symbol)
    (t t)))

(defstruct (results (:constructor make-results*))
  "The type of multiple values: NVALUES is the number of possible finite values exponentiated by 2 and LOGIOR'd together; FINITE is of type (LIST TYPE); and INFINITE is of type TYPE."
  (nvalues -1 :type integer :read-only t) ;the number of values of the result, -1 means any number of values
  (finite nil :type list) ;the beginning of the values list of the result
  (infinite nil :type (or symbol list))) ;the infinite part of the values list of the result

(defun make-results (&rest results)
  (let ((results-head (nreverse (member-if (lambda (result) (not (eql 'null result))) (reverse results)))))
    (make-results* :nvalues (expt 2 (length results)) :finite (copy-list results-head) :infinite 'null)))

(defun make-results-infinite (result-type)
  (make-results* :nvalues -1 :finite nil :infinite result-type))

(defun is-results-infinite (results infinite-part)
  (and (= (results-nvalues results) -1) (eql (results-finite results) nil) (eql (results-infinite results) infinite-part)))

(defun make-results-t ()
  (make-results-infinite t))

(defun is-results-t (results)
  (is-results-infinite results t))

(defun make-results-nil ()
  (make-results-infinite nil))

(defun is-results-nil (results)
  (is-results-infinite results nil))

(defun make-results-0 ()
  (make-results* :nvalues 0))

(defun is-results-0 (results)
  (= (results-nvalues results) 0))

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
  (declare (optimize (debug 3)))
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
		      (process-results (apply #'make-results fun-results-types)
				       results
				       (lambda (t1 t2) (unless (is-subtypep t1 t2 +builtin-typehash+) (return-from always nil)) nil)
				       (constantly 0)) ;the value doesn't matter
		      t))
	   (push fun-arg-types possible)))
    (assert name-found () "Unknown function ~S" fun)
    ;;(when (null possible) (return-from fun-arguments-lookup '(nil))) ;TODO: return the correct number of arguments
    (assert (not (null possible)) () "Function ~S with result types ~S has unknown arguments" fun results)
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

(defmethod print-object ((object function-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S"
	    (append (type-required object)
		    (let ((optional (type-optional object)))
		      (unless (null optional)
			(list* '&optional optional)))
		    (let ((rest (type-rest object)))
		      (unless (null rest)
			(list '&rest rest)))
		    (let ((key (type-key object)))
		      (unless (null key)
			(list* '&key key))))
	    (type-values object))))

(defun parse-function-declaration (decl)
  (multiple-value-bind (args vals) (walker:parse-function-declaration decl)
    (if (eq args 'function)
	'function
	(make-instance 'function-type
		       :required (cdr (assoc t args))
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
    ((eq result2 'function)
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
	 (types (mapcar #'walker:declspec-type (remove-if (lambda (x) (not (or (typep x 'walker:declspec-type) (typep x 'walker:declspec-ftype)))) declspecs)))
	 (type (etypecase sym
		 (walker:var
		  (reduce #'meet types :initial-value t))
		 (walker:fun
		  (reduce #'meet-function types :initial-value 'function)))))
    (assert (not (null type)) () "Impossible type declarations for variable ~S: ~S" sym types)
    type))

#|
(defun test-forward-result (form)
  (declare (optimize (debug 3)))
  (let* ((parser (walker:make-parser :type 'walker-plus:parser-plus :variables nil :macros (append '(prind namespace) walker:+common-lisp-macros+)))
	 (ast (walker:parse-with-namespace form :parser parser))
	 (ast (progn
		(walker-plus:remove-dead-code! ast)
		(prepare-ast ast (make-empty-namespace) (make-empty-namespace)))))
    (deduce-forward ast)
    ast))

(defun test-forward ()
  (declare (optimize (debug 3)))
  (labels ((assert-result (form upper)
	     (declare (optimize (debug 3)))
	     (let* ((ast (test-forward-result form))
		    (results (form-upper ast))
		    (finite (results-finite results))
		    (results (loop for i below (length upper) collect
				  (if (< i (length finite)) (elt finite i) (results-infinite results)))))
	       (assert (equal results upper) () "failed assertion for form ~S:~%results:~S wanted:~S" form results upper))))
    (macrolet ((assert-error (form)
		 (let ((ast-sym (gensym "AST")) (form-sym (gensym "FORM")))
		   `(let* ((,form-sym ,form)
			   (,ast-sym (walker:parse-with-namespace ,form-sym))
			   (,ast-sym (prepare-ast ,ast-sym (make-empty-namespace) (make-empty-namespace))))
		      (handler-case (progn (deduce-forward ,ast-sym) (assert nil () "failed (ASSERT-ERROR ~S)" ,form-sym))
			;; TODO: replace T with something like TYPE-ERROR.
			(t () t))))))
      (assert-result '1 '(fixnum))
      (assert-result '(+ 1 2) '(fixnum))
      (assert-result '(+ 1 2.0) '(single-float))
      (assert-result '(let ((a 1) (b 2)) (+ a b)) '(fixnum))
      (assert-result '(let ((a 1)) 1.0 a) '(fixnum))
      (assert-result '(let ((a 1)) a 1.0) '(single-float))
      (assert-result '(let* ((a 1) (a 1.0)) a) '(single-float))
      (assert-error '(let ((a 1) (b 2)) (declare (type single-float a)) (+ a b)))
      (assert-error '(let ((a 1)) (assert (typep a 'single-float)) a))
      (assert-result '(let ((a 1) (b 2)) (declare (type number a)) (+ a b)) '(fixnum))
      (assert-result '(let ((a 1.0)) (setq a 1) a) '(fixnum))
      (assert-result '(let ((a 1.0)) (setq a 1) (+ a 2)) '(fixnum))
      (assert-result '(let ((a 1) (b 2)) (setq a 1.0 b a) b) '(single-float))
      (assert-error '(let ((a 1.0)) (declare (type single-float a)) (setq a 1) (+ a 2)))
      (assert-result '(let ((a nil)) (let ((b (setq a 1)))) a) '(fixnum))
      (assert-result '(let* ((a nil) (b (setq a 1))) a) '(fixnum))
      (assert-result '(if 1 2 3) '(fixnum))
      (assert-result '(if 1 2 nil) '(t))
      (assert-result '(if 1 2) '(t))
      (assert-result '(let ((a 1)) (if 1 (setq a 2)) a) '(fixnum))
      (assert-result '(let ((a nil)) (if 1 (setq a 1) (setq a 2.0)) a) '(number))
      (assert-result '(let ((a nil)) (if 1 (setq a 1) (setq a nil)) a) '(t))
      (assert-result '(let ((a nil)) (let () (setq a 1)) a) '(fixnum))
      (assert-result '(tagbody a (go b) b) '(null))
      (assert-result '(let ((a 10)) (tagbody s (setq a (1- a)) (if (<= a 0) (go e)) (go s) e) a) '(number)) ;TODO: should be '(FIXNUM)
      (assert-result '(let ((a nil)) (tagbody (if (null a) (setq a 10)) s (setq a (1- a)) (if (<= a 0) (go e)) (go s) e) a) '(number)) ;TODO: should be '(FIXNUM)
      (assert-result '(let ((a nil)) (tagbody (setq a 1) a (setq a 1.0) (go b) (setq a nil) b) a) '(single-float))
      (assert-result '(let ((a 1) (b 1)) (tagbody a (setq a 1.0) (go b) c (setq b nil) (setq a nil) (go c) b) (values a b)) '(single-float fixnum))
      (assert-result '(let ((a 1) (b 1)) (tagbody a (setq a 1.0) (go b) c (setq b nil) (go c) (setq a nil) b) (values a b)) '(single-float fixnum))
      (assert-result '(let ((r nil)) (tagbody (let ((a (setq r 1))) (go l) (setq r 1.0)) l) r) '(fixnum))
      (assert-result '(let ((r nil)) (tagbody (let ((a (setq r 1)) (b (go l)) (c (setq r 1.0))) (setq r 1.0)) l) r) '(fixnum))
      (assert-result '(let ((r nil)) (tagbody (let ((a (let ((b (setq r 1)) (a (go l))))))) l) r) '(fixnum))
      (assert-result '(block b (return-from b nil)) '(null))
      (assert-result '(block b (tagbody a (return-from b nil) (go a))) '(null))
      (assert-result '(block b (tagbody a (go a) (return-from b nil))) '())
      (assert-result '(tagbody (block f1 (go l)) l) '(null))
      (assert-result '(let ((r nil)) (tagbody (block f1 (setq r 1) (go l)) l) r) '(fixnum))
      (assert-result '(block b (let* ((a 1) (a (return-from b a)) (a 1.0)) nil)) '(fixnum))
      (assert-result '(block b (let ((a 1)) (return-from b (setq a nil)) a)) '(null))
      (assert-result '(let ((a 1)) (block b (return-from b a))) '(fixnum))
      (assert-result '(block b (if 1 (return-from b 1)) (if 2 (return-from b 1.0) 0)) '(number))
      (assert-result '(values 1 1.0) '(fixnum single-float))
      (assert-result '(block b (values 1 2 (return-from b nil) 3)) '(null))
      (assert-result '(multiple-value-bind (a b) (values 1 1.0) b) '(single-float))
      (assert-result '(multiple-value-bind (a b) 1 (values a b)) '(fixnum null))
      (assert-result '(block b (multiple-value-bind (a b) (progn (return-from b 1)) a)) '(fixnum))
      (assert-result '(flet ((f1 (a) (let ((b a)) b))) nil) '(null))
#|
      (assert-result '(flet ((f1 (a) a)) (f1 1)) '(fixnum))
      (assert-result '(flet ((f1 (a) a)) (f1 1) (f1 1.1)) '(single-float))
      (assert-result '(flet ((f1 (a) (return-from f1 a))) (f1 1)) '(fixnum))
      (assert-result '(flet ((f1 (a) (return-from f1 a))) (f1 1) (f1 1.1)) '(single-float))
      (assert-result '(let ((r nil)) (tagbody (flet ((f1 () (setq r 1.0) (go l) (setq r 1))) (f1) (setq r 1)) l) r) '(single-float))
      (assert-result '(let ((r nil)) (tagbody (flet ((f1 (x) (setq r x) (go l) (setq r 1))) (f1 1.0) (setq r 1)) l) r) '(single-float))
|#
      ;;(assert-result '(labels ((f1 (x) (f2 x)) (f2 (x) 2)) (f1 10)) 'fixnum)
      ;;(assert-result '(labels ((f1 (x) (if (= x 0) x (f2 x))) (f2 (x) (f1 (1- x)))) (f1 10)) 'fixnum)
      ;;(assert-result '(labels ((f1 (a) (if (= 0 a) (return-from f1 0) (f1 (1- a))))) (f1 1)) '(fixnum))
      ;;(assert-result '(labels ((f1 (a) (if (= 0 a) 0 (f1 (1- a))))) (f1 1)) '(fixnum))
      ;;(assert-result '(labels ((f1 (a) (if (= 0 a) 0 (labels ((f2 (a) (f1 (1- a)))) (f2 a))))) (f1 1)) '(fixnum))
      ;;(assert-result '((lambda (x) x) 1) '(fixnum))
      )))
|#

#|
(defun test-backward-result (form form-result-types variables &key forward (loop 1))
  (declare (optimize (debug 3)))
  (let* ((parser (walker:make-parser :type 'walker-plus:parser-plus :variables variables :macros (append '(prind namespace) walker:+common-lisp-macros+)))
	 (ast (walker:parse-with-namespace form :parser parser))
	 (upper (make-empty-namespace))
	 (lower (make-empty-namespace)))
    (loop for var in variables do
	 (let ((walker-var (walker:namespace-lookup 'walker:var var (walker:parser-free-namespace parser))))
	   (setf upper (augment-namespace walker-var t upper))
	   (setf lower (augment-namespace walker-var nil lower))))
    (walker-plus:remove-dead-code! ast)
    (let ((ast (prepare-ast ast upper lower)))
      (setf (form-upper ast) (apply #'make-results form-result-types))
      (setf (form-lower ast) (make-results* :infinite nil))
      (loop for i below loop do
	   (when forward
	     (deduce-forward ast))
	   (deduce-backward ast))
      (let ((actual-types (loop for var in variables collect
			       (namespace-lookup (walker:namespace-lookup 'walker:var var (walker:parser-free-namespace parser)) upper))))
	(apply #'values ast actual-types)))))
|#

#|
(defun test-backward ()
  (declare (optimize (debug 3)))
  (flet ((assert-result (form form-result-types variables wanted-types &key forward (loop 1))
	   (declare (optimize (debug 3)))
	   (destructuring-bind (ast &rest actual-types) 
	       (multiple-value-list (test-backward-result form form-result-types variables :forward forward :loop loop))
	     (assert (equal wanted-types actual-types) () "failed assertion for form ~S:~%results:~S wanted:~S~%annotated form:~S" form actual-types wanted-types (annotate ast)))))
    (assert-result '(setq x y) '(fixnum) '(x y) '(t fixnum))
    (assert-result '(let ((a x)) a) '(fixnum) '(x) '(fixnum))
    (assert-result '(let* ((a x) (b a)) b) '(fixnum) '(x) '(fixnum))
    (assert-result '(let ((a 1)) (let ((a x) (b a)) b)) '(fixnum) '(x) '(t))
    (assert-result '(+ x 1) '(t) '(x) '(number))
    (assert-result '(progn (null x) (+ x 1)) '(t) '(x) '(number) :forward t :loop 10)
    (assert-result '(aref x 1) '(fixnum) '(x) '((array fixnum)))
    (assert-result '(if 1 (assert (typep x 'integer)) (assert (typep x 'single-float))) '(t) '(x) '(number))
    (assert-result '(values x y) '(fixnum null) '(x y) '(fixnum null))
    (assert-result '(multiple-value-bind (a b) (values x y) (values a b)) '(fixnum null) '(x y) '(fixnum null))
    (assert-result '(let ((a x)) (tagbody s (aref a 1))) '(null) '(x) '(array))
    ;; I don't know how to test that the type of X in (NULL X) in '(LET ((A 1)) (TAGBODY S (ASSERT (TYPEP X 'NUMBER)) (NULL X) (GO S))) should be 'NUMBER after one round of #'DEDUCE-BACKWARD. The problem is that I cannot extract the 
    ))
(test-backward)
|#

#|
(defun test-backward2 ()
  (declare (optimize (debug 3)))
  (let* ((form '(aref (make-array-single-float 10 20) 2))
	 (ast (walker:parse-with-namespace form))
	 (ast (prepare-ast ast (make-empty-namespace) (make-empty-namespace))))
    (deduce-backward ast)
    (assert (eq 'array (result1 (form-upper (car (walker:form-arguments ast)))))))
  (let* ((form '(1+ (block bl (return-from bl 1))))
	 (ast (walker:parse-with-namespace form))
	 (ast (prepare-ast ast (make-empty-namespace) (make-empty-namespace))))
    (deduce-backward ast)
    (assert (eq 'number (result1 (form-upper (car (walker:form-arguments ast)))))))
  (let* ((form '(flet ((f1 (a) a)) (1+ (f1 1))))
	 (ast (walker:parse-with-namespace form))
	 (ast (prepare-ast ast (make-empty-namespace) (make-empty-namespace))))
    (deduce-backward ast)
    (assert (eq 'number (result1 (form-upper (car (walker:form-body (car (walker:form-bindings ast)))))))))
  (let* ((form '(flet ((f1 (a) (if a (1+ a) 1)))))
	 (ast (walker:parse-with-namespace form))
	 (ast (prepare-ast ast (make-empty-namespace) (make-empty-namespace))))
    (deduce-backward ast)
    (deduce-forward ast)
    (assert (eq 'number (result1 (form-upper (car (walker:form-body (car (walker:form-bindings ast)))))))))
  ;;TODO: check that X in both (NULL X) has type FIXNUM in '(PROGN (BLOCK B (IF 1 (PROGN (NULL X) (RETURN-FROM B NIL))) (NULL X)) (ASSERT (TYPEP X 'FIXNUM))) after doing #'DEDUCE-FORWARD and then #'DEDUCE-BACKWARD.
  ;;TODO: check that X in the first (NULL X) has type NUMBER in '(PROGN (BLOCK B (IF 1 (PROGN (NULL X) (RETURN-FROM B NIL)) (ASSERT (TYPEP X 'FIXNUM))) (NULL X)) (ASSERT (TYPEP X 'NUMBER))) after doing #'DEDUCE-FORWARD and then #'DEDUCE-BACKWARD.
  )
(test-backward2)

(defun test-all ()
  ;; (LET ((A 0) (B 0)) (BLOCK NIL (SETQ A NIL X (RETURN) B NIL)) (VALUES A B)) should return '(NULL FIXNUM)
  ;; (LET ((A NIL)) (BLOCK NIL (IF (PROGN (SETQ A T) (RETURN)) (SETQ A 1) (SETQ A 1))) A) should return 'BOOLEAN
  ;; (LET ((A 1.0)) (BLOCK NIL (IF 1 (RETURN) (SETQ A 1))) A) should return 'NUMBER
  ;; (LET ((A 1.0)) (BLOCK NIL (IF 1 (SETQ A 1) (RETURN))) A) should return 'NUMBER
  ;; (LET ((A NIL)) (BLOCK NIL (FLET ((F1 (&OPTIONAL (X (RETURN)) (Y (SETQ A 0))))) (F1) (SETQ A 0))) A) should return 'NULL
  ;; (LET ((A NIL)) (BLOCK NIL (FLET ((F1 (&KEY (X (RETURN)) (Y (SETQ A 0))))) (F1) (SETQ A 0))) A) should return 'NULL
  ;; (LET ((A NIL)) (BLOCK NIL (FLET ((F1 (&AUX (X (RETURN)) (Y (SETQ A 0))))) (F1) (SETQ A 0))) A) should return 'NULL
  )
|#

#|
(unless (boundp '+scan-line-stepped-from-below-code+)
  (defparameter +scan-line-stepped-from-below-code+ nil))
(defun deduce-voxelneu-code (&optional (form +scan-line-stepped-from-below-code+))
  (let* ((variables '(*mipmap-factor* *automatic-stepsize* *step-size* *exact-exit* *mouse-info*))
	 (parser (walker:make-parser :variables variables :macros (append '(prind) walker:+common-lisp-macros+)))
	 (ast (walker:parse-with-namespace form :parser parser))
	 (free-namespace (walker:parser-free-namespace parser))
	 (upper (make-empty-namespace))
	 (lower (make-empty-namespace)))
    (loop for var in variables do
	 (let ((walker-var (walker:namespace-lookup 'walker:var var free-namespace)))
	   (setf upper (augment-namespace walker-var t upper))
	   (setf lower (augment-namespace walker-var nil lower))))
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
|#


#|
(annotate (test-backward-result '(PROGN (BLOCK B (IF 1 (PROGN (NULL X) (RETURN-FROM B NIL))) (NULL X)) (ASSERT (TYPEP X 'FIXNUM))) '(t) '(x) :forward t :loop 1))


(let* ((form '(labels ((f1 (bla) bla (if 0 bla (f1 bla)))) (f1 1)))
       ;;(form '(labels ((f1 (a) (if (= a 0) a 1))) (f1 1) (f1 1.1)))
       ;;(form '(labels ((f1 (a) (if (= a 0) a (f1 (1- a))))) (f1 1)))
       ;;(form '(labels ((f1 (a) (if (= a 0) a (f1 (1- a))))) (f1 1) (f1 1.1)))
			       (free-namespace (walker:make-free-namespace :variables nil :macros (append '(prind namespace) walker:+common-lisp-macros+)))
			       (ast (walker:parse-with-namespace form :free-namespace free-namespace :parser (walker:make-parser (list #'walker-plus:parse-p #'walker:parse-p))))
			       (ast (progn (walker-plus:remove-dead-code! ast)
					   (prepare-ast ast (make-empty-namespace) (make-empty-namespace))))
			       (f1 (walker:form-sym (car (walker:form-bindings ast))))
			       (binding (namespace-lookup f1 (form-prev-upper (car (walker:form-body ast))))))
			  (deduce-forward ast)
			  (annotate ast))
|#

;;; CLIST, a list that stores the last cons separately. It can be appended to in constant time.

(defstruct (clist (:constructor make-clist*))
  "A list that can be appended to."
  (head nil :type list :read-only t)
  (tailcons nil :type cons))

(defun make-clist (&rest list)
  "Make a new clist from list LIST."
  (let ((start (cons nil list)))
    (make-clist* :head start :tailcons (last start))))

(defun clist-list (cl)
  "Returns the list stored in the clist CL."
  (cdr (clist-head cl)))

(defun clist-pushend (cl object)
  "Push the OBJECT to the end of the clist CL. Returns FG."
  (let ((tailcons2 (cons object nil)))
    (setf (cdr (clist-tailcons cl)) tailcons2)
    (setf (clist-tailcons cl) tailcons2))
  cl)

(defun clist-last (cl)
  "Return the last element of the clist CL."
  (car (clist-tailcons cl)))

(defun clist-nconc (cl1 cl2)
  "Append clist CL2 to the end of clist CL1, thereby modifying CL1. Returns CL1."
  (unless (null (cdr (clist-head cl2)))
    (setf (cdr (clist-tailcons cl1)) (cdr (clist-head cl2)))
    (setf (clist-tailcons cl1) (clist-tailcons cl2)))
  cl1)

(defun test-clist ()
  (let ((cl (make-clist)))
    (clist-pushend cl 1)
    (assert (equal (clist-list cl) '(1)))
    (assert (eql (clist-last cl) 1))
    (clist-pushend cl 2)
    (assert (equal (clist-list cl) '(1 2)))
    (assert (eql (clist-last cl) 2))
    (let ((cl2 (make-clist)))
      (clist-nconc cl cl2)
      (assert (equal (clist-list cl) '(1 2)))
      (assert (eql (clist-last cl) 2)))
    (let ((cl2 (make-clist 3)))
      (clist-nconc cl cl2)
      (assert (equal (clist-list cl) '(1 2 3)))
      (assert (eql (clist-last cl) 3)))
    (let ((cl2 (make-clist 4 5)))
      (clist-nconc cl cl2)
      (assert (equal (clist-list cl) '(1 2 3 4 5)))
      (assert (eql (clist-last cl) 5)))))
(test-clist)

(defmethod print-object ((object clist) stream)
  (format stream "~W" (clist-list object)))

;;; Type inference properties of forms.
#|
(defclass userproperties ()
  ((lexical-namespace :initform nil :initarg :lexical-namespace :accessor userproperties-lexical-namespace :documentation "The lexical namespace before the form was parsed.")
   (free-namespace :initform nil :initarg :free-namespace :accessor userproperties-free-namespace :documentation "The free namespace before the form was parsed.")
   (funbinding :initform nil :initarg :funbinding :accessor userproperties-funbinding :documentation "For APPLICATION-FORMs, a copy of the AST of the function binding being called.")
   (exits :initform nil :initarg :exits :accessor userproperties-exits :type nil
	  :documentation "NIL if no part of the form is reachable (non-deterministically) from the root AST, or the list of exits (TAGs and BLOs outside the form, and the form itself) that are exited from inside the form.")
   (upper :initform (make-results-t) :initarg :upper :accessor userproperties-upper :type results :documentation "The upper type bound of the form.")
   (lower :initform (make-results-nil) :initarg :lower :accessor userproperties-lower :type results :documentation "The lower type bound of the form.")))
|#
(defstruct userproperties
  (lexical-namespace nil)
  (free-namespace nil)
  (funbinding nil)
  (exits nil)
  (upper (make-results-t) :type results)
  (lower (make-results-nil) :type results))

(defmethod print-object ((user userproperties) stream)
  (print-unreadable-object (user stream :type t :identity nil)
    (format t "~A~S ~S" (if (userproperties-exits user) "" "DEAD ") (userproperties-upper user) (userproperties-lower user))))

(defun ast-lexical-namespace (ast)
  (userproperties-lexical-namespace (walker:user ast)))

(defun ast-free-namespace (ast)
  (userproperties-free-namespace (walker:user ast)))

(defun ast-exits (ast)
  (userproperties-exits (walker:user ast)))

(defun (setf ast-exits) (value ast)
  (setf (userproperties-exits (walker:user ast)) value))

(defun ast-upper (ast)
  (userproperties-upper (walker:user ast)))

(defun (setf ast-upper) (value ast)
  (setf (userproperties-upper (walker:user ast)) value))

(defun ast-lower (ast)
  (userproperties-lower (walker:user ast)))

(defun (setf ast-lower) (value ast)
  (setf (userproperties-lower (walker:user ast)) value))

(defun mapc-namespaces (function asts)
  "FUNCTION is a function of the variables stored in the namespaces of ASTS."
  (apply #'mapc
	 (lambda (&rest vars)
	   (assert (loop for var in (cdr vars) always (eql (walker:nso-name (car vars)) (walker:nso-name var))))
	   (apply function vars))
	 (mapcar (lambda (namespace) (loop for acons in namespace collect (cdr acons)))
		 (mapcar #'walker:namespace-var
			 (mapcar #'ast-lexical-namespace asts)))))

(defun meet-ast! (ast-target ast2)
  "Meet the results of AST-TARGET and AST2 and save the result in AST-TARGET."
  (setf (ast-upper ast-target) (meet-results (ast-upper ast-target) (ast-upper ast2)))
  (setf (ast-lower ast-target) (meet-results (ast-upper ast-target) (ast-lower ast2))))

(defun meet-namespace! (ast-target ast2)
  (mapc-namespaces (lambda (target-var ast2-var)
		     (setf (ast-upper target-var) (meet-results (ast-upper target-var) (ast-upper ast2-var))
			   (ast-lower target-var) (meet-results (ast-upper target-var) (ast-lower ast2-var))))
		   (list ast-target ast2)))

(defun join-ast! (ast-target asts)
  "Join the results of ASTS and meet the result with AST-TARGET."
  (setf (ast-upper ast-target) (meet-results (ast-upper ast-target) (reduce #'join-results (mapcar #'ast-upper (cdr asts)) :initial-value (ast-upper (car asts))))
	(ast-lower ast-target) (meet-results (ast-upper ast-target) (reduce #'join-results (mapcar #'ast-lower (cdr asts)) :initial-value (ast-lower (car asts)))))) ;meet AST-UPPER again

(defun join-ast-results! (ast-target results-upper results-lower)
  "Join the results of ASTS and meet the result with AST-TARGET."
  (setf (ast-upper ast-target) (meet-results (ast-upper ast-target) (reduce #'join-results (cdr results-upper) :initial-value (car results-upper)))
	(ast-lower ast-target) (meet-results (ast-upper ast-target) (reduce #'join-results (cdr results-lower) :initial-value (car results-lower))))) ;meet AST-UPPER again

(defun join-namespaces! (ast-target asts)
  "Join the common namespace of ASTS and meet the result with AST-TARGET."
  (mapc-namespaces (lambda (target-var ns0-var &rest nss-var)
		     ;; it should be correct to set (AST-UPPER TARGET-VAR) directly to the join instead of set the meet of TARGET-VAR with the join of the new namespaces? If it isn't then how does an IF-FORM prevent only being able to calculate subtypes of the variable types (of variables in the namespace) before the IF-FORM?
		     #|(setf (ast-upper target-var)
			   (meet-results (ast-upper target-var)
					 (reduce (lambda (v1 v2)
						   (prind (ast-upper v1) (ast-upper v2))
						   (join-results (ast-upper v1) (ast-upper v2)))
						 nss-var :initial-value ns0-var)))
		     (setf (ast-lower target-var)
			   (meet-results (ast-upper target-var) ;meet AST-UPPER again
					 (reduce (lambda (v1 v2)
						   (join-results (ast-lower v1) (ast-lower v2)))
						 nss-var :initial-value ns0-var)))
		     |#
		     (setf (ast-upper target-var)
			   (reduce #'join-results nss-var :initial-value (ast-upper ns0-var) :key #'ast-upper))
		     (setf (ast-lower target-var)
			   (reduce #'join-results nss-var :initial-value (ast-lower ns0-var) :key #'ast-lower)))
		   (cons ast-target asts)))

(defun set-ast-result (ast type)
  (let ((type (make-results type)))
    (setf (ast-upper ast) type
	  (ast-lower ast) type)))

;;; PARSER

(defclass parser-nti (walker:parser)
  ())

(defmethod walker:copy-parser ((parser parser-nti))
  (make-instance 'parser-nti :lexical-namespace (walker:parser-lexical-namespace parser) :free-namespace (walker:parser-free-namespace parser)))

(defmethod set-userproperties ((ast walker:application-form) (parser parser-nti) last-parser parent)
  ;; If this is a non-recursive call, then copy the FUN-BINDING and store the copy in USERPROPERTIES.
  (let ((userproperties (make-userproperties :lexical-namespace (walker:parser-lexical-namespace parser) :free-namespace (walker:parser-free-namespace parser))))
    (cond
      ((not (find-builtin-function (walker:nso-name (walker:form-fun ast))))
       (cond
	 ((walker:form-recursivep ast)
	  nil)
	 (t
	  (let* ((fun-ast (walker:nso-definition (walker:form-fun ast)))
		 (deparser (make-instance 'walker:deparser))
		 (fun-source (walker:deparse deparser fun-ast))
		 (labels-fun-source (list 'labels (list fun-source)))
		 (labels-ast (walker:parse last-parser labels-fun-source parent))
		 (fun-binding-ast-copy (walker:form-binding-1 labels-ast)))
	    (assert (typep fun-ast 'walker:fun-binding))
	    ;; Note that (WALKER:FORM-FUN AST) is different from (WALKER:FORM-SYM FUN-BINDING-AST-COPY).
	    (setf (userproperties-funbinding userproperties) fun-binding-ast-copy))))))
    (setf (walker:user ast) userproperties)))

(defun redefine-var! (parser symbol new-var)
  "Redefine the variable defined for SYMBOL in PARSER to be NEW-VAR. Return NIL."
  (let ((new-lexical-namespace (walker:namespace-var (walker:parser-lexical-namespace parser)))
	(new-free-namespace (walker:namespace-var (walker:parser-free-namespace parser))))
    (let ((cons-lexical (assoc symbol new-lexical-namespace :test #'equal))
	  (cons-free (assoc symbol new-free-namespace :test #'equal)))
      (cond
	(cons-lexical
	 (setf (cdr cons-lexical) new-var))
	(cons-free
	 (setf (cdr cons-free) new-var))
	(t (error "Var ~S is neither in lexical nor in free namespace" symbol))))
    (setf (walker:namespace-var (walker:parser-lexical-namespace parser)) new-lexical-namespace
	  (walker:namespace-var (walker:parser-free-namespace parser)) new-free-namespace))
  nil)

(defmethod set-userproperties ((ast walker:setq-form) (parser parser-nti) last-parser parent)
  (flet ((copy-var (var)
	   (walker:make-ast parser 'walker:var :name (walker:nso-name var) :freep (walker:nso-freep var) :definition (walker:nso-definition var) :sites (walker:nso-sites var) :declspecs (walker:nso-declspecs var) :macrop (walker:nso-macrop var)))) ;slot USER is set by the overridden #'MAKE-AST
    (loop for var-rest on (walker:form-vars ast) do
	 (let* ((var (car var-rest))
		(new-var (copy-var var)))
	   (redefine-var! parser (walker:nso-name var) new-var)
	   (setf (car var-rest) new-var))))
  (setf (walker:user ast) (make-userproperties :lexical-namespace (walker:parser-lexical-namespace parser) :free-namespace (walker:parser-free-namespace parser))))

(defmethod set-userproperties (ast (parser parser-nti) form parent)
  (setf (walker:user ast) (make-userproperties :lexical-namespace (walker:parser-lexical-namespace parser) :free-namespace (walker:parser-free-namespace parser))))

(defmethod walker:parse :around ((parser parser-nti) form parent)
  (let* ((last-parser (walker:copy-parser parser))
	 (ast (call-next-method parser form parent)))
    (set-userproperties ast parser last-parser parent)
    ast))

(defmethod walker:make-ast :around ((parser parser-nti) type &rest arguments)
  (let ((ast (apply #'call-next-method parser type arguments)))
    (let ((userproperties (make-userproperties :lexical-namespace (walker:parser-lexical-namespace parser) :free-namespace (walker:parser-free-namespace parser))))
      (setf (walker:user ast) userproperties))
    ast))

;;; ANNOTATE

(defclass deparser-annotate (walker:deparser)
  ((borders :initarg :borders :initform '(:upper) :accessor deparser-borders :documentation "What type of borders to show for a form: may be one of:
NIL (no annotation)
:UPPER (annotation using the special form THE)
:LOWER (annotation using the new form THE-L)
:UPPER-LOWER (annotation using the new form THE-UL).")
   (show :initarg :show :initform :form :accessor deparser-show :documentation "Whether to show :AST or :FORM.") ;TODO
   (notannotating1 :initarg :notannotating1 :accessor deparser-notannotating1 :documentation "An instance of class 'DEPARSER-NOTANNOTATE."))
  (:documentation "Annotating deparser for #'ANNOTATE"))

(defclass deparser-notannotate (walker:deparser)
  ((next :initarg :next :accessor deparser-next :documentation "An instance of class DEPARSER-ANNOTATE."))
  (:documentation "Deparser that doesn't annotate the first level for #'ANNOTATE."))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:declspec-ftype))
  (walker:deparse (deparser-notannotating1 deparser) ast))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:llist))
  (walker:deparse (deparser-notannotating1 deparser) ast))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:fun))
  (walker:deparse (deparser-notannotating1 deparser) ast))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:fun-binding))
  ;; TODO: in DEPARSER-INFER, add declarations to the function.
  (walker:deparse (deparser-notannotating1 deparser) ast))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:var-binding))
  (let ((form (call-next-method deparser ast)))
    (list (caddr (caaddr form)) (cadr (caddr form)))))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:setq-form))
  (let ((form (call-next-method deparser ast)))
    (do ((rest (cdaddr form) (cddr rest))) ((null rest) form)
      (setf (car rest) (caddar rest)))))

(defmethod walker:deparse :around ((deparser deparser-annotate) (ast walker:tag))
  (walker:deparse (deparser-notannotating1 deparser) ast))

(defun results-format (deparser ast form)
  (nconc
   (let* ((u (ast-upper ast))
	  (l (ast-lower ast))
	  (m (max (results-nvalues u) (results-nvalues l))))
     (ecase (deparser-borders deparser)
       (:upper-lower
	(cons 'the-ul
	      (cond
		((or (is-results-0 u) (is-results-0 l))
		 (list nil nil))
		((= m -1)
		 (list (list '&rest (results-infinite u) '&rest (results-infinite l))))
		(t
		 (loop for i below (most-significant-bit m) collect
		      (list (resultn u i) (resultn l i)))))))
       (:upper
	(cons 'the
	      (list
	       (cond
		 ((= m -1)
		  (list 'values '&rest (results-infinite u)))
		 (t
		  (list* 'values (loop for i below (most-significant-bit m) collect
				      (resultn u i))))))))
       (t (error "TODO"))))
   (list form)))

(defmethod walker:deparse :around ((deparser deparser-annotate) ast)
  (results-format deparser ast (call-next-method deparser ast)))

(defmethod walker:deparse :around ((deparser deparser-notannotate) ast)
  (call-next-method (deparser-next deparser) ast))

(defun annotate (ast &key (borders :upper-lower) (show :form))
  (let* ((deparser-annotate (make-instance 'deparser-annotate :borders borders :show show))
	 (deparser-notannotating1 (make-instance 'deparser-notannotate :next deparser-annotate)))
    (setf (deparser-notannotating1 deparser-annotate) deparser-notannotating1)
    (walker:deparse deparser-annotate ast)))

;;; INFERER

(defclass inferer (walker:deparser-map-ast)
  ((stack :initarg :stack :initform (make-clist) :accessor inferer-stack :documentation "A stack of ASTs, started to be processed. and finished or unfinished.")
   (order :initarg :order :initform (make-clist) :accessor inferer-order :documentation "A CLIST containing the order the forms were evaluated in during the forward pass.")))

;; INFER AROUND METHODS: save the order the ASTs are evaluated in and track the liveness of ASTs.

(defun ast-exits-normal (ast)
  "Return normal exits of AST. Normal exits are defined as exits that do not jump."
  (remove-if (lambda (ast)
	       (when (is-results-0 (ast-upper ast)) (assert (is-results-0 (ast-lower ast))))
	       (is-results-0 (ast-upper ast)))
	     (ast-exits ast)))

(defun ast-exits-abnormal (ast)
  "Return abnormal exits of AST. Abnormal exits are defined as exits that do jump."
  (remove-if (lambda (ast)
	       (when (is-results-0 (ast-upper ast)) (assert (is-results-0 (ast-lower ast))))
	       (not (is-results-0 (ast-upper ast))))
	     (ast-exits ast)))

(defmethod walker:deparse :around ((inferer inferer) ast)
  (let ((stack (inferer-stack inferer))
	(order (inferer-order inferer)))
    (cond
      ((eql (clist-last stack) ast) ;necessary because :AROUND-methods on more specific forms (e.g. TAGBODY-FORMs) call this fallback-:AROUND-method using CALL-NEXT-METHOD. TODO: maybe avoid this by using a non-standard method resolution order.
       (call-next-method inferer ast))
      (t
       (setf (ast-exits ast)
	     (cond
	       ((or (null (clist-list order))
		    (find (clist-last order) (ast-exits-normal (clist-last order))))
		(clist-pushend stack ast) ;must be before #'CALL-NEXT-METHOD
		(call-next-method inferer ast) ;infer the form
		(prog1 
		    (let ((last (clist-last order)))
		      (cond
			((null last)
			 (list ast))
			((find last (ast-exits-normal last))
			 (cons ast (remove last (ast-exits-normal last))))
			(t
			 (ast-exits last))))
		  (clist-pushend order ast)))
	       (t ;dead forms are not inferred.
		(warn "dead form ~S" ast)
		nil)))))))

(defmethod walker:deparse :around ((inferer inferer) (ast walker:tag))
  (clist-pushend (inferer-stack inferer) ast)
  (call-next-method inferer ast)
  (setf (ast-exits ast) (list ast))
  (clist-pushend (inferer-order inferer) ast))

(defmethod walker:deparse :around ((inferer inferer) (ast walker:go-form))
  (clist-pushend (inferer-stack inferer) ast)
  (call-next-method inferer ast)
  (setf (ast-exits ast) (list ast))
  (setf (ast-upper ast) (make-results-0)
	(ast-lower ast) (make-results-0))
  (clist-pushend (inferer-order inferer) ast))

(defmethod walker:deparse :around ((inferer inferer) (ast walker:tagbody-form))
  (declare (optimize (debug 3)))
  (clist-pushend (inferer-stack inferer) ast)
  (let ((outside-exits nil) ;the list of TAGs and BLOs that are jumped to outside AST
	(goforms (list (walker:form-body ast))) ;a list of list of forms inside AST that are jumped to.
	(tag-gopoints (mapcar #'car (mapcar #'walker:nso-gopoint (walker:form-tags ast))))
	(inside-exits (make-hash-table))
	(visited (make-hash-table)))
    (loop while (not (null goforms)) do
	 (let ((goforms0 (pop goforms)))
	   (let* ((first (car goforms0))
		  (entrances (gethash first inside-exits)))
	     (when entrances
	       (join-namespaces! first entrances)))
	   (loop for form-cdr on goforms0 do
		(let ((form (car form-cdr))
		      (next-form (cadr form-cdr)))
		  (when (gethash form visited nil) ;prevent infinite loops
		    (return))
		  (setf (gethash form visited) t)
		  (walker:deparse inferer form)
		  (let ((abnormal-exits (ast-exits-abnormal form)))
		    (setf outside-exits
			  (nconc (remove-if (lambda (exit)
					      (if (and (typep exit 'walker:go-form)
						       (find (walker:form-tag exit) (walker:form-tags ast)))
						  (let ((gopoint (walker:nso-gopoint (walker:form-tag exit))))
						    (push gopoint goforms)
						    (push exit (gethash (car gopoint) inside-exits)))
						  nil))
					    abnormal-exits)
				 outside-exits))
		    (when (not (find form (ast-exits-normal form)))
		      (return)))
		  (when (and next-form (find next-form tag-gopoints))
		    (push form (gethash next-form inside-exits)))))))
    (let ((last (walker:form-body-last ast)))
      (setf (ast-exits ast)
	    (nconc (when (or (null last) (find last (ast-exits-normal last)))
		     (list ast))
		   outside-exits))
      (meet-namespace! ast last)))
  (loop for form in (walker:form-body ast) do
       (unless (find form (clist-list (inferer-order inferer)))
	 (warn "dead form ~S" form)))
  (set-ast-result ast 'null)
  (clist-pushend (inferer-order inferer) ast))

(defmethod walker:deparse :around ((inferer inferer) (ast walker:return-from-form))
  (clist-pushend (inferer-stack inferer) ast)
  (call-next-method inferer ast)
  (setf (ast-exits ast) (list ast))
  (setf (ast-upper ast) (make-results-0)
	(ast-lower ast) (make-results-0))
  (clist-pushend (inferer-order inferer) ast))

(defmethod walker:deparse :around ((inferer inferer) (ast walker:block-naming-form))
  (declare (optimize (debug 3)))
  (clist-pushend (inferer-stack inferer) ast)
  (let ((outside-exits nil) ;the list of TAGs and BLOs that are jumped to outside AST
	(inside-exits nil)) ;the list of RETURN-FORMs that leave this AST
    (loop for form in (walker:form-body ast) do
	 (walker:deparse inferer form)
	 (let ((abnormal-exits (remove form (ast-exits-abnormal form))))
	   (setf outside-exits (nconc (remove-if (lambda (exit)
						   (and (eql exit (walker:form-blo ast))
							(push form inside-exits)))
						 abnormal-exits)
				      outside-exits))
	   (when (not (find form (ast-exits-normal form)))
	     (return))))
    (join-ast! ast inside-exits)
    (setf (ast-exits ast)
	  (nconc (let ((last (walker:form-body-last ast)))
		   (when (or (null last) (find last (ast-exits-normal last)))
		     (list ast)))
		 outside-exits)))
  (loop for form in (walker:form-body ast) do
       (unless (find form (clist-list (inferer-order inferer)))
	 (warn "dead form ~S" form)))
  (clist-pushend (inferer-order inferer) ast))

(defmethod walker:deparse :around ((inferer inferer) (ast walker:if-form))
  (clist-pushend (inferer-stack inferer) ast)
  (let ((test (walker:form-test ast))
	(then (walker:form-then ast))
	(else (walker:form-else ast)))
    (walker:deparse inferer test)
    (let ((test-exits (find test (ast-exits-normal test)))
	  (else (if else else (walker:make-ast (walker:make-parser :type 'parser-nti) 'walker:selfevalobject :object nil))))
      (when test-exits
	(walker:deparse inferer then)
	(walker:deparse (make-instance 'inferer) else)) ;new DEPARSE-INFER so that (AST-EXITS ELSE) doesn't return NIL if THEN-EXITS==NIL.
      (let ((then-exits (find then (ast-exits-normal then)))
	    (else-exits (find else (ast-exits-normal else))))
	(cond
	  (test-exits
	   (setf (ast-exits ast) (nconc (when (or then-exits else-exits) (list ast))
					(ast-exits-abnormal then)
				        (ast-exits-abnormal else))))
	  (t
	   (warn "dead form ~S" then)
	   (warn "dead form ~S" else)
	   (setf (ast-exits ast) (ast-exits test))))
	(let ((exiting (append (when then-exits (list then)) (when else-exits (list else)))))
	  (if (and test-exits (not (null exiting)));must be after (JOIN-NAMESPACES! AST ...), so that namespaces in THEN and ELSE are correct.
	      (join-ast! ast exiting)
	      (setf (ast-upper ast) (make-results-0)
		    (ast-lower ast) (make-results-0)))
	  (join-namespaces! ast exiting)))))
  (clist-pushend (inferer-order inferer) ast))

;; INFER PRIMARY METHODS: type inference.
    
(defmethod walker:deparse ((inferer inferer) (ast walker:selfevalobject))
  ;; maybe move this to #'WALKER:PARSE, which would save some time but a person reading code wouldn't know where to look for.
  (clist-pushend (inferer-stack inferer) ast)
  (setf (ast-exits ast) (list ast))
  ;; this must set the sharpest bound possible.
  (set-ast-result ast
		  (etypecase (walker:selfevalobject-object ast)
		    (fixnum 'fixnum)
		    (float 'single-float)
		    (null 'null)
		    (boolean 'boolean) ;must be after NULL
		    (symbol 'symbol)
		    (t t)))
  (clist-pushend (inferer-order inferer) ast))

(defmethod walker:deparse :after ((inferer inferer) (ast walker:application-form))
  (let* ((fun (walker:form-fun ast))
	 (args (walker:form-arguments ast))
	 (arg-types-upper (loop for arg in args collect (result1 (ast-upper arg))))
	 (arg-types-lower (loop for arg in args collect (result1 (ast-lower arg))))
	 (fun-result-upper (fun-result-lookup-upper fun arg-types-upper))
	 (fun-result-lower (fun-result-lookup-lower fun arg-types-lower)))
    (setf (ast-upper ast) fun-result-upper
	  (ast-lower ast) fun-result-lower)))

(defmethod walker:deparse :after ((inferer inferer) (ast walker:var-binding))
  (meet-ast! (walker:form-sym ast) (walker:form-value ast)))

(defmethod walker:deparse :after ((inferer inferer) (ast walker:body-form))
  (let ((last-form (walker:form-body-last ast)))
    (cond
      ((null last-form)
       (set-ast-result ast 'null))
      (t
       (meet-ast! ast last-form)))))

(defmethod walker:deparse :after ((inferer inferer) (ast walker:setq-form))
  (loop for var in (walker:form-vars ast) for value in (walker:form-values ast) do
       (meet-ast! var value))
  (meet-ast! ast (car (last (walker:form-values ast)))))

;;; INFER FUNCTION AND TESTs.

(defun infer (form &key (only-forward t) (rounds -1) (borders :upper-lower) (show :form))
  "ROUNDS<0 infers until type inference doesn't find any sharper types.
ROUNDS=0 only parses and annotates the FORM, but doesn't do any type inference rounds."
  (declare (ignore only-forward))
  (let* ((parser (walker:make-parser :type 'parser-nti))
	 (ast (walker:parse-with-namespace form :parser parser))
	 (inferer (make-instance 'inferer)))
    (do* ((a 0 (1+ a)) (old nil res) (res t (annotate ast)))
	 ((if (minusp rounds) (equal old res) (= a rounds)))
      (setf (inferer-order inferer) (make-clist))
      (setf (inferer-stack inferer) (make-clist))
      (walker:deparse inferer ast)
      ;;(format t "~S~%" (annotate ast :borders borders :show show))
      )
    (annotate ast :borders borders :show show)))

(defun test-infer ()
  (flet ((assert-infer (form desired-result)
	   (let ((actual-result (infer form :borders :upper-lower)))
	     (assert (equal desired-result actual-result) () "TEST-INFER failed for~%form: ~S~%desired-result:~S~%actual-result:~S" form desired-result actual-result))))
    (assert-infer '1 '(the-ul (fixnum fixnum) 1))
    (assert-infer '(1+ 1) '(the-ul (fixnum nil) (1+ (the-ul (fixnum fixnum) 1))))
    (assert-infer '(+ 1 1.0) '(the-ul (single-float nil) (+ (the-ul (fixnum fixnum) 1) (the-ul (single-float single-float) 1.0))))
    (assert-infer '(let ((a 1)) a) '(the-ul (fixnum fixnum) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (fixnum fixnum) a))))
    (assert-infer '(let ((a 1)) (setq a 1.0)) '(the-ul (single-float single-float) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (single-float single-float) (setq a (the-ul (single-float single-float) 1.0))))))
    ;; test IF-FORM join.
    (assert-infer '(if 1 1 1.0) '(the-ul (number number) (if (the-ul (fixnum fixnum) 1) (the-ul (fixnum fixnum) 1) (the-ul (single-float single-float) 1.0))))
    (assert-infer '(let ((a nil)) (if 1 (setq a 1.0) (setq a 1))) '(the-ul (number number) (let ((a (the-ul (null null) nil))) (the-ul (number number) (if (the-ul (fixnum fixnum) 1) (the-ul (single-float single-float) (setq a (the-ul (single-float single-float) 1.0))) (the-ul (fixnum fixnum) (setq a (the-ul (fixnum fixnum) 1))))))))
    (assert-infer '(let ((a nil)) (if (setq a 1) (setq a 1.0)) a) '(the-ul (number number) (let ((a (the-ul (null null) nil))) (the-ul (t t) (if (the-ul (fixnum fixnum) (setq a (the-ul (fixnum fixnum) 1))) (the-ul (single-float single-float) (setq a (the-ul (single-float single-float) 1.0))))) (the-ul (number number) a)))) ;SETQ in TEST-FORM
    (assert-infer '(let ((a 1)) a (if a (setq a 1.0)) a) '(the-ul (number number) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (number number) a) (the-ul (t t) (if (the-ul (number number) a) (the-ul (single-float single-float) (setq a (the-ul (single-float single-float) 1.0))))) (the-ul (number number) a))))
    ;; IF-FORM: check that joining does not join the wrong variables.
    (assert-infer '(if 1 (let ((a 1)) a) (let ((b 1)) b)) '(the-ul (fixnum fixnum) (if (the-ul (fixnum fixnum) 1) (the-ul (fixnum fixnum) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (fixnum fixnum) a))) (the-ul (fixnum fixnum) (let ((b (the-ul (fixnum fixnum) 1))) (the-ul (fixnum fixnum) b))))))
    ;; TAGBODY-FORM
    (assert-infer '(tagbody a (go b) (go a) b) '(the-ul (null null) (tagbody a (the-ul nil nil (go b)) (the-ul (&rest t &rest nil) (go a)) b)))
					;(assert-infer '(let ((a 1)) (tagbody (if a (progn (setq a 1.0) (go a)) a) a) a) )
    ;; check that an empty else-form works.
    (assert-infer '(let ((a 1)) (tagbody (if 1 (progn (setq a 1.0) (go a))) a) a) '(the-ul (number number) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (null null) (tagbody (the-ul (null null) (if (the-ul (fixnum fixnum) 1) (the-ul nil nil (progn (the-ul (single-float single-float) (setq a (the-ul (single-float single-float) 1.0))) (the-ul nil nil (go a)))))) a)) (the-ul (number number) a))))
    ;;(assert-infer '(let ((a 1)) (tagbody (if a (progn (setq a 1.0) (go a))) a) a)
#|    ;; check that the test in IF works
    '(let ((a 1)) (tagbody (if (go a) (setq a 1.0) (setq a 1.0)) a) a)
    (test '(tagbody a (if (go b) (go a) (go a)) b)
    ;; check that :LOOPS are detected, and :ALT works
    (test '(tagbody a (if 1 (go b) (go a)) b)
    ;; check that the GO inside the LET correctly aborts the LET
    (test '(tagbody a (if 1 (let ((x 1)) (go b) 2) (go a)) b)
    (test '(tagbody a (if 1 (let ((x (go b))) 2) (go a)) b)
    ;; check that labels works
    (test '(tagbody a (if 1 (labels ((f () 1)) (go b)) (go a)) b)
    ;; check that detecting dead forms works: the NILs should be dead
    '(tagbody (go a) nil a)
    '(tagbody b (go b) nil)
    '(block a 1 (return-from a) nil) ;the 1 shouln't be dead
    '(block a 1 (return-from a 1) nil) ;the 1s shouln't be dead
    ;; check that function application works
    '(tagbody a (if 1 (labels ((f () (go b))) (f)) (go a)) b)
    '(tagbody a (if 1 (labels ((f () (go b)) (g () (f))) (g)) (go a)) b)
|#
    ))
;;(test-infer)
