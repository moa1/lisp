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

;;; UTILS

(defun id-of (x)
  "This function returns a string that identifies object X. This can be useful for debugging."
  (let* ((s (with-output-to-string (stream)
	      (print-unreadable-object (x stream :type nil :identity t))))
	 (id (subseq s (1+ (position #\< s)) (position #\> s :from-end t))))
    (subseq id (position #\Space id :test-not #'char-equal) (1+ (position #\Space id :test-not #'char-equal :from-end t)))))

(defmacro ignore-warnings (&rest forms)
  `(handler-bind ((warning
		   (lambda (warning)
		     (muffle-warning warning))))
     ,@forms))

(defun copy-hash-table (ht)
  (let ((new (make-hash-table :test (hash-table-test ht) :size (hash-table-size ht) :rehash-size (hash-table-rehash-size ht) :rehash-threshold (hash-table-rehash-threshold ht))))
    (maphash (lambda (k v) (setf (gethash k new) v)) ht)
    new))

(defun last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defmacro pushend (list1 &rest lists)
  "Concatenate LIST1 and all LISTS, using #'NCONC, and set LIST1 to the resulting list."
  `(setf ,list1 (apply #'nconc ,list1 ,@(butlast lists) (list ,(last1 lists)))))

(defmacro pushfront (list-last &rest lists)
  "Concatenate all LISTS and LIST-LAST, using #'NCONC, and set LIST-LAST to the resulting list."
  `(setf ,list-last (apply #'nconc ,@lists (list ,list-last))))

(defun vars-difference (list1 list2 &key (test #'eql) (key1 #'identity) (key2 #'identity))
  "Return the list of elements in LIST1 that are not also in LIST2. The comparison of the elements is done using TEST.
Note that this is the same function performed as #'SET-DIFFERENCE, but it computes the set difference in linear time."
  (let ((ht2 (make-hash-table :test test :size (length list2))))
    (loop for element in list2 do
	 (setf (gethash (funcall key2 element) ht2) nil))
    (let* ((result (cons nil nil))
	   (tail result))
      (loop for element in list1 do
	   (when (gethash (funcall key1 element) ht2 t)
	     (setf (cdr tail) (cons element nil)
		   tail (cdr tail))))
      (cdr result))))

(defun nvars-difference (list1 list2 &key (test #'eql) (key1 #'identity) (key2 #'identity))
  "Return the list of elements in LIST1 that are not also in LIST2, while destructively modifying LIST1. The comparison of the elements is done using TEST.
Note that this is the same function performed as #'NSET-DIFFERENCE, but it computes the set difference in linear time of (+ (LENGTH LIST1) (LENGTH LIST2))."
  (let ((ht2 (make-hash-table :test test :size (length list2))))
    (loop for element in list2 do
	 (setf (gethash (funcall key2 element) ht2) nil))
    (do ((list1cdr list1 (cdr list1cdr))) ((null list1cdr) list1)
      (unless (gethash (funcall key1 (car list1cdr)) ht2 t)
	(setf (car list1cdr) (cadr list1cdr)
	      (cdr list1cdr) (cddr list1cdr))))))

(defun vars-union (list1 list2 &key (test #'eql) (modify nil) (key1 #'identity) (key2 #'identity))
  "Assuming that LIST1 and LIST2 are lists that only contain unique values, compute the list consisting of the union of both sets. This function preserves the order of LIST2. If MODIFY is NIL, LIST1 is not modified, otherwise (the default), LIST1 is modified."
  (let ((ht1 (make-hash-table :test test :size (length list1))))
    (loop for element in list1 do
	 (setf (gethash (funcall key1 element) ht1) nil))
    (funcall (if modify #'nconc #'append)
	     list1
	     (let* ((result (cons nil nil))
		    (tail result))
	       (loop for element in list2 do
		    (when (gethash (funcall key2 element) ht1 t)
		      (setf (cdr tail) (cons element nil)
			    tail (cdr tail))))
	       (cdr result)))))

(defun nvars-union (list1 list2 &key (test #'eql) (key1 #'identity) (key2 #'identity))
  "Assuming that LIST1 and LIST2 are lists that only contain unique values, compute the list consisting of the union of both sets. This function preserves the order of LIST2. LIST1 is not modified."
  (vars-union list1 list2 :test test :modify t :key1 key1 :key2 key2))

;;; BUILT-IN FUNCTIONS

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
  (let ((results-head (nreverse (member-if (lambda (result) (not (eql 'null result)))
					   (reverse results)))))
    (make-results* :nvalues (expt 2 (length results)) :finite (copy-list results-head) :infinite 'null)))

(defun make-results-infinite (result-type)
  (make-results* :nvalues -1 :finite nil :infinite result-type))

(defun is-results-infinite (results infinite-part)
  (and (= (results-nvalues results) -1)
       (eql (results-finite results) nil)
       (eql (results-infinite results) infinite-part)))

(defun make-results-t ()
  "Return a RESULTS instantiation meaning the function returns any number of values, all of type T."
  (make-results-infinite t))

(defun is-results-t (results)
  (is-results-infinite results t))

(defun make-results-nil ()
  "Return a RESULTS instantiation meaning the function returns any number of values, all of type NIL." ;where NIL can legally be only a lower bound. If a function returns NIL, its return type is NULL, not NIL.
  (make-results-infinite nil))

(defun is-results-nil (results)
  (is-results-infinite results nil))

(defun make-results-0 ()
  "Return a RESULTS instantiation meaning that the function does not return."
  (make-results* :nvalues 0))

(defun is-results-0 (results)
  (= (results-nvalues results) 0))

(defun is-results (results1 results2)
  "Return non-NIL if RESULTS1 is equal to RESULTS2, NIL otherwise."
  (and (= (results-nvalues results1) (results-nvalues results2))
       (equal (results-finite results1) (results-finite results2))
       (eql (results-infinite results1) (results-infinite results2))))

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
    (cond
      ((= (results-nvalues object) -1)
       (loop for type in (append (results-finite object) (list (results-infinite object))) do
	    (format stream " ~A" type))
       (format stream "..."))
      (t
       (loop for i below (most-significant-bit (results-nvalues object)) do
	    (format stream " ~A" (if (< i (length (results-finite object)))
				     (elt (results-finite object) i)
				     (results-infinite object))))))))

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
	 (finite (append (loop
			    for i below min-nf
			    for t1 in (results-finite results1)
			    for t2 in (results-finite results2) collect
			      (funcall function t1 t2))
			 (loop for i from min-nf below max-nf collect
			      (let ((t1 (if (>= i nf1)
					    (results-infinite results1)
					    (elt (results-finite results1) i)))
				    (t2 (if (>= i nf2)
					    (results-infinite results2)
					    (elt (results-finite results2) i))))
				(funcall function t1 t2)))))
	 (infinite (funcall function (results-infinite results1) (results-infinite results2)))
	 (nv1 (results-nvalues results1))
	 (nv2 (results-nvalues results2))
	 (nv (funcall nvalues-function nv1 nv2)))
    (flet ((crop-finite (finite)
	     (let* ((last 0))
	       (loop for i from (1- (length finite)) downto 0 do
		    (when (not (equal (elt finite i) infinite)) (setf last (1+ i)) (return)))
	       (subseq finite 0 last))))
      (setf finite (crop-finite finite))
      ;; hack to make (JOIN-RESULTS (MAKE-RESULTS 'FIXNUM) (MAKE-RESULTS-NIL)) == (MAKE-RESULTS 'FIXNUM). This is legal, because (NTH-VALUE X 1) == NULL for X > 0.
      (when (and (< nv 0) (eql infinite 'null))
	(assert (= nv -1) () "If this happens, try to find out what is correct for the next expression instead of (SETF NV (EXPT 2 (LENGTH FINITE))). (SETF NV (LOGXOR NV -1)) doesn't work.")
	(setf nv (expt 2 (length finite))))
      (cond
	((or (and (null finite) (null infinite)) (some #'null finite))
	 (make-results-nil))
	(t
	 (make-results* :nvalues nv :finite finite :infinite infinite))))))

(defun meet-results (results1 results2)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Meet them and return the new RESULTS."
  (process-results results1 results2 #'meet #'logand))

(defun join-results (results1 results2)
  "RESULTS1 and RESULTS2 are each of type RESULTS. Join them and return the new RESULTS."
  (process-results results1 results2 #'join #'logior))

(defun test-meet-join-results ()
  (assert (is-results (meet-results (make-results 'fixnum) (make-results 'single-float))
		      (make-results-nil)))
  (assert (is-results (join-results (make-results 'fixnum) (make-results 'single-float))
		      (make-results 'number)))
  (assert (is-results (meet-results (make-results 'fixnum) (make-results 'number))
		      (make-results 'fixnum)))
  (assert (is-results (join-results (make-results 'fixnum) (make-results 'number))
		      (make-results 'number)))
  (assert (is-results (meet-results (make-results 'fixnum) (make-results-t))
		      (make-results 'fixnum)))
  (assert (is-results (join-results (make-results 'fixnum) (make-results-t))
		      (make-results-t)))
  (assert (is-results (meet-results (make-results 'fixnum) (make-results-nil))
		      (make-results-nil)))
  (assert (is-results (join-results (make-results 'fixnum) (make-results-nil))
		      (make-results 'fixnum)))
  (assert (is-results (meet-results (make-results-t) (make-results-nil))
		      (make-results-nil)))
  (assert (is-results (join-results (make-results-t) (make-results-nil))
		      (make-results-t)))
  (assert (is-results (meet-results (make-results 'fixnum) (make-results-0))
		      (make-results-nil)))
  (assert (is-results (join-results (make-results 'fixnum) (make-results-0))
		      (make-results 'fixnum)))
  (assert (is-results (meet-results (make-results-t) (make-results-0))
		      (make-results-nil)))
  (assert (is-results (meet-results (make-results-nil) (make-results-0))
		      (make-results-nil)))
  (assert (is-results (join-results (make-results-t) (make-results-0))
		      (make-results-t)))
  (assert (is-results (join-results (make-results-nil) (make-results-0))
		      (make-results-nil))))
(test-meet-join-results)

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
  "Push the OBJECT to the end of the clist CL. Returns CL."
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

(defclass bounds ()
  ((upper :initarg :upper :initform (make-results-t) :accessor bounds-upper :documentation "The upper bound of the form.")
   (lower :initarg :lower :initform (make-results-nil) :accessor bounds-lower :documentation "The lower bounds of the form.")))

(defmethod print-object ((bounds bounds) stream)
  (print-unreadable-object (bounds stream :type t :identity t)
    (format stream "upper:~S lower:~S" (bounds-upper bounds) (bounds-lower bounds))))

(defun make-bounds (upper lower)
  (make-instance 'bounds :upper upper :lower lower))

(defclass userproperties ()
  ((parser-prev :initarg :parser-prev :accessor userproperties-parser-prev :documentation "The parser before the form was parsed.")
   (parser-next :initarg :parser-next :accessor userproperties-parser-next :documentation "The parser after the form was parsed.")
   (form :initarg :form :accessor userproperties-form :documentation "Form-specific properties")
   (bounds :initform (make-instance 'bounds) :initarg :bounds :accessor userproperties-bounds :type bounds :documentation "The upper and lower bound of the form.")))

(defun make-userproperties* (&key parser-prev parser-next form (bounds (make-instance 'bounds)))
  (make-instance 'userproperties :parser-prev parser-prev :parser-next parser-next :form form :bounds bounds))

(defmethod print-object ((user userproperties) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (let ((bounds (userproperties-bounds user)))
      (format stream "~S" bounds))))

(defun ast-parser-prev (ast)
  (userproperties-parser-prev (walker:user ast)))
(defun (setf ast-parser-prev) (value ast)
  (setf (userproperties-parser-prev (walker:user ast)) value))
(defun ast-parser-next (ast)
  (userproperties-parser-next (walker:user ast)))
(defun (setf ast-parser-next) (value ast)
  (setf (userproperties-parser-next (walker:user ast)) value))
(defun ast-form (ast)
  (userproperties-form (walker:user ast)))
(defun (setf ast-form) (value ast)
  (setf (userproperties-form (walker:user ast)) value))

(defmethod ast-bounds ((ast walker:form))
  (userproperties-bounds (walker:user ast)))
(defmethod ast-bounds ((ast walker:sym))
  (userproperties-bounds (walker:user ast)))
(defmethod ast-bounds ((ast walker:var-binding))
  (userproperties-bounds (walker:user ast)))
(defmethod ast-bounds ((ast walker:var-reading))
  (userproperties-bounds (walker:user ast)))
(defmethod ast-bounds ((ast walker:var-writing))
  (userproperties-bounds (walker:user ast)))
(defmethod ast-bounds ((ast walker:tagpoint))
  (userproperties-bounds (walker:user ast)))

(defmethod (setf ast-bounds) (value (ast walker:form))
  (setf (userproperties-bounds (walker:user ast)) value))
(defmethod (setf ast-bounds) (value (ast walker:sym))
  (setf (userproperties-bounds (walker:user ast)) value))
(defmethod (setf ast-bounds) (value (ast walker:var-binding))
  (setf (userproperties-bounds (walker:user ast)) value))
(defmethod (setf ast-bounds) (value (ast walker:var-reading))
  (setf (userproperties-bounds (walker:user ast)) value))
(defmethod (setf ast-bounds) (value (ast walker:var-writing))
  (setf (userproperties-bounds (walker:user ast)) value))
(defmethod (setf ast-bounds) (value (ast walker:tagpoint))
  (setf (userproperties-bounds (walker:user ast)) value))

(defun mapc-vars-namespaces (function namespaces)
  "FUNCTION is a function of the variables stored in the NAMESPACES."
  (apply #'mapc
	 (lambda (&rest vars)
	   (assert (loop for var in (cdr vars) always (eql (walker:nso-name (car vars)) (walker:nso-name var))))
	   (apply function vars))
	 namespaces))

(defun mapc-namespaces (function namespaces)
  "FUNCTION is a function of the variables stored in the namespaces of ASTS."
  (mapc-vars-namespaces function
			(mapcar (lambda (namespace) (mapcar #'cdr namespace))
				(mapcar #'walker:namespace-var namespaces))))

(defun mapc-asts (function asts)
  "FUNCTION is a function of the variables stored in the namespaces of ASTS."
  (mapc-namespaces function (mapcar (lambda (ast) (walker:parser-lexical-namespace (ast-parser-next ast))) asts))
  (mapc-namespaces function (mapcar (lambda (ast) (walker:parser-free-namespace (ast-parser-next ast))) asts)))

(defun meet-bounds (bounds-target bounds2)
  "Meet the bounds BOUNDS-TARGET and BOUNDS2 and return the met bounds."
  (let ((new-upper (meet-results (bounds-upper bounds-target) (bounds-upper bounds2)))
	;; meet (BOUNDS-UPPER BOUNDS-TARGET) again
	(new-lower (meet-results (bounds-upper bounds-target) (bounds-lower bounds2))))
    (make-bounds new-upper new-lower)))

(defun join-bounds (&rest asts-bounds)
  "Join the bounds ASTS-BOUNDS and return the joined bounds."
  (let ((asts-upper (mapcar #'bounds-upper asts-bounds))
	(asts-lower (mapcar #'bounds-lower asts-bounds)))
    (let ((new-upper (reduce #'join-results (cdr asts-upper) :initial-value (car asts-upper)))
	  (new-lower (reduce #'join-results (cdr asts-lower) :initial-value (car asts-lower))))
      (make-bounds new-upper new-lower))))

;; (defun meet-ast! (ast-target ast2)
;;   "Meet the results of AST-TARGET and AST2 and store the result in AST-TARGET."
;;   (setf (ast-bounds ast-target) (meet-bounds (ast-bounds ast-target) (ast-bounds ast2))))
;;
;; (defun join-ast! (ast-target asts)
;;   "Join the bounds of ASTS, meet the resulting bounds with AST-TARGET's bounds, and store them there."
;;   (let* ((new-bounds (apply #'join-bounds (mapcar #'ast-bounds asts))))
;;     (setf (ast-bounds ast-target) (meet-bounds (ast-bounds ast-target) new-bounds))))
;;
;; (defun meet-namespaces! (ast-target ast2)
;;   (mapc-asts (lambda (target-var ast2-var)
;; 	       (setf (ast-bounds target-var) (meet-bounds (ast-bounds target-var) (ast-bounds ast2-var))))
;; 	     (list ast-target ast2)))

(defun set-namespaces! (ast1 ast2 &key (key1 #'identity) (key2 #'identity))
  "Set the namespace AST1 to AST2."
  ;; should maybe be named SET-NAMESPACE!, but MEET-NAMESPACES! and JOIN-NAMESPACES! are plural.
  (labels ((set-var! (ast1-var ast2-var)
	     (setf (ast-bounds ast1-var) (ast-bounds ast2-var)))
	   (mapc-namespaces-call (namespace-fun)
	     (mapc-namespaces
	      #'set-var!
	      (mapcar namespace-fun (list (funcall key1 ast1) (funcall key2 ast2))))))
    (mapc-namespaces-call #'walker:parser-lexical-namespace)
    (mapc-namespaces-call #'walker:parser-free-namespace)))
(defun set-namespaces-prev-next! (ast1 ast2)
  (set-namespaces! ast1 ast2 :key1 #'ast-parser-prev :key2 #'ast-parser-next))
(defun set-namespaces-next-next! (ast1 ast2)
  (set-namespaces! ast1 ast2 :key1 #'ast-parser-next :key2 #'ast-parser-next))

(defun join-namespaces! (target-parser parsers)
  "Join the common namespaces of PARSERS and set the result to TARGET-PARSER."
  (flet ((join! (target-var &rest join-vars)
	   ;; it should be correct to set (AST-UPPER TARGET-VAR) directly to the join instead of the meet of TARGET-VAR with the join of the new namespaces? If it isn't then how does an IF-FORM prevent only being able to calculate subtypes of the variable types (of variables in the namespace) before the IF-FORM?
	   (setf (ast-bounds target-var) (apply #'join-bounds (mapcar #'ast-bounds join-vars))))) ;TODO: meet the joined bounds with the declared bounds of TARGET-VAR. (This must be done also at MULTIPLE-VALUE-BIND, and everywhere a variable is set.)
    (mapc-namespaces #'join! (cons (walker:parser-lexical-namespace target-parser)
				   (mapcar #'walker:parser-lexical-namespace parsers)))
    (mapc-namespaces #'join! (cons (walker:parser-free-namespace target-parser)
				   (mapcar #'walker:parser-free-namespace parsers)))))

;;; NTIPARSER

(defclass ntiparser (walker:parser)
  ())

(defmethod walker:copy-parser ((parser ntiparser))
  (make-instance 'ntiparser
		 :lexical-namespace (walker:parser-lexical-namespace parser)
		 :free-namespace (walker:parser-free-namespace parser)))

(defun parser-redefine-var! (parser symbol new-var)
  "Redefine the variable defined for SYMBOL in PARSER to be NEW-VAR. Return the modified PARSER."
  (let ((lexical-namespace (walker:namespace-var (walker:parser-lexical-namespace parser)))
	(free-namespace (walker:namespace-var (walker:parser-free-namespace parser))))
    (let ((cons-lexical (assoc symbol lexical-namespace :test #'equal))
	  (cons-free (assoc symbol free-namespace :test #'equal)))
      (cond
	(cons-lexical
	 (setf (cdr cons-lexical) new-var))
	(cons-free
	 (setf (cdr cons-free) new-var))
	(t (error "Var ~S is neither in lexical nor in free namespace" symbol)))))
  parser)

(defun copy-var (parser var)
  (walker:make-ast parser 'walker:var :name (walker:nso-name var) :freep (walker:nso-freep var) :definition (walker:nso-definition var) :sites (walker:nso-sites var) :declspecs (walker:nso-declspecs var) :macrop (walker:nso-macrop var))) ;slot USER is set by the overridden #'MAKE-AST

(defun prepare-frankensteined-parser! (parser)
  "Modify PARSER in-place with its namespaces deep-copied until and including aconses, but excluding the contents of the aconses."
  (let ((newparser (walker:copy-deep-parser parser)))
    (setf (walker:parser-lexical-namespace parser) (walker:parser-lexical-namespace newparser)
	  (walker:parser-free-namespace parser) (walker:parser-free-namespace newparser))))

(defun postpare-frankensteined-namespaces! (parser branches-parsers)
  "Modify PARSER in-place so that a VAR differing in any of the parsers BRANCHES-PARSERS namespaces will be replaced by a new copy. This means the contents of the aconses of the namespaces may be changed."
  (flet ((postpare-namespace! (parser ast-namespace branches-namespace)
	   (mapc-vars-namespaces (lambda (new-var &rest branches-vars)
				   (unless (loop for var in (cdr branches-vars) always (eql var (car branches-vars)))
				     (let* ((name (walker:nso-name new-var))
					    (new-var (copy-var parser new-var)))
				       (parser-redefine-var! parser name new-var))))
				 (cons (mapcar #'cdr (walker:namespace-var ast-namespace))
				       (loop for ast in branches-namespace collect
					    (mapcar #'cdr (walker:namespace-var ast)))))))
    (postpare-namespace! parser
			 (walker:parser-lexical-namespace parser)
			 (mapcar #'walker:parser-lexical-namespace branches-parsers))
    (postpare-namespace! parser
			 (walker:parser-free-namespace parser)
			 (mapcar #'walker:parser-free-namespace branches-parsers))))

(defmethod make-userproperties ((ast walker:application-form) (parser-next ntiparser) parser-prev parent)
  ;; If this is a recursive call, make a note in the userproperties (or the callstack or something like that), that the type bounds of this call have to be looked up at runtime. Insert the type (MAKE-BOUNDS (MAKE-RESULTS-0) (MAKE-RESULTS-0)), so that another branch of an IF-FORM in the function gets to set the type bounds. (If there is no IF-FORM, then the function really loops forever, or calls another function, which must have an IF-FORM, otherwise there would be an infinite loop through the two functions.)
  ;; The idea in the line above is not enough, since there could be a function which modifies lexical variables outside the function's scope, as in (LET ((A 1)) (FLET ((FB () (SETQ A 1.0))) (FLET ((FA () (FB))) (FA))) A). After the application-form (FA), the variable A must have type SINGLE-FLOAT.
  ;; It could be even worse, because functions defined in a LABELS-form can call each other, like in (LET ((A 1)) (LABELS ((FA () (IF 1 (FB) (FC))) (FB () (IF 2 (SETQ A 1.0) (IF 3 (FA) (FB)))) (FC () (IF 4 (FA) (FB)))) (FA))). As a human, you can see that the SETQ-form is the only form that is not (directly or indirectly) recursive. But it would be complicated (or impossible?) to construct a parser that is both functional and has CLOS-style :AROUND methods. It would have to allow being called on any of the three functions, like in (PARSE '(FA () (IF 1 (FB) (FC)))), and allow the :AROUND method to access the (FB) APPLICATION-form's - already parsed - body of #'FB. Therefore I think the current PARSE-interface of package WALKER is okay, but I have to think of another way to circumvent the chicken-egg problem here.
  ;; I need to have a way to access the (WALKER:FORM-FUN AST)'s source so that I can parse it anew and put the parsed representation into USERPROPERTIES's FORM slot. I could extend package WALKER so that it defines a SOURCE slot for every NSO- and FORM-object, which is bound for all functions defined in a LABELS-form before parsing the body.
  ;; Secondly, #'FIND-EXITS is called in #'MAKE-USERPROPERTIES, when called on the function body of FA, FB, or FC. And #'FIND-EXITS has to work on the finished BODY-parsings of those functions (except slot USER), because its returned value (e.g. a GO-FORM, or a VAR-READ-FORM) must be a form from the finished parsing. So parsing must be finished (except slot USER), but #'MAKE-USERPROPERTIES (and #'FIND-EXITS) is called during a not-yet-finished parsing of the bodies of FA, FB, or FC because they can call each other. This doesn't work.
  ;; In the FA-FB-FC-example above, in any of those functions' bodies, the type of variable A must be defined before and after every form, because there could be a form that needs it, like (NULL A), or (IF X (RETURN-FROM Y A)).
  (cond
    ((not (find-builtin-function (walker:nso-name (walker:form-fun ast))))
     (cond
       ((walker:form-recursivep ast)
	nil)
       (t
	;; If this is a non-recursive call, copy the FUN-BINDING and store the copy in USERPROPERTIES.
	(let* ((fun-source (walker:nso-source (walker:form-fun ast)))
	       (labels-fun-source (list 'labels (list fun-source)))
	       (labels-ast (walker:parse parser-prev labels-fun-source parent))
	       (fun-binding-ast-copy (walker:form-binding-1 labels-ast)))
	  ;; Note that (WALKER:FORM-FUN AST) is different from (WALKER:FORM-SYM FUN-BINDING-AST-COPY).
	  (make-userproperties* :parser-prev parser-next :parser-next parser-next :form fun-binding-ast-copy)))))))

(defmethod walker:make-ast :around ((parser ntiparser) (type (eql 'walker:var-writing)) &rest args)
  (declare (optimize (debug 3)))
  ;; modify PARSER in-place, because we want the changes to persist in the forms after the VAR-WRITING.
  (let* ((parser-prev (walker:copy-parser parser))
	 (var-tail (member :var args))
	 (old-var (cadr var-tail))
	 (new-var (copy-var parser old-var)))
    (assert (not (null var-tail)))
    (prepare-frankensteined-parser! parser)
    (parser-redefine-var! parser (walker:nso-name old-var) new-var)
    (setf (cadr var-tail) new-var)
    (let* ((args (progn (setf (cadr (member :var args)) new-var) args))
	   (ast (apply #'call-next-method parser type args)))
      (setf (walker:user ast)
	    (make-userproperties* :parser-prev parser-prev :parser-next (walker:copy-parser parser) :form old-var))
      ast)))

;; userproperties have already been set by WALKER:PARSE-FORM :AROUND.
(defmethod make-userproperties ((ast walker:setq-form) parser-next parser-prev parent)
  ;; the WRITE-VARs have been modified by #'WALKER:MAKE-AST :AROUND above, but we still need to correct all references to them.
  (loop for write-var in (walker:form-vars ast) do
       (let* ((new-var (walker:form-var write-var))
	      (old-var (ast-form write-var)))
	 ;; correct the NSO-SITES.
	 (setf (walker:nso-sites old-var) (remove ast (walker:nso-sites old-var)))
	 (push ast (walker:nso-sites new-var))))
    (make-userproperties* :parser-prev parser-prev :parser-next parser-next))

(defmethod make-userproperties ((ast walker:if-form) (parser-next ntiparser) parser-prev parent)
  "Redefine in PARSER-NEXT the variables that are defined as different variables in the namespaces of the branches of AST. Return NIL."
  ;; modify PARSER-NEXT in-place where some of its VAR-namespace may be frankensteined.
  (let* ((test-exits (find-exits (make-instance 'exit-finder) (walker:form-test ast)))
	 (then-exits (when (normal-exits test-exits) (find-exits (make-instance 'exit-finder) (walker:form-then ast))))
	 (else-exits (when (and (normal-exits test-exits) (walker:form-else ast)) (find-exits (make-instance 'exit-finder) (walker:form-else ast))))
	 (branches (cond
		     ((not (normal-exits test-exits))
		      test-exits)
		     ((walker:form-else ast)
		      (nconc then-exits else-exits))
		     (t
		      then-exits))))
    ;; Up to now, PARSER-NEXT of TEST-FORM is the same as PARSER-NEXT of AST(i.e. this IF-FORM). If we keep it this way, then updating PARSER-NEXT of AST will modify PARSER-NEXT of TEST. This would screw up VAR types (it would retrospectively change types in TEST-FORM). We have to make PARSER-NEXT of AST a new copy with completely new variables if any of them is different in the TEST-, THEN-, or ELSE-form.
    (let ((parser-join (walker:copy-deep-parser parser-next)))
      (prepare-frankensteined-parser! parser-join)
      (postpare-frankensteined-namespaces! parser-join (cons parser-prev (mapcar #'ast-parser-next branches)))
      ;; Note that PARSER-PREV and PARSER-NEXT of FORM-THEN and FORM-ELSE have been deep-copied in #'WALKER:PARSE-FORM of 'IF.
      (prepare-frankensteined-parser! parser-next)
      (make-userproperties* :parser-prev parser-prev :parser-next parser-next :form parser-join))))

(defmethod walker:make-ast :around ((parser ntiparser) (type (eql 'walker:tagpoint)) &rest args)
  (let ((parser-prev (walker:copy-parser parser))
	(parser-join (walker:copy-deep-parser parser)))
    (prepare-frankensteined-parser! parser-join)
    (prepare-frankensteined-parser! parser)
    (let* ((ast (apply #'call-next-method parser type args)))
      (setf (walker:user ast) (make-userproperties* :parser-prev parser-prev :parser-next (walker:copy-parser parser) :form parser-join))
      ast)))

;; userproperties have already been set by WALKER:PARSE-FORM :AROUND.
(defmethod make-userproperties ((ast walker:tagbody-form) parser-next parser-prev parent)
  (loop for tag in (walker:form-tags ast) do
       (let* ((tagpoint (car (walker:nso-gopoint tag)))
	      (tagpoint-parser-prev (ast-parser-prev tagpoint))
	      (parser-join (ast-form tagpoint))
	      (branches (mapcar #'ast-parser-next (remove tagpoint (walker:nso-sites tag)))))
	 (postpare-frankensteined-namespaces! parser-join (cons tagpoint-parser-prev branches))))
  (make-userproperties* :parser-prev parser-prev :parser-next parser-next))

(defmethod make-userproperties (ast (parser-next ntiparser) parser-prev parent)
  (make-userproperties* :parser-prev parser-prev :parser-next parser-next))

(defmethod walker:parse :around ((parser ntiparser) form parent)
  (let* ((parser-prev (walker:copy-parser parser))
	 (ast (call-next-method parser form parent))
	 (parser-next (walker:copy-parser parser)))
    (setf (walker:user ast) (make-userproperties ast parser-next parser-prev parent))
    ast))

(defmethod walker:make-ast :around ((parser ntiparser) type &rest arguments)
  (declare (optimize (debug 3)))
  (declare (ignore arguments))
  (let ((ast (call-next-method)))
    (setf (walker:user ast) (make-userproperties* :parser-prev (walker:copy-parser parser) :parser-next (walker:copy-parser parser)))
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
   (let* ((bounds (ast-bounds ast))
	  (u (bounds-upper bounds))
	  (l (bounds-lower bounds))
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

#|
;;; INFERER

(defclass inferer (walker:deparser)
  ((stack :initarg :stack :initform (make-clist) :accessor inferer-stack :documentation "A stack of ASTs, started to be processed. and finished or unfinished.")
   (order :initarg :order :initform (make-clist) :accessor inferer-order :documentation "A CLIST containing the order the forms were evaluated in during the forward pass.")))

;; INFER AROUND METHODS: save the order the ASTs are evaluated in and track the liveness of ASTs.

(defun set-ast-result (ast type)
  (let ((type (make-results type)))
    (setf (ast-upper ast) type
	  (ast-lower ast) type)))

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
		(clist-pushend stack ast) ;must come before #'CALL-NEXT-METHOD
		(unless (null (clist-last order))
		  (meet-namespaces! ast (clist-last order)))
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

;;(defmethod walker:deparse :around ((inferer inferer) (ast walker:var-binding))


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
		  (prind form (annotate form))
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
		  (when next-form
		    ;;(meet-namespaces! next-form form)
		    (when (find next-form tag-gopoints)
		      (push form (gethash next-form inside-exits))))))))
    (let ((last (walker:form-body-last ast)))
      (setf (ast-exits ast)
	    (nconc (when (or (null last) (find last (ast-exits-normal last)))
		     (list ast))
		   outside-exits))
      (meet-namespaces! ast last)))
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
    (loop for form-cdr on (walker:form-body ast) do
	 (let ((form (car form-cdr))
	       (next-form (cadr form-cdr)))
	   (walker:deparse inferer form)
	   (when next-form
	     ;;(meet-namespaces! next-form form)
	     (let ((abnormal-exits (remove form (ast-exits-abnormal form))))
	       (setf outside-exits (nconc (remove-if (lambda (exit)
						       (and (eql exit (walker:form-blo ast))
							    (push form inside-exits)))
						     abnormal-exits)
					  outside-exits))
	       (when (not (find form (ast-exits-normal form)))
		 (return))))))
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
	  (else (if else else (walker:make-ast (walker:make-parser :type 'ntiparser) 'walker:object-form :object nil))))
      (when test-exits
	(walker:deparse (make-instance 'inferer) then) ;new INFERER for consistency with the ELSE branch.
	(walker:deparse (make-instance 'inferer) else)) ;new INFERER so that (AST-EXITS ELSE) doesn't return NIL if THEN-EXITS==NIL.
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
    
(defmethod walker:deparse ((inferer inferer) (ast walker:object-form))
  ;; maybe move this to #'WALKER:PARSE, which would save some time but a person reading code wouldn't know where to look for.
  (clist-pushend (inferer-stack inferer) ast)
  (setf (ast-exits ast) (list ast))
  ;; this must set the sharpest bound possible.
  (set-ast-result ast
		  (etypecase (walker:form-object ast)
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
  (let* ((parser (walker:make-parser :type 'ntiparser))
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
    ;; check that an empty else-form works.
    (assert-infer '(let ((a 1)) a (if a (setq a 1.0)) a) '(the-ul (number number) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (number number) a) (the-ul (t t) (if (the-ul (number number) a) (the-ul (single-float single-float) (setq a (the-ul (single-float single-float) 1.0))))) (the-ul (number number) a))))
    ;; IF-FORM: check that joining does not join the wrong variables.
    (assert-infer '(if 1 (let ((a 1)) a) (let ((b 1)) b)) '(the-ul (fixnum fixnum) (if (the-ul (fixnum fixnum) 1) (the-ul (fixnum fixnum) (let ((a (the-ul (fixnum fixnum) 1))) (the-ul (fixnum fixnum) a))) (the-ul (fixnum fixnum) (let ((b (the-ul (fixnum fixnum) 1))) (the-ul (fixnum fixnum) b))))))
    ;; TAGBODY-FORM
    (assert-infer '(tagbody a (go b) (go a) b) '(the-ul (null null) (tagbody a (the-ul nil nil (go b)) (the-ul (&rest t &rest nil) (go a)) b)))
    ;;(assert-infer '(let ((a 1)) (tagbody (if a (progn (setq a 1.0) (go a)) a) a) a) )
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
|#

;;; FIND EXITS

(defclass exit-finder ()
  ((callstack :initarg :callstack :initform nil :accessor finder-callstack :documentation "A call stack used to abort recursive APPLICATION-FORMs.")
   (warn-dead :initarg :warn-dead :initform t :accessor finder-warn-dead :documentation "Whether to show dead form warnings or not.")))

(defgeneric find-exits (exit-finder ast)
  (:documentation "Return the abstract syntax tree within the given AST, which is the form that determinesthe returned result of AST.
EXIT-FINDER is an instance of class EXIT-FINDER and stores information shared between the forms."))

(defun normal-exits (exits)
  (remove-if (lambda (exit)
	       (or (typep exit 'walker:go-form)
		   (typep exit 'walker:return-from-form)))
	     exits))

(defun jumping-exits (exits)
  (remove-if (lambda (exit)
	       (not (or (typep exit 'walker:go-form)
			(typep exit 'walker:return-from-form))))
	     exits))

(define-condition nti-condition ()
  ())
(define-condition dead-form-warning (simple-warning nti-condition)
  ((form :initarg :form :accessor condition-form :type walker:form))
  (:report (lambda (condition stream)
	     (format stream "Form ~A~%cannot ever be reached." (walker:deparse (make-instance 'walker:deparser) (condition-form condition)))))
  (:documentation "Warning that FORM will not be evaluated."))
(defmacro warn-dead-form (exit-finder form)
  `(when (finder-warn-dead ,exit-finder)
     (restart-case (warn (make-condition 'dead-form-warning :form ,form))
       (muffle-warning ()
	 nil))))

;; Forms (in the same order as exported from packages WALKER and WALKER-PLUS)

(defmethod find-exits ((exit-finder exit-finder) (ast walker:var-reading))
  (list ast))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:object-form))
  (list ast))

(defun find-exits-forms-list (exit-finder ast forms)
  "Return the list of forms consisting of 1. the last evaluated form in FORMS, or 2. a jump out of FORMS."
  (let ((ast-exits nil))
    (loop for form in (butlast forms) do
	 (let* ((form-exits (find-exits exit-finder form)))
	   (pushend ast-exits (jumping-exits form-exits))
	   (unless (normal-exits form-exits)
	     (loop for dead in (cdr (member form forms)) do
		  (warn-dead-form exit-finder dead))
	     (return-from find-exits-forms-list form-exits))))
    (let* ((last-form (last1 forms))
	   (last-form-exits (if last-form (find-exits exit-finder last-form) (list ast))))
      (pushend ast-exits last-form-exits)
      ast-exits)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:body-form))
  ;; The NIL for NEXT-METHOD-P means that subtypes of AST below WALKER:BODY-FORM are not called using #'FIND-EXITS.
  (find-exits-forms-list exit-finder ast (walker:form-body ast)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:function-form))
  (list ast))

;; PROGN-FORM is handled by BODY-FORM.

(defmethod find-exits ((exit-finder exit-finder) (ast walker:var-bindings-form))
  (find-exits-forms-list exit-finder ast (nconc (mapcar #'walker:form-value (walker:form-bindings ast)) (walker:form-body ast))))

(defun find-exits-functiondef (exit-finder funbinding arguments)
  (let ((callstack (finder-callstack exit-finder)))
    (cond
      ((find funbinding callstack)
       nil) ;this is a recursive call(-loop) of(between) function(s)
      (t
       (push funbinding (finder-callstack exit-finder))
       (let* ((llist (walker:form-llist funbinding))
	      (parser (make-instance 'walker:parser)) ;FIXME? needed by ARGUMENTS-ASSIGN-TO-LAMBDA-LIST
	      (arg-alist (walker-plus:arguments-assign-to-lambda-list parser llist arguments))
	      (ast-exits nil))
	 (loop for acons in arg-alist for acons-rest on arg-alist do
	      (let* ((form (cdr acons))
		     (form-exits (find-exits exit-finder form)))
		(pushend ast-exits (jumping-exits form-exits))
		(unless (normal-exits form-exits)
		  (loop for form-acons in (cdr acons-rest) do
		       (warn-dead-form exit-finder (cdr form-acons)))
		  (loop for form in (walker:form-body funbinding) do
		       (warn-dead-form exit-finder form))
		  (return-from find-exits-functiondef ast-exits))))
	 (let* ((body (walker:form-body funbinding))
		(body-exits (find-exits-forms-list exit-finder funbinding body)))
	   (pushend ast-exits body-exits))
	 (pop (finder-callstack exit-finder))
	 ast-exits)))))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:fun-bindings-form))
  ;; only have to process the body, i.e. pass to (FIND-EXITS (EXIT-FINDER EXIT-FINDER) (AST BODY-FORM)).
  (call-next-method))

;; LET-FORM and LET*-FORM are handled by VAR-BINDINGS-FORM and BODY-FORM.

(defmethod find-exits ((exit-finder exit-finder) (ast walker:return-from-form))
  (list ast))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:block-form))
  (let ((blo (walker:form-blo ast))
	(ast-exits (find-exits-forms-list exit-finder ast (walker:form-body ast)))
	(local-returns nil))
    (setf ast-exits
	  (remove-if (lambda (exit)
		       (when (and (typep exit 'walker:return-from-form)
				  (eql (walker:form-blo exit) blo))
			 (setf local-returns t)))
		     ast-exits))
    (when (or (null ast-exits) local-returns)
      (pushend ast-exits (list ast)))
    ast-exits))

;; FLET-FORM and LABELS-FORM are handled by FUN-BINDINGS-FORM and BODY-FORM.

(defmethod find-exits ((exit-finder exit-finder) (ast walker:lambda-form))
  (warn "TODO FIXME: the Lisp code (TAGBODY S (LET ((F (LAMBDA () (GO A)))) (FUNCALL F)) (GO S) A) is not handled correctly.")
  (list ast))

;; LOCALLY-FORM is handled by BODY-FORM.

(defmethod find-exits ((exit-finder exit-finder) (ast walker:the-form))
  (find-exits exit-finder (walker:form-value ast)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:if-form))
  (let* ((test-form (walker:form-test ast))
	 (then-form (walker:form-then ast))
	 (else-form (walker:form-else ast))
	 (test-exits (find-exits exit-finder test-form)))
    (cond
      ((null (normal-exits test-exits))
       (warn-dead-form exit-finder then-form)
       (when else-form (warn-dead-form exit-finder else-form))
       test-exits)
      (t
       (let ((then-exits (find-exits exit-finder then-form))
	     (else-exits (when else-form (find-exits exit-finder else-form))))
	 (nconc (if else-form (jumping-exits test-exits) test-exits)
		then-exits
	        else-exits))))))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:setq-form))
  (find-exits-forms-list exit-finder ast (walker:form-values ast)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:catch-form))
  (error "TODO: all subforms of AST that are a THROW-FORM potentially jump to this CATCH-FORM (AST), so we should merge the types of this CATCH-FORM with the types of those forms")
  (find-exits-forms-list exit-finder ast (walker:form-values ast)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:throw-form))
  (error "TODO: look above at CATCH-FORM."))

;; EVAL-WHEN-FORM should be handled by BODY-FORM.

(defmethod find-exits ((exit-finder exit-finder) (ast walker:load-time-value-form))
  (find-exits exit-finder (walker:form-value ast)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:quote-form))
  (list ast))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:multiple-value-call-form))
  (error "TODO: the control flow first evaluates the FUNCTION-form to get back the function (named function or LAMBDA) and then goes to the BODY-FORMs to get the parameters and then the FUNCTION is called with the concatenated list of parameters. Example test cases: (TAGBODY S (FLET ((F (&OPTIONAL (A (GO E))) (GO S))) (MULTIPLE-VALUE-CALL F)) E) exits through tag E and (TAGBODY S (FLET ((F (&OPTIONAL (A (GO E))) (GO S))) (MULTIPLE-VALUE-CALL F 1)) E) doesn't exit."))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:multiple-value-prog1-form))
  (let ((prog1-exits (find-exits exit-finder (walker:form-values ast))))
    (if (normal-exits prog1-exits)
	(nconc (jumping-exits prog1-exits) (call-next-method))
	prog1-exits)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:progv-form))
  (let ((symbols-exits (find-exits exit-finder (walker:form-symbols ast))))
    (if (normal-exits symbols-exits)
	(let ((values-exits (find-exits exit-finder (walker:form-values ast))))
	  (if (normal-exits values-exits)
	      (nconc (jumping-exits symbols-exits) (jumping-exits values-exits) (call-next-method))
	      (nconc (jumping-exits symbols-exits) values-exits)))
	symbols-exits)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:unwind-protect-form))
  (error "TOOD: (see CLHS UNWIND-PROTECT) due to protecting the PROTECTED-FORM, the UNWIND-PROTECT-FORM can transfer control after any form or subform consisting of GO-, HANDLER-CASE-, IGNORE-ERRORS-, RESTART-CASE-, RETURN-FROM-, THROW-, WITH-SIMPLE-RESTART-form to the CLEANUP-FORM (which is called BODY-FORM in WALKER)."))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:application-form))
  (let ((arguments (walker:form-arguments ast))
	(funobj (walker:function-object (walker:form-fun ast))))
    (etypecase funobj
      (walker:lambda-form
       (find-exits-functiondef exit-finder funobj arguments))
      (walker:fun
       (find-exits-functiondef exit-finder (walker:nso-definition funobj) arguments)))))

;; MACROAPPLICATION-FORM, SYMBOL-MACROLET-FORM, and MACROLET-FORM don't have to be implemented, since evaluation of the program starts after all macros have been expanded.

(defmethod find-exits ((exit-finder exit-finder) (ast walker:tagpoint))
  (list ast))

(defun go-form-jumps-inside-ast (form ast)
  (and (typep form 'walker:go-form)
       (find (walker:form-tag form) (walker:form-tags ast))))

(defun go-form-jumps-outside-ast (form ast)
  (and (typep form 'walker:go-form)
       (not (find (walker:form-tag form) (walker:form-tags ast)))))

(defun visit-tagbody-form-exits (exit-finder ast form-function)
  "Visits the alive forms of AST and calls FORM-FUNCTION on them.
EXIT-FINDER must be an instance of class EXIT-FINDER.
FORM-FUNCTION must be a function of two parameters FORM, the currently processed form, and FORM-EXITS, its exits.
Returns the list of exits of AST's last form, or NIL if this form cannot ever be executed (is dead)."
  (declare (optimize (debug 3)))
  (let ((goforms (list (walker:form-body ast))) ;a list of list of forms inside AST that are jumped to.
	(visited (make-hash-table))
	(last-form (walker:form-body-last ast)))
    (loop while (not (null goforms)) do
	 (let* ((goforms0 (pop goforms)))
	   (loop for form in goforms0 do
		(when (gethash form visited) ;prevent infinite loops
		  (return))
		(setf (gethash form visited) t)
		(if (typep form 'walker:tagpoint)
		    (funcall form-function form (find-exits exit-finder form))
		    (let* ((form-exits (find-exits exit-finder form)))
		      (loop for exit in form-exits do
			   (when (go-form-jumps-inside-ast exit ast)
			     (push (walker:nso-gopoint (walker:form-tag exit)) goforms)))
		      (funcall form-function form form-exits)
		      (unless (normal-exits form-exits) ;skip the rest of GOFORMS0
			(return)))))))
    (loop for form in (walker:form-body ast) do
	 (unless (gethash form visited nil)
	   (warn-dead-form exit-finder form)))
    (and (gethash last-form visited) (find-exits exit-finder last-form))))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:tagbody-form))
  (declare (optimize (debug 3)))
  (let ((ast-exits nil)) ;the list of forms in AST that jump outside AST
    (flet ((form-function (form form-exits)
	     (declare (ignore form))
	     (pushend ast-exits
		      (remove-if (lambda (exit) ;keep exits that jump outside AST
				   (cond
				     ((go-form-jumps-inside-ast exit ast)
				      t)
				     ((not (null (jumping-exits (list exit))))
				      nil) ;this includes GO-forms jumping to a tag outside AST.
				     (t
				      t)))
				 form-exits))))
      (let ((last-form-exits (visit-tagbody-form-exits exit-finder ast #'form-function)))
	(when (or (null (walker:form-body ast)) (normal-exits last-form-exits))
	  (pushend ast-exits (list ast)))
	ast-exits))))

(defmethod find-exits ((exit-finder exit-finder) (ast walker:go-form))
  (list ast))

(defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:multiple-value-bind-form))
  (find-exits-forms-list exit-finder ast (cons (walker:form-values ast) (walker:form-body ast))))

(defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:values-form))
  (find-exits-forms-list exit-finder ast (walker:form-values ast)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:nth-value-form))
  (find-exits-forms-list exit-finder ast (list (walker:form-value ast) (walker:form-values ast))))

;;TODO: (defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:defun-form)))

(defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:funcall-form))
  (let ((arguments (walker:form-arguments ast)))
    (cond
      ((typep (walker:form-var ast) 'walker:lambda-form)
       (find-exits-functiondef exit-finder (walker:form-var ast) arguments))
      ((let ((var (walker:form-var ast))) (and (typep var 'walker:function-form) (typep (walker:form-object var) 'walker:lambda-form)))
       (find-exits-functiondef exit-finder (walker:form-object (walker:form-var ast)) arguments))
      ((let ((var (walker:form-var ast))) (and (typep var 'walker:function-form) (typep (walker:form-object var) 'walker:fun)))
       (find-exits-functiondef exit-finder (walker:nso-definition (walker:form-var ast)) arguments))
      (t
       (error "TODO")))))

;;TODO: (defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:assert-form)))

;; TEST FIND-EXITS

(defclass capturing-parser (walker:parser)
  ((container :initarg :container :initform (make-hash-table) :accessor parser-container :documentation "A hash-table that contains all captured forms.")))
(defun copy-capturing-parser (parser)
  (make-instance (type-of parser)
		 :free-namespace (walker:parser-free-namespace parser)
		 :lexical-namespace (walker:parser-lexical-namespace parser)
		 :container (parser-container parser)))
(defmethod walker:copy-parser ((parser capturing-parser))
  (copy-capturing-parser parser))

(defclass capturing-parser-plus (walker-plus:parser-plus)
  ((container :initarg :container :initform (make-hash-table) :accessor parser-container :documentation "A hash-table that contains all captured forms.")))
(defmethod walker:copy-parser ((parser capturing-parser-plus))
  (copy-capturing-parser parser))

(defmacro capture (name form)
  "This macro is here just to aid in constructing test cases in #'TEST-FIND-EXITS."
  ;; FIXME: Capturing a TAG form inside a TAGBODY-form doesn't work. It will create a VAR-READING.
  (declare (ignore name))
  form)

(defun parse-capture-form (parser head rest parent)
  (assert (and (consp rest) (null (cdddr rest))) () "Cannot parse CAPTURE-form ~S" (cons head rest))
  (let ((name (car rest))
	(form (cadr rest)))
    (let ((ast (walker:parse parser form parent)))
      (setf (gethash name (parser-container parser)) ast)
      ast)))
(defmethod walker:parse-form ((parser capturing-parser) (head (eql 'capture)) rest parent source)
  (declare (ignore source))
  (parse-capture-form parser head rest parent))
(defmethod walker:parse-form ((parser capturing-parser-plus) (head (eql 'capture)) rest parent source)
  (declare (ignore source))
  (parse-capture-form parser head rest parent))

(defun capturing-parse (form)
  (let* (;;(capturing-parser (make-instance 'capturing-parser))
	 (capturing-parser (make-instance 'capturing-parser-plus))
	 (ast (walker:parse-with-namespace form :parser capturing-parser)))
    (values ast (parser-container capturing-parser))))

(defun test-find-exits-form (form)
  (let* ((ast (walker:parse-with-namespace form :parser (make-instance 'walker-plus:parser-plus))))
    (find-exits (make-instance 'exit-finder) ast)))

(defun test-find-exits ()
  (flet ((assert-find-exit (form desired-exits &optional desired-dead-forms)
	   (multiple-value-bind (ast container) (capturing-parse form)
	     (flet ((get-captured (exits)
		      (loop for exit in exits collect
			   (if (symbolp exit)
			       (let ((r (gethash exit container nil)))
				 (if r r (error "Symbol ~S is not captured. Captured symbols are ~S" exit (loop for k being the hash-key of container collect k))))
			       (error "Unknown exit symbol ~S" exit))))
		    (find-exits-and-dead-forms (ast)
		      (let ((dead-forms nil))
			(values (handler-bind ((dead-form-warning
						(lambda (warning)
						  (push (condition-form warning) dead-forms)
						  (muffle-warning warning))))
				  (find-exits (make-instance 'exit-finder) ast))
				(nreverse dead-forms)))))
	       (let ((desired-exits-1 (get-captured desired-exits))
		     (desired-dead-forms-1 (get-captured desired-dead-forms)))
		 (multiple-value-bind (actual-exits actual-dead-forms) (find-exits-and-dead-forms ast)
		   (assert (equal actual-exits desired-exits-1) () "(FIND-EXIT ~S)~%returned ~S,~%but expected ~S~%" form actual-exits desired-exits-1)
		   (assert (equal actual-dead-forms desired-dead-forms-1) () "(FIND-EXIT ~S)~%found dead forms ~S,~%but expected ~S~%" form actual-dead-forms desired-dead-forms)))))))
    (assert-find-exit '(capture a 1) '(a))
    (assert-find-exit '(capture a a) '(a))
    (assert-find-exit '(progn (capture a 1)) '(a))
    (assert-find-exit '(capture a (progn)) '(a))
    (assert-find-exit '(progn (progn (capture a 1))) '(a))
    (assert-find-exit '(if (capture a (go a)) (capture b 1) (capture c 2)) '(a) '(b c))
    (assert-find-exit '(if (capture a 1) (capture b (go a))) '(a b))
    (assert-find-exit '(if 1 (capture a (go a)) (capture b (go a))) '(a b))
    (assert-find-exit '(if 1 (capture a 2) (capture b 3)) '(a b))
    (assert-find-exit '(if (if 1 (capture a (go a)) t) (capture b 2) (capture c 3)) '(a b c))
    (assert-find-exit '(if (if 1 (capture a (go a))) (capture b 2) (capture c 3)) '(a b c))
    (assert-find-exit '(if (if 1 (progn (capture a (go a)))) (capture b 2) (capture c 3)) '(a b c))
    (assert-find-exit '(if 1 (progn (capture a (go e))) (capture b 2)) '(a b))
    (assert-find-exit '(if 1 (if 1 (capture a (go e)) (capture b 2)) (capture c 2)) '(a b c))
    (assert-find-exit '(capture a (tagbody)) '(a))
    (assert-find-exit '(capture a (tagbody s (if 1 (go s) (go e)) e)) '(a))
    (assert-find-exit '(capture a (tagbody (if (go e) (capture b 1)) e)) '(a) '(b))
    (assert-find-exit '(tagbody (if (capture a (go a)) (capture b (go e))) (capture c e)) '(a) '(b c))
    (assert-find-exit '(capture a (tagbody (if 1 (capture b (go a)) (go e)) e)) '(b a))
    (assert-find-exit '(tagbody (if 1 (capture a (go a)) (capture b (go b)))) '(a b))
    (assert-find-exit '(tagbody (if 1 (capture a (go a)) (capture b (go a)))) '(a b))
    (assert-find-exit '(tagbody (capture a (go e)) (capture b (progn 2))) '(a) '(b))
    (assert-find-exit '(tagbody (progn (capture a (go e))) (capture b (progn 1))) '(a) '(b))
    (assert-find-exit '(capture a (tagbody (if 1 (go a) (if 2 (go b))) (capture z (go z)) a b)) '(z a))
    (assert-find-exit '(tagbody (if 1 (go a) (if 2 (go b))) (capture z (go z)) a b (go a)) '(z))
    (assert-find-exit '(tagbody (if 1 (go a) (if 2 (go b))) (capture z (return-from z)) a b (go a)) '(z))
    (assert-find-exit '(capture a (block nil (return-from nil))) '(a))
    (assert-find-exit '(capture a (block nil)) '(a))
    (assert-find-exit '(block nil (capture a (return-from x))) '(a))
    (assert-find-exit '(block nil (if 1 (capture a (return-from x)) (capture b (return-from x)))) '(a b))
    (assert-find-exit '(capture a (block nil (if (capture b 1) (return-from nil)))) '(b a))
    (assert-find-exit '(flet ((fa () (capture a 1))) (fa)) '(a))
    (assert-find-exit '(labels ((fa () (if 1 (capture a 1) (fa)))) (fa)) '(a))
    (assert-find-exit '(labels ((fa () (fb)) (fb () (capture a 1))) (fa)) '(a))
    (assert-find-exit '(labels ((fa (x) (if x (fa x) (capture a 1)))) (fa 1)) '(a))
    (assert-find-exit '(labels ((fa (x) (if x (fb x) (capture a 1))) (fb (x) (fa x))) (fa 1)) '(a))
    (assert-find-exit '(labels ((fa () (fa) (capture a 1))) (fa)) '() '(a))
    (assert-find-exit '(labels ((fa (&optional (x (capture a (go a))) (y (capture b 1))) (capture c 1))) (fa)) '(a) '(b c))
    (assert-find-exit '(labels ((fa (&optional (x (capture a (fa)))) (capture b 1))) (fa)) '() '(b))
    (assert-find-exit '((lambda () (capture a 1))) '(a))
    (assert-find-exit '(multiple-value-bind (a b) (values 2 3) a (capture a b)) '(a))
    (assert-find-exit '(multiple-value-bind (a b) (capture a (go a)) (capture b a)) '(a) '(b))
    (assert-find-exit '(funcall (lambda () (capture a 1))) '(a))
    (assert-find-exit '(funcall #'(lambda () (capture a 1))) '(a))
    (assert-find-exit '(flet ((f1 () (capture a 1))) (funcall #'f1)) '(a))
    (assert-find-exit '(nth-value 1 (values 1 (capture a 2))) '(a))
    (assert-find-exit '(nth-value (capture a (go a)) (capture b (values 1 2))) '(a) '(b))
    (assert-find-exit '(nth-value 1 (values 1 (capture a (go a)))) '(a))
    ))

(test-find-exits)

;;; FIND VAR-READINGs and VAR-WRITINGs

(defclass accesses-finder ()
  ((exit-finder :initarg :exit-finder :initform (make-instance 'exit-finder) :accessor finder-exit-finder :documentation "The exit-finder used to compute dead and alive forms.")
   (callstack :initarg :callstack :initform nil :accessor finder-callstack :documentation "A call stack used to abort recursive APPLICATION-FORMs.")))

(defgeneric find-accesses (accesses-finder ast)
  (:documentation "Return two values: the list of read variables defined outside AST that determine the evaluation (computation) of AST, and the list of written variables defined outside AST that are changed as a result of evaluating AST.
Note that a variable can be in both lists, for example as in (+ A (SETQ A 2)). The variable A is defined outside the AST, its state before the AST influences the result of the AST (it is read), and it is changed (written) in the process. In the following example, however, A is only in the list of written variables: (+ (SETQ A 2) A). Although A is read from, its state before the AST does not influence the computation of the AST, because its value is first overwritten. (For the implementor of #'FIND-ACCESSES, this means that if a variable A is already in the written-to list, and a subform of the current AST returns that A is read from, A is not included in the ASTs read-from list.) (This also applies when two variables are swapped, because a variable's values must first be read, as in (SETQ T A A B B T). #'FIND-ACCESSES here returns A and B as both read and written to, and T only written to.)
ACCESSES-FINDER is an instance of class ACCESSES-FINDER and stores information shared between the forms."))

(defmacro find-accesses-update! (read0 written0 read1 written1)
  (declare (type symbol read0 written0))
  ;; A variable that has already been written to overwrote the state of a read-from-in-FORM variable.
  `(setf ,read0 (nvars-union ,read0
			     (nvars-difference ,read1 ,written0))
	 ,written0 (nvars-union ,written0 ,written1)))

(defun find-accesses-form (finder read0 written0 form)
  "Assuming that READ0 and WRITTEN0 are the lists of variables already read and written, determine the list of read and written variables after FORM is executed and return the updated lists. Also return the exits of FORM."
  (declare (type list read0 written0)
	   (type accesses-finder finder)
	   (type walker:form form))
  ;; a jumping exit within FORM will exclude the following forms from being evaluated.
  (multiple-value-bind (read1 written1) (find-accesses finder form)
    (let ((exits (find-exits (finder-exit-finder finder) form)))
      (find-accesses-update! read0 written0 read1 written1)
      (values exits read0 written0))))

(defmacro find-accesses-form! (finder read0 written0 form)
  "Updates the list of already read and already written variables, READ0 and WRITTEN0, with the variable accesses done by FORM."
  (declare (type (or symbol cons) read0 written0)) ;may also be accessors
  (let ((read1-sym (gensym "READ1"))
	(written1-sym (gensym "WRITTEN1"))
	(exits-sym (gensym "EXITS")))
    `(multiple-value-bind (,exits-sym ,read1-sym ,written1-sym)
	 (find-accesses-form ,finder ,read0 ,written0 ,form)
       (setf ,read0 ,read1-sym
	     ,written0 ,written1-sym)
       (values ,exits-sym ,read0 ,written0))))

(defun find-accesses-forms-list (finder read0 written0 forms-list)
  "Assuming that READ0 and WRITTEN0 are the lists of variables already read and written, determine the list of read and written variables after the list of forms FORMS-LIST are executed and return the updated lists."
  (let (exits)
    (loop for form in forms-list do
	 (setq exits (find-accesses-form! finder read0 written0 form))
	 (unless (normal-exits exits)
	   (return-from find-accesses-forms-list (values exits read0 written0))))
    (values exits read0 written0)))

(defmacro find-accesses-forms-list! (finder read0 written0 forms-list)
  "Updates the list of already read and already written variables, READ0 and WRITTEN0, with the variable accesses done by FORMS-LIST."
  (declare (type (or symbol cons) read0 written0)) ;may also be accessors
  (let ((exits-sym (gensym "EXITS"))
	(read1-sym (gensym "READ1"))
	(written1-sym (gensym "WRITTEN1")))
    `(multiple-value-bind (,exits-sym ,read1-sym ,written1-sym)
	 (find-accesses-forms-list ,finder ,read0 ,written0 ,forms-list)
       (setf ,read0 ,read1-sym
	     ,written0 ,written1-sym)
       (values ,exits-sym ,read0 ,written0))))

(defmacro no-exits (find-accesses-forms-list-values)
  (let ((exits-sym (gensym "EXITS"))
	(read0-sym (gensym "READ0"))
	(written0-sym (gensym "WRITTEN0")))
    `(multiple-value-bind (,exits-sym ,read0-sym ,written0-sym) ,find-accesses-forms-list-values
       (declare (ignore ,exits-sym))
       (values ,read0-sym ,written0-sym))))

;; Forms (in the same order as exported from packages WALKER and WALKER-PLUS)

(defmethod find-accesses ((finder accesses-finder) (ast walker:var-reading))
  (values (list ast) nil))

(defmethod find-accesses ((finder accesses-finder) (ast walker:object-form))
  (values nil nil))

(defmethod find-accesses ((finder accesses-finder) (ast walker:body-form))
  (no-exits (find-accesses-forms-list finder nil nil (walker:form-body ast))))

(defmethod find-accesses ((finder accesses-finder) (ast walker:function-form))
  (values (list ast) nil))

;; PROGN-FORM is handled by BODY-FORM.

(defmethod find-accesses ((finder accesses-finder) (ast walker:var-bindings-form))
  (let ((init-values (loop for binding in (walker:form-bindings ast) collect
			  (let ((value (walker:form-value binding)))
			    (if (null value) (walker:make-nil binding) value)))))
    (multiple-value-bind (exits read written) (find-accesses-forms-list finder nil nil init-values)
      (cond
	((normal-exits exits)
	 (find-accesses-forms-list! finder read written (walker:form-body ast))
	 (let ((init-vars (mapcar #'walker:form-sym (walker:form-bindings ast))))
	   (values (vars-difference read init-vars :key1 #'walker:form-var)
		   (vars-difference written init-vars :key1 #'walker:form-var))))
	(t
	 (values read written))))))

;; this is analogous to #'FIND-EXITS-FUNCTIONDEF.
(defun find-accesses-functiondef (finder funbinding arguments)
  (declare (type accesses-finder finder))
  (let ((callstack (finder-callstack finder))
	(exit-finder (finder-exit-finder finder)))
    (cond
      ((find funbinding callstack)
       nil) ;this is a recursive call(-loop) of(between) function(s)
      (t
       (push funbinding (finder-callstack finder))
       (let* ((llist (walker:form-llist funbinding))
	      (parser (make-instance 'walker:parser)) ;FIXME? needed by ARGUMENTS-ASSIGN-TO-LAMBDA-LIST
	      (arg-alist (walker-plus:arguments-assign-to-lambda-list parser llist arguments))
	      (read nil)
	      (written nil))
	 (flet ((return-values (read written)
		  (let* ((arg-vars (mapcar #'car arg-alist))
			 (read0 (vars-difference read arg-vars :key1 #'walker:form-var))
			 (written0 (vars-difference written arg-vars :key1 #'walker:form-var)))
		    (return-from find-accesses-functiondef (values read0 written0)))))
	   (loop for acons in arg-alist for acons-rest on arg-alist do
		(let* ((form (cdr acons))
		       (form-exits (find-exits exit-finder form)))
		  (find-accesses-form! finder read written form)
		  (unless (normal-exits form-exits)
		    (return-values read written))))
	   (find-accesses-forms-list! finder read written (walker:form-body funbinding))
	   (pop (finder-callstack finder))
	   (return-values read written)))))))

(defmethod find-accesses ((finder accesses-finder) (ast walker:fun-bindings-form))
  ;; only have to process the body.
  (no-exits (find-accesses-forms-list finder nil nil (walker:form-body ast))))

;; LET-FORM and LET*-FORM are handled by VAR-BINDINGS-FORM and BODY-FORM.

(defmethod find-accesses ((finder accesses-finder) (ast walker:return-from-form))
  (no-exits (find-accesses-form finder nil nil (walker:form-value ast))))

(defmethod find-accesses ((finder accesses-finder) (ast walker:block-form))
  (no-exits (find-accesses-forms-list finder nil nil (walker:form-body ast))))

;; FLET-FORM and LABELS-FORM are handled by FUN-BINDINGS-FORM.

(defmethod find-accesses ((finder accesses-finder) (ast walker:lambda-form))
  (warn "TODO FIXME: the Lisp code (TAGBODY (LET ((F (LAMBDA () (GO A)))) (FUNCALL F)) (SETQ X 1) A) is not handled correctly.")
  (values nil nil))

;; LOCALLY-FORM is handled by BODY-FORM.

(defmethod find-accesses ((finder accesses-finder) (ast walker:the-form))
  (find-accesses finder (walker:form-value ast)))

(defmethod find-accesses ((finder accesses-finder) (ast walker:if-form))
  (let* ((read nil)
	 (written nil)
	 (test-exits (find-accesses-form! finder read written (walker:form-test ast))))
    (cond
      ((null (normal-exits test-exits))
       (values read written))
      (t
       (find-accesses-form! finder read written (walker:form-then ast))
       (when (walker:form-else ast) (find-accesses-form! finder read written (walker:form-else ast)))
       (values read written)))))

(defmethod find-accesses ((finder accesses-finder) (ast walker:setq-form))
  (let ((read0 nil)
	(written0 nil))
    (loop for var-writing in (walker:form-vars ast) for value in (walker:form-values ast) do
	 (let ((exits (find-exits (finder-exit-finder finder) value)))
	   (cond
	     ((normal-exits exits)
	      (multiple-value-bind (value-read value-written) (find-accesses finder value)
		(find-accesses-update! read0 written0 value-read value-written))
	      (setf written0 (vars-union written0 (list var-writing))))
	     (t
	      (return-from find-accesses (values read0 written0))))))
    (values read0 written0)))

(defmethod find-accesses ((finder accesses-finder) (ast walker:catch-form))
  (find-accesses finder (walker:form-values ast)))

(defmethod find-accesses ((finder accesses-finder) (ast walker:throw-form))
  (find-accesses finder (walker:form-value ast)))

;; EVAL-WHEN-FORM should be handled by BODY-FORM.

(defmethod find-accesses ((finder accesses-finder) (ast walker:load-time-value-form))
  (find-accesses finder (walker:form-value ast)))

(defmethod find-accesses ((finder accesses-finder) (ast walker:quote-form))
  (values nil nil))

(defmethod find-accesses ((finder accesses-finder) (ast walker:multiple-value-call-form))
  (error "TODO: we need value prediction for the following form: (TAGBODY (FLET ((F (&OPTIONAL (A (GO E))) (GO S))) (MULTIPLE-VALUE-CALL F)) (SETQ X 1) E) doesn't set X, but (TAGBODY (FLET ((F (&OPTIONAL (A (GO E))) (GO S))) (MULTIPLE-VALUE-CALL F 1)) (SETQ X 1) E) sets X"))

(defmethod find-accesses ((finder accesses-finder) (ast walker:multiple-value-prog1-form))
  (no-exits (find-accesses-forms-list finder nil nil (cons (walker:form-values ast)
							   (walker:form-body ast)))))

(defmethod find-accesses ((finder accesses-finder) (ast walker:progv-form))
  (error "TODO: (tagbody (progv (if 1 (go e) '(a b)) (list 1 2) (setq x 1)) e)"))

(defmethod find-accesses ((finder accesses-finder) (ast walker:unwind-protect-form))
  (error "TODO: the body of the unwind-protect-form can transfer control out of the AST, which triggers evaluating the cleanup-form (see CLHS UNWIND-PROTECT)."))

(defmethod find-accesses ((finder accesses-finder) (ast walker:application-form))
  (let ((arguments (walker:form-arguments ast))
	(funobj (walker:function-object (walker:form-fun ast))))
    (etypecase funobj
      (walker:lambda-form
       (find-accesses-functiondef finder funobj arguments))
      (walker:fun
       (find-accesses-functiondef finder (walker:nso-definition funobj) arguments)))))

;; MACROAPPLICATION-FORM, SYMBOL-MACROLET-FORM, and MACROLET-FORM don't have to be implemented, since evaluation of the program starts after all macros have been expanded.

(defmethod find-accesses ((finder accesses-finder) (ast walker:tagpoint))
  (values nil nil))

(defmethod find-accesses ((finder accesses-finder) (ast walker:tagbody-form))
  (let ((exit-finder (finder-exit-finder finder))
	(read nil)
	(written nil))
    (flet ((form-function (form form-exits)
	     (declare (ignore form-exits))
	     (find-accesses-form! finder read written form)))
      (visit-tagbody-form-exits exit-finder ast #'form-function))
    (values read written)))

(defmethod find-accesses ((finder accesses-finder) (ast walker:go-form))
  (values nil nil))

(defmethod find-accesses ((finder accesses-finder) (ast walker-plus:multiple-value-bind-form))
  (let ((read0 nil)
	(written0 nil))
    (let ((exits0 (find-accesses-form! finder read0 written0 (walker:form-values ast))))
      (cond
	((null (normal-exits exits0))
	 (values read0 written0))
	(t
	 (multiple-value-bind (exits1 read1 written1)
	     (find-accesses-forms-list finder nil nil (walker:form-body ast))
	   (declare (ignore exits1))
	   (let ((locals (mapcar #'walker:form-var (walker:form-vars ast))))
	     (values (vars-union read0
				 (vars-difference read1 locals :key1 #'walker:form-var))
		     (vars-union written0
				 (vars-difference written1 locals :key1 #'walker:form-var))))))))))

(defmethod find-accesses ((finder accesses-finder) (ast walker-plus:values-form))
  (no-exits (find-accesses-forms-list finder nil nil (walker:form-values ast))))

(defmethod find-accesses ((finder accesses-finder) (ast walker-plus:nth-value-form))
  (no-exits (find-accesses-forms-list finder nil nil (list (walker:form-value ast) (walker:form-values ast)))))

;;TODO: (defmethod find-accesses ((finder accesses-finder) (ast walker-plus:defun-form)))

(defmethod find-accesses ((finder accesses-finder) (ast walker-plus:funcall-form))
  (let ((arguments (walker:form-arguments ast)))
    (cond
      ((typep (walker:form-var ast) 'walker:lambda-form)
       (find-accesses-functiondef finder (walker:form-var ast) arguments))
      ((let ((var (walker:form-var ast))) (and (typep var 'walker:function-form) (typep (walker:form-object var) 'walker:lambda-form)))
       (find-accesses-functiondef finder (walker:form-object (walker:form-var ast)) arguments))
      ((let ((var (walker:form-var ast))) (and (typep var 'walker:function-form) (typep (walker:form-object var) 'walker:fun)))
       (find-accesses-functiondef finder (walker:nso-definition (walker:form-var ast)) arguments))
      (t
       (error "TODO")))))

;;TODO: (defmethod find-exits ((exit-finder exit-finder) (ast walker-plus:assert-form)))

;;; TEST FIND VAR-READINGs and VAR-WRITINGs

(defun test-find-accesses-form (form)
  (let* ((parser (make-instance 'walker-plus:parser-plus))
	 (ast (walker:parse-with-namespace form :parser parser)))
    (find-accesses (make-instance 'accesses-finder) ast)))

(defun test-find-accesses ()
  (flet ((assert-find-accesses (form desired-read desired-written)
	   (let* ((parser (make-instance 'walker-plus:parser-plus))
		  (ast (walker:parse-with-namespace form :parser parser))
		  (finder (make-instance 'accesses-finder :exit-finder (make-instance 'exit-finder :warn-dead nil))))
	     (labels ((lookup-desired (name)
			(flet ((sym-lookup (type name parser)
				 (let ((free-namespace (walker:parser-free-namespace parser)))
				   (if (walker:namespace-boundp type name free-namespace)
				       (walker:namespace-lookup type name free-namespace)
				       (error "Free variable ~S does not occur in ~S" name form)))))
			  (if (symbolp name)
			      (sym-lookup 'walker:var name parser)
			      (sym-lookup 'walker:fun (cadr name) parser))))
		      (lookup-actual (sym)
			(etypecase sym
			  (walker:var-reading (walker:form-var sym))
			  (walker:var-writing (walker:form-var sym))
			  (walker:function-form (walker:form-object sym)))))
	       (multiple-value-bind (actual-read actual-written) (find-accesses finder ast)
		 (assert (equal (mapcar #'lookup-actual actual-read) (mapcar #'lookup-desired desired-read)) () "(FIND-ACCESSES ~S)~%returned read ~S,~%but expected ~S~%" form actual-read desired-read)
		 (assert (equal (mapcar #'lookup-actual actual-written) (mapcar #'lookup-desired desired-written)) () "(FIND-ACCESSES ~S)~%returned written ~S,~%but expected ~S~%" form actual-written desired-written))))))
    (assert-find-accesses '1 '() '())
    (assert-find-accesses 'a '(a) '())
    (assert-find-accesses '#'+ '(#'+) '())
    (assert-find-accesses '(let ((a 1)) a) '() '())
    (assert-find-accesses '(setq x 1) '() '(x))
    (assert-find-accesses '(setq x y) '(y) '(x))
    (assert-find-accesses '(let (t) (setq t x x y y t)) '(x y) '(x y))
    (assert-find-accesses '(let* ((a 1) (b a)) b) '() '())
    (assert-find-accesses '(flet ((f (a) (setq x a))) (f 1)) '() '(x))
    (assert-find-accesses '(flet ((f (&optional (a (setq x 1))) 1)) (f)) '() '(x))
    (assert-find-accesses '(flet ((f (&key (a (setq x 1))) 1)) (f)) '() '(x))
    (assert-find-accesses '(labels ((f (a) (if 1 (g (setq x a)) (h))) (g (a) y) (h () (f 2))) (f 1)) '(y) '(x))
    (assert-find-accesses '(tagbody (setq x 1)) '() '(x))
    (assert-find-accesses '(tagbody (setq x 1) (go a) a) '() '(x))
    (assert-find-accesses '(tagbody (go a) (setq x 1) a) '() '())
    (assert-find-accesses '(tagbody (labels ((f (&optional (a (go a))) (setq x 1))) (f)) a) '() '())
    (assert-find-accesses '(tagbody (labels ((f (&optional (a (go a))) (setq x 1))) (f 1)) a) '() '(x))
    (assert-find-accesses '(tagbody (labels ((f () (go a) (setq x 1))) (f)) a) '() '())
    (assert-find-accesses '(tagbody (labels ((f () (setq x 1) (go a))) (f)) a) '() '(x))
    (assert-find-accesses '(block b (setq x 1) (return-from b 1)) '() '(x))
    (assert-find-accesses '(block b (return-from b 1) (setq x 1)) '() '())
    (assert-find-accesses '(locally (setq x 1)) '() '(x))
    (assert-find-accesses '(quote x) '() '())
    (assert-find-accesses '(multiple-value-bind (x y) (values 1 2) (setq x 1)) '() '())
    (assert-find-accesses '(multiple-value-bind (x y) (values (setq x 1) 2)) '() '(x))
    (assert-find-accesses '(multiple-value-bind (x y) (values (setq x 1) 2) (setq y 3)) '() '(x))
    (assert-find-accesses '(nth-value 1 (values 1 2)) '() '())
    (assert-find-accesses '(nth-value (setq x 1) (values 1 2)) '() '(x))
    (assert-find-accesses '(nth-value 1 (values (setq x 1) 2)) '() '(x))
    ))

(test-find-accesses)

;;TODO: I have to know which forms of the TAGBODY are the last form before a TAG and are alive, so that I can merge their namespaces in.

;;; INFER THE FORWARD PASS.

(defclass fwd-inferer ()
  ((exit-finder :initarg :exit-finder :initform (make-instance 'exit-finder) :accessor inferer-exit-finder)
   (callstack :initarg :callstack :initform nil :accessor inferer-callstack)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:var-reading))
  (ast-bounds (walker:form-var ast)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:object-form))
  (let* ((type (etypecase (walker:form-object ast)
		 (fixnum 'fixnum)
		 (float 'single-float)
		 (null 'null)
		 (boolean 'boolean) ;must be after NULL
		 (symbol 'symbol)
		 (t t)))
	 (upper (make-results type))
	 (lower (make-results type)))
    (make-bounds upper lower)))

(defun fwd-infer-list (fwd-inferer forms)
  (let ((value (make-bounds (make-results 'null) (make-results 'null))))
    (loop for form in forms do
	 (let ((form-exits (find-exits (inferer-exit-finder fwd-inferer) form)))
	   (setf value (fwd-infer fwd-inferer form))
	   (unless (normal-exits form-exits)
	     (return))))
    value))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:progn-form))
  (fwd-infer-list fwd-inferer (walker:form-body ast)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:var-bindings-form))
  (if (loop for binding in (walker:form-bindings ast) do
	   (let* ((var (walker:form-sym binding))
		  (value (walker:form-value binding))
		  (value-exits (find-exits (inferer-exit-finder fwd-inferer) value)))
	     (if (normal-exits value-exits)
		 (setf (ast-bounds var) (fwd-infer fwd-inferer value)) ;TODO: meet (AST-BOUNDS VALUE) with the declared bounds of VAR. (This must be done also at MULTIPLE-VALUE-BIND, and everywhere a variable is set.)
		 (progn (fwd-infer fwd-inferer value)
			(return nil))))
	 finally (return t))
      (fwd-infer-list fwd-inferer (walker:form-body ast))
      (make-bounds (make-results-0) (make-results-0))))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:setq-form))
  (loop for write-var in (walker:form-vars ast) for value in (walker:form-values ast) do
       (let ((var (walker:form-var write-var))
	     (value-exits (find-exits (inferer-exit-finder fwd-inferer) value)))
	 (if (normal-exits value-exits)
	     (setf (ast-bounds var) (fwd-infer fwd-inferer value)) ;TODO: meet (AST-BOUNDS VALUE) with the declared bounds of VAR. (This must be done also at MULTIPLE-VALUE-BIND, and everywhere a variable is set.)
	     (progn (fwd-infer fwd-inferer value)
		    (return (make-bounds (make-results-0) (make-results-0))))))
     finally (return (ast-bounds (walker:form-var (last1 (walker:form-vars ast)))))))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:if-form))
  (let* ((test-exits (find-exits (inferer-exit-finder fwd-inferer) (walker:form-test ast)))
	 (then-exits (when (normal-exits test-exits) (find-exits (inferer-exit-finder fwd-inferer) (walker:form-then ast))))
	 (else-exits (when (and (normal-exits test-exits) (walker:form-else ast)) (find-exits (inferer-exit-finder fwd-inferer) (walker:form-else ast))))
	 (parser-join (ast-form ast))
	 (then-bounds (make-bounds (make-results-0) (make-results-0)))
	 (else-bounds (make-bounds (make-results-0) (make-results-0))))
    (fwd-infer fwd-inferer (walker:form-test ast))
    (when (normal-exits test-exits)
      (set-namespaces-prev-next! (walker:form-then ast) (walker:form-test ast))
      (setf then-bounds (fwd-infer fwd-inferer (walker:form-then ast))))
    (if (walker:form-else ast)
	(when (and (normal-exits test-exits))
	  (set-namespaces-prev-next! (walker:form-else ast) (walker:form-test ast))
	  (setf else-bounds (fwd-infer fwd-inferer (walker:form-else ast))))
	(setf else-bounds (make-bounds (make-results 'null) (make-results 'null))))
    (let ((branches (if (normal-exits test-exits)
			(append then-exits (if else-exits else-exits test-exits))
			test-exits)))
      (join-namespaces! parser-join (mapcar #'ast-parser-next branches)))
    (set-namespaces! (ast-parser-next ast) parser-join)
    (if (normal-exits test-exits)
	(join-bounds then-bounds else-bounds)
	(make-bounds (make-results-0) (make-results-0)))))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:go-form))
  (make-bounds (make-results-0) (make-results-0)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:tagpoint))
  (make-bounds (make-results-nil) (make-results-nil)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:tagbody-form))
  ;; First, find all exits that reach a TAGPOINT either by normally exiting the previous form before the TAGPOINT, or by jumping to it from a JUMPING-EXIT of one of ASTs forms. We do not have to care about GO-FORMs or RETURN-FROM-FORMs that jump outside the AST, since they are handled by their parents (i.e. the TAGBODY-FORM that has the GO-FORM's tag, or the BLOCK-FORM that has the RETURN-FROM-FORM's blo).
  (let ((tags-prev-namespaces (make-hash-table)))
    ;; If the first form is a tagpoint. set its previous-form namespace to AST-PARSER-PREV of AST.
    (when (typep (walker:form-body-1 ast) 'walker:tagpoint)
      (push (ast-parser-prev ast) (gethash (walker:form-body-1 ast) tags-prev-namespaces)))
    (visit-tagbody-form-exits (inferer-exit-finder fwd-inferer)
			       ast
			       (lambda (form form-exits)
				 (let ((next-form (cadr (member form (walker:form-body ast)))))
				   (when (and (typep next-form 'walker:tagpoint) (normal-exits form-exits)) ;if the NEXT-FORM is a tagpoint, and FORM has at least one normal exit, add prev-namespace of FORM
				     (push (ast-parser-next form) (gethash next-form tags-prev-namespaces))))
				 (loop for exit in form-exits do
				      (when (go-form-jumps-inside-ast exit ast)
					(let* ((tag (walker:form-tag exit))
					       (tagpoint (car (walker:nso-gopoint tag)))
					       (exit-parser (ast-parser-next exit)))
					  (push exit-parser (gethash tagpoint tags-prev-namespaces)))))))
    ;; Second, compute the next-namespaces of all forms, and, when reaching a TAGPOINT, assign its prev-namespace to the join of all incoming exits computed in the first step.
    (let ((last-exits
	   (visit-tagbody-form-exits (inferer-exit-finder fwd-inferer)
				     ast
				     (lambda (form form-exits)
				       (declare (ignore form-exits))
				       (when (typep form 'walker:tagpoint)
					 (let ((parser-join (ast-form form))
					       (branches (gethash form tags-prev-namespaces)))
					   (join-namespaces! parser-join branches)
					   (set-namespaces! (ast-parser-next form) parser-join)))
				       (fwd-infer fwd-inferer form)))))
      (cond
	((null (walker:form-body ast))
	 (set-namespaces! ast ast :key1 #'ast-parser-next :key2 #'ast-parser-prev))
	(t
	 (join-namespaces! (ast-parser-next ast) (mapcar #'ast-parser-next (normal-exits last-exits)))))))
  (make-bounds (make-results 'null) (make-results 'null)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:fun-bindings-form))
  (let ((body (walker:form-body ast)))
    (fwd-infer-list fwd-inferer body)))

(defmethod fwd-infer ((fwd-inferer fwd-inferer) (ast walker:application-form))
  (let* ((arguments (walker:form-arguments ast))
	 (exit-finder (inferer-exit-finder fwd-inferer))
	 (funbinding (let ((funobj (walker:function-object (walker:form-fun ast))))
		       (etypecase funobj
			 (walker:lambda-form
			  funobj)
			 (walker:fun
			  (walker:nso-definition funobj))))))
    (cond
      ((find funbinding (inferer-callstack fwd-inferer)) ;this is a recursive call(-loop) of(between) function(s)
       (make-bounds (make-results-0) (make-results-0)))
      (t
       (push funbinding (inferer-callstack fwd-inferer))
       (let* ((llist (walker:form-llist funbinding))
	      (parser (make-instance 'walker:parser)) ;FIXME? needed by ARGUMENTS-ASSIGN-TO-LAMBDA-LIST
	      (arg-alist (walker-plus:arguments-assign-to-lambda-list parser llist arguments)))
	 (if (loop for acons in arg-alist for acons-rest on arg-alist do
		  (let* ((var (car acons))
			 (form (cdr acons))
			 (form-exits (find-exits exit-finder form)))
		    (cond
		      ((normal-exits form-exits)
		       (setf (ast-bounds var) (fwd-infer fwd-inferer form)))
		      (t
		       (fwd-infer fwd-inferer form)
		       (return nil))))
		  finally (return t))
	     ;; Compute the list of bounds that the function returns.
	     (let* ((body (walker:form-body funbinding))
		    (body-exits (find-exits-forms-list exit-finder funbinding body)))
	       (cond
		 ((normal-exits body-exits)
		  (fwd-infer-list fwd-inferer body))
		 (t
		  (fwd-infer-list fwd-inferer body)
		  (make-bounds (make-results-0) (make-results-0)))))))))))

(defmethod fwd-infer :around ((fwd-inferer fwd-inferer) ast)
  (let ((bounds (call-next-method)))
    (setf (ast-bounds ast) bounds)
    bounds))

;;; TEST INFER THE FORWARD PASS.

(defun test-fwd-infer-form (form)
  (let* ((parser (make-instance 'ntiparser))
	 (ast (walker:parse-with-namespace form :parser parser))
	 (fwd-inferer (make-instance 'fwd-inferer)))
    (fwd-infer fwd-inferer ast)
    (annotate ast)))

;;; TEST INFER THE FORWARD PASS.

(defun test-fwd-infer ()
  (flet ((assert-result (form desired-upper)
	   (let* ((ast (walker:parse-with-namespace form :parser (make-instance 'ntiparser)))
		  (actual-bounds (fwd-infer (make-instance 'fwd-inferer) ast))
		  (actual-upper (loop for i below (length desired-upper) collect
				     (resultn (bounds-upper actual-bounds) i))))
	     (assert (equal desired-upper actual-upper) () "form ~S~%yielded actual upper ~S,~%but desired upper ~S~%"
		     (test-fwd-infer-form form) actual-upper desired-upper))))
    (assert-result '0 '(fixnum))
    (assert-result '(let ()) '(null))
    (assert-result '(let ((a 0)) a) '(fixnum))
    (assert-result '(let ((a 0)) a (setq a 1.0) a) '(single-float))
    (assert-result '(let ((a 0)) a (setq a 1.0) a (setq a 1) a) '(fixnum))
    (assert-result '(let ((a 0)) a (if 1 (setq a 1.0)) a) '(number))
    (assert-result '(let ((a 0)) a (if 1 (setq a 1.0) a) a) '(number))
    (assert-result '(let ((a 0)) a (if (setq a 1.0) a a)) '(single-float))
    (assert-result '(let ((a 0)) a (if (setq a 1.0) (setq a 1.0)) a) '(single-float))
    (assert-result '(let ((a 0)) a (if (setq a 1.0) (setq a 1.0) a) a) '(single-float))
    (assert-result '(let ((a 0) (b 0.0)) a b (if 1 (setq a 2.0 b a)) b) '(single-float))
    (assert-result '(let ((a 0) (b 0.0)) (setq a 2.0 b a) b) '(single-float))
    ;;(assert-result '(let ((a 1.0) (b 1) m) (setq m a a b b m) a) '(fixnum))
    (assert-result '(let ((a 1.0) (b 1) (m nil)) (setq m a a b b m) a) '(fixnum))
    (assert-result '(let ((a 1.0) (b 1) (m nil)) (setq m a a b b m) b) '(single-float))
    (assert-result '(let ((a 1)) (tagbody) a) '(fixnum))
    (assert-result '(let ((a 1)) (tagbody (setq a 1.0)) a) '(single-float))
    (assert-result '(let ((a 1)) (tagbody (go x) (setq a 1.0) x) a) '(fixnum))
    (assert-result '(let ((a 1) (b 1)) (tagbody (go x) (setq a 1.0 b a) x) a) '(fixnum))
    (assert-result '(let ((a 1) (b 1)) (tagbody (setq a 1.0 b a)) b) '(single-float))
    (assert-result '(let ((a 1)) (tagbody (if 1 (go x)) (setq a 1.0) x) a) '(number))
    (assert-result '(let ((a 1)) (tagbody e (setq a 1.0) (if 1 (go e))) a) '(single-float))
    (assert-result '(let ((a 1)) (tagbody e (if 1 (setq a 1.0)) (if 1 (go e))) a) '(number))
    (assert-result '(flet ((f () 1)) (f)) '(fixnum))
    (assert-result '(let ((a 1)) (flet ((fb () (setq a 1.0))) (flet ((fa () (fb))) (fa))) a) '(single-float))
    (assert-result '(let ((a 1)) (labels ((fb () (if 1 (fb) (setq a 1.0)))) (flet ((fa () (fb))) (fa))) a) '(single-float))
    (assert-result '(let ((a 1)) (labels ((fa () (if 1 (fb) (fc))) (fb () (if 2 (fa) (fc))) (fc () (setq a 1.0))) (fa))) '(single-float))
    (assert-result '(let ((a 1)) (labels ((fa (x) (setq a x)) (fb (x) (if 1 (fa x)))) (fb 1) (fb 1.0)) a) '(number))
    ;;(assert-result '(let ((a t) (b t) (c t)) (flet ((fa (x) (setq a b b c c x))) (fa 1) (fa 1.0) (fa t)) (values a b c)) '(fixnum single-float boolean))
    ;;(assert-result '(let ((b 1) (c 1)) (labels ((fa () (if 1 (fb) (fc))) (fb () (setq b 1.0)) (fc () (setq c 1.0))) (fa)) (values b c)) '(number number))
    ;;(assert-result '(let ((b 1) (c 1)) (labels ((fa () (if 1 (fb) (fc))) (fb () (if 1 (fa) (setq b 1.0))) (fc () (if 1 (fa) (setq c 1.0)))) (fa)) (values b c)) '(number number))
    ))
