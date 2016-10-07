;;;; This tests how type information of functions can be represented in lisp.

;; for example types could be represented as a list of OR-concatenated types. This has the disadvantage that the correspondence of input types and output types is not clearly readable.
(declaim (ftype (function ((or float integer) (or float integer)) (or (vector integer) (vector float))) f1))
(defun f1 (a b)
  (make-array 2 :initial-contents (list a b)))

#|
;; declaring function type as concatenated declare specifications doesn't work, since they are interpreted as AND-concatenated types, leading to conflicts.
(defun test2 ()
  (flet ((f1 (a b)
	   (make-array 2 :initial-contents (list a b))))
    (declare (ftype (function (integer integer) (vector integer)) f1)
	     (ftype (function (float float) (vector float)) f1))
    (f1 2 3)
    (f1 2.0 3.0)))
|#

;;;; This tests subtype relationships between Common Lisp types.

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

(setf *print-circle* t)

(defparameter +cl-types+
  '(arithmetic-error function simple-condition 
    array generic-function simple-error 
    atom hash-table simple-string 
    base-char integer simple-type-error 
    base-string keyword simple-vector 
    bignum list simple-warning 
    bit logical-pathname single-float 
    bit-vector long-float standard-char 
    broadcast-stream method standard-class 
    built-in-class method-combination standard-generic-function 
    cell-error nil standard-method 
    character null standard-object 
    class number storage-condition 
    compiled-function package stream 
    complex package-error stream-error 
    concatenated-stream parse-error string 
    condition pathname string-stream 
    cons print-not-readable structure-class 
    control-error program-error structure-object 
    division-by-zero random-state style-warning 
    double-float ratio symbol 
    echo-stream rational synonym-stream 
    end-of-file reader-error t 
    error readtable two-way-stream 
    extended-char real type-error 
    file-error restart unbound-slot 
    file-stream sequence unbound-variable 
    fixnum serious-condition undefined-function 
    float short-float unsigned-byte 
    floating-point-inexact signed-byte vector 
    floating-point-invalid-operation simple-array warning 
    floating-point-overflow simple-base-string 
    floating-point-underflow simple-bit-vector)
  "CLHS: Figure 4-2 lists symbols that are standardized atomic type specifiers")

(defparameter +nti-types+
  '(t
    single-float
    short-float
    double-float
    long-float
    (complex single-float)
    (complex short-float)
    (complex double-float)
    (complex long-float)
    fixnum
    integer
    rational
    (complex rational)
    nil ;NIL added although it doesn't occur in the example
    )
  "Types occurring in the \"(defun asinh (z) ...\" Nimble type inferencer paper example.")

(defparameter +my-types+
  '(t
    number
    float
    single-float
    double-float
    integer
    fixnum
    rational
    cons
    unsigned-byte
    bit ;added to demonstrate that types can converge in the graph to a type other than NIL
    nil
    ))

(defun all-against-all (types)
  (let ((ret '()))
    (loop for type1 in types do
	 (loop for type2 in types do
	      (push (list type1 type2
			  (multiple-value-bind (sub1 cert1) (subtypep type1 type2)
			    (multiple-value-bind (sub2 cert2) (subtypep type2 type1)
			      (cond
				((or (null cert1) (null cert2))
				 'uncert)
				((and sub1 (not sub2))
				 '1-sub-of-2)
				((and sub2 (not sub1))
				 '2-sub-of-1)
				(t
				 'indep)))))
		    ret)))
    ret))

(defun subtypes-of-all (types &key print)
  "From the output O of this function it would be possible to construct a graph, where T is at the top, NIL is at the bottom, and for all lists in O, the there are lines from (CAR O) to all types in (CDR O)."
  (let* ((all-against-all-ret (all-against-all types))
	 (ret (mapcar (lambda (type) (cons type nil)) (remove-duplicates (mapcar #'car all-against-all-ret)))))
    (loop for (type1 type2 rel) in all-against-all-ret do
	 (when (eq rel '2-sub-of-1)
	   (let ((assoc (assoc type1 ret)))
	     (assert (not (null assoc)))
	     (setf (cdr assoc) (cons type2 (cdr assoc))))))
    (when print
      (loop for (supertype . subtypes) in ret do
	   (format t "~S is a supertype of ~S~%" supertype subtypes)))
    ret))

;;;; Comments on "The Nimble Type Inferencer for Common Lisp-84"

;; In the paper, "upper bound" and "lower bound" are not defined, but I assume them to be upper and lower borders in the type graph, in which T (most general type, if a variable is of type T, it can have any value) is at the top and NIL (most restrictive type, no variable can ever be of type NIL) is at the bottom. A variable with upper bound U and lower bound L then can have values that are of types in-between the upper border U and the lower border L in the type graph.

;; the computation "C := meet(A,B)" apparently sets C to the types in the intersection of A and B. (For example, if A is (OR FLOAT CONS) and B is NUMBER, then C can is FLOAT.)
;; the computation "C := join(A,B)" sets C to the types in the union of A and B. (For example, if A is (OR CONS NUMBER) and B is FLOAT, then C is (OR CONS NUMBER).)
;; I infer this from the following sentence: "With this encoding for the number of components in a multiple value, it is easy to perform lattice meet and join--they are simply logand and logior of the representation numbers."

;;;; GRAPH

;; How to construct the graph from the return values of #'SUBTYPES-OF-ALL?
;; for example, (SUBTYPES-OF-ALL '(SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE T)) returns
;;
;; T is a supertype of (SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
;; UNSIGNED-BYTE is a supertype of (BIT NIL)
;; FIXNUM is a supertype of (BIT NIL)
;; NIL is a supertype of NIL
;; BIT is a supertype of (NIL)
;; BIGNUM is a supertype of (NIL)
;; SIGNED-BYTE is a supertype of (BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
;;
;; This implies the following graph:
;;                   T
;;                   |
;;             SIGNED-BYTE 
;;            /      |    \
;;  UNSIGNED-BYTE  FIXNUM  BIGNUM
;;            \   /       /
;;             BIT    ___/
;;               \   /
;;                NIL

(defstruct typenode
  (type (error "must specify TYPE for TYPENODE"))
  (supertypes nil :type list)
  (subtypes nil :type list))

(defun print-typegraph (typegraph-topnode)
  ;;(when (eq (typenode-type typegraph-topnode) nil)
  (let ((type (typenode-type typegraph-topnode))
	(subtypes (typenode-subtypes typegraph-topnode)))
    (format t "supertype: ~S subtypes: ~S~%" type (mapcar #'typenode-type subtypes))
    (loop for subtype in subtypes do
	 (print-typegraph subtype))))

#|
initialize the visited nodes with NIL.

T is a supertype of (SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
UNSIGNED-BYTE is a supertype of (BIT NIL)
FIXNUM is a supertype of (BIT NIL)
NIL is a supertype of ()
BIT is a supertype of (NIL)
BIGNUM is a supertype of (NIL)
SIGNED-BYTE is a supertype of (BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
:
remove NIL from the relations, because it has no subtypes.

T is a supertype of (SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
UNSIGNED-BYTE is a supertype of (BIT NIL)
FIXNUM is a supertype of (BIT NIL)
BIT is a supertype of (NIL)
BIGNUM is a supertype of (NIL)
SIGNED-BYTE is a supertype of (BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
:
BIT and BIGNUM have NIL as the only subtype, and NIL was already visited.
add BIT as supertype of NIL.
add BIGNUM as supertype of NIL.
remove NIL everywhere.

T is a supertype of (SIGNED-BYTE BIGNUM BIT FIXNUM UNSIGNED-BYTE)
UNSIGNED-BYTE is a supertype of (BIT)
FIXNUM is a supertype of (BIT)
BIT is a supertype of ()
BIGNUM is a supertype of ()
SIGNED-BYTE is a supertype of (BIGNUM BIT FIXNUM UNSIGNED-BYTE)
:
remove BIT and BIGNUM from the relations, because they have no subtypes.

T is a supertype of (SIGNED-BYTE BIGNUM BIT FIXNUM UNSIGNED-BYTE)
UNSIGNED-BYTE is a supertype of (BIT)
FIXNUM is a supertype of (BIT)
SIGNED-BYTE is a supertype of (BIGNUM BIT FIXNUM UNSIGNED-BYTE)
:
FIXNUM and UNSIGNED-BYTE have BIT as the only subtype, and BIT was already visited.
add FIXNUM as supertype of BIT.
add UNSIGNED-BYTE as supertype of BIT.
remove BIT everywhere.

T is a supertype of (SIGNED-BYTE BIGNUM FIXNUM UNSIGNED-BYTE)
UNSIGNED-BYTE is a supertype of ()
FIXNUM is a supertype of ()
SIGNED-BYTE is a supertype of (BIGNUM FIXNUM UNSIGNED-BYTE)
:
remove FIXNUM and UNSIGNED-BYTE from the relations, because they have no subtypes.

T is a supertype of (SIGNED-BYTE BIGNUM FIXNUM UNSIGNED-BYTE)
SIGNED-BYTE is a supertype of (BIGNUM FIXNUM UNSIGNED-BYTE)
:
SIGNED-BYTE has BIGNUM FIXNUM UNSIGNED-BYTE as the only subtypes, and all were already visited.
add SIGNED-BYTE as supertype of BIGNUM FIXNUM UNSIGNED-BYTE.
remove BIGNUM FIXNUM UNSIGNED-BYTE everywhere.

T is a supertype of (SIGNED-BYTE)
SIGNED-BYTE is a supertype of ()
:
remove SIGNED-BYTE from the relations, because it has no subtypes.

T is a supertype of (SIGNED-BYTE)
:
T has SIGNED-BYTE as the only subtype, and SIGNED-BYTE was already visited.
add T as supertype of SIGNED-BYTE.
remove SIGNED-BYTE everywhere.

T is a supertype of ()
:
remove T from the relations, because it has no subtypes.
|#

(defun set-equal (seq1 seq2 &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (map nil (lambda (a) (setf (gethash a ht) 0)) seq1)
    (map nil (lambda (a)
	       (unless (nth-value 1 (gethash a ht))
		 (return-from set-equal nil))
	       (incf (gethash a ht)))
	 seq2)
    (maphash (lambda (key value)
	       (declare (ignore key))
	       (unless (> value 0)
		 (return-from set-equal nil)))
	     ht)
    t))

(defun make-typegraph (types)
  (declare (optimize (debug 3)))
  (assert (position nil types))
  (assert (find t types))
  (let ((created (make-hash-table :test #'equal)))
    (labels ((visit-typenode (type)
	       (multiple-value-bind (value present) (gethash type created)
		 (if present
		     value
		     (let ((node (make-typenode :type type)))
		       (setf (gethash type created) node)
		       node))))
	     (was-visited (type)
	       (nth-value 1 (gethash type created))))
      (visit-typenode nil)
      (let ((relations (subtypes-of-all types)))
	;; remove types from the relations that have no subtypes.
	(setf relations (remove-if (lambda (relation) (null (cdr relation))) relations))
	(loop until (null relations) do
	   ;; find visited types that are the only subtypes in at least one relation
	     (let ((vtypes (loop for (type2 . subtypes) in relations do
				(when (loop for subtype in subtypes always (was-visited subtype))
				  (return subtypes)))))
	       (prind vtypes)
	       ;; add TYPES as subtypes of all types that have TYPES as the only subtypes.
	       (loop for (type . subtypes) in relations do
		    (when (set-equal subtypes vtypes :test #'equal)
		      (prind type)
		      (let ((type (visit-typenode type)))
			(loop for vtype in vtypes do
			     (let ((vtype (gethash vtype created)))
			       (push vtype (typenode-subtypes type))
			       (push type (typenode-supertypes vtype)))))))
	       ;; remove vtypes everywhere.
	       (loop for relation in relations do
		    (loop for vtype in vtypes do
			 (setf (cdr relation) (remove vtype (cdr relation))))))
	   ;; remove types from the relations that have no subtypes.
	     (setf relations (remove-if (lambda (relation) (null (cdr relation))) relations))
	     )))
    (gethash t created)))
