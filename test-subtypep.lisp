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

;; TODO: figure out how to handle equal type specifications, like INTEGER and SIGNED-BYTE: see "(SUBTYPES-OF-ALL '(INTEGER SIGNED-BYTE NIL T) :PRINT T) prints:" below

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
				((and sub2 sub1)
				 'equal)
				(t
				 'indep)))))
		    ret)))
    ret))

(defun subtypes-of-all (types &key print)
  "From the output O of this function it would be possible to construct a graph, where T is at the top, NIL is at the bottom, and for all lists in O, the there are lines from (CAR O) to all types in (CDR O)."
  (let* ((all-against-all-ret (all-against-all types))
	 (ret (mapcar (lambda (type) (cons type nil)) (remove-duplicates (mapcar #'car all-against-all-ret)))))
    (loop for (type1 type2 rel) in all-against-all-ret do
	 (when (or (eq rel '2-sub-of-1) (and (not (equal type1 type2)) (eq rel 'equal)))
	 ;;(when (eq rel '2-sub-of-1)
	   (let ((assoc (assoc type1 ret)))
	     (assert (not (null assoc)))
	     (setf (cdr assoc) (cons type2 (cdr assoc))))))
    (when print
      (loop for (supertype . subtypes) in ret do
	   (format t "~S is a supertype of ~S~%" supertype subtypes)))
    ret))

;;;; Comments on "The Nimble Type Inferencer for Common Lisp-84"

;; In the paper, "upper bound" and "lower bound" are not defined, but I assume them to be upper and lower borders in the type graph, in which T (most general type, if a variable is of type T, it can have any value) is at the top and NIL (most restrictive type, no variable can ever be of type NIL) is at the bottom. A variable with upper bound U and lower bound L then can have values that are of types in-between the upper border U and the lower border L in the type graph.

;; the computation "C := meet(A,B)" apparently sets C to the types in the intersection of A and B. (For example, if A is (OR FLOAT CONS) and B is NUMBER, then C is FLOAT.)
;; the computation "C := join(A,B)" sets C to the types in the union of A and B. (For example, if A is (OR CONS NUMBER) and B is FLOAT, then C is (OR CONS NUMBER).)
;; I infer this from the following sentence: "With this encoding for the number of components in a multiple value, it is easy to perform lattice meet and join--they are simply logand and logior of the representation numbers."

;;;; GRAPH

;; How to construct the graph from the return values of #'SUBTYPES-OF-ALL?
;; for example, (SUBTYPES-OF-ALL '(SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE T) :PRINT T) prints
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

;; (SUBTYPES-OF-ALL '(SYMBOL NULL CONS LIST NIL T) :PRINT T) prints:
;;
;; T is a supertype of (SYMBOL NULL CONS LIST NIL)
;; NIL is a supertype of NIL
;; LIST is a supertype of (NULL CONS NIL)
;; CONS is a supertype of (NIL)
;; NULL is a supertype of (NIL)
;; SYMBOL is a supertype of (NULL NIL)
;;
;; this implies the following graph:
;;           T 
;;          / \
;;       LIST  SYMBOL
;;        / \   |
;;     CONS  NULL
;;       \   /
;;        NIL

;; (SUBTYPES-OF-ALL '(SYMBOL LIST SEQUENCE ARRAY ATOM VECTOR NIL T) :PRINT T) prints:
;;
;; T is a supertype of (SYMBOL LIST SEQUENCE ARRAY ATOM VECTOR NIL)
;; NIL is a supertype of NIL
;; VECTOR is a supertype of (NIL)
;; ATOM is a supertype of (SYMBOL ARRAY VECTOR NIL)
;; ARRAY is a supertype of (VECTOR NIL)
;; SEQUENCE is a supertype of (LIST VECTOR NIL)
;; LIST is a supertype of (NIL)
;; SYMBOL is a supertype of (NIL)
;;
;; this implies the following graph:
;;                T
;;             __/ \
;;            /     ATOM
;;           |       |  \
;;      SEQUENCE  ARRAY  \
;;           |  \  |      |
;;          LIST VECTOR  SYMBOL
;;            \   /______/   
;;             NIL

;; (SUBTYPES-OF-ALL '(INTEGER SIGNED-BYTE NIL T) :PRINT T) prints:
;;
;; T is a supertype of (INTEGER SIGNED-BYTE NIL)
;; NIL is a supertype of NIL
;; SIGNED-BYTE is a supertype of (INTEGER NIL)
;; INTEGER is a supertype of (SIGNED-BYTE NIL)
;;
;; this means that SIGNED-BYTE and INTEGER are the same type. TODO: figure out how to handle this.

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

(defun set-subset (seq1 seq2 &key (test #'eql))
  "Return whether SEQ1 is a subset of SEQ2, where SEQ1 and SEQ2 are passed as sequences."
  (let ((ht (make-hash-table :test test)))
    (map nil (lambda (a) (setf (gethash a ht) 0)) seq2)
    (map nil (lambda (a)
	       (unless (nth-value 1 (gethash a ht))
		 (return-from set-subset nil)))
	 seq1)
    t))

(defun set-equal (seq1 seq2 &key (test #'eql))
  "Return whether SEQ1 contains the same set of elements as SEQ2, where SEQ1 and SEQ2 are passed as sequences."
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

;; TODO: FIXME: (MAKE-TYPEGRAPH '(SYMBOL NULL CONS LIST NIL T)) loops infinitely.
;; TODO: FIXME: (MAKE-TYPEGRAPH '(SYMBOL LIST SEQUENCE ARRAY ATOM VECTOR NIL T)) doesn't return that VECTOR is a subtype of SEQUENCE.
(defun make-typegraph-buggy (types)
  (declare (optimize (debug 3)))
  (assert (position nil types))
  (assert (position t types))
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
	     ;;(prind relations)
	     (let ((vtypes (loop for (type2 . subtypes) in relations do
				(when (loop for subtype in subtypes always (was-visited subtype))
				  (return subtypes)))))
	       ;;(prind vtypes)
	       ;; add TYPES as subtypes of all types that have TYPES as the only subtypes.
	       (loop for (type . subtypes) in relations do
		    ;;(when (set-equal subtypes vtypes :test #'equal)
		    (when (set-subset subtypes vtypes :test #'equal)
		      ;;(prind type)
		      (let ((type (visit-typenode type)))
			(loop for stype in subtypes do
			     (let ((stype (gethash stype created)))
			       (push stype (typenode-subtypes type))
			       (push type (typenode-supertypes stype)))))))
	       ;; remove vtypes everywhere.
	       (loop for relation in relations do
		    (loop for vtype in vtypes do
			 (setf (cdr relation) (remove vtype (cdr relation))))))
	   ;; remove types from the relations that have no subtypes.
	     (setf relations (remove-if (lambda (relation) (null (cdr relation))) relations))
	     )))
    ;;(gethash t created)
    created
    ))

(defun make-typegraph (types)
  (declare (optimize (debug 3)))
  (assert (position nil types))
  (assert (position t types))
  (let ((relations (subtypes-of-all types))
	(relhash (make-hash-table :test #'equal)))
    (loop for (supertype . subtypes) in relations do
	 (let ((set (make-hash-table :test #'equal)))
	   (loop for subtype in subtypes do
		(setf (gethash subtype set) t))
	   (setf (gethash supertype relhash) set)))
    (let ((typehash (make-hash-table :test #'equal)))
      (labels ((visit-typenode (type)
		 (multiple-value-bind (value present) (gethash type typehash)
		   (if present
		       value
		       (let ((node (make-typenode :type type)))
			 (setf (gethash type typehash) node)
			 node)))))
	(loop for (supertype . subtypes) in relations do
	     (prind supertype subtypes)
	     (let ((immediate-subtypes nil))
	       (loop for t1 in subtypes do
		    (unless
			(loop for t2 in subtypes thereis
			     (cond
			       ((eq t1 t2)
				nil)
			       ((and (gethash t1 (gethash t2 relhash))
				     (gethash t2 (gethash t1 relhash)))
				nil)
			       ((gethash t1 (gethash t2 relhash))
				;;(prind "excluding" t1 "because of" t2)
				t)
			       (t
				nil)))
		      (push t1 immediate-subtypes)))
	       (prind supertype immediate-subtypes)
	       (let ((supertype-node (visit-typenode supertype)))
		 (loop for subtype in immediate-subtypes do
		      (let ((subtype-node (visit-typenode subtype)))
			(push supertype-node (typenode-supertypes subtype-node))
			(push subtype-node (typenode-subtypes supertype-node))))))))
      typehash)))

(defun meet-type (type1 type2 typehash)
  "Return the first common descendant of TYPE1 and TYPE2 in the typegraph stored in TYPEHASH."
  (declare (optimize (debug 3)))
  (assert (and (gethash type1 typehash) (gethash type2 typehash)))
  ;; check if TYPE1 and TYPE2 have a common descendant
  (let ((types1 (list type1))
	(types2 (list type2))
	(visited1 nil)
	(visited2 nil)
	(subtypes1 (list type1))
	(subtypes2 (list type2)))
    (loop do
	 (let* ((t1 (car types1))
		(t2 (car types2))
		(n1 (gethash t1 typehash))
		(n2 (gethash t2 typehash))
		(s1 (mapcar #'typenode-type (typenode-subtypes n1)))
		(s2 (mapcar #'typenode-type (typenode-subtypes n2))))
	   (setf types1 (cdr types1))
	   (setf types2 (cdr types2))
	   (setf types1 (append types1 s1))
	   (setf types2 (append types2 s2))
	   (setf subtypes1 (append subtypes1 s1))
	   (setf subtypes2 (append subtypes2 s2))
	   (let ((c1 (loop for subtype1 in subtypes1 when (find subtype1 subtypes2) return subtype1))
		 (c2 (loop for subtype2 in subtypes2 when (find subtype2 subtypes1) return subtype2)))
	     ;;(prind t1 t2 (position t1 visited1) (position t2 visited2) c1 c2 subtypes1 subtypes2)
	     (cond
	       (c1
		(return c1))
	       (c2
		(return c2))
	       ((and (null types1) (null types2))
		(return nil))))
	   (when (and (loop for t1 in types1 always (position t1 visited1))
		      (loop for t2 in types2 always (position t2 visited2)))
	     (return nil))
	   (setf visited1 (cons t1 visited1))
	   (setf visited2 (cons t2 visited2))))))

(defun is-subtypep (type1 type2 typehash)
  (let ((m (meet-type type1 type2 typehash)))
    ;;(prind m)
    (cond
      ((eq m type1)
       t)
      ((or (eq m type2) (null m))
       nil)
      (t
       nil))))

(defun test1 ()
  (let ((typehash (make-typegraph +cl-types+)))
    (loop for t1 in +cl-types+ do
	 (loop for t2 in +cl-types+ do
	      ;;(prind t1 t2)
	      (if (not (eq (is-subtypep t1 t2 typehash) (subtypep t1 t2)))
		  (format t "(IS-SUBTYPEP '~A '~A)=~A but (SUBTYPEP '~A '~A)=~A~%" t1 t2 (is-subtypep t1 t2 typehash) t1 t2 (subtypep t1 t2)))))))

(defun meet-types (types1 types2 typehash)
  "Take TYPES1 and TYPES2 to be the types (OR ,@TYPES1) and (OR ,@TYPES2), respectively, and compute the intersection of TYPES1 and TYPES2."
  (let ((res nil))
    (loop for t1 in types1 do
	 (loop for t2 in types2 do
	      (let ((m (meet-type t1 t2 typehash)))
		(unless (or (null m) (find m res))
		  (push m res)))))
    res))
