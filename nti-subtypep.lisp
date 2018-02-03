;;;; This tests how type information of functions can be represented in lisp.

;; for example types could be represented as a list of OR-concatenated types. This has the disadvantage that the correspondence of input types and output types is not clearly readable.
#|
(declaim (ftype (function ((or float integer) (or float integer)) (or (vector integer) (vector float))) f1))
(defun f1 (a b)
  (make-array 2 :initial-contents (list a b)))
|#

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
		     (dolist (,i (handler-case (multiple-value-list ,a)
				   (error (e) (list e))))
		       (prin1 ,i)
		       (princ " ")))))
       (format t "~%"))))

;;(setf *print-circle* t)

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
    signed-byte
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
;; for example, (SUBTYPES-OF-ALL '(SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE INTEGER T) :PRINT T) prints
;;
;; T is a supertype of (SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE INTEGER)
;; INTEGER is a supertype of (SIGNED-BYTE BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE)
;; UNSIGNED-BYTE is a supertype of (BIT NIL)
;; FIXNUM is a supertype of (BIT NIL)
;; NIL is a supertype of NIL
;; BIT is a supertype of (NIL)
;; BIGNUM is a supertype of (NIL)
;; SIGNED-BYTE is a supertype of (BIGNUM BIT NIL FIXNUM UNSIGNED-BYTE INTEGER)
;;
;; This implies the following graph:
;;                   T
;;                   |
;;         SIGNED-BYTE=INTEGER
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

;; CL-USER> (SUBTYPES-OF-ALL '(NIL ERROR CONDITION SERIOUS-CONDITION SIMPLE-ERROR SIMPLE-CONDITION T) :PRINT T)
;;
;; T is a supertype of (NIL ERROR CONDITION SERIOUS-CONDITION SIMPLE-ERROR SIMPLE-CONDITION)
;; SIMPLE-CONDITION is a supertype of (NIL SIMPLE-ERROR)
;; SIMPLE-ERROR is a supertype of (NIL)
;; SERIOUS-CONDITION is a supertype of (NIL ERROR SIMPLE-ERROR)
;; CONDITION is a supertype of (NIL ERROR SERIOUS-CONDITION SIMPLE-ERROR SIMPLE-CONDITION)
;; ERROR is a supertype of (NIL SIMPLE-ERROR)
;; NIL is a supertype of NIL
;;
;; this implies the following graph:
;;                    T
;;                    |
;;                CONDITION
;;               /         \
;;      SERIOUS-CONDITION   SIMPLE-CONDITION
;;              |          /
;;            ERROR     __/
;;              |      / 
;;         SIMPLE-ERROR
;;
;; This means that searching for the first common descendant is harder than just walking down the subtypes, because walking down the subtypes first finds SIMPLE-ERROR as first common descendant, while we would like to find ERROR.

(defstruct typenode
  "An instance of this structure represents a type in a type lattice.
TYPE is the name of the type.
SUPERTYPES is the list of TYPENODEs that are supertypes of TYPE in the lattice.
SIBLINGS is the list of TYPENODEs that are the same type as TYPE in the lattice.
SUBTYPES is the list of TYPENODEs that are subtypes of TYPE in the lattice."
  (type (error "must specify TYPE for TYPENODE") :type (or symbol list))
  (supertypes nil :type list)
  (siblings nil :type list)
  (subtypes nil :type list))

(defun print-typegraph (typegraph-topnode)
  ;;(when (eq (typenode-type typegraph-topnode) nil)
  (let ((type (typenode-type typegraph-topnode))
	(subtypes (typenode-subtypes typegraph-topnode)))
    (format t "supertype: ~S subtypes: ~S~%" type (mapcar #'typenode-type subtypes))
    (loop for subtype in subtypes do
	 (print-typegraph subtype))))

(defun unique (seq &key (test #'eql) (count nil))
  "Return the list of all unique (under TEST) elements of sequence SEQ.
When COUNT is specified, only elements that occur COUNT times in SEQ are returned."
  (let ((ht (make-hash-table :test test)))
    (map nil (lambda (elt) (incf (gethash elt ht 0))) seq)
    (let ((res nil))
      (cond
	((null count)
	 (loop for k being the hash-key of ht do (push k res)))
	((numberp count)
	 (maphash (lambda (k v) (when (= v count) (push k res))) ht)))
      res)))

(defun make-typehash (relations)
  (let ((relhash (make-hash-table :test #'equal)))
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
			 node))))
	       (siblingp (type1 type2)
		 "Return whether TYPE1 and TYPE2 are siblings, i.e. both are subtypes of each other."
		 (and (gethash type1 (gethash type2 relhash))
		      (gethash type2 (gethash type1 relhash)))))
	(loop for (supertype . subtypes) in relations do
	     ;;(prind supertype subtypes)
	     (let ((siblings (remove-if (lambda (x) (not (siblingp x supertype))) subtypes))
		   (subtypes-nosiblings (remove-if (lambda (x) (siblingp x supertype)) subtypes))
		   (immediate-subtypes nil))
	       (loop for t1 in subtypes-nosiblings do
		    (unless
			(loop for t2 in subtypes-nosiblings thereis
			     (cond
			       ((eq t1 t2)
				nil)
			       ((siblingp t1 t2)
				;; otherwise both siblings would exclude each other.
				nil)
			       ((gethash t1 (gethash t2 relhash))
				;;(prind "excluding" t1 "because of" t2)
				t)
			       (t
				nil)))
		      (push t1 immediate-subtypes)))
	       ;;(prind supertype immediate-subtypes)
	       (let ((supertype-node (visit-typenode supertype)))
		 (loop for subtype in siblings do
		      (let ((subtype-node (visit-typenode subtype)))
			;;(prind "SIBLINGS:" subtype supertype)
			(unless (find subtype-node (typenode-siblings supertype-node))
			  (push subtype-node (typenode-siblings supertype-node)))
			(unless (find supertype-node (typenode-siblings subtype-node))
			  (push supertype-node (typenode-siblings subtype-node)))))
		 (loop for subtype in immediate-subtypes do
		      (let ((subtype-node (visit-typenode subtype)))
			(push supertype-node (typenode-supertypes subtype-node))
			(push subtype-node (typenode-subtypes supertype-node))))))))
      typehash)))

(defun maxmin-of (subtype-relations &key (maxmin :max))
  "Starting from the node NIL, find all top-most supertypes (those reachable nodes which have no supertypes) in TYPEHASH.
When MAXMIN == :MAX, SUBTYPE-RELATIONS must be pairs of (SUPERTYPE . SUBTYPES) relations.
When MAXMIN == :MIN, SUBTYPE-RELATIONS must be pairs of (SUBTYPE . SUPERTYPES) relations."
  (let ((typehash (make-typehash subtype-relations)))
    (let ((toptypes nil))
      (labels ((rec (node)
		 (let ((supertypes (typenode-supertypes node)))
		   (if (null supertypes)
		       (push (typenode-type node) toptypes)
		       (loop for supertype in supertypes do
			    (rec supertype))))))
	(rec (gethash (ecase maxmin ((:max) 'nil) ((:min) 't)) typehash))
	(unique toptypes :test #'equal)))))

(defun maxima-of (subtype-relations)
  "Starting from the node NIL, find all top-most supertypes (those reachable nodes which have no supertypes) in TYPEHASH."
  (maxmin-of subtype-relations :maxmin :max))

(defun minima-of (subtype-relations)
  "Starting from the node NIL, find all top-most supertypes (those reachable nodes which have no supertypes) in TYPEHASH."
  (maxmin-of subtype-relations :maxmin :min))

(defun make-typegraph (types)
  (assert (position nil types))
  (assert (position t types))
  (let ((relations (subtypes-of-all types)))
    (make-typehash relations)))

(defun subsupertypes-of (type typehash &key (subsuper :sub))
  (assert (gethash type typehash))
  (labels ((rec (type)
	     (if (null type)
		 (ecase subsuper
		   ((:sub)
		    (values '((nil)) '(nil)))
		   ((:super)
		    (values '((t)) '(t))))
		 (let* ((type1 (gethash type typehash))
			(subtypes (mapcar #'typenode-type (ecase subsuper
							    ((:sub) (typenode-subtypes type1))
							    ((:super) (typenode-supertypes type1)))))
			(recsubtypes nil)
			(recrels))
		   (loop for subtype in subtypes do
			(multiple-value-bind (rels subtypes) (rec subtype)
			  (setf recsubtypes (append subtypes recsubtypes))
			  (setf recrels (append rels recrels))))
		   (setf recsubtypes (unique recsubtypes :test #'equal))
		   (values (cons (cons type recsubtypes) recrels)
			   (cons type recsubtypes))))))
    (unique (rec type) :test #'equal)))

(defun subtypes-of (type typehash)
  "Compute all (also recursive) subtypes of TYPE in the typegraph stored in TYPEHASH.
Return the type relations of the subtypes."
  (subsupertypes-of type typehash :subsuper :sub))

(defun supertypes-of (type typehash)
  "Compute all (also recursive) supertypes of TYPE in the typegraph stored in TYPEHASH.
Return the type relations of the supertypes."
  (subsupertypes-of type typehash :subsuper :super))

(defun meet-types (type1 type2 typehash)
  "Return the common maximal descendants of TYPE1 and TYPE2 in the typegraph stored in TYPEHASH."
  (assert (gethash type1 typehash) () "TYPE ~S is not in TYPEHASH." type1)
  (assert (gethash type2 typehash) () "TYPE ~S is not in TYPEHASH." type2)
  (if (eq type1 type2)
      (list type1)
      (let* ((subtypes1 (subtypes-of type1 typehash))
	     (subtypes2 (subtypes-of type2 typehash))
	     (intersection (unique (append subtypes1 subtypes2) :test #'equal :count 2)))
	(maxima-of intersection))))

(defun join-types (type1 type2 typehash)
  "Return the common minimal ancestors of TYPE1 and TYPE2 in the typegraph stored in TYPEHASH."
  (assert (gethash type1 typehash) () "TYPE ~S is not in TYPEHASH." type1)
  (assert (gethash type2 typehash) () "TYPE ~S is not in TYPEHASH." type2)
  (if (eq type1 type2)
      (list type1)
      (let* ((supertypes1 (supertypes-of type1 typehash))
	     (supertypes2 (supertypes-of type2 typehash))
	     (intersection (unique (append supertypes1 supertypes2) :test #'equal :count 2)))
	(minima-of intersection))))

(defun meet-typelist (typelist typehash)
  (let ((m typelist))
    (loop until (<= (length m) 1) do
	 (let ((m1 (car m))
	       (m2 (cadr m)))
	   (setf m (append (cddr m) (meet-types m1 m2 typehash) ))))
    (car m)))

(defun meet-type (type1 type2 typehash)
  "Return the common maximal descendant of TYPE1 and TYPE2 in the typegraph stored in TYPEHASH."
  (meet-typelist (meet-types type1 type2 typehash) typehash))

(defun join-typelist (typelist typehash)
  (let ((m typelist))
    (loop until (<= (length m) 1) do
	 (let ((m1 (car m))
	       (m2 (cadr m)))
	   (setf m (append (cddr m) (join-types m1 m2 typehash) ))))
    (car m)))

(defun join-type (type1 type2 typehash)
  "Return the common minimal ancestor of TYPE1 and TYPE2 in the typegraph stored in TYPEHASH."
  (join-typelist (join-types type1 type2 typehash) typehash))

(defun is-subtypep (type1 type2 typehash)
  (cond
    ((let ((siblings1 (mapcar #'typenode-type (typenode-siblings (gethash type1 typehash)))))
       (position type2 siblings1 :test #'equal))
     t)
    (t
     (let ((m (meet-type type1 type2 typehash)))
       ;;(when (> (length m) 1) (prind m))
       (cond
	 ((eq m type1) t)
	 (t
	  nil))))))

(defun test1 ()
  (let ((typehash (make-typegraph +cl-types+)))
    (loop for t1 in +cl-types+ do
	 (loop for t2 in +cl-types+ do
	      ;;(prind t1 t2)
	      (if (not (eq (is-subtypep t1 t2 typehash) (subtypep t1 t2)))
		  (format t "(IS-SUBTYPEP '~A '~A)=~A but (SUBTYPEP '~A '~A)=~A~%" t1 t2 (is-subtypep t1 t2 typehash) t1 t2 (subtypep t1 t2)))))))
(test1)
