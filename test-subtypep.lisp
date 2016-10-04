;;;; This tests subtype relationships between Common Lisp types.

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
    nil ;NIL added although it doesn't occur in the example
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

(defun subtypes-of-all (types)
  "From the output O of this function it would be possible to construct a graph, where T is at the top, NIL is at the bottom, and for all lists in O, the there are lines from (CAR O) to all types in (CDR O)."
  (let* ((all-against-all-ret (all-against-all types))
	 (ret (mapcar (lambda (type) (cons type nil)) (remove-duplicates (mapcar #'car all-against-all-ret)))))
    (loop for (type1 type2 rel) in all-against-all-ret do
	 (when (eq rel '2-sub-of-1)
	   (let ((assoc (assoc type1 ret)))
	     (assert (not (null assoc)))
	     (setf (cdr assoc) (cons type2 (cdr assoc))))))
    (loop for (supertype . subtypes) in ret do
	 (format t "~S is a supertype of ~S~%" supertype subtypes))
    ret))

;;;; Comments on "The Nimble Type Inferencer for Common Lisp-84"

;; In the paper, "upper bound" and "lower bound" are not defined, but I assume them to be upper and lower borders in the type graph, in which T (most general type, if a variable is of type T, it can have any value) is at the top and NIL (most restrictive type, no variable can ever be of type NIL) is at the bottom. A variable with upper bound U and lower bound L then can have values that are of types in-between the upper border U and the lower border L in the type graph.

;; the computation "C := meet(A,B)" apparently sets C to the types in the intersection of A and B. (For example, if A is (OR FLOAT CONS) and B is NUMBER, then C can is FLOAT.)
;; the computation "C := join(A,B)" sets C to the types in the union of A and B. (For example, if A is (OR CONS NUMBER) and B is FLOAT, then C is (OR CONS NUMBER).)
;; I infer this from the following sentence: "With this encoding for the number of components in a multiple value, it is easy to perform lattice meet and join--they are simply logand and logior of the representation numbers."

