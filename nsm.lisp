;; Nested State Machines

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

(defun any-hash-table-key (ht &optional default-key)
  "Return any key stored in hash-table HT, or DEFAULT-KEY."
  (with-hash-table-iterator (next ht)
    (multiple-value-bind (present-p key value) (next)
      (declare (ignore value))
      (if present-p
	  key
	  default-key))))

(defun any-hash-table-value (ht &optional default-value)
  "Return any value stored in hash-table HT, or DEFAULT-VALUE."
  (with-hash-table-iterator (next ht)
    (multiple-value-bind (present-p key value) (next)
      (declare (ignore key))
      (if present-p
	  value
	  default-value))))

(defclass sm ()
  ((states :initform 0 :initarg :states :type (and unsigned-byte integer) :accessor sm-states :documentation "The maximal number of states.")
   (trans :initform (make-array 0 :element-type '(and unsigned-byte integer) :initial-element 0) :initarg :trans :type (array (and unsigned-byte integer)) :accessor sm-trans :documentation "The state transition array")))

(defun make-sm (states &key (trans (loop for i below states collect i)))
  (let ((len (length trans)))
    (when (/= len states)
      (warn "overwriting STATES=~A with (LENGTH TRANS)=~A" states len)
      (setf states len)))
  (make-instance 'sm
		 :states states
		 :trans (make-array states :element-type '(and unsigned-byte integer) :initial-contents trans)))

(defmethod print-object ((sm sm) stream)
  (print-unreadable-object (sm stream :type t :identity t)
    (format stream "~A ~A" (sm-states sm) (sm-trans sm))))

(defun sm-next (sm state)
  (aref (sm-trans sm) state))

(defun make-hash-table-const-value (n value &optional (start 0))
  "Fill a hash-table with keys from START below N with value VALUE"
  (let ((ht (make-hash-table)))
    (loop for i from start below n do
	 (setf (gethash i ht) value))
    ht))

#|
(defun sm-loop (sm state &key (visited (make-hash-table-const-value (sm-states sm) 1)))
  "Return the loop of states - in reverse order - that state-machine SM goes through when starting at STATE (and VISITED, a hash-table containing the visited elements)."
  (let ((order nil))
    (loop do
	 (push state order)
	 (decf (gethash state visited))
	 (let* ((next (sm-next sm state)))
	   (when (= 0 (gethash next visited))
	     (return (values order visited)))
	   (setf state next)))))
|#

(defun sm-loop (sm state &key (visited (make-hash-table)))
  "Return the loop of states - in reverse order - that state-machine SM goes through when starting at STATE (and VISITED, a hash-table containing the visited elements)."
  (let ((order nil))
    (loop do
	 (push state order)
	 (setf (gethash state visited) t)
	 (let* ((next (sm-next sm state)))
	   (when (gethash next visited)
	     (return (values order visited)))
	   (setf state next)))))

(defun sm-loop-min (sm loop)
  "Return the least state that LOOP goes through when it is caught in a loop."
  (let* ((loop (sm-loop sm (car loop))) ;(CAR LOOP), since LOOP is in reverse order
	 (min (apply #'min loop)))
    min))

(defun sm-loop-circle (sm loop)
  "Return the circle portion of a LOOP."
  (let ((ht (make-hash-table)))
    (loop for e in loop collect e until (gethash (sm-next sm e) ht) do (setf (gethash e ht) t))))

(defun sm-loop-non-circle (sm loop)
  "Return the non-circle portion of a LOOP."
  (let ((ht (make-hash-table)))
    (loop for el on loop do
	 (let ((e (car el)))
	   (when (gethash (sm-next sm e) ht)
	     (return (cdr el)))
	   (setf (gethash e ht) t)))))

(defun sm-loops (sm)
  "Return a list of circles of states that state-machine SM goes through when starting at all possible states."
  )

#|
Possible nestings:
e.g. SM(4), i.e. with 4 states
NSM(1, SM1(4), SM2(4), SM3(4), SM4(4)), i.e. of level 1, and with 4 state machines, each of which have 4 possible states.

How to do input-output of the state-machine and a nested state-machine? I could make it all stateless, ... or with state? ... hm... In the end I want to be able to make the (nested?) state-machine be able to compute the operation "-" (minus).

One way would be to mark an output state by an infinite circle of 1 state, e.g. 0->5, 5->5. This way the operation "-" (minus) could be modeled as:
output states:
0->0, 1->1, 2->2, 3->3
input states, where the numbers in brackets are the implicit input states (numbers; think of X(A,B)->C as C==A-B):
3(0,0)->0,  4(0,1)->3,  5(0,2)->2,  6(0,3)->1
7(1,0)->1,  8(1,1)->0,  9(1,2)->3,  10(1,3)->2
11(2,0)->2, 11(2,1)->1, 12(2,2)->0, 13(2,3)->3
14(3,0)->3, 15(3,1)->2, 16(3,2)->1, 17(3,3)->0
This is equivalent to
(DEFUN MINUS-SM (A B)
  (ASSERT (AND (<= 0 A 3) (<= 0 B 3)))
  (LOGAND (- A B) 3)).

Another way would be to divide a state into output,input,memory bits. With memory bits a state-machine could model a number storage that has 1 input bit, and output bits=memory bits, and if the input bit is set to 0, then the output bits are stored in the memory bits, and for the input bit set to 1 the memory bits are stored in in the output bits.
e.g. a state machine MEM-SM(3 bits output,4 bits input,3 bits memory) like:
(LET ((N 0))
  (DEFUN MEM (I M)
    (ASSERT (AND (<= 0 I 1) (<= 0 M 7)))
    (WHEN (= I 0) (SETF N M))
    N)).

Nesting of state machines could be done with:
NSM(1, SM1(4), SM2(4), SM3(4), SM4(4)), i.e. of level 1, and with 4 state machines, each of which output 4 possible states.
|#
