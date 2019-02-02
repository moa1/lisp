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
1. Possible nestings:
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

2. It's not clear to me how input/output works with nested state machines. That is, how does an NSM(x,y1,...,yn) receive its input and where do I read its output from?

An idea: An NSM(level=1, SM1(4), ..., SM4(4)) could be implemented like this:
each SMx(4) receives the following states: P, I, M, T, O, where
P = Position, I = Input, M = Memory, T = Target(position), O = Output, where
P and T are positions on the 1-dimensional Memory array A(n), I is the number A(P), O is a number 0 <= O <= n-1, and T is an index in the array A.
I don't like the special purposes of the states PIMTO. Ideally, there should be only two special purpose states: Input I, and output O. And maybe a Memory state M, which specifies the state that the NSM is in.

Another idea for that: An NSM is specified by its contained SMs, and each SM has for example 2 bits of memory, and a position of 2 bit as its position in the NSM 1-dimensional array. For exmaple, the 1-d-array A could be this [0:SM2(1,1), 1:empty, 2:SM1(0,1),SM4(1,0), 3:SM3(0,1)]. Thus, the NSM would be specified as memory M=(1,1,   0,1,1,0, 0,1) binary = 217 decimal, positions P = (2,0,3,2)d = (1,0, 0,0, 1,1, 1,0)b = 142d, and state transitions SMx(inputstate Ix,outputstate Ox), where Ix and Ox are mappings from (P,I,M) to (T,O,M) for all possible P,I,M.
The output of such a NSM (NSM1) would be computed by computing tuples (Tx,Ox,Mx) for all SMx. A problem is then further nesting: how to nest such a NSM1 in a higher-level NSMa, together with NSM2,NSM3,NSM4, i.e. NSMa:=NSM(level=2,NSM1,NSM2,NSM3,NSM4).
NSM1 is defined as NSM1(Px,Ix,Mx,Tx,Ox,Mx | where Xx is X for all x from the 4 state machines SMx, e.g. SM1(P1,I1,M1,T1,O1,M1)). But this does not suffice, as NSM1 also needs the positional parameters P1(level=2.NSMx), and T1(level=2,NSMx) for 1<=x<=4.

An example of NSMa could be: NSMa:=NSM(level=2, NSM1,...,NSM4) where
NSM1:=NSM(level=1, set T:=P, M:=I) (store input in M.)
NSM2:=NSM(level=1; set T:=P, and O:=I+M) (Note that this computes the sum of M and I, regardless of P.)
NSM3:=NSM(level=1; set T:=P+1 or 0 if P+1 would be > 3, or T:=0 otherwise, and M:=M+I for all normal P (i.e. those below 3)). (Note that this successively computes the cumsum of the inputs I, for all input positions 0 up to 3.)
NSM4:=NSM(level=1, set T:=P, O:=I) (constant function.)
and to specify the initial conditions, Mx:=0 for all 0<=x<=3, and initially, P0 = 0, P1 = 1, P2 = 2, P3 = 3.
This specifies NSM1, which will compute the cumsum-array for all inputs I.

Example invocation, for I0=3,I1=1,I2=0,I3=1:
after NSM1, M:=
#|

|#
NSM1 definition : [
M:=0 initially,
SM0 definition: M:=O:=M+I,
SM1 definition: M:=O:=(M>>4)+(M&0xF)+1*16
]
NSM1(SMs=[SM0,SM1],I=2):  M:=O:=0x12
NSM1(SMs=[SM0,SM1],I=3):  M:=O:=0x25
NSM1(SMs=[SM0,SM1],I=-1): M:=O:=0x34
(this computes the sum, and counts in the bits above bit 4 the invocations.)

NSM2 definition : [
M not present,
SM2 definition: O:=I*I
]
NSM2(SMs=SM2,I=2):  O:=4
(this computes the square. )

... (interrupted by longer and longer thinking pauses) ... I think I first want to find so-called "base"-state-machines, i.e. state-machines which are useful for a wide range of machine-learning tasks, for, example, one state-machine could compute the addition of its inputs (where I1:=I&0xF, I2:=(I2>>4)&0xF, and O:=I1+I2), another could compute the cumsum of succesive inputs (where M:=(M+I)&0xFF, O:=M), and so on.
#|

|#
to this end,
probably I should first make the sex-experiment, where different SMs interpret the output of each other:
states M1,M2, where each Mx has a low number of bits, say 4.
SM1(O2=0,I1,M1):=O1
SM2(I1=O1,M2):=O2
SM1(I1=O2,M1):=O3
SM2(I2=O3,M2):=O4
SM1(I1=O4,M1):=O5 ...
then, analyze which (pairs of) state machines are redundant.

"Normalize" lists of state machines, for example, for 2 state machines, which are defined as the following:
pair1:
SM1: 0->1, 1->2, 2->3, 3->0
SM2: 0->1, 1->2, 2->3, 3->3
and
pair2:
SM3: 3->2, 2->1, 1->0, 0->3
SM4: 3->2, 2->1, 1->0, 0->0.
These two pairs are equivalent, when the states (0,1,2,3) of pair1 are re-ordered as (3,2,1,0).
The "normalization function" SMNORM(SMx, SMy, ..., SMn) should find a re-ordering of states, so that e.g. SMNORM(SM1,SM2) == SMNORM(SM3,SM4).
#|
