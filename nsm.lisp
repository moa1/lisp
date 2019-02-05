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

(defun last1 (list)
  (car (last list)))

(defun make-hash-table-const-value (n value &optional (start 0))
  "Fill a hash-table with keys from START below N with value VALUE"
  (let ((ht (make-hash-table)))
    (loop for i from start below n do
	 (setf (gethash i ht) value))
    ht))

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

(defun sm-path-circle (sm state)
  "Return two values: the non-circle part of a path starting at STATE and the circle part."
  (let ((ht (make-hash-table))
	(path nil)
	(circle nil))
    (loop do
	 (let ((c (gethash state ht 0)))
	   (cond
	     ((= c 0)
	      (push state path)
	      (setf (gethash state ht) 1))
	     ((= c 1)
	      (incf (gethash state ht))
	      (push state circle))
	     ((>= c 2)
	      (return))))
	 (setf state (sm-next sm state)))
    ;; remove CIRCLE from PATH
    (loop for e in circle do
	 (pop path))
    (values (nreverse path) (nreverse circle))))

(defun test-sm-path-circle ()
  (let* (                       ;0 1 2 3
	 (sm (make-sm 4 :trans '(2 2 3 3))))
    (multiple-value-bind (path circle) (sm-path-circle sm 1)
      (assert (equal path '(1 2)))
      (assert (equal circle '(3))))))
(test-sm-path-circle)

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
|#

#|
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
|#

#|
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

how to think about this: the state-transitions of the state-machines are a set of graphs, where each state-transition is an edge in the graph of the respective state-machine.
e.g.
SMa: 0->1, 1->2, 2->1, 3->2
SMb: 0->2, 1->2, 2->3, 3->3
is represented by the graphs
             /->\              0b ->-\      /->\
  a: 0a -> 1a    2a <- 3a   b:       2b -> 3b  |
             \<-/              1b ->-/      \->/
If another set of graphs SMc, and SMd have the following graphs
             /->\              2c ->-\      /->\
  d: 2d -> 1d    3d <- 0d   c:       3c -> 0c  |
             \<-/              1c ->-/      \->/
then these two sets of graphs are equivalent under the translations
    0a    1a    2a    3a       0b    1b    2b    3b
    ||    ||    ||    ||       ||    ||    ||    ||
    2d    1d    3d    0d   ,   2c    1c    3c    0c
where "||" is a single sign and means "=", i.e. equals, read vertically.

Thus, all the following state-machines SMc and SMd would be equal to SMa,SMb, after normalization:
SMc: 0->0, 1->3, 2->3, 3->0  (=:SMc1)
SMd: 0->3, 1->3, 2->1, 3->1  (=:SMd1)
or
SMc: =SMc1
SMd: 0->1, 1->3, 2->3, 3->1  (=:SMd2) (i.e. swapping of 2d with 0d)
(or any permutations of the numbers 0,1,2,3 in both pairs above.)
|#

(defun sm-trans-norm (sm)
  (let* ((length (length (sm-trans sm)))
	 (inputs (let ((inputs (make-array length :element-type 'list :initial-element nil)))
		   (loop for s across (sm-trans sm) for e from 0 do
			(push e (aref inputs s)))
		   inputs))
	 (inputs-count (loop for e from 0
			  for i across inputs
			  collect
			    (multiple-value-bind (path circle)
				(sm-path-circle sm e)
			      (list e (+ (* (length circle) length) (length path))))))
	 (inputs-sorted (sort inputs-count #'< :key #'cadr))
	 (inputs-reverse (let ((array (make-array length)))
			   (loop for s from 0 for e in inputs-sorted do
				;;(prind s (car e))
				(setf (aref array (car e)) s))
			   array))
	 (trans-ordered (loop for pair in inputs-sorted collect
			     (let* ((next (sm-next sm (car pair))))
			       (aref inputs-reverse next)))))
    ;; Sort states by cumulative STATE-INPUTS, e.g. (MAKE-SM 4 :TRANS '(1 2 1 2)) should have cumulative input-weights: '(0 3 3 0), because a circle counts
    ;;(prind inputs-count inputs-sorted inputs-reverse)
    trans-ordered))

(defun test-sm-trans-norm ()
  (let* (                       ;0 1 2 3
	 (sm (make-sm 4 :trans '(1 2 1 2))))
    (assert (equal (sm-trans-norm sm) '(1 0 0 1)))
    (multiple-value-call #'values (sm-path-circle sm 1) (sm-trans-norm sm))))
(test-sm-trans-norm)

#|
How to input/output:
1. A state-machine has an input and an output state, i.e. the state the machine goes into on the given input state.
2. Every state-machine has thus a output state
3. The containing NSM has, let's say 4 SMs. Each SM is run in turn, once. Each SM's output state is interpreted as the array element index that it should jump into.
4. The containing NSM has, for each of its SMs, a index. use it as the output of the NSM.
5. The input of the NSM is interpreted as the input for the SMs.
6. Thus, the NSM has input and output, and we can encapsulate it in an NNSM (NSM of level 2).

Example:
NSM with 4 SMs, and 4 elements in the one-dimensional array. Let's say the input is 3,2,3,0. Then SM1 has as input state 3, SM2 2, SM3 3, SM4 0. Let's say SM1 goes into output state 0, SM2 3, SM3 3, SM4 1. Then the output of the NSM is 0,3,3,1. As easy as that. How to do while loops is left as an excercise for the Genetic Algorithm, that should evolve suitable SMs, and NSMs. I don't have a clue how to do loops, BUT, are they really necessary? Start with the basics. And the basics are NESTED STATE MACHINES.
|#
