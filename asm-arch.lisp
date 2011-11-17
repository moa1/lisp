(defstruct ins
  (type nil :type symbol)
  (opcode1 nil)
  (opcode2 nil))

(defclass asm ()
  ((ax :accessor asm-ax
       :initform 0
       :initarg :ax)
   (bx :accessor asm-bx
       :initform 0
       :initarg :bx)
   (cx :accessor asm-cx
       :initform 0
       :initarg :cx)
   (dx :accessor asm-dx
       :initform 0
       :initarg :dx)
   (code :accessor asm-code
	 :initform nil
	 :initarg :code)
   (ip :accessor asm-ip
       :initform 0
       :initarg :ip)
   (cmpflag :accessor asm-cmpflag
	    :initform 0
	    :initarg :cmpflag)
   (rdp :accessor asm-rdp ; position read
	:initform 0
	:initarg :rdp)
   (rdc :accessor asm-rdc ; content read
	:initform nil
	:initarg :rdc)
   (out :accessor asm-out
	:initform nil
	:initarg :out)))

(defun cmp (a b)
  (if (< a b)
      -1
      (if (> a b)
	  1
	  0)))

(defmethod run ((a asm) maxsteps)
  (declare (optimize (debug 3)))
  ;;(print (list "run" (asm-code a)))
  (if (null (asm-code a))
      (return-from run maxsteps))
  (dotimes (step maxsteps maxsteps)
    (let* ((instruction (elt (asm-code a) (asm-ip a)))
	   (ins (ins-type instruction))
	   (opcode1 (ins-opcode1 instruction))
	   (opcode2 (ins-opcode2 instruction))
	   (val (cond ((eq opcode1 'ax) (asm-ax a))
		      ((eq opcode1 'bx) (asm-bx a))
		      ((eq opcode1 'cx) (asm-cx a))
		      ((eq opcode1 'dx) (asm-dx a))
		      ((numberp opcode1) opcode1))))
      (assert (< (asm-ip a) (length (asm-code a))))
      ;; (print (list ins opcode1 opcode2 "ip" (asm-ip a)))
      (setf (asm-ip a) (mod (1+ (asm-ip a)) (length (asm-code a)))) ; advance ip
      (ecase ins
	(:mov (cond ((eq opcode2 'ax) (setf (asm-ax a) val))
		    ((eq opcode2 'bx) (setf (asm-bx a) val))
		    ((eq opcode2 'cx) (setf (asm-cx a) val))
		    ((eq opcode2 'dx) (setf (asm-dx a) val))))
	(:add (cond ((eq opcode2 'ax) (setf (asm-ax a) (+ (asm-ax a) val)))
		    ((eq opcode2 'bx) (setf (asm-bx a) (+ (asm-bx a) val)))
		    ((eq opcode2 'cx) (setf (asm-cx a) (+ (asm-cx a) val)))
		    ((eq opcode2 'dx) (setf (asm-dx a) (+ (asm-dx a) val)))))
	(:sub (cond ((eq opcode2 'ax) (setf (asm-ax a) (- (asm-ax a) val)))
		    ((eq opcode2 'bx) (setf (asm-bx a) (- (asm-bx a) val)))
		    ((eq opcode2 'cx) (setf (asm-cx a) (- (asm-cx a) val)))
		    ((eq opcode2 'dx) (setf (asm-dx a) (- (asm-dx a) val)))))
	(:cmp (setf (asm-cmpflag a) (cmp (cond ((eq opcode2 'ax) (asm-ax a))
					       ((eq opcode2 'bx) (asm-bx a))
					       ((eq opcode2 'cx) (asm-cx a))
					       ((eq opcode2 'dx) (asm-dx a))
					       ((numberp opcode2) opcode2))
					 val)))
	(:jl (setf (asm-ip a) (mod (+ (asm-ip a) (if (= (asm-cmpflag a) -1)
							val
							0))
				   (length (asm-code a)))))
	(:jg (setf (asm-ip a) (mod (+ (asm-ip a) (if (= (asm-cmpflag a) 1)
							val
							0))
				   (length (asm-code a)))))
	(:je (setf (asm-ip a) (mod (+ (asm-ip a) (if (= (asm-cmpflag a) 0)
							val
							0))
				   (length (asm-code a)))))
	(:jmp (setf (asm-ip a) (mod (+ (asm-ip a) val)
				    (length (asm-code a)))))
	(:nop nil)
	(:prd (setf (asm-rdp a) (mod val (length (asm-code a)))))
	(:rd (setf (asm-rdc a) (elt (asm-code a) (asm-rdp a)))
	     (setf (asm-rdp a) (mod (1+ (asm-rdp a)) (length (asm-code a)))))
	(:out (if (not (null (asm-rdc a)))
		  (setf (asm-out a) (cons (asm-rdc a) (asm-out a)))))
	(:qt (return step)))
      ;;(print (list (asm-ax a) (asm-bx a) (asm-cx a) (asm-dx a) (asm-ip a) (asm-cmpflag a)
	;;	   (asm-rdp a) (asm-rdc a)))
      ;;(print (asm-out a))
      )))

(defun choice (l)
  (elt l (random (length l))))

(defun mutate-instruction (i p)
  (if (>= p (random 1.0))
      (let* ((type (choice '(:mov :add :sub :cmp :jl :jg :je :jmp :nop :prd :rd :out :qt)))
	     opcode1 opcode2)
	(setf opcode1 (choice (list 'ax 'bx 'cx 'dx (- (random 201) 100))))
	(setf opcode2 (choice (list 'ax 'bx 'cx 'dx (- (random 201) 100))))
;;	(ecase (ins-type i)
;;	  ((:mov :add :sub :cmp) (ecase (random 2)
;;				   (0 (setf opcode1 (choice (list 'ax 'bx 'cx 'dx (- (random 201) 100)))))
;;				   (1 (setf opcode2 (choice (list 'ax 'bx 'cx 'dx (- (random 201) 100)))))))
;;	  ((:jl :jg :je :jmp :prd) (setf (ins-opcode1 i) (choice (list 'ax 'bx 'cx 'dx (- (random 201) 100)))))
;;	  ((:nop :rd :out :qt) nil))
	(make-ins :type type :opcode1 opcode1 :opcode2 opcode2))
      i))
				     
(defmethod mutate ((a asm) p)
  (let ((result nil))
    ;;(print (list "mutate a" (asm-code a)))
    (dotimes (i (length (asm-code a)) a)
      (setf result (cons (mutate-instruction (elt (asm-code a) i) p) result)))
    ;;(print (list "mutate result" result))
    (reverse result)))

(defmethod crossover-one-point ((a asm) (b asm) p)
  (if (>= 0.5 (random 1.0))
      (let ((c a))
	(setf a b)
	(setf b c)))
  (if (>= p (random 1.0))
      (let* ((length1 (length (asm-code a)))
	     (length2 (length (asm-code b)))
	     (point1 (random length1))
	     (point2 (random length2))
	     (result (concatenate 'list (subseq (asm-code a) 0 point1) (subseq (asm-code b) point2))))
	;;(print (list "crossover-one-point result" result))
	result)
      (asm-code a)))

(defmethod get-out ((a asm))
  (reverse (asm-out a)))

(defun make-code (shortcode)
  (labels ((rec (shortcode instructions)
	     (if (null shortcode)
		 (nreverse instructions)
		 (let ((ins (car shortcode)))
		   (rec (cdr shortcode)
			(cons (ecase (car ins) 
				(mov (make-ins :type :mov :opcode1 (cadr ins) :opcode2 (caddr ins)))
				(add (make-ins :type :add :opcode1 (cadr ins) :opcode2 (caddr ins)))
				(sub (make-ins :type :sub :opcode1 (cadr ins) :opcode2 (caddr ins)))
				(cmp (make-ins :type :cmp :opcode1 (cadr ins) :opcode2 (caddr ins)))
				(jl  (make-ins :type :jl :opcode1 (cadr ins)))
				(jg  (make-ins :type :jg :opcode1 (cadr ins)))
				(je  (make-ins :type :je :opcode1 (cadr ins)))
				(jmp (make-ins :type :jmp :opcode1 (cadr ins)))
				(nop (make-ins :type :nop))
				(prd (make-ins :type :prd :opcode1 (cadr ins)))
				(rd  (make-ins :type :rd))
				(out (make-ins :type :out))
				(qt  (make-ins :type :qt)))
			      instructions))))))
    (rec shortcode nil)))

()

(defparameter init-asm (make-instance 'asm :code (make-code '((prd 0)
							      (mov 8 ax)
							      (rd)
							      (out)
							      (sub 1 ax)
							      (cmp ax bx)
							      (nop)
							      (nop)
							      (jl -7)
							      (jmp -1)))))

(defun seq (from to)
  (if (= from to)
      nil
      (cons from (seq (1+ from) to))))

(defun tournament (steps poolsize)
  (declare (optimize (debug 3)))
  (let* ((mutate-p .01)
	 (crossover-one-point-p .9)
	 (runsteps 100)
	 (pool (mapcar (lambda (x)
			 (declare (ignore x))
			 (make-instance 'asm :code (mutate init-asm mutate-p)))
		       (seq 0 poolsize))))
    ;;(print (list "pool" (mapcar (lambda (x) (asm-code x)) pool)))
    (dotimes (step steps pool)
      (print (list "step" step))
      (let* ((n1 (random poolsize))
	     (n2 (random poolsize))
	     (contestant-num (random poolsize))
	     (contestant (elt pool contestant-num))
	     (child (make-instance 'asm
				   :code (mutate (make-instance 'asm
								:code (crossover-one-point (elt pool n1)
											   (elt pool n2)
											   crossover-one-point-p))
						 mutate-p)))
	     (steps-child (run child runsteps))
	     (steps-contestant (run contestant runsteps))
	     (child-child (make-instance 'asm :code (asm-out child)))
	     (contestant-child (make-instance 'asm :code (asm-out contestant)))
	     (steps-child-child (run child-child runsteps))
	     (steps-contestant-child (run contestant-child runsteps)))
	(print (list steps-child steps-contestant))
	(if (< steps-child-child steps-contestant-child)
	    (progn
	      (print (list "contestant" (asm-code contestant) "child" (asm-code child)))
	      (setf (elt pool contestant-num) child)))))))

(mapcar (lambda (x) (asm-code x)) (tournament 100 2))
