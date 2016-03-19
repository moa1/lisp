(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :lightningfn)

(defstruct lifun ;"lightningfunction"
  name
  documentation
  state ;we must keep the JIT-STATE object as long as we have a reference to the emitted code!
  jit ;the emitted code
  lisp)

(defparameter *lifuns* nil "The ALIST of lightningfunctions")

(defmacro deflifun (name arguments return-type documentation &body emit-jit-forms)
  "Define the lightningfunction named NAME.
ARGUMENTS is a CFFI arguments list, RETURN-TYPE a CFFI return type.
DOCUMENTATION is a docstring.
EMIT-JIT-FORMS are the forms that emit the "
  (declare (type symbol name)
	   (type string documentation))
  (labels ((cffi-argument-symbols (arguments)
	     (if (null arguments)
		 nil
		 (cons (cadr arguments) (cffi-argument-symbols (cddr arguments))))))
    (let ((state-sym (gensym "STATE"))
	  (jit-sym (gensym "JIT"))
	  (lisp-sym (gensym "LISP"))
	  (lifun-sym (gensym "LIFUN"))
	  (assoc-sym (gensym "ASSOC")))
      `(multiple-value-bind (,state-sym ,jit-sym)
	   (lightningfn:with-new-state ()
	     (lightningfn:prolog)
	     ,@emit-jit-forms
	     (lightningfn:epilog)
	     (values lightningfn:*jit* (lightningfn:emit)))
	 (let* ((,lisp-sym (lambda ,(cffi-argument-symbols arguments)
			     ,documentation
			     (cffi:foreign-funcall-pointer ,jit-sym (:convention :cdecl) ,@arguments ,return-type)))
		(,lifun-sym (make-lifun :name ',name
					:documentation ,documentation
					:state ,state-sym ;we must keep the JIT-STATE object as long as we have a reference to the emitted code!
					:jit ,jit-sym
					:lisp ,lisp-sym)))
	 (let ((,assoc-sym (assoc ',name *lifuns*)))
	   (if ,assoc-sym
	       (progn
		 (warn "Redefining lightningfunction ~S" ',name)
		 (setf (cdr ,assoc-sym) ,lifun-sym))
	       (setf *lifuns* (acons ',name ,lifun-sym *lifuns*))))
	 ,lisp-sym)))))

(defun funcall-lifun (name &rest arguments)
  "Call the lightningfunction named NAME with ARGUMENTS."
  (let ((assoc (assoc name *lifuns*)))
    (assert assoc () "Unknown lightningfunction ~S" name)
    (apply (lifun-lisp (cdr assoc)) arguments)))

;; Each #'GETARG must receive its own #'ARG: See lightning.info: "An example of a simple (recursive) tail call optimization"

(defmacro getarg (reg name)
  "Call (LIGHTNINGFN:GETARG-* REG IN), where REG is the register to set and IN is the input node defined by WITH-ARGS for argument NAME."
  (declare (ignore reg name))
  (error "Cannot use GETARG outside WITH-ARGS"))

(defmacro with-args ((&rest args) &body body)
  "ARGS is a list of ARGs.
Each ARG must be in this format: (NAME TYPE), where NAME is a symbol and TYPE is one of (C UC S US I UI L F D)."
  ;; check format of ARGS
  (do ((rest args (cdr rest)))
      ((null rest))
    (assert (and (consp rest) (consp (car rest)) (symbolp (caar rest)) (find (cadar rest) '(C UC S US I UI L F D)))
	    () "ARG must be of format (NAME TYPE), where NAME is a symbol and TYPE is one of (C UC S US I UI L F D), but is ~S"
	    (if (consp rest) (ldiff rest (cddr rest)) rest)))
  (flet ((intern-arg (type)
	   (let ((type-string (case type
				((f d) (concatenate 'string "-" (string (symbol-name type))))
				(t ""))))
	     (intern (string-upcase (concatenate 'string "arg" type-string)) :lightningfn)))
	 (intern-getarg (type)
	   (intern (string-upcase (concatenate 'string "getarg-" (string (symbol-name type)))) :lightningfn)))
    (let ((arg-alist-sym (gensym "ARG-ALIST"))
	  (arg-alist (loop for (name type) in args collect (cons name (list type (gensym "IN") (intern-arg type) (intern-getarg type)))))
	  (assoc-sym (gensym "ASSOC")))
      `(macrolet ((getarg (reg name)
		    (let* ((,arg-alist-sym ',arg-alist)
			   (,assoc-sym (assoc name ,arg-alist-sym)))
		      (assert (not (null ,assoc-sym)) () "Argument ~S not defined in the argument list of macro with-args" name)
		      `(,(nth 3 (cdr ,assoc-sym)) ,reg ,(nth 1 (cdr ,assoc-sym))))))
	 (let (,@(loop for assoc in arg-alist collect
		      (destructuring-bind (name . (type in arg getarg)) assoc
			(declare (ignore name type getarg))
			(list in `(,arg)))))
	   ,@body)))))

(defmacro retr (reg)
  "Call (LIGHTNINGFN:RETR-* REG), where REG is the register to return."
  (declare (ignore reg))
  (error "Cannot use RETR outside WITH-REGS"))

(defmacro with-regs ((&rest regs) &body body)
  "REGS is a list of REGs.
Each REG must be a list (NAME TYPE), where NAME is a symbol and TYPE is one of (R V F).
The register index will be chosen in increasing order for each TYPE"
  ;; check format of REGS
  (do ((rest regs (cdr rest)))
      ((null rest))
    (assert (and (consp rest) (consp (car rest)) (symbolp (caar rest)) (find (cadar rest) '(R V F)))
	    () "REG must be a list (NAME TYPE), where NAME is a symbol and TYPE is one of '(R V F), but is ~S"
	    (if (consp rest) (ldiff rest (cddr rest)) rest)))
  (flet ((intern-reg (type)
	   (intern (string-upcase (concatenate 'string "reg-" (string (symbol-name type)))) :lightningfn))
	 (intern-retr (type)
	   (let ((type-string (case type
				((f d) (concatenate 'string "-" (string (symbol-name type))))
				(t ""))))
	     (intern (string-upcase (concatenate 'string "retr" type-string)) :lightningfn))))
    (let ((r-index -1)
	  (v-index -1)
	  (f-index -1)
	  (reg-alist-sym (gensym "REG-ALIST"))
	  (reg-alist (loop for (name type) in regs collect (cons name (list type (intern-reg type) (intern-retr type)))))
	  (assoc-sym (gensym "ASSOC")))
      (labels ((make-binding (reg)
		 (destructuring-bind (reg type) reg
		   (list reg (list (intern-reg type) (ecase type ((r) (incf r-index)) ((v) (incf v-index)) ((f) (incf f-index))))))))
	`(macrolet ((retr (reg)
		      (let* ((,reg-alist-sym ',reg-alist)
			     (,assoc-sym (assoc reg ,reg-alist-sym)))
			(assert (not (null ,assoc-sym)) () "Register ~S not defined in the register list of macro with-regs" reg)
			`(,(nth 2 (cdr ,assoc-sym)) ,reg))))
	   (let (,@(loop for reg in regs collect (make-binding reg)))
	     ,@body))))))

;; The macros WITH-REGS and WITH-ARGS can be used so that the following DEFLIFUN
;; (deflifun 1+-i (:int x) :int "A function that increments a given 32-bit number."
;;   (let (in r)
;;     (setf in (lightningfn:arg))
;;     (setf r (lightningfn:reg-r 0))
;;     (lightningfn:getarg-i r in)
;;     (lightningfn:addi r r 1)
;;     (lightningfn:retr r))))
;; becomes
;; (deflifun 1+-i (:int x) :int "A function that increments a given 32-bit number."
;;   (with-regs ((r r))
;;     (with-args ((in i))
;;       (getarg r in)
;;       (lightningfn:addi r r 1)
;;       (retr r))))

(deflifun 1+-i (:int x) :int "increment a 32-bit integer."
  (with-regs ((r r))
    (with-args ((in i))
      (getarg r in)
      (lightningfn:addi r r 1)
      (retr r))))
(assert (= (funcall-lifun '1+-i 5) 6))

(deflifun 1--i (:int x) :int "decrement a 32-bit integer."
  (with-regs ((r r))
    (with-args ((in i))
      (getarg r in)
      (lightningfn:subi r r 1)
      (retr r))))
(assert (= (funcall-lifun '1--i 5) 4))

(deflifun plus2-i (:int x :int y) :int "add two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:addr x x y)
      (retr x))))
(assert (= (funcall-lifun 'plus2-i 5 -4) 1))

(deflifun minus2-i (:int x :int y) :int "subtract two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:subr x x y)
      (retr x))))
(assert (= (funcall-lifun 'minus2-i 5 1) 4))

(deflifun multiply2-i (:int x :int y) :int "multiply two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:mulr x x y)
      (retr x))))
(assert (= (funcall-lifun 'multiply2-i 5 2) 10))

(deflifun divide2-i (:int x :int y) :int "divide two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:divr x x y)
      (retr x))))
(assert (= (funcall-lifun 'divide2-i 16 3) 5))

;; Like LISP's #'REM.
(deflifun rem-i (:int x :int y) :int "return the remainder of dividing two 32-bit integers"
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:remr x x y)
      (retr x))))
(assert (= (funcall-lifun 'rem-i 56 10) 6))
(assert (= (funcall-lifun 'rem-i -56 10) -6))
(assert (= (funcall-lifun 'rem-i 56 -10) 6))
(assert (= (funcall-lifun 'rem-i -56 -10) -6))

;; Like LISP's #'MOD.
(deflifun mod-i (:int x :int y) :int "return the modulus of dividing two 32-bit integers, with the correct value for negative arguments."
  (with-regs ((x r) (y r) (c r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:lti x x 0)
      (lightningfn:lti c y 0)
      (lightningfn:xorr c c x)
      (getarg x in-x)
      (lightningfn:negr c c)
      (lightningfn:remr x x y)
      (lightningfn:andr c c y)
      (lightningfn:nei y x 0) ;this handles the special case that if the remainder is 0, then don't add c
      (lightningfn:negr y y)
      (lightningfn:andr c c y)
      (lightningfn:addr x x c)
      (retr x))))
(assert (= (funcall-lifun 'mod-i 56 10) 6))
(assert (= (funcall-lifun 'mod-i -56 10) 4))
(assert (= (funcall-lifun 'mod-i 56 -10) -4))
(assert (= (funcall-lifun 'mod-i -56 -10) -6))
(assert (= (funcall-lifun 'mod-i 74 -74) 0)) ;fails

#|
(loop for i below 1000 do
     (flet ((random/=0 ()
	      (let ((x (* (1+ (random 100)) (1- (* (random 2) 2)))))
		x)))
       (let ((x (random/=0)) (y (random/=0)))
	 (let ((lisp-result (mod x y))
	       (lightningfn-result (funcall-lifun 'mod-i x y)))
	   (format t "x:~A y:~A lisp-result:~A lightningfn-result:~A~%" x y lisp-result lightningfn-result)
	   (assert (= lisp-result lightningfn-result))))))
|#

(deflifun and2-i (:int x :int y) :int "bitwise AND of two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:andr x x y)
      (retr x))))
(assert (= (funcall-lifun 'and2-i 93 -7) 89))

(deflifun or2-i (:int x :int y) :int "bitwise OR of two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:orr x x y)
      (retr x))))
(assert (= (funcall-lifun 'or2-i 93 -7) -3))

(deflifun xor2-i (:int x :int y) :int "bitwise exclusive OR of two 32-bit integers."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:xorr x x y)
      (retr x))))
(assert (= (funcall-lifun 'xor2-i 93 -7) -92))

(deflifun lshr-i (:int x :int y) :int "left shift first 32-bit integer by second 32-bit integer."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:lshr x x y)
      (retr x))))
(assert (= (funcall-lifun 'lshr-i 93 2) 372))

(deflifun rshr-i (:int x :int y) :int "left shift first 32-bit integer by second 32-bit integer."
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:rshr x x y)
      (retr x))))
(assert (= (funcall-lifun 'rshr-i 93 2) 23))

(deflifun ltr (:int x :int y) :int "return (x < y)"
  (with-regs ((x r) (y r))
    (with-args ((in-x i) (in-y i))
      (getarg x in-x)
      (getarg y in-y)
      (lightningfn:ltr x x y)
      (retr x))))
