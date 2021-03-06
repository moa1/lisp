;; The 5 fields of #'GET-SETF-EXPANSION are necessary to be able to implement PSETF. See "DEFINE-SETF-EXPANDER LASTGUY-example" below for a rationale of the 5th field, GETTER. Another example where it's needed is (SETF LDB), see CLHS 5.1.1.2.1 Examples of Setf Expansions, and Macro DEFINE-SETF-EXPANDER.

;; DEFINE-SETF-EXPANDER is the more general form of DEFSETF.

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

;; FROM CLHS Function GET-SETF-EXPANSION: Note that there is an error: "(IF (CDR STORES)" must probably be "(IF ,(CDR STORES)".
(defmacro xpop (place &environment env)
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion place env)
    ;;(prind place ":" temps vals stores setter getter)
    `(progn
       (format t "XPOP PLACE:~S (GET-SETF-EXPANSION ~S):~%" ',place ',place)
       (format t "  (TEMPS=~S VALS=~S STORES=~S SETTER=~S GETTER=~S)~%" ',temps ',vals ',stores ',setter ',getter)
       (let* (,@(mapcar #'list temps vals) (,(car stores) ,getter))
	 (prind "XPOP EVALUATION" ,(car stores) ,(cdr stores))
	 (if ,(cdr stores)
	     (error "Can't expand this."))
	 (prog1 (car ,(car stores))
	   (setq ,(car stores) (cdr ,(car stores)))
	   ,setter)))))

(defmacro test-xpop (place list-name)
  (let* ((xpop-form `(xpop ,place))
	 (xpop-expanded (macroexpand xpop-form)))
    `(progn
       (format t "macroexpanding: ~S~%" ',xpop-form)
       (format t "macroexpansion: ~S~%" ',xpop-expanded)
       (format t "executing macroexpansion~%")
       (format t "~S:~S ~S:~S~%" ',xpop-form ,xpop-expanded ',list-name ,list-name))))

(defun test1 ()
  (let ((a (list 1 2 3 4 5)))
    (test-xpop a a)))

(defun test2 ()
  (let ((a (list 1 2 3 4 5)))
    (test-xpop (cddr a) a)))

(defstruct (bla
	     (:constructor make-bla (a b c)))
  a b c)

(let ((x (make-bla 1 2 3)))
  (setf (bla-a x) 5))

;; CL-USER> (get-setf-expansion '(bla-a x))
;; (#:OBJ)
;; (X)
;; (#:NEW632)
;; (SB-KERNEL:%INSTANCE-SET (THE BLA #:OBJ) 1 #:NEW632)
;; (SB-KERNEL:%INSTANCE-REF (THE BLA #:OBJ) 1)
;; CL-USER> (get-setf-expansion '(bla-b x))
;; (#:OBJ)
;; (X)
;; (#:NEW633)
;; (SB-KERNEL:%INSTANCE-SET (THE BLA #:OBJ) 2 #:NEW633)
;; (SB-KERNEL:%INSTANCE-REF (THE BLA #:OBJ) 2)
;; CL-USER> (get-setf-expansion '(bla-c x))
;; (#:OBJ)
;; (X)
;; (#:NEW634)
;; (SB-KERNEL:%INSTANCE-SET (THE BLA #:OBJ) 3 #:NEW634)
;; (SB-KERNEL:%INSTANCE-REF (THE BLA #:OBJ) 3)

(defun psetf-example ()
  (let ((place '(bla-a x)))
    (multiple-value-bind (temps vals stores setter getter)
	(get-setf-expansion place nil)
      (prind place ":" temps vals stores setter getter)))
  (macroexpand '(psetf
		 (bla-a x) (bla-b x)
		 (bla-b x) (bla-a x))))

;; From CLHS Macro DEFINE-SETF-EXPANDER
(defun lastguy (x) (car (last x)))

;; The DEFINE-SETF-EXPANDER LASTGUY-example demonstrates why the fifth field, GETTER, is required in #'GET-SETF-EXPANSION and #'DEFINE-SETF-EXPANDER.
;; (Note that it is impossible to define (SETF LDB) using DEFSETF, because the place, i.e. INTEGER in the case of LDB, is bound by DEFSETF to a local variable:
;; CL-USER> (defsetf ldb1 (bytespec integer) (new-value)
;; 	   (let ((int (gensym "INT")))
;; 	     (prind int integer)
;; 	     `(let ((,int ,integer))
;; 		(prind ,int ,new-value ,bytespec)
;; 		(setf ,integer (dpb ,new-value ,bytespec ,int)))))
;; LDB1
;; CL-USER> (let ((a 127))
;; 	   (setf (ldb1 (byte 2 1) a) 0)
;; 	   a)
;; INT:#:INT0 INTEGER:#:INTEGER ;WRONG: this should be INTEGER:A (but that's the way DEFSETF is defined, see CLHS Macro DEFSETF: "During the evaluation of the forms, the variables in the lambda-list and [...] are bound to names of temporary variables")
;; INT0:127 NEW1:0 (BYTE 2 1):(2 . 1)
;; 127) ;;WRONG: this should be 121

(define-setf-expander lastguy (x &environment env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignore newval setter))
    (let ((store (gensym)))
      (values dummies
	      vals
	      `(,store)
	      `(progn (rplaca (last ,getter) ,store) ,store)
	      `(lastguy ,getter)))))

(defun atest1 ()
  (let ((a (list 'a 'b 'c 'd))
	(b (list 'x))
	(c (list 1 2 3 (list 4 5 6))))
    (setf (lastguy a) 3)
    (setf (lastguy b) 7)
    (setf (lastguy (lastguy c)) 'lastguy-symbol)
    (values a b c)))

;; CL-USER> (get-setf-expansion '(lastguy c))
;; NIL
;; NIL
;; (#:G615)
;; (PROGN (RPLACA (LAST C) #:G615) #:G615)
;; (LASTGUY C)
;; CL-USER> (get-setf-expansion '(lastguy (lastguy c)))
;; NIL
;; NIL
;; (#:G612)
;; (PROGN (RPLACA (LAST (LASTGUY C)) #:G612) #:G612)
;; (LASTGUY (LASTGUY C))

;; see in CLHS "Macro DEFINE-SETF-EXPANDER" the SETF expander for LDB for an example with multiple arguments.

(defun flet1 ()
  (let ((cons (cons 1 2)))
    (setf (lastguy cons) 5)
    cons))

(defun flet2 ()
  (values
   (flet (((setf bla) (value cons)
	    (setf (car cons) value)))
     (let ((cons (list 1 2 3)))
       (setf (bla cons) 5)
       cons))
   (flet (((setf lastguy) (value cons) ;This function is not used, because in CLHS on FLET it says "Also, within the scope of flet, global setf expander definitions of the function-name defined by flet do not apply. Note that this applies to (defsetf f ...), not (defmethod (setf f) ...)." In CLHS Glossary on "function name", it is defined as "A symbol or a list (setf symbol) that is the name of a function in that environment." So the function-name of (SETF LASTGUY) is (SETF LASTGUY), not LASTGUY, so in the form (SETF (LASTGUY CONS) 5) below the global setf expander definition of LASTGUY does apply. Note that if we name the function LASTGUY instead of (SETF LASTGUY), then the global setf expansion definition of LASTGUY does not apply, and (SETF LASTGUY) is undefined in the body of FLET. (As is demonstrated in #'FLET-ERROR1 below.)
	    (setf (car cons) value)))
     (let ((cons (list 1 2 3)))
       (setf (lastguy cons) 5)
       cons)))) ;returns (VALUES (5 2 3) (1 2 5))

;; CLHS on FLET says "Also, within the scope of flet, global setf expander definitions of the function-name defined by flet do not apply. Note that this applies to (defsetf f ...), not (defmethod (setf f) ...).". Therefore the form (SETF (LASTGUY CONS) 5) refers to an unknown setf expansion.
;; (defun flet-error1 ()
;;   (flet ((lastguy (cons)
;; 	   (car cons)))
;;     (let ((cons (cons 1 2)))
;;       (setf (lastguy cons) 5)
;;       cons)))

(defun flet3 ()
  (let ((cons (list 1 2 3)))
    (flet ((lastguy (cons)
	     (setf (lastguy cons) 5))) ;global setf-expander defintions only do not apply in the body of FLET and LABELS, but they do apply in the definitions.
      (lastguy cons)
      cons))) ;returns (1 2 5)

(defsetf bar baru)
(defun bar (c)
  (car c))
(defun baru (c n)
  (rplaca c n))
(defsetf bar baru)
(defun flet-error2 ()
  (flet ((bar (c)
	   (cdr c)))
    (let ((x (cons 1 2)))
      (setf (bar x) 5)))) ;execution must raise an error since (CLHS FLET) "within the scope of FLET, global setf expander definitions of the function-name defined by FLET do not apply. Note that this applies to (defsetf f ...), not (defmethod (setf f) ...)."

(defmethod (setf bar2) (n c) ;note that N and C must be swapped compared to #'BARU.
  ;;(format t "method (setf bar2)~%")
  (rplaca c n))
(defun flet4 ()
  (let ((x (cons 1 2)))
    (flet ((bar2 (c)
	     (format t "bar2~%")
	     (car c)))
      (declare (ignorable #'bar2))
      (setf (bar2 x) 5)))) ;returns (5 . 2)

(defun macrolet1 ()
  (let ((cons2 (list 8 9)))
    (macrolet ((lastguy (cons)
		 'cons2))
      (let ((cons (cons 1 2)))
	(setf (lastguy cons) 5) ;CLHS on MACROLET says "Within the body of macrolet, global setf expander definitions of the names defined by the macrolet do not apply; rather, setf expands the macro form and recursively process the resulting form.", thus (LASTGUY CONS) is expanded to CONS2.
	(values cons cons2))))) ;returns (VALUES (1 . 2) 5)

#|
(defun firsttwo (list)
  (values (car list) (cadr list)))

(defun listmatrix (list i j)
  (nth j (nth i list)))

(define-setf-expander listmatrix (list i j &environment env)
  (multiple-value-bind (temps values stores setter getter)
      (get-setf-expansion list env)
    (declare (ignore stores setter))
    (let ((store (gensym "STORE")))
      (values temps
	      values
	      `(,store)
	      `(setf (nth ,j (nth ,i ,getter)) ,store)
	      `(listmatrix ,list ,i ,j)))))

(defun setf-all-form (places value env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion places env)
    (prind stores)
    `(let ((,stores ,value)
	   ,@(mapcar #'list vars vals))
       ,setter)))

(defmacro setf-all (places value &environment env)
  (setf-all-form places value env))
|#

(defun setf-values ()
  (let (i r)
    (setf (values i r) (truncate 10 7))
    (values i r)))
