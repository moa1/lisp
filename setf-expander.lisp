;; I don't understand why there are 5 fields of #'GET-SETF-EXPANSION. Maybe to be able to guarantee left-to-right evaluation in macros?
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



;; From CLHS Macro DEFINE-SETF-EXPANDER
(defun lastguy (x) (car (last x)))

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
