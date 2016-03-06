(load "~/quicklisp/setup.lisp")
(ql:quickload :dlist3)
(use-package :dlist3)

;; TODO: don't use package dlist, write a dllist package, which is like dlist, but the semantics is like this: (dlist-first d) points to the left-most element, and (dlist-last d) points to the element after the right-most element. this makes checking if we passed an empty dlist easier, because in that case (dlist-first d) == (dlist-last d). (Another advantge is that we don't need to re-use NIL as the empty DLIST. We will always have at least one element per DLIST.) However, there is a downside to simpler code: storage of a dlist with n elements takes up (1+ n) elements.

;; necessary here, b/c otherwise compilation of refal-eval fails.
(defun make-counter (&optional (counter 0))
  (declare (type fixnum counter))
  (if (<= counter 0)
      (values (lambda () 1)
	      (lambda (counter-delta) (declare (ignore counter-delta))))
      (let ((c counter))
	(check-type c fixnum)
	(values (lambda () (decf c))
		(lambda (counter-delta) (incf c counter-delta))))))

;; Refal pattern matching

(defun make-svar ()
  (gensym "S."))

(defun make-tvar ()
  (gensym "T."))

(defun make-evar ()
  (gensym "E."))

(declaim (inline svarp))
(defun svarp (symbol)
  (and (symbolp symbol)
       (let ((s (symbol-name symbol)))
	 (and (>= (length s) 2)
	      (eq (elt s 0) #\S)
	      (eq (elt s 1) #\.)))))

(declaim (inline tvarp))
(defun tvarp (symbol)
  (and (symbolp symbol)
       (let ((s (symbol-name symbol)))
	 (and (>= (length s) 2)
	      (eq (elt s 0) #\T)
	      (eq (elt s 1) #\.)))))

(declaim (inline evarp))
(defun evarp (symbol)
  (and (symbolp symbol)
       (let ((s (symbol-name symbol)))
	 (and (>= (length s) 2)
	      (eq (elt s 0) #\E)
	      (eq (elt s 1) #\.)))))

;; TODO: make var-type by making it a macro "var-type-case" and accepting the symbol and three bodies (for svar, tvar, and evar. (so that comparison against e.g. 'svar can be avoided.
(declaim (inline var-type))
(defun var-type (symbol)
  (declare (type symbol symbol)
	   ;;(optimize speed)
	   )
  (and (symbolp symbol)
       (let ((s (symbol-name symbol)))
	 (and (>= (length s) 2)
	      (eq (elt s 1) #\.)
	      (case (elt s 0)
		(#\S 'svar)
		(#\T 'tvar)
		(#\E 'evar)
		(t nil))))))

(deftype s-variable ()
  "A Refal S-variable, i.e. a symbol starting with 'S.'"
  `(and symbol (satisfies svarp)))

(deftype t-variable ()
  "A Refal T-variable, i.e. a symbol starting with 'T.'"
  `(and symbol (satisfies tvarp)))

(deftype e-variable ()
  "A Refal E-variable, i.e. a symbol starting with 'E.'"
  `(and symbol (satisfies evarp)))

(declaim (inline bind-closed))
(defun bind-closed (var val closed)
  "Return an object representing the variables and their values bound in CLOSED, and the variable named VAR bound to VAL.
Return 'FAIL if the variable VAR is already bound to another value than VAL.
Return the unmodified CLOSED if VAR is bound to a value equal to VAL (under equality test TEST)."
  (declare (type symbol var)
	   (type list closed))
  ;; NOTE: this function must not modify CLOSED, since OPEN-VAR depends on an unmodified CLOSED.
  ;;(when (<= (funcall c) 0)
  ;;  (throw 'overrun nil))
  (let ((acons (assoc var closed)))
    (if (null acons)
	(acons var val closed)
	(let ((stored (cdr acons)))
	  (if (or (equal val stored) (and (dlistp stored) (dlistp val) (dlist= stored val)))
	      closed
	      (throw 'patmat 'fail))))))

;;;; Pattern matching

(defun map-open-dcons-list-to-dlist (open)
  (labels ((rec (open result)
	     (if (null open)
		 (reverse result)
		 (destructuring-bind (exp-l exp-r pat-l pat-r &rest open) open
		   (declare (type dcons exp-l exp-r pat-l pat-r))
		   (rec open (cons (list (make-instance 'dlist :first exp-l :last exp-r)
					 (make-instance 'dlist :first pat-l :last pat-r))
				   result))))))
    (rec open nil)))

;; One idea here was to (throw (values closed open)) when finished in close-r or close-l. (i.e. don't only throw on error, but on normal return to bypass some stack frames.). However, that needs different program logic, because recursion on a dlist needs close-lr to return a value. I could (catch 'patmat) around the call to close-lr, but then we'd have to check if it returned 'fail, and rethrow that. So I rather pass values back, and only throw on 'FAILing pattern matching.

(defun close-exp-null (pat-l pat-r closed)
  "Bind the pattern described by PAT-L and PAT-R to a NIL-expression (therefore, the only possible binding pattern is a list of e-variables, which are all bound to NIL).
If any closed variables in the pattern have conflicting bindings in CLOSED, return 'FAIL.
This function returns NIL as the open variables, because there can't be open variables in an NIL-expression."
  (declare (type dcons pat-l pat-r)
	   (type list closed)
	   ;;(optimize speed)
	   )
;;  (print (list "close-exp-null pat" (make-instance 'dlist :first pat-l :last pat-r)))
  (let ((pat-l-first (next pat-l)))
    (if (eq pat-l-first pat-r)
	closed
	(let ((pat (data pat-l-first)))
;;	  (print (list "close-exp-null pat" pat))
	  (if (and (symbolp pat) (evarp pat))
	      (close-exp-null pat-l-first pat-r (bind-closed pat (dlist) closed))
	      (throw 'patmat 'fail))))))

(defun close-l (exp-l exp-r pat-l pat-r closed open-l)
  "As long as the first element in EXP (in the dlist delimited by EXP-L and EXP-R) matches to the first element in PAT (in the dlist delimited by PAT-L and PAT-R), extend EXP-L and PAT-L to the right."
  (declare (type dcons exp-l exp-r pat-l pat-r)
	   (type list closed open-l)
	   ;;(optimize speed)
	   )
;;  (print (list "close-l exp" (make-instance 'dlist :first exp-l :last exp-r)
;;	       "pat" (make-instance 'dlist :first pat-l :last pat-r)))
  (let ((pat-l-next (next pat-l))
	(exp-l-next (next exp-l)))
    (declare (type dcons pat-l-next exp-l-next))
    (if (eq pat-l-next pat-r)
	;; If exp is not empty, (but pat is,) fail.
	(if (eq exp-l-next exp-r)
	    (values closed open-l)
	    (throw 'patmat 'fail))
	(if (eq exp-l-next exp-r)
	    (values (close-exp-null pat-l pat-r closed) open-l)
	    (let ((pat (data pat-l-next))
		  (exp (data exp-l-next)))
;;	      (print (list "close-l pat" pat "exp" exp))
	      (etypecase pat
		(number (if (eq pat exp)
			    (close-l exp-l-next exp-r pat-l-next pat-r closed open-l)
			    (throw 'patmat 'fail)))
		(symbol (case (var-type pat)
			  ((svar) (if (or (symbolp exp) (numberp exp))
				      (close-l exp-l-next exp-r pat-l-next pat-r (bind-closed pat exp closed) open-l)
				      (throw 'patmat 'fail)))
			  ((tvar) (if (or (symbolp exp) (numberp exp) (dlistp exp))
				      (close-l exp-l-next exp-r pat-l-next pat-r (bind-closed pat exp closed) open-l)
				      (throw 'patmat 'fail)))
			  ((evar) (if (eq (next pat-l-next) pat-r)
				      ;; there is no more unbound variable after the variable named by PAT, therefore bind PAT to the compound or term determined by EXP-L and EXP-R.
				      (values
				       ;; TODO: don't copy when binding e-variables to the EXPression, just (make-instance 'dlist :left exp-l :right exp-r) or something like that. only copy when concatenating the variables to the result dlist.
				       (bind-closed pat (make-instance 'dlist :first exp-l :last exp-r) closed)
				       open-l)
				      (close-r exp-l exp-r pat-l pat-r closed open-l nil)))
			  (t (if (eq pat exp)
				 (close-l exp-l-next exp-r pat-l-next pat-r closed open-l)
				 (throw 'patmat 'fail)))))
		(dlist (multiple-value-bind (closed open-1) (close-lr exp pat closed)
;;			 (print (list "close-lr returned into close-l with open-1" (map-open-dcons-list-to-dlist open-1) "closed" closed))
			 (close-l exp-l-next exp-r pat-l-next pat-r closed (nconc open-l open-1))))))))))

(defun close-r (exp-l exp-r pat-l pat-r closed open-l open-r)
  "As long as the last element in EXP (in the dlist delimited by EXP-L and EXP-R) matches to the last element in PAT (in the dlist delimited by PAT-L and PAT-R), extend EXP-R and PAT-R to the left."
  (declare (type dcons exp-l exp-r pat-l pat-r)
	   (type list closed open-l)
	   ;;(optimize speed)
	   )
;;  (print (list "close-r exp" (make-instance 'dlist :first exp-l :last exp-r)
;;	       "pat" (make-instance 'dlist :first pat-l :last pat-r)))
  (let ((pat-r-prev (prev pat-r))
	(exp-r-prev (prev exp-r)))
    (declare (type dcons pat-r-prev exp-r-prev))
    (if (eq pat-l pat-r-prev)
	;; If exp is not empty, (but pat is,) fail.
	(if (eq exp-l exp-r-prev)
	    (values closed (nconc open-l open-r))
	    (throw 'patmat 'fail))
	(if (eq exp-l exp-r-prev)
	    (values (close-exp-null pat-l pat-r closed)
		    (nconc open-l open-r))
	    (let ((pat (data pat-r-prev))
		  (exp (data exp-r-prev)))
;;	      (print (list "close-r pat" pat "exp" exp))
	      (etypecase pat
		(number (if (eq pat exp)
			    (close-r exp-l exp-r-prev pat-l pat-r-prev closed open-l open-r)
			    (throw 'patmat 'fail)))
		(symbol (case (var-type pat)
			  ((svar) (if (or (symbolp exp) (numberp exp))
				      (close-r exp-l exp-r-prev pat-l pat-r-prev (bind-closed pat exp closed) open-l open-r)
				      (throw 'patmat 'fail)))
			  ((tvar) (if (or (symbolp exp) (numberp exp) (dlistp exp))
				      (close-r exp-l exp-r-prev pat-l pat-r-prev (bind-closed pat exp closed) open-l open-r)
				      (throw 'patmat 'fail)))
			  ((evar) (if (eq pat-l (prev pat-r-prev))
				      ;; there is no more unbound variable after the variable named by PAT, therefore bind PAT to the compound or term determined by EXP-L and EXP-R.
				      (values
				       ;; TODO: don't copy when binding e-variables to the EXPression, just (make-instance 'dlist :left exp-l :right exp-r) or something like that. only copy when concatenating the variables to the result dlist.
				       (bind-closed pat (make-instance 'dlist :first exp-l :last exp-r) closed)
				       (nconc open-l open-r))
				      (values closed
					      (nconc open-l
						     (list exp-l exp-r pat-l pat-r)
						     open-r))
				      ;;(open-dcons exp-l exp-r pat-l pat-r closed) doesn't work b/c I have to try multiple open regions.
				      ))
			  (t (if (eq pat exp)
				 (close-r exp-l exp-r-prev pat-l pat-r-prev closed open-l open-r)
				 (throw 'patmat 'fail)))))
		(dlist (multiple-value-bind (closed open-1) (close-lr exp pat closed)
;;			 (print (list "close-lr returned into close-r with open-1" (map-open-dcons-list-to-dlist open-1) "closed" closed))
			 (close-r exp-l exp-r-prev pat-l pat-r-prev closed open-l (nconc open-1 open-r))))))))))

(defun close-lr (exp pat closed)
  (declare (type list closed)
	   ;;(optimize speed)
	   )
;;  (print (list "close-lr exp" exp "pat" pat "closed" closed))
  (if (and (dlistp exp) (dlistp pat))
      (close-l (dlist-first exp) (dlist-last exp) (dlist-first pat) (dlist-last pat) closed nil)
      (throw 'patmat 'fail)))

(defun alist-dlist->list* (closed)
  (mapcar (lambda (a)
	    (if (dlistp (cdr a))
		(cons (car a) (dlist->list (cdr a) :deep t))
		a))
	  closed))

(defun test-close-var (exp pat closed)
  ;;(print (list "exp" exp "pat" pat))
  (catch 'patmat
    (let ((closed (close-lr (list->dlist exp) (list->dlist pat) closed)))
      (alist-dlist->list* closed))))

(let ((exp-1 '(1 2 3))
      (exp-2 '(1 nil 3))
      (exp-3 '((1) (2) (3)))
      (pat-1a '(s.a s.b s.c))
      (pat-1b '(t.a t.b t.c))
      (pat-1c '(s.a e.b s.c))
      (pat-2a '(s.a 2 s.c))
      (pat-2b '(t.a 2 t.c))
      (pat-3a '(s.a 3 s.c))
      (pat-3b '(t.a 3 t.c))
      (pat-4a '((s.a) (s.b) (s.c)))
      (pat-4b '((t.a) (t.b) (t.c)))
      (pat-4c '((s.a) (e.b) (s.c)))
      (pat-5a '((s.a) s.b (s.c)))
      (pat-5b '((t.a) t.b (t.c)))
      (pat-5c '((s.a) e.b (s.c))))
  (assert (equal '((s.c . 3) (s.b . 2) (s.a . 1)) (test-close-var exp-1 pat-1a nil)))
  (assert (equal '((t.c . 3) (t.b . 2) (t.a . 1)) (test-close-var exp-1 pat-1b nil)))
  (assert (equal '((s.c . 3) (s.a . 1)) (test-close-var exp-1 pat-2a nil)))
  (assert (equal '((t.c . 3) (t.a . 1)) (test-close-var exp-1 pat-2b nil)))
  (assert (equal 'fail (test-close-var exp-1 pat-3a nil)))
  (assert (equal 'fail (test-close-var exp-1 pat-3b nil)))
  (assert (equal '((e.b . (2)) (s.c . 3) (s.a . 1)) (test-close-var exp-1 pat-1c nil)))
  (assert (equal 'fail (test-close-var exp-2 pat-1a nil)))
  (assert (equal '((t.c . 3) (t.b . nil) (t.a . 1)) (test-close-var exp-2 pat-1b nil)))
  (assert (equal '((s.c . 3) (s.b . 2) (s.a . 1)) (test-close-var exp-3 pat-4a nil)))
  (assert (equal '((t.c . 3) (t.b . 2) (t.a . 1)) (test-close-var exp-3 pat-4b nil)))
  (assert (equal 'fail (test-close-var exp-3 pat-5a nil)))
  (assert (equal '((t.c . 3) (t.b . (2)) (t.a . 1)) (test-close-var exp-3 pat-5b nil)))
  (assert (equal '((e.b . ((2))) (s.c . 3) (s.a . 1)) (test-close-var exp-3 pat-5c nil)))
  (assert (equal 'fail (test-close-var exp-1 pat-4c nil)))
  (assert (equal '((e.c . nil) (t.b . 2) (s.a . 1)) (test-close-var '(1 2) '(s.a t.b e.c) nil)))
  (assert (equal '((e.b . nil) (t.c . 2) (s.a . 1)) (test-close-var '(1 2) '(s.a e.b t.c) nil)))
  (assert (equal (test-close-var '(1 1) '(s.1 s.1 e.1) nil)
		 '((e.1) (s.1 . 1))))
  (assert (equal (test-close-var '(A (X Y Z)) '(E.4 (E.2 Z)) `((E.2 . ,(DLIST 'X 'Y))))
		 '((E.4 A) (E.2 X Y)))))

(defun open-dcons (closed open)
  "Try all possible bindings for the e-var at EXP-L."
  (declare (type list closed open)
	   ;;(optimize speed)
	   )
;;  (print (list "open-dcons closed" closed "open" (map-open-dcons-list-to-dlist open)))
  (if (null open)
      (return-from open-dcons closed))
  (destructuring-bind (exp-l exp-r pat-l pat-r &rest open) open
    (declare (type dcons exp-l exp-r pat-l pat-r))
;;    (print (list "open-dcons open exp" (make-instance 'dlist :first exp-l :last exp-r) "pat" (make-instance 'dlist :first pat-l :last pat-r) "closed" closed))
    (let* ((pat-l-next (next pat-l))
	   (pat (data pat-l-next)))
      (assert (evarp pat))
      (flet ((try (val exp-l-next)
;;	       (print (list "try pat" pat "val" val "closed" (alist-dlist->list* closed)))
	       (multiple-value-bind (closed open-1)
		   (catch 'patmat
		     (let* ((closed (bind-closed pat val closed)))
		       (close-l exp-l-next exp-r pat-l-next pat-r closed nil)))
		 (when (listp closed)
;;		   (print (list "open-dcons seemingly succeeded pat" pat "with closed" closed))
		   (let ((closed
			  (catch 'patmat
			    (open-dcons closed (nconc open-1 open)))))
		     (when (listp closed)
		       (return-from open-dcons closed))
;;		     (print (list "backtracking from seemingly successful pat" pat "val" val))
		     )))))
	(do ((try-exp-r (next exp-l) (next try-exp-r))) (())
	  (let* ((val (make-instance 'dlist :first exp-l :last try-exp-r)))
	    (try val (prev try-exp-r)))
	  (when (eq try-exp-r exp-r)
	    (throw 'patmat 'fail)))))))

(defun patmat (exp pat &key closed)
  "Return a variable binding compatible with EXP, PAT, and CLOSED.
EXP: expression, PAT: pattern, CLOSED: ((a . value) (b . 1))."
  (catch 'patmat
    (multiple-value-bind (closed open)
	(close-lr exp pat closed)
      (open-dcons closed open))))

(defun test-patmat (exp pat)
  (let ((closed (patmat (list->dlist exp) (list->dlist pat))))
    (if (listp closed)
	(mapcar (lambda (a)
		  (if (dlistp (cdr a))
		      (cons (car a) (dlist->list (cdr a) :deep t))
		      a))
		closed)
	'fail)))

;; TODO: put tests here
(assert (equal (test-patmat '((1       2 3)       4       ())
			    '((s.1 e.1 e.2) e.3 e.4 (e.5)))
	       '((e.4 . (4)) (e.3 . nil) (e.2 . (2 3)) (e.1 . nil) (e.5 . nil) (s.1 . 1))))
(assert (equal 'fail (test-patmat '(b) '(a))))
(assert (equal '((s.a . b)) (test-patmat '(b) '(s.a))))
(assert (equal (test-patmat '((x y z)   ()  (x y z) a  )
			    '((e.2 e.6) e.3 (e.2 z) e.7))
	       '((E.7 A) (E.3 NIL) (E.6 Z) (E.2 X Y))))
(assert (equal (test-patmat '((    x y    ) (x y))
			    '((e.5 e.2 e.6) (e.2)))
	       '((e.6) (e.5) (e.2 . (x y)))))
(assert (equal (test-patmat '(a   (x y z)       (a b c)   (a   (x y z)) a  )
			    '(e.1 (e.5 e.2 e.6) e.3       (e.4 (e.2 z)) e.7))
	       '((E.7 A) (E.4 A) (E.3 (A B C)) (E.6 Z) (E.2 X Y) (E.5) (E.1 A))))
;; lots of backtracking
(assert (equal (test-patmat '(a (x) (x y) (x y z) (a b c) (a b x (x y) (    x y z  )) a b c)
			    '(e.1         (e.2 z) e.3     (e.4         (e.5 e.2 e.6)) e.7  ))
	       '((e.6 . (z)) (e.5 . nil) (e.7 . (a b c)) (e.4 . (a b x (x y))) (e.3 . ((a b c))) (e.2 . (x y)) (e.1 . (a (x) (x y))))))
(assert (equal (test-patmat '(a (x) (    x y    ) (x y z) (a b c) (a b x (x y) (x y z)) a b c)
			    '(e.1   (e.5 e.2 e.6) e.3             (e.4         (e.2 z)) e.7  ))
	       '((e.7 . (a b c)) (e.4 . (a b x (x y))) (e.3 . ((x y z) (a b c))) (e.6 . nil) (e.2 . (x y)) (e.5 . nil) (e.1 . (a (x))))))
(assert (equal (test-patmat '(a (x) (x y) (x y z) (a b c) (a b x (x y) (x y z)))
			    '(e.1 (e.2 z) e.3 (e.4 (e.5 e.2 e.6))))
	       '((e.6 . (z)) (e.5 . nil) (e.3 . ((a b c))) (e.2 . (x y)) (e.1 . (a (x) (x y))) (e.4 . (a b x (x y))))))
(assert (equal (test-patmat '(c b a (a b x (x y) (x y z)) (a b c) (x y z) (x y) (x) a)
			    '(e.7 (e.4 (e.5 e.2 e.6)) e.3 (e.2 z) e.1))
	       '((e.1 . ((x y) (x) a)) (e.3 . ((a b c))) (e.6 . (z)) (e.2 . (x y)) (e.5 . nil) (e.4 . (a b x (x y))) (e.7 . (c b a)))))
(assert (equal (test-patmat '(a b b a b) '(e.1 t.1 t.1 e.2))
	       '((E.2 A B) (T.1 . B) (E.1 A))))
(assert (equal (test-patmat '(a b b a b) '(e.1 s.1 s.1 e.2))
	       '((E.2 A B) (S.1 . B) (E.1 A))))
(assert (equal 'fail (test-patmat '(1 2 3 4) '(s.1))))
(assert (equal 'fail (test-patmat '(1 2 3 4) '(t.1))))
(assert (equal 'fail (test-patmat '(1 2 3 4) '(1 2 s.1))))
(assert (equal 'fail (test-patmat '(1 2 3 4) '(s.1 3 4))))

;; In refal/html/ch_1.3.html it says "A t-variable takes any term as its value (recall that a term is either a symbol or an expression in structure brackets). An e-variable can take any expression as its value.", so I think t-variables differ from e-variables in that they may not bind NIL, but e-variables may. EDIT: I am now pretty sure that t-variables also may not bind to sublists (t-variables bind only if the sublist is in brackets), only e-variables may. Exercises 1.3(b) and (c) support that: "Write patterns that can be described in words as follows: [(a) ...] (b) an expression which contains at least two identical terms on the top level of structure; (c) a non-empty expression." and their answers (file refal/html/answers.html) are: "(b) e.1 t.X e.2 t.X e.3 ; (c) t.1 e.2 or e.1 t.2".
;; Chapter 1.3, exercise 1.4(b) says: "Find the results of the following matchings: (a) 'abbab' : e.1 t.X t.X e.2 (b) 'ab(b)ab' : e.1 t.X t.X e.2" and their answers are: "(a) t.X becomes b ; (b) failure;", so I think that t-variables may be either a symbol/number/character or an expression in structure brackets (meaning described as a regexp: "." OR "\(.*\)").


;; TODO (or rather, reminder) when implementing the rest of refal-patmat.lisp: only copy e-variables when concatenating the variables to the result dlist. (And only copy an e-variable if it occurs more than once; if it occurs only once, we can probably just splice it into the result dlist.)

(defun patternp (p)
  "Examples:
  ()
  (1 2 e.1 (s.1 t.1) (a b c))."
  (or (null p)
      (and (listp p)
	   (let ((h (car p)))
	     (and (or (symbolp h)
		      (numberp h)
		      (and (listp h)
			   (patternp h)))
		  (patternp (cdr p)))))))

(defstruct nest[]
  (list))

;; boa constrictor = by-order-of-arguments constructor.
(defun make-nest[]-boa (list)
  (make-nest[] :list list))

;; TODO: instead of using function nest-brackets, write reader-macros or what they are called (this has the advantage that you can write reader-macros for nested <> [] {} (), which is not possible with nest-brackets).
;; TODO: instead of the approach with the escape-symbol in this function, maybe consider using a reader like in lisp itself, i.e. a function that converts characters (or symbols) to structures understandable by the refal interpreter. The ability to encode a literal '[ symbol would then also be implemented by a single-escape-symbol, but be part of another function (which maybe would also handle a multiple-escape-symbol).
(defun nest-brackets (list open-bracket close-bracket nest-function &key (escape-symbol '\\) (test #'eq))
  "Return the tree that results from recursively embedding parts of LIST enclosed with OPEN-BRACKET and CLOSE-BRACKET.
The NEST-FUNCTION is called with the sublist of LIST that will be embedded (and is enclosed by the brackets).
On encountering ESCAPE-SYMBOL, it is deleted and the next symbol is inserted literally, i.e. it is read without considering it to be a bracket or an ESCAPE-SYMBOL.
Returns the resulting tree and a second value, which is T if LIST was well-formed, or a string describing its error.
TEST is used to compare elements of LIST with the symbols (OPEN-BRACKET, CLOSE-BRACKET, ESCAPE-SYMBOL)."
  (labels ((chop ()
	     (let ((r (car list)))
	       (setf list (cdr list))
	       r))
	   (rec (res open)
	     "Returns the resulting nested list and the number of not yet closed opening brackets."
	     (if (null list)
		 (values (nreverse res) open)
		 (let ((h (chop)))
		   (cond
		     ((funcall test h escape-symbol)
		      (cond
			((null list) (values (nreverse res) "expected escaped symbol, not end of input"))
			((listp (car list)) (values (nreverse res) "expected escaped symbol, not a list"))
			(t (rec (cons (chop) res) open))))
		     ((funcall test h open-bracket)
		      (multiple-value-bind (embedded-res embedded-open)
			  (rec nil (1+ open))
			(if (and (numberp embedded-open) (= embedded-open open))
			    (rec (cons embedded-res res) open)
			    (values (nreverse (cons embedded-res res)) "nesting with too many CLOSE-BRACKETs"))))
		     ((funcall test h close-bracket)
		      (let ((embedded (funcall nest-function (nreverse res))))
			(values embedded (1- open))))
		     ((listp h)
		      (multiple-value-bind (h-res h-accepted)
			  (nest-brackets h open-bracket close-bracket nest-function :escape-symbol escape-symbol :test test)
			(if (eq t h-accepted)
			    (rec (cons h-res res) open)
			    (values (nreverse (cons h-res res)) h-accepted))))
		     (t
		      (rec (cons h res) open)))))))
    (multiple-value-bind (res error-code)
	(rec nil 0)
      (values res (if (and (numberp error-code) (= 0 error-code)) t error-code)))))

(assert (equalp (multiple-value-list (nest-brackets '(1 [ [ 2 (3) ] 4 ]) '[ '] #'make-nest[]-boa))
		`((1 ,(make-nest[]-boa (list (make-nest[]-boa '(2 (3))) 4))) t)))
;;	       '((1 #S(NEST[] :LIST (#S(NEST[] :LIST (2 (3))) 4))) t))) ;error I don't understand.
(assert (not (eq t (nth-value 1 (nest-brackets '(1 [ 2 (3) ] 4 ])
					       '[ '] #'make-nest[]-boa)))))
(assert (not (eq t (nth-value 1 (nest-brackets '(1 [ [ 2 (3) ] 4)
					       '[ '] #'make-nest[]-boa)))))
(assert (not (eq t (nth-value 1 (nest-brackets '(1 [ [ 2 (3) (]) 4 ])
					       '] '] #'make-nest[]-boa)))))
(multiple-value-bind (res well-formed-p)
    (nest-brackets '(1 [ 2 ] \\ [ 3) '[ '] #'identity)
  (assert (and (equal res '(1 (2) [ 3)) (eq t well-formed-p))))

(defstruct call
  (name nil :type symbol :read-only t)
  (args nil :type t :read-only t))

(defun refal-function-name-p (value)
  (symbolp value))

(defstruct consts
  (list nil :type (or list dlist) :read-only t))

(defun parse-result (result)
  "Examples:
  ()
  (1 2 e.1 #S(NEST[] :LIST (function 2 (3))) (a b c))."
  (labels ((consts-region (r constants)
	     "Parse list R until a non-constant is encountered, then return the list up to the (excluded) non-constant, and the rest of the list (including the non-constant)."
	     ;;(print (list "consts-region" r constants))
	     (if (null r)
		 (values (nreverse constants) nil)
		 (let ((h (car r)))
		   (cond
		     ((or (and (symbolp h) (not (svarp h)) (not (tvarp h)) (not (evarp h))) (numberp h))
		      (consts-region (cdr r) (cons h constants)))
		     ((listp h)
		      (multiple-value-bind (h-constants h-rest) (consts-region h nil)
			(if (null h-rest)
			    (consts-region (cdr r) (cons h-constants constants))
			    (values (nreverse constants) r))))
		     (t (values (nreverse constants) r))))))
	   (rec (r res)
	     (if (null r)
		 (values res t)
		 (let ((h (car r)))
		   (cond
		     ((or (symbolp h) (numberp h))
		      (multiple-value-bind (constants rest)
			  (consts-region r nil)
			(if (eq rest r) ;FIXME: ugly (plus I don't like the helper consts-region anyways, because it duplicates parsing the list; it should maybe be expressed in function #'rec completely).
			    (rec (cdr rest) (dlist-push h res :at-end t))
			    (rec rest (dlist-push (make-consts :list (list->dlist constants)) res :at-end t)))))
		     ((listp h) (multiple-value-bind (presults accepted)
				    (parse-result h)
				  (if accepted
				      (rec (cdr r) (dlist-push presults res :at-end t))
				      (values res nil))))
		     ((nest[]-p h)
		      (let ((l (nest[]-list h)))
			(if (or (null l) (not (refal-function-name-p (car l))))
			    nil
			    (multiple-value-bind (presults accepted)
				(parse-result (cdr l))
			      (if accepted
				  (let* ((call (make-call :name (car l)
							  :args presults)))
				    (rec (cdr r) (dlist-push call res :at-end t)))
				  (values res nil))))))
		     (t (values res nil)))))))
    (if (listp result)
	(rec result (dlist))
	(values nil nil))))

(flet ((test-parse-result (input expected-output expected-accepted)
	 (multiple-value-bind (presult accepted) (parse-result input)
	   (if expected-accepted
	       (let ((e (dlist-equal presult (list->dlist expected-output) :test #'equalp)))
		 (if e t (progn (print (list "expected" (list->dlist expected-output) "result" presult)) nil)))
	       (eq accepted nil)))))
  (assert (test-parse-result '() '() t))
  ;;doesn't work since equalp-comparison of dlists containing structures with slots containing a dlist does not work: (assert (test-parse-result '(1 2 e.1 (3 4) 5) `(,(make-consts :list (dlist 1 2)) e.1 (,(make-consts :list (dlist 3 4))) ,(make-consts :list (dlist 5))) t))
  (assert (test-parse-result 5 nil nil))
  ;;doesn't work since equalp-comparison of dlists containing structures with slots containing a dlist does not work: (assert (test-parse-result `(1 ,(make-nest[] :list '(function 2 (3))) 4) `(,(make-consts :list (dlist 1)) ,(make-CALL :NAME 'FUNCTION :ARGS (dlist (make-consts :list (list->dlist '(2 (3)))))) ,(make-consts :list (dlist 4))) t))
  )
;; these need to be adapted to dlists: (and they won't work anyways, since (let ((d1 (dlist 1 (make-consts :list (dlist 2)) 3)) (d2 (dlist 1 (make-consts :list (dlist 2)) 3))) (dlist-equal d1 d2 :test #'equalp)) is NIL, but should be T.
;;(assert (equal (multiple-value-list (parse-result '(1 #S(NEST[] :LIST (function 2 (3))) 4)))
;;		'((1 #S(CALL :NAME FUNCTION :ARGS (2 (3))) 4) T)))
;;(assert (equalp (multiple-value-list (parse-result `(1 ,(make-nest[]-boa '(function 2 (3))) 4)))
;;		`((,(make-consts :list '(1)) ,(make-CALL :NAME 'FUNCTION :ARGS `(,(make-consts :list '(2 (3))))) ,(make-consts :list '(4))) T)))

(defun parse-clause (clause)
  "CLAUSE == (PATTERN RESULT1 .. RESULTn)"
  (when (or (not (listp clause)) (null clause))
    (return-from parse-clause nil))
  (let ((pattern (car clause))
	(results (cdr clause)))
    (when (not (patternp pattern))
      (return-from parse-clause nil))
    (multiple-value-bind (presults accepted)
	(parse-result results)
      (if accepted
	  (cons (list->dlist pattern) presults)
	  nil))))

(defun parse-function (function)
  "FUNCTION == '(NAME CLAUSE1 ... CLAUSEn)"
  (when (or (not (listp function)) (null function))
    (return-from parse-function nil))
  (let ((name (car function))
	(body (cdr function))
	(pclauses nil))
    (when (not (refal-function-name-p name))
      (return-from parse-function nil))
    (when (or (not (listp body)) (null body))
      (return-from parse-function nil))
    (dolist (clause body)
      (push (parse-clause clause) pclauses))
    (cons name (nreverse pclauses))))

;;does not work since comparison of dlists doesn't work:
;;(assert (equalp (parse-function (nest-brackets '(ITAL-ENGL ((E.W) [ TRANS (E.W) [ TABLE ] ]))
;;					       '[ '] #'make-nest[]-boa))
;;		`(ITAL-ENGL ((E.W) ,(make-CALL :NAME 'TRANS :ARGS `((E.W) ,(make-CALL :NAME 'TABLE :ARGS NIL)))))))

(defun parse-program (program)
  "Given a refal program as (nested) list PROGRAM, return the parsed representation."
  (when (or (not (listp program)) (null program))
    (return-from parse-program nil))
  (let ((pprogram nil))
    (dolist (function program)
      (let ((pfunction (parse-function function)))
	(when (null pfunction)
	  (return-from parse-program nil))
	(let ((name (car pfunction))
	      (clauses (cdr pfunction)))
	  (setf pprogram (acons name clauses pprogram)))))
    (nreverse pprogram)))

(defun parse-program* (program)
  (if (listp program)
      (multiple-value-bind (r accepted)
	  (nest-brackets program '[ '] #'make-nest[]-boa)
	(if (eq t accepted)
	    (parse-program r)
	    nil))
      nil))

#|
(defun result-list->dlist (results)
  (mapcar (lambda (result)
	    (etypecase result
	      (call 
	       (make-call :name (call-name result) :args (list->dlist (result-list->dlist (call-args result)))))
	      (consts
	       (make-consts :list (list->dlist (result-list->dlist (consts-list result)))))
	      (t
	       result)))
	  results))

(defun program-list->dlist (pprogram)
  "Example: (program-list->dlist (parse-program* program))."
  (mapcar (lambda (function)
	    (cons (car function)
		  (mapcar (lambda (clause)
			    (destructuring-bind (pattern . results) clause
			      (cons (list->dlist pattern) (list->dlist (result-list->dlist results)))))
			  (cdr function))))
S	  pprogram))
|#

(defparameter *prog-trans-ital-engl*
  '((ital-engl
     ((e.W) [ Trans (e.W) [ Table ] ]))
    (table
     (() ;nil becomes
      ((cane) dog)
      ((gatto) cat)
      ((cavallo) horse)
      ((rana) frog)
      ((porco) pig)))
    (trans
     (((e.Word) e.1 ((e.Word) e.Trans) e.2) e.Trans)
     (((e.Word) e.1) ***))))

(assert (not (null (parse-program* *prog-trans-ital-engl*))))

(defun default-no-function (f n a)
  (declare (ignore f n a))
  (throw 'refal-eval-error 'unknown-function-error))

;; Idea: A "lazy-evaluating" Refal, which can bind variables to lists with calls in them. That way calls could appear in a pattern, and a program could modify its meaning. Something like:
;;   Func-a { 0 s.1 = s.1; s.2 s.1 = <Func-a <- s.2 1> <+ s.1 1>> };
;;   Func-b { <Func-a s.1 s.2> = <+ s.1 s.2> }
;;   $ENTRY Go { =  <Func-b <Func-a 2 3>>  }
;; That way Func-b could accelerate calls to Func-a, like compiler-macros in lisp.

(defun numeric-list-p (a)
  (and (listp a)
       (loop for i in a always (numberp i))))

;; upon call:
;; view: (1 2 [ f1 3 4 ] 5)          can save insertion point for result of f1
;; view: (1 2 [ f1 3 [ f2 4 ] ] )    must know that f2 must be called before f1 (this is not a property of a RESULT of f1, because we don't know yet what RESULT of f1 will pattern-match.)
;; view: (1 2 [ f1 3 [ f2 4 [ f3 ] ] ] ) must execute f3 before we can know what RESULT of f2 matches the sub-view '(4 {f3-VIEW}). This dependency cannot be stored in a RESULT of f1, because we don't know which RESULT (of f1) it should be stored in!
;; view: (1 [ f1 3 [ f2 4 [ f3 e.1 ] ] [ f1 ] ]) first execute f3 (which could call more functions), then f2 (which could call more functions), then f1 (which could call more functions). So upon seeing this view-field, push onto the control-stack: 1st f1, f2, f3, 2nd f1. Then pop off the stack the top element (2nd f1), which could push more functions onto the stack. Pop off the top of the stack, etc.

;; Recursion in EVAL-VIEW upon seeing a view-field works like this: look at the first element of the current view-field: if it is a constants list, insert it into the view-field, and recurse by calling EVAL-VIEW with the next position and the updated view-field. If it is a variable, insert the corresponding value from the current binding, and recurse EVAL-VIEW with the next position and the updated view-field. If a function call is reached, push the current view, the continue position (which is equal to the insertion-position), the current bindings and the function name onto the control stack, and call EVAL-VIEW with the new view-field, the extended control stack, and the old bindings. If upon iterating a sub-list is reached, we know that it has a non-constant in it, b/c otherwise it would be a constants-list; therefore push the insertion/continue position (and the current bindings) onto the control stack and recurse by calling EVAL-VIEW with the list as the new view-field, and the insertion/continue position (and the current bindings) on top of the control stack. If upon iterating the end of the current view is reached, pop off the insertion/continue position (which is in the view 1 level more outer than the current view) from the control stack, and branch depending on the type of this top of the control stack. If the top of the control stack was pushed there because of a sub-list, insert the current view into this position, and recurse into EVAL-VIEW with the view more outer 1 level, starting at the position after the insertion/continue position. If the top of the control-stack was pushed there because of a function, find the correct RESULTS by pattern-matching the current view-field with all PATTERNs of the function (thereby determining the new view-field, i.e. the view-field of the pattern-matched RESULT), extend the control-stack to as if the matched RESULT was a sub-list, and call EVAL-VIEW with the new view-field, the extended control stack, and new bindings derived from pattern-matching the correct PATTERN of the function. If upon iterating the end of the current view is reached, and the control-stack is empty, we are done and can return the current view-field as the result.
;; Each control-stack entry consists of the following: view-field, insertion/continue position of this view, variable bindings for this view field, function to be called when the current view-field is completely evaluated (or NIL if it is a sub-list).
;; (defun EVAL-VIEW (current-view current-view-next-position control-stack &key f b c no-op) ...)

(defun eval-call-builtin (n a)
  (case n
    ((+) (if (numeric-list-p a)
	     (make-consts :list (dlist (apply #'+ a)))
	     (throw 'refal-eval-error 'numeric-error)))
    ((-) (if (and (numeric-list-p a) (not (null a)))
	     (make-consts :list (dlist (apply #'- a)))
	     (throw 'refal-eval-error 'numeric-error)))
    ((*) (if (numeric-list-p a)
	     (make-consts :list (dlist (apply #'* a)))
	     (throw 'refal-eval-error 'numeric-error)))
    ((/) (if (and (numeric-list-p a)
		  (not (null a))
		  (loop for i in (cdr a) always (not (= i 0))))
	     (make-consts :list (dlist (apply #'/ a)))
	     (throw 'refal-eval-error 'numeric-error)))
    ((<) (if (and (numeric-list-p a) (not (null a)))
	     (make-consts :list (dlist (apply #'< a)))
	     (throw 'refal-eval-error 'numeric-error)))
    ((>) (if (and (numeric-list-p a) (not (null a)))
	     (make-consts :list (dlist (apply #'> a)))
	     (throw 'refal-eval-error 'numeric-error)))
    ((sample) (if (and (listp a) (not (null a)))
		  (make-consts :list (dlist (let ((l (length a))) (elt a (random l)))))
		  (throw 'refal-eval-error 'sample-error)))
    (t 'unknown-function-error)))

(defun eval-call-userdef (f n a)
  "Evaluate the call of user-defined function N with arguments A by looking up the function in F(unctions)."
  ;;(print (list "eval-call-userdef" f n a))
  (let* ((clauses (assoc n f)))
    (if (null clauses)
	'unknown-function-error
	(let ((clauses (cdr clauses)))
	  ;; find the first matching clause
	  (loop for (pattern . result) in clauses do
	     ;;(prind "trying" pattern)
	       (let ((b (patmat a pattern)))
		 (when (not (eq b 'fail))
		   (return-from eval-call-userdef (values result b)))))
	  ;;(print (list "could not recognize function: f" f "n" n "a" a))
	  (throw 'refal-eval-error 'recognition-error)))))

(defun eval-view (current-view left-position control-stack functions bindings c no-op)
  (let ((head-dcons (next left-position)))
    ;;(print (list "eval-view current-view" current-view "head-dcons" head-dcons "head" (data head-dcons) "control-stack" control-stack))
    ;;(print (list "eval-view current-view" current-view "head" (data head-dcons) "control-stack" control-stack))
    ;;(print (list "eval-view current-view" current-view "head" (data head-dcons) "bindings" bindings))
    ;;(print (list "eval-view current-view" current-view "head" (data head-dcons)))
    ;;(print (list "eval-view current-view" current-view "head-dcons" head-dcons))
    (if (eq head-dcons (dlist-last current-view))
	(if (null control-stack)
	    current-view
	    (destructuring-bind (function view-1 left-position-1 bindings-1)
		(car control-stack)
	      ;;(print (list "view-1" view-1 "left-position-1" left-position-1))
	      (if (null function)
		  ;; the sub-list in view-1 has already been replaced with its evaluation because we modify the current-view in-place.
		  (eval-view view-1 left-position-1 (cdr control-stack) functions bindings-1 c no-op)
		  ;; now current-view holds the evaluated arguments to function FUNCTION
		  (multiple-value-bind (result bindings-f)
		      (eval-call-userdef functions function current-view)
		    (if (eq result 'unknown-function-error)
			(progn
			  ;; TODO: maybe update eval-call-builtin to accept dlists if it's not too complicated, but actually I think it is too complicated.
			  (setf result (eval-call-builtin function (dlist->list current-view :deep t)))
			  (when (eq result 'unknown-function-error)
			    (throw 'refal-eval-error 'unknown-function-error))
			  (setf result (dlist result)))
			;; result needs to be deep copied because it is the unevaluated template and could contain sub-lists or function calls.
			(setf result (copy-dlist result :deep-copy t)))
		    ;;(print (list "result" result))
		    ;; splice out the call, splice in the result
		    (let* ((result-old-first (next (dlist-first result)))
			   (result-old-last (prev (dlist-last result)))
			   (result-empty? (eq result-old-first (dlist-last result))))
		      ;; splice the (unevaluated) result into view-1 so that it is in the right position when it has been evaluated.
		      (dlist-nconc (make-instance 'dlist :first (dlist-first view-1) :last left-position-1)
				   result
				   (make-instance 'dlist :first left-position-1 :last (dlist-last view-1)))
		      ;; the following re-definition of result is necessary, b/c the original (dlist-first result) and (dlist-last result) are not in the new dlist of view-1 (they were thrown away by dlist-nconc).
		      (let ((result (if result-empty? (dlist) (make-instance 'dlist :first (prev result-old-first) :last (next result-old-last)))))
			;;(print (list "result" result))
			(eval-view result (dlist-first result) (cons (list nil view-1 left-position-1 bindings-1) (cdr control-stack)) functions bindings-f c no-op)))))))
	(let ((head (data head-dcons)))
	  (etypecase head
	    (consts
	     ;; consts-list doesn't need to be deep-copied because we know that its sub-lists don't have variables or function calls in them. consts-lists consist entirely of constants.
	     (let ((consts-list (copy-dlist (consts-list head))))
	       (dlist-nconc (make-instance 'dlist :first (dlist-first current-view) :last head-dcons)
			    consts-list
			    (make-instance 'dlist :first head-dcons :last (dlist-last current-view)))
	       ;;(print (list "consts-list" consts-list))
	       (eval-view current-view (prev (dlist-last consts-list)) control-stack functions bindings c no-op)))
	    (s-variable
	     (let ((binding (assoc head bindings)))
	       (when (null binding)
		 (throw 'refal-eval-error 'unknown-variable))
	       (setf (data head-dcons) (cdr binding))
	       (eval-view current-view head-dcons control-stack functions bindings c no-op)))
	    (t-variable
	     (let ((binding (assoc head bindings)))
	       (when (null binding)
		 (throw 'refal-eval-error 'unknown-variable))
	       ;; the binding doesn't need to be deep-copied because multiple insertions of the same variable contain only data, no code that needs to be evaluated further.
	       (setf (data head-dcons) (let ((binding (cdr binding))) (if (dlistp binding) (copy-dlist binding) binding)))
	       (eval-view current-view head-dcons control-stack functions bindings c no-op)))
	    (e-variable
	     (let ((binding (assoc head bindings)))
	       (when (null binding)
		 (throw 'refal-eval-error 'unknown-variable))
	       ;; the binding doesn't need to be deep-copied because multiple insertions of the same variable contain only data, no code that needs to be evaluated further.
	       (let ((binding (copy-dlist (cdr binding))))
		 (dlist-nconc (make-instance 'dlist :first (dlist-first current-view) :last head-dcons)
			      binding
			      (make-instance 'dlist :first head-dcons :last (dlist-last current-view)))
		 (eval-view current-view (prev (dlist-last binding)) control-stack functions bindings c no-op))))
	    (dlist
	     (eval-view head (dlist-first head) (cons (list nil current-view head-dcons bindings) control-stack) functions bindings c no-op))
	    (call
	     ;;(print (list "call name" (call-name head) "args" (call-args head)))
	     ;; arguments needs to be a deep copy because (call-args head) is the unevaluated template and can contain unevaluated sub-lists and function calls.
	     (let ((arguments (copy-dlist (call-args head) :deep-copy t)))
	       (eval-view arguments (dlist-first arguments) (cons (list (call-name head) current-view head-dcons bindings) control-stack) functions bindings c no-op))))))))

(defun refal-eval (program view &key (c (make-counter)) (no-op #'default-no-function) view-function)
  "Input: a not yet parsed refal PROGRAM and a VIEW field.
Output: the result, when applying the VIEW field to the function named VIEW-FUNCTION (default: first function) in PROGRAM."
  (let ((pprogram (parse-program* program)))
    (if (or (null pprogram) (not (listp view)))
	'parsing-error
	(catch 'refal-eval-error
	  (when (null view-function)
	    (setf view-function (caar program)))
	  (let ((view (dlist (make-call :name view-function :args (parse-result view)))))
	    (dlist->list (eval-view view (dlist-first view) nil pprogram nil c no-op) :deep t))))))

(let ((view (list->dlist '(s.1 t.1 e.1))))
  (assert (dlist= (eval-view view (dlist-first view) nil nil `((s.1 . 1) (t.1 . 2) (e.1 . ,(dlist 'a 'b 'c))) 0 nil)
		  (list->dlist '(1 2 a b c)))))
(let ((view (list->dlist '(s.1 (t.1) e.1))))
  (assert (dlist= (eval-view view (dlist-first view) nil nil `((s.1 . 1) (t.1 . 2) (e.1 . ,(dlist 'a 'b 'c))) 0 nil)
		  (list->dlist '(1 (2) a b c)))))
(assert (equal (refal-eval '((f1 ((s.1) )))
			     '(2.5))
	       '()))
(assert (equal (refal-eval '((f1 ((t.1) 1 2 t.1 3)))
			     '(2.5))
	       '(1 2 2.5 3)))
(let ((v '(1 2 () 3)))
  (assert (equal (refal-eval '((f ((e.1) e.1))) v) v)))
(assert (equal (refal-eval '((f1 ((* t.1 /) 1 [ f2 2 t.1 ] 3))
			       (f2 ((1 e.1) - e.1 +) ((2 e.1) + e.1 -)))
			     '(* 2.5 /))
	       '(1 + 2.5 - 3)))
(assert (equal (refal-eval '((f1 ((t.1) 1 [ f2 2 t.1 ] 3))
			       (f2 ((1 e.1) - e.1 +) ((2 e.1) + e.1 -)))
			     '(2.5))
	       '(1 + 2.5 - 3)))
(assert (equal (refal-eval *prog-trans-ital-engl* '(porco)) '(pig)))
(assert (equal (refal-eval *prog-trans-ital-engl* '(abc)) '(***)))

(defparameter *prog-fak*
  '((fak
     ((1) 1)
     ((s.1) [ * s.1 [ fak [ - s.1 1 ] ] ]))))

(assert (not (null (parse-program* *prog-fak*))))
(assert (equal (refal-eval *prog-trans-ital-engl* '(cane)) '(dog)))
(assert (equal (refal-eval *prog-fak* '(3)) '(6)))

(defparameter *prog-fak-unnested*
  '( { fak
    { { 1 } 1 }
    { { s.1 } [ * s.1 [ fak [ - s.1 1 ] ] ] } } ))
;; Symbols necessary for *prog-fak-unnested*:
;; (length (unique *prog-fak-unnested*)) == 9
;; (length *prog-fak-unnested*) == 26
;; P = 9 ** 26 = 6461081889226673298932241L
(defparameter *prog-fak-unnested-X*
  '( X fak ;X means this symbol is the only symbol possible, which means a factor of only 1 in above P-calculation for that position.
    X X 1 } 1 } ; the } are necessary b/c we need to know when the (parameter-,result-) lists are finished.
    X X s.1 } [ * s.1 [ fak [ - s.1 1 ] ] ] } } ))
;; (length (remove 'X *prog-fak-unnested-X*)) == 21
;; P2 = 9 ** 21 == 109418989131512359209L
;; i.e. at a speed of 51552.152 calls per second (measured using: (timecps (1000 :stats t :time 5.0) (refal-eval *prog-fak* '(3))) on purasuchikku), we need (round (/ 109418989131512359209 51552 60 60 24 365)) = 67303953 = 67 M years.

;; add the function "< quote ... >", which evaluates to "...".
;; add the function "< selfquote ... >", which evaluates to "< selfquote ... >". Also add function "< unquote ... >", which, if inside selfquote or quote, evaluates "..." and inserts it at the position that unquote was at. Make selfquote and quote be allowed to be nested, and evaluation of unquote only takes place when there are equally many nested unquotes as there were nested quotes/selfquotes before.
;; Is it possible to write a self-replicating or self-modifying program using these functions?
;; (fak ((s.1) < selfquote < unquote < + s.1 1 > >

(defun refal-eval-replace (program view &key (prefix-string "R") (c (make-counter)) (no-op #'default-no-function) (view-function (caar program)))
  "Like refal-eval, but insert symbol PREFIX-STRING into the E-, T-, and S-variable before evaluation of VIEW (using PROGRAM) and remove PREFIX-STRING after evaluation. This allows a program to output E-, T-, and S-symbols (which, using normal evaluation by refal-eval, would be substituted by their value (which is most likely NIL in the top level view)."
  (let ((back (make-hash-table :test #'eq)))
    (labels ((subst-symbol (tree predicate genf result)
	       (if (null tree)
		   (nreverse result)
		   (if (listp tree)
		       (let ((head (car tree)))
			 (if (listp head)
			     (subst-symbol (cdr tree) predicate genf (cons (subst-symbol head predicate genf nil) result))
			     (if (funcall predicate head)
				 (let ((replacement (funcall genf head)))
				   (setf (gethash replacement back) head)
				   (subst-symbol (cdr tree) predicate genf (cons replacement result)))
				 (subst-symbol (cdr tree) predicate genf (cons head result)))))))))
      (let* ((prefixer (lambda (head)
			 (let* ((heads (string head)))
			   (intern (concatenate 'string (subseq heads 0 1) prefix-string (subseq heads 1))))))
	     (view1 (subst-symbol view #'svarp prefixer nil))
	     (view2 (subst-symbol view1 #'tvarp prefixer nil))
	     (view3 (subst-symbol view2 #'evarp prefixer nil)))
	;;(print (list "view3" view3))
	(let ((result (refal-eval program view3 :c c :no-op no-op :view-function view-function))
	      (gethasher (lambda (head) (gethash head back head))))
	  ;;(print (list "result" result))
	  (if (listp result)
	      (subst-symbol result (constantly t) gethasher nil)
	      result))))))

(let ((program-walker `((next
			 ((e.1) [ helper (e.1) ]))
			(helper
			 (((s.1 e.1) e.2) [ helper (e.1) e.2 s.1 ])
			 ((((e.3) e.1) e.2) [ helper (e.1) e.2 ([ helper (e.3) ]) ])
			 ((() e.2) e.2)))))
  (let ((result (refal-eval-replace '((f ((e.1) e.1))) program-walker)))
    (assert (equal result program-walker)))
  (let* ((view '(1 2 (3) 4))
	 (result (refal-eval program-walker view)))
    (assert (equal result view)))
  (let* ((view program-walker)
	 (result (refal-eval-replace program-walker view)))
    (assert (equal result view))))

(defun specific-random-state ()
  (let ((random-state #+sbcl '#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                     :INITIAL-CONTENTS
                                     '(0 2567483615 459 342356071 3531489510
                                       2331605190 910510831 3852095994
                                       4202880162 1527001109 1423137360
                                       1497663022 11727107 2966933401
                                       2813248289 4231446232 1298940309
                                       2003327448 3410065843 338997485
                                       2581164106 1825993724 2896753516
                                       3280497728 3610815070 624026016
                                       1451757706 1348391397 3106175362
                                       3584823840 3051798390 2947295603
                                       1664100055 2803849467 3920647366
                                       1693707886 490731719 302461619
                                       3737999297 1394839664 4228713627
                                       1697243927 1958593516 4282874565
                                       3969917809 3518298967 3417696408
                                       3041927890 462301860 569657629
                                       1158413058 4037726451 2655892867
                                       2228663279 3536157597 614999141
                                       247219327 639389729 379663007 1237710599
                                       1413421193 266283264 2973365297
                                       3441228628 3525041971 3188687859
                                       421587397 3271095977 613947257
                                       2049087192 3671195113 893763367
                                       1301529008 535423016 59024731 3388416732
                                       3097531775 1656668087 322602302
                                       2185327525 2798166193 590435172
                                       2623992906 3555566275 3891919751
                                       764963539 356565127 433005935 2845910488
                                       347746604 2313044898 3731380088
                                       3376022583 3614653152 1884299836
                                       3623125032 118489741 4035206462
                                       3932959384 3023718315 3364191260
                                       2245140121 1333071173 2054927196
                                       2207128959 446854282 2168099879
                                       2900458807 1724201022 3793771891
                                       2617694133 301813571 313726463
                                       1290141575 4181567244 2039846765
                                       2061670507 2170371104 4231698981
                                       1967418915 1517728564 713436507
                                       553233325 3175121003 1755935476
                                       3856957552 2720647366 3396466985
                                       674662271 1807143535 2143932277
                                       3119863456 594645515 517231231
                                       3333810332 1583738791 529013062
                                       425407806 361673727 3861545406
                                       3227055049 793042916 2053590196
                                       2954318366 4117060584 804799153
                                       1243136012 3369015107 2108839289
                                       2371037911 929795473 60793658 3478557054
                                       2740070287 2305359292 2338163338
                                       1195787871 2801488327 384611551
                                       1887103223 1702440399 348020356
                                       2854693453 564463937 2145972519
                                       459107265 366630108 3506293016
                                       2018002996 403046825 795528816
                                       2370859746 2419827044 1147694397
                                       2016579573 3224051923 1883031447
                                       745200135 2298517560 385272032
                                       2172153055 69489288 2629291163
                                       1129222570 965659200 1155796049
                                       3928619149 1659190775 1537083257
                                       3471671777 2174138364 3051061010
                                       2040561856 313465648 599774017
                                       1753703409 933511560 466554574 267173538
                                       2185195631 4094730659 2833774394
                                       3279791375 910811291 1474602565
                                       4233456620 1140655447 3660760279
                                       1187407560 4288222814 455840018
                                       719310903 973071888 4145529182
                                       1242025915 2772660878 416732638
                                       3036790309 4214532125 2080798503
                                       1198620460 1304193484 3133550204
                                       129298334 2251674348 3943715771
                                       257939585 3374280140 1254037535
                                       3658834551 3130342464 69922196
                                       1934298010 3304439724 3091871582
                                       3686720449 3262141780 3897999143
                                       1964694553 2101821337 1486173869
                                       1068753904 4094895082 2262227164
                                       1615595154 2676899399 2121789744
                                       2304962548 2102964776 2944534556
                                       1995741696 61974444 4085785442
                                       2192937711 2599295070 3288774604
                                       1777259253 4031098415 921014748
                                       171508675 1237852088 1239168217
                                       820917217 3264664027 222950123
                                       2875259163 3667518194 2441152677
                                       344178408 1559478973 4293254217
                                       743782196 37681975 1291102430 2002818375
                                       747389544 3275566238 3956971939
                                       1981820012 689082530 2304993907
                                       4220370413 287079016 552850515
                                       1776970372 290473974 382589397
                                       1415569603 3966303834 1504900275
                                       2792480605 2510281068 3419609447
                                       3073140306 2107290405 4229297061
                                       316094428 2310476804 1920400586
                                       757356561 4225625100 2882513183
                                       2599621865 3907351779 205069488
                                       2409035038 2925016874 266395043
                                       1773908431 3904118629 1147734296
                                       300375969 2286717561 3564248522
                                       3495498719 1023803051 945456547
                                       1264364535 3231608563 1378655916
                                       3846558795 1556598467 926540887
                                       3410108984 929408372 706372534
                                       3403142580 1115994939 986595585
                                       2007565629 543512255 2418473068
                                       3825463277 1531869277 1642414192
                                       2046312211 1296515696 1562742994
                                       1848834470 2759026753 2013813965
                                       329384848 3158010380 619680419
                                       2462142382 3540074558 4062930309
                                       3405007752 3693995437 3070022794
                                       3735994777 2297555538 3988710477
                                       3511088731 1301887930 4095215042
                                       4023294730 3614360577 1182979211
                                       260454173 1325514449 2341585004
                                       1801572076 60195649 3306042421
                                       3701314928 1650537004 696393296
                                       1227754209 3614308529 155852724
                                       3303884471 4269552420 4220679938
                                       441734492 3577448193 2149516342
                                       3816976454 3985069136 1570316162
                                       1610904123 1956549018 3883864033
                                       2714214744 4117701521 1418508986
                                       2927279455 2682121715 1627258994
                                       2402725982 3068749684 2112476842
                                       2664548074 3923891900 1977236077
                                       630152456 3018667797 1598781580
                                       159075906 1898305707 271055881
                                       3229247773 2454673633 1407583344
                                       1737592538 1902696259 1845898862
                                       482330873 2264877980 637351596
                                       1901132154 1319007120 2333267171
                                       998505585 4282560935 504523050
                                       1641739403 4108123736 3556981237
                                       2710898870 4051560986 2490719873
                                       414464851 261440870 3208044958
                                       1128224283 1958804751 2094740042
                                       1109676315 1467347069 2902731556
                                       1348457958 443060129 3756421876
                                       3112831914 2464736583 678306321
                                       340863420 1839855771 2673489230
                                       3765511803 2161005337 1819096756
                                       1705175721 321692852 3257776644
                                       634810272 1395115670 1379581059
                                       3161857328 205644762 2885167874
                                       3839758006 3015134811 3459946759
                                       2936545016 1783273096 3482452452
                                       546156936 1585022986 3871559550 50866935
                                       2730304177 1560369716 1441276093
                                       1886176732 4245509335 1390484423
                                       1498994828 2181637891 176332687
                                       1274570902 3888845820 3198841227
                                       3275199641 3184359130 3372879515
                                       2603643576 1641135947 2262810253
                                       827744497 1920085539 192673116
                                       1748874440 2861548757 2707952706
                                       1884519319 694598487 414938621 477524198
                                       2533639185 4016935996 3764406212
                                       3141263681 345262425 3019663839
                                       2819188486 1290322549 328227962
                                       4063561793 2029599851 2005959022
                                       2008965792 1714094596 17719375
                                       2798977865 3670851562 2490910722
                                       3791128697 1334022894 107859597
                                       1675825105 3457762305 790710923
                                       747003369 3631307388 381899322
                                       1017711874 368026066 2304894888
                                       3966238897 3911224296 3877365412
                                       21809795 3635055734 1777446963
                                       1236379298 2404581663 137975924
                                       665504771 1569629486 3770187407
                                       2514770236 1624498690 1383422909
                                       3191706511 2725669271 645935718
                                       3258319282 716798710 2161085541
                                       2240588074 478683702 2411706703
                                       124483497 3841702244 3152473350
                                       3639772494 372343803 4023035020
                                       1059942317 1833908388 828949747
                                       1721658905 1979373920 3157575587
                                       203666631 471175682 564680895 2808502943
                                       2751530411 4251805242 1613180502
                                       3310240096 3343523010 3717756947
                                       2680159318 1709542511 3053045519
                                       4181219402 1478032163 3212255056
                                       907550976 1623460999 510898614
                                       2315417921 1948965158 1388117905
                                       3506145128 574100255 1132395905
                                       4107155016 1274514825 4035043485
                                       3436711560 573429070 2240963303
                                       4241093724 2441281561 3646049126
                                       244262740 639914648 667065410 2975092258
                                       1941802615 3713231663 3321212043
                                       1863008084 3710651319 485908780
                                       572464902 1317007614 1100184214
                                       3232057334 1969145531 4002617049
                                       2246793623 803033061 1524204879
                                       3380853201 2955332832 3374886794
                                       3655724309 3696319408 4291537153
                                       541650108 3019281367 2835527281
                                       1339202218 4096643635 806422991
                                       2552384368 1710065464 2756887688
                                       1140454553 3295426802 3793653831)))
		      #+clisp #S(RANDOM-STATE
   #*0101001010001110101111101000100101000100001011100110110111000110)))
    (make-random-state random-state)))

;;;; Implementation of a better sxhash for lists
;; this is copied from SBCL
;; originally this was "(ftype (sfunction". what's an sfunction? 
(declaim (ftype (function ((and fixnum unsigned-byte)
			   (and fixnum unsigned-byte))
			  (and fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun mix (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (and fixnum unsigned-byte) x y))
  (let* ((xy (+ (* x 3) y)))
    (logand most-positive-fixnum
            (logxor 441516657
                    xy
                    (ash xy -5)))))

;;(declaim (inline lsxhash))
(defun lsxhash (x)
  "Return a hash value for value X.
X, (car X), and (cdr X) may be a list, a symbol, or a number."
  ;; FIXME: handle circular lists
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  ;;(declare (values (and fixnum unsigned-byte))) ;inferred automatically (see describe 'lsxhash)
  (declare (ftype (function (t) (and fixnum unsigned-byte))))
  ;; in SBCL, etypecase takes 80% of the time of defmethod-ing on the different types of X: (let ((h (make-hash-table))) (timediff (lsxhash h) (mlsxhash h) :showtimes t))
  (etypecase x
    (single-float (sxhash x))
    (double-float (sxhash x))
    (ratio (sxhash x))
    (fixnum (sxhash x)) ;;in SBCL, close numbers seem to have close hashes.
    (string (sxhash x)) ;;in SBCL, (sxhash "ABC") == (sxhash 'abc).
    ;;(number (sxhash x))
    (symbol (sxhash x))
    ;; here, X can't be nil since (symbolp nil) == T.
    (list (mix (lsxhash (car x)) (lsxhash (cdr x))))
    (hash-table (let ((ret 448291823))
		  (declare (type (and fixnum unsigned-byte) ret))
		  (setf ret (mix (sxhash (hash-table-count x))
				 (mix ret (sxhash (hash-table-test x)))))
		  ;; use logxor for speed and so that the order of key/value pairs does not matter
		  (maphash (lambda (k v) (setf ret (logxor ret (mix (lsxhash k) (lsxhash v)))))
			   x)
		  ret))
    (simple-array (let* ((size (array-total-size x))
			 (dim (array-dimensions x))
			 (type (array-element-type x))
			 (ret 518591303))
		    (declare (type (and fixnum unsigned-byte) ret))
		    (setf ret (mix (mix ret (sxhash type))
				   (lsxhash dim)))
		    (ecase type
		      ((fixnum)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (declare (type fixnum e))
			      (setf ret (mix ret (sxhash e))))))
		      ((t)
		       (loop for i below size do
			    (let ((e (row-major-aref x i)))
			      (setf ret (mix ret (lsxhash e))))))
		      )
		    ret))))

(defun test (&optional (evaluator #'refal-eval-replace))
  #+sbcl (gc :full t)
  #+clisp (gc)
  (let* ((*random-state* (specific-random-state))
	 (joy-ops '(+ and branch branch1 concat cons dip / dup equal gensym i ifte list * nill not or patmat patsub pop pred quote rem si sample < stack step - succ swap times true uncons unstack while define))
	 (generator `((genenv
		       ((e.1) [ sample 1 0 -1 ] [ sample walk-x walk-y ] e.1))
		      (gengen
		       ((e.1) [ helper (e.1) ]
			;; allow inserting symbols at the end
			[ extend [ insert ] [ sample ,@(loop for i below 400 collect 'del) ins nop mod ] ]))
		      (helper
		       (((s.1 e.1) e.2) [ helper (e.1) e.2 [ extend s.1
			[ sample
			del ins mod ,@(loop for i below 400 collect 'nop) ] ] ]
			;;		     nop ] ] ]
			)
		       ((((e.3) e.1) e.2) [ helper (e.1) e.2 ([ helper (e.3) ]) ])
		       ((() e.2) e.2))
		      (insert
		       (() [ sample 0 1 () \\ [ \\ ] ,@joy-ops ]))
		      (extend
		       ((s.1 del) )
		       ((s.1 ins) [ insert ] [ extend s.1 [ sample ins nop nop nop ] ])
		       ((s.1 mod) [ insert ])
		       ((s.1 nop) s.1)))))
    ;;(utils:timesec (lambda () (funcall evaluator generator generator :c (make-counter 1000000) :view-function 'gengen)))
    (time
     (loop for i below 50 collect
	  (let ((ret (progn (funcall evaluator generator generator :c (make-counter 1000000) :view-function 'gengen))))
	    (lsxhash ret))))
    ))

(test)
