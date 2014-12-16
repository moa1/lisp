(load "~/quicklisp/setup.lisp")
(ql:quickload :dlist)
(use-package :dlist)

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

(defun print-dcons (dcons stream depth)
  "Print a doubly linked list.
Doesn't yet print whether dll has any circularities in it (but detects them already)."
  (declare (ignore depth))
  ;; FIXME: rewrite this function to print a reader-readable format (or define a reader function or whatever is supposed to work).
  (print-unreadable-object (dcons stream :type nil :identity nil)
    (format stream "DCONS")
    (let ((visited nil))
      ;; FIXME: only detect loops if *PRINT-CIRCLE* is true.
      ;; FIXME: print where the loop occurs (using "#1=OBJ ...more-objs... #1#" syntax).
      (format stream " O:~A" (data dcons))
      (push dcons visited)
      (do ((cur (next dcons) (next cur))) ((or (null cur) (find cur visited)))
        (push cur visited)
        (format stream " ~A" (data cur)))
      (do ((cur (prev dcons) (prev cur))) ((or (null cur) (find cur visited)))
        (push cur visited)
        (format stream " B:~A" (data cur))))))

(defmethod print-object ((dcons dcons) stream)
  (print-dcons dcons stream 0))

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

(declaim (inline var-type))
(defun var-type (var)
  (declare (type symbol var)
	   (optimize speed))
  (cond
    ((svarp var) 'svar)
    ((tvarp var) 'tvar)
    ((evarp var) 'evar)
    (t nil)))

(declaim (inline bind-closed))
(defun bind-closed (var val closed &key (test #'equal)) ;; (c (make-counter)))
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
	(if (funcall test val (cdr acons))
	    closed
	    'fail))))

(defmacro and-fail-last (fail-form &rest forms)
  "If one of the FORMS but the last fails (i.e. evaluates to NIL), return the result of evaluating FAIL-FORM, otherwise the last form(which can be NIL)."
  ;; Couldn't this macro be defined as:
  ;; `(or (and ,@forms) ,fail-form))
  ;; No, because (and-fail-last 'fail (1+ 1) nil) should be NIL, not 'FAIL.
  (labels ((rec (forms last)
	     (if (null forms)
		 last
		 `(if ,(car forms)
		     ,(rec (cdr forms) last)
		     ,fail-form))))
    (assert (not (null forms)))
    (rec (butlast forms) (car (last forms)))))

(defun dlist->list* (dlist)
  "Deeply convert the dlist DLIST to a list."
  ;; Note: this doesn't convert circular lists:
  ;; (let ((d (dlist 1 2 3)))
  ;;   (setf (next (dlist-last d)) (dlist-first d))
  ;;   d)
  ;; However, the structures possible with a dlist cannot all be represented with lists, circular or not. So maybe we shouldn't handle circularities anyways.
  (declare (type dlist dlist))
  (do ((cur (dlist-last dlist) (prev cur)) (l nil)) ((eq cur nil) l)
    (push (let ((d (data cur)))
	    (if (dlistp d)
		(dlist->list* d)
		d))
	  l)))

(defun list->dlist (l)
  "Deeply convert list L to a dlist."
  ;; Note: this hangs on converting circular lists:
  ;; (let ((*PRINT-CIRCLE* t))
  ;;   (let ((l (list 1 2 3)))
  ;;     (setf (cdr (last l)) l)
  ;;     (print l)
  ;;     (list->dlist l)))
  (declare (type list l))
  (if (null l)
      nil
      (let ((content (loop for e in l collect
			  (if (listp e)
			      (list->dlist e)
			      e))))
	(apply #'dlist content))))

(defun dcons->list (dcons-start dcons-stop &key (iterate #'prev))
  "Iteratively push the elements between DCONS-STOP and DCONS-START (both inclusively) onto a newly constructed list. Start the pushing with the element at DCONS-STOP and substitute DCONS-STOP with (funcall ITERATE DCONS-STOP) after each iteration.
If an element is a DLIST, it is converted into a list before pushing it.
This function returns a list with at least one element."
  (declare (type dcons dcons-start dcons-stop))
  (do ((cur dcons-stop (funcall iterate cur)) (l nil))
      ((eq cur dcons-start) (push (let ((d (data cur))) (if (dlistp d) (dlist->list* d) d)) l))
    (declare (type dcons cur)
	     (type list l))
    (push (let ((d (data cur))) (if (dlistp d) (dlist->list* d) d)) l)))

(declaim (inline new-open-region))
(defun new-open-region (exp-l exp-r pat-l pat-r)
  "Make a new open region determined by EXP-L, EXP-R, PAT-L, PAT-R."
  (declare (type dcons exp-l exp-r pat-l pat-r))
  (let ((region (list exp-l exp-r pat-l pat-r)))
    region))

(declaim (inline extract-open-region))
(defun extract-open-region (region)
  (destructuring-bind (exp-l exp-r pat-l pat-r) region
    (declare (type dcons exp-l exp-r pat-l pat-r))
    (values exp-l exp-r pat-l pat-r)))

(defun close-var-exp-null (pat-l pat-r closed)
  "Bind the pattern described by PAT-L and PAT-R to a NIL-expression (therefore, the only possible binding pattern is a list of e-variables, which are all bound to NIL).
If any closed variables in the pattern have conflicting bindings in CLOSED, return 'FAIL.
This function returns NIL as the open variables, because there can't be open variables in an NIL-expression."
  (declare (type dcons pat-l pat-r)
	   (type list closed)
	   (values t list)
	   (optimize speed))
  ;;(prind pat-l) (prind pat-r)
  (let ((pat (data pat-l)))
    ;;(prind pat)
    (and-fail-last (values 'fail nil)
		   (symbolp pat)
		   (evarp pat)
		   (let ((closed (bind-closed pat nil closed)))
		     (and-fail-last
		      (values 'fail nil)
		      (not (eq 'fail closed))
		      (if (eq pat-l pat-r)
			  (values closed nil)
			  (close-var-exp-null (next pat-l) pat-r closed)))))))

(let ((pat-1 (list->dlist '(e.1)))
      (pat-2 (list->dlist '(e.1 e.2)))
      (pat-2a (list->dlist '(e.1 s.1)))
      (pat-2b (list->dlist '(e.1 t.1)))
      (pat-3 (list->dlist '(e.1 e.2 ()))))
  (assert (equal '((e.1 . nil)) (close-var-exp-null (dlist-first pat-1) (dlist-last pat-1) nil)))
  (assert (equal '((e.2 . nil) (e.1 . nil)) (close-var-exp-null (dlist-first pat-2) (dlist-last pat-2) nil)))
  (assert (equal 'fail (close-var-exp-null (dlist-first pat-3) (dlist-last pat-3) nil)))
  (assert (equal 'fail (close-var-exp-null (dlist-first pat-2a) (dlist-last pat-2a) nil)))
  (assert (equal 'fail (close-var-exp-null (dlist-first pat-2b) (dlist-last pat-2b) nil))))

(defun close-var-notnull (exp-l exp-r pat-l pat-r closed open-l open-r from-r)
  "Bind closed variables in the pattern delimited by the pointers PAT-L and PAT-R to the expression delimited by EXP-L and EXP-R.
Binding works by recursively moving EXP-L and PAT-L right and EXP-R and PAT-R left until an open variable is found (and binding all found closed variables), or until PAT-L and PAT-R are equal.
FROM-R determines whether the -L or -R variables are moved towards the middle of the dlist.
If a found closed variable is already bound with a different value in CLOSED, or the type of the variable is not compatible with the element in EXP, 'FAIL is returned.
If a symbol or number in PAT is not matched with the same symbol or number in EXP, 'FAIL is returned.
OPEN-L and OPEN-R are the open regions on the left and on the right side of the current point we're looking at (the point is determined by EXP-L, EXP-R, PAT-L, and PAT-R)."
  (declare (type dcons exp-l exp-r pat-l pat-r)
	   (type list closed open-l open-r)
	   (type (or null t) from-r)
	   (values t list)
	   (optimize speed))
  (flet ((recurse (closed)
	   (declare (type (or (member fail) list) closed)
		    (values t list))
	   (if (or (eq pat-l pat-r) (eq closed 'fail))
	       ;; If exp is not empty, (but pat is,) fail.
	       (if (not (eq exp-l exp-r))
		   (values 'fail nil)
		   (values closed (nconc open-l open-r)))
	       ;; the multiple-value-bind is necessary for "Return type not fixed values, so can't use known return convention" to go away.
	       (multiple-value-bind (c o)
		   (if (eq exp-l exp-r)
		       (if from-r
			   (close-var-exp-null pat-l (prev pat-r) closed)
			   (close-var-exp-null (next pat-l) pat-r closed))
		       (if from-r
			   (close-var-notnull exp-l (prev exp-r) pat-l (prev pat-r) closed open-l open-r from-r)
			   (close-var-notnull (next exp-l) exp-r (next pat-l) pat-r closed open-l open-r from-r)))
		 (values c o))))
	 (pat-data ()
	   (if from-r
	       (data pat-r)
	       (data pat-l)))
	 (exp-data ()
	   (if from-r
	       (data exp-r)
	       (data exp-l))))
    (declare (inline recurse pat-data exp-data))
    ;;(prind from-r) (prind exp-l) (prind exp-r) (prind pat-l) (prind pat-r)
    (let ((pat (pat-data))
	  (exp (exp-data)))
      ;;(prind pat exp)
      (etypecase pat
	(list (assert (null pat)) (and-fail-last (values 'fail nil) (null exp) (recurse closed)))
	(number (and-fail-last (values 'fail nil) (eq pat exp) (recurse closed)))
	(symbol ;;(prind (var-type pat))
	 (case (var-type pat)
	   ((svar) (and-fail-last (values 'fail nil)
				  (or (and (symbolp exp) (not (null exp))) (numberp exp))
				  (recurse (bind-closed pat exp closed))))
	   ((tvar) (and-fail-last (values 'fail nil)
				  (or (symbolp exp) (numberp exp) (dlistp exp))
				  ;; here, checking whether exp is a dlist is necessary, since tvars match to symbols and lists.
				  (recurse (bind-closed pat (if (and exp (dlistp exp)) (dlist->list* exp) exp) closed))))
	   ((evar) (if (eq pat-l pat-r)
		       ;; there is no more unbound variable after the variable named by PAT, therefore bind PAT to the compound or term determined by EXP-L and EXP-R.
		       (values (let* ((val (dcons->list exp-l exp-r)))
				 (bind-closed pat val closed))
			       (nconc open-l open-r))
		       (if from-r
			   ;; check right end, or return
			   (values closed
				   (nconc open-l
					  (list (new-open-region exp-l exp-r pat-l pat-r))
					  open-r))
			   (close-var-notnull exp-l exp-r pat-l pat-r closed open-l open-r t))))
	   (t (and-fail-last (values 'fail nil)
			     (eq pat exp) (recurse closed)))))
	(dlist (multiple-value-bind (closed open)
		   (if (null exp)
		       (close-var-exp-null (dlist-first pat) (dlist-last pat) closed)
		       (and-fail-last (values 'fail nil)
				      (dlistp exp)
				      (close-var-notnull (dlist-first exp) (dlist-last exp) (dlist-first pat) (dlist-last pat) closed nil nil nil)))
		 (and-fail-last (values 'fail nil)
				(not (eq closed 'fail))
				(progn
				  (setf open-l (if from-r open-l (nconc open-l open)))
				  (setf open-r (if from-r (nconc open open-r) open-r))
				  (recurse closed)))))))))

(declaim (inline close-var-dlist))
(defun close-var-dlist (exp pat closed)
  (declare (type (or null dlist) exp pat)
	   (type list closed))
  (if (null pat)
      (values (and-fail-last 'fail (null exp) closed) nil)
      (if (null exp)
	  (close-var-exp-null (dlist-first pat) (dlist-last pat) closed)
	  (close-var-notnull (dlist-first exp) (dlist-last exp) (dlist-first pat) (dlist-last pat) closed nil nil nil))))

(declaim (inline close-var))
(defun close-var (exp pat closed)
  (let ((exp-dlist (list->dlist exp))
	(pat-dlist (list->dlist pat)))
    (close-var-dlist exp-dlist pat-dlist closed)))

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
  (assert (equal '((s.c . 3) (s.b . 2) (s.a . 1)) (close-var exp-1 pat-1a nil)))
  (assert (equal '((t.c . 3) (t.b . 2) (t.a . 1)) (close-var exp-1 pat-1b nil)))
  (assert (equal '((s.c . 3) (s.a . 1)) (close-var exp-1 pat-2a nil)))
  (assert (equal '((t.c . 3) (t.a . 1)) (close-var exp-1 pat-2b nil)))
  (assert (equal 'fail (close-var exp-1 pat-3a nil)))
  (assert (equal 'fail (close-var exp-1 pat-3b nil)))
  (assert (equal '((e.b . (2)) (s.c . 3) (s.a . 1)) (close-var exp-1 pat-1c nil)))
  (assert (equal 'fail (close-var exp-2 pat-1a nil)))
  (assert (equal '((t.c . 3) (t.b . nil) (t.a . 1)) (close-var exp-2 pat-1b nil)))
  (assert (equal '((s.c . 3) (s.b . 2) (s.a . 1)) (close-var exp-3 pat-4a nil)))
  (assert (equal '((t.c . 3) (t.b . 2) (t.a . 1)) (close-var exp-3 pat-4b nil)))
  (assert (equal 'fail (close-var exp-3 pat-5a nil)))
  (assert (equal '((t.c . 3) (t.b . (2)) (t.a . 1)) (close-var exp-3 pat-5b nil)))
  (assert (equal '((e.b . ((2))) (s.c . 3) (s.a . 1)) (close-var exp-3 pat-5c nil)))
  (assert (equal 'fail (close-var exp-1 pat-4c nil)))
  (assert (equal '((e.c . nil) (t.b . 2) (s.a . 1)) (close-var '(1 2) '(s.a t.b e.c) nil)))
  (assert (equal '((s.2 . 7) (t.1 . (8)) (e.1 . (2 3)) (s.1 . 1))
		 (close-var '((1 2 3) (4) 5 6 (7) (8))
			    '((s.1 e.1) e.2 e.3 (s.2) t.1)
			    nil))))

(defun open-var (closed open)
  "OPEN contains regions of the expression and the pattern.
Iteratively extend the first e-variable in OPEN (which is at the top, because OPEN was returned by CLOSE-VAR) and call CLOSE-VAR on the remaining regions of expression and pattern.
If this fails, try an extension of the first e-variable, or fail if all extensions were tried."
  (declare (type list closed open)
	   (optimize speed))
  (if (null open)
      closed
      (multiple-value-bind (exp-l exp-r pat-l pat-r)
	  (extract-open-region (car open))
	(let* ((pat (data pat-l))
	       (pat-l-next (next pat-l))
	       (open-rest (cdr open)))
	  (flet ((try (val exp-l-next)
		   ;;(prind pat val closed)
		   (let* ((closed (bind-closed pat val closed)))
		     (and-fail-last
		      'fail
		      (not (eq closed 'fail))
		      (multiple-value-bind (closed open-1)
			  (if (null exp-l-next)
			      (close-var-exp-null pat-l-next pat-r closed)
			      (close-var-notnull exp-l-next exp-r pat-l-next pat-r closed nil nil nil))
			;;(prind closed)
			(and-fail-last
			 'fail
			 (not (eq closed 'fail))
			 (open-var closed (nconc open-1 open-rest))))))))
	    (assert (evarp pat))
	    (let* ((closed (try nil exp-l)))
	      (when (not (eq 'fail closed))
		(return-from open-var closed)))
	    (do ((cur exp-l (next cur))) (())
	      (let* ((val (dcons->list exp-l cur))
		     (closed (try val (next cur))))
		(when (not (eq 'fail closed))
		  (return-from open-var closed))
		(when (eq cur exp-r)
		  (return-from open-var 'fail)))))))))

(defun patmat (exp pat &key (closed nil))
  "Return a variable binding compatible with EXP, PAT, and CLOSED.
EXP: expression, PAT: pattern, CLOSED: ((a . value) (b . 1))."
  (multiple-value-bind (closed open)
      (close-var exp pat closed)
    ;;(prind closed)
    (and-fail-last 'fail
		   (not (eq 'fail closed))
		   (open-var closed open))))

(assert (equal (patmat '((1 2 3) 4 ())
		       '((s.1 e.1 e.2) e.3 e.4 (e.5)))
	       '((e.4 . (4)) (e.3 . nil) (e.2 . (2 3)) (e.1 . nil) (e.5 . nil) (s.1 . 1))))
(assert (equal 'fail (patmat '(b) '(a))))
(assert (equal '((s.a . b)) (patmat '(b) '(s.a))))
(assert (equal (patmat '(a (x) (x y) (x y z) (a b c) (a b x (x y) (x y z)) a b c)
		       '(e.1 (e.2 z) e.3 (e.4 (e.5 e.2 e.6)) e.7))
	       '((e.6 . (z)) (e.5 . nil) (e.7 . (a b c)) (e.4 . (a b x (x y))) (e.3 . ((a b c))) (e.2 . (x y)) (e.1 . (a (x) (x y))))))
;; lots of backtracking
(assert (equal (patmat '(a (x) (x y) (x y z) (a b c) (a b x (x y) (x y z)) a b c)
		       '(e.1 (e.5 e.2 e.6) e.3 (e.4 (e.2 z)) e.7))
	       '((e.7 . (a b c)) (e.4 . (a b x (x y))) (e.3 . ((x y z) (a b c))) (e.6 . nil) (e.2 . (x y)) (e.5 . nil) (e.1 . (a (x))))))
(assert (equal (patmat '(a (x) (x y) (x y z) (a b c) (a b x (x y) (x y z)))
		       '(e.1 (e.2 z) e.3 (e.4 (e.5 e.2 e.6))))
	       '((e.6 . (z)) (e.5 . nil) (e.3 . ((a b c))) (e.2 . (x y)) (e.1 . (a (x) (x y))) (e.4 . (a b x (x y))))))
(assert (equal (patmat '(c b a (a b x (x y) (x y z)) (a b c) (x y z) (x y) (x) a)
		       '(e.7 (e.4 (e.5 e.2 e.6)) e.3 (e.2 z) e.1))
	       '((e.1 . ((x y) (x) a)) (e.3 . ((a b c))) (e.6 . (z)) (e.2 . (x y)) (e.5 . nil) (e.4 . (a b x (x y))) (e.7 . (c b a)))))
(assert (equal (patmat '(a b b a b) '(e.1 t.1 t.1 e.2))
	       '((E.2 A B) (T.1 . B) (E.1 A))))
(assert (equal (patmat '(a b b a b) '(e.1 s.1 s.1 e.2))
	       '((E.2 A B) (S.1 . B) (E.1 A))))
(assert (equal 'fail (patmat '(1 2 3 4) '(s.1))))
(assert (equal 'fail (patmat '(1 2 3 4) '(t.1))))
(assert (equal 'fail (patmat '(1 2 3 4) '(1 2 s.1))))
(assert (equal 'fail (patmat '(1 2 3 4) '(s.1 3 4))))

;; In refal/html/ch_1.3.html it says "A t-variable takes any term as its value (recall that a term is either a symbol or an expression in structure brackets). An e-variable can take any expression as its value.", so I think t-variables differ from e-variables in that they may not bind NIL, but e-variables may. EDIT: I am now pretty sure that t-variables also may not bind to sublists (t-variables bind only if the sublist is in brackets), only e-variables may. Exercises 1.3(b) and (c) support that: "Write patterns that can be described in words as follows: [(a) ...] (b) an expression which contains at least two identical terms on the top level of structure; (c) a non-empty expression." and their answers (file refal/html/answers.html) are: "(b) e.1 t.X e.2 t.X e.3 ; (c) t.1 e.2 or e.1 t.2".
;; Chapter 1.3, exercise 1.4(b) says: "Find the results of the following matchings: (a) 'abbab' : e.1 t.X t.X e.2 (b) 'ab(b)ab' : e.1 t.X t.X e.2" and their answers are: "(a) t.X becomes b ; (b) failure;", so I think that t-variables may be either a symbol/number/character or an expression in structure brackets (meaning described as a regexp: "." OR "\(.*\)").

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
(defun nest-brackets (list open-bracket close-bracket nest-function &key (test #'eq))
  "Return the tree that results from recursively embedding parts of LIST enclosed with OPEN-BRACKET and CLOSE-BRACKET.
The NEST-FUNCTION is called with the sublist of LIST that will be embedded (and is enclosed by the brackets).
Returns the resulting tree and a value indicating if LIST was well-formed, i.e. doesn't contain a CLOSE-BRACKETs before OPEN-BRACKET and contains as many OPEN-BRACKETs as CLOSE-BRACKETs.
TEST is used to compare elements of LIST with the BRACKETs."
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
		     ((funcall test h open-bracket)
		      (multiple-value-bind (embedded-res embedded-open)
			  (rec nil (1+ open))
			(rec (cons embedded-res res) (if (= embedded-open open) open most-positive-fixnum))))
		     ((funcall test h close-bracket)
		      (let ((embedded (funcall nest-function (nreverse res))))
			(values embedded (1- open))))
		     ((listp h)
		      (multiple-value-bind (h-res h-accepted)
			  (nest-brackets h open-bracket close-bracket nest-function :test test)
			(rec (cons h-res res) (if h-accepted open most-negative-fixnum))))
		     (t
		      (rec (cons h res) open)))))))
    (multiple-value-bind (res open)
	(rec nil 0)
      (values res (= 0 open)))))

(assert (equalp (multiple-value-list (nest-brackets '(1 [ [ 2 (3) ] 4 ]) '[ '] #'make-nest[]-boa))
		`((1 ,(make-nest[]-boa (list (make-nest[]-boa '(2 (3))) 4))) t)))
;;	       '((1 #S(NEST[] :LIST (#S(NEST[] :LIST (2 (3))) 4))) t))) ;error I don't understand.
(assert (null (nth-value 1 (nest-brackets '(1 [ 2 (3) ] 4 ])
					  '[ '] #'make-nest[]-boa))))
(assert (null (nth-value 1 (nest-brackets '(1 [ [ 2 (3) ] 4)
					  '[ '] #'make-nest[]-boa))))
(assert (null (nth-value 1 (nest-brackets '(1 [ [ 2 (3) (]) 4 ])
					  '] '] #'make-nest[]-boa))))

(defstruct call
  (name nil :type symbol :read-only t)
  (args nil :type t :read-only t))

(defun refal-function-name-p (value)
  (symbolp value))

(defun parse-result (result)
  "Examples:
  ()
  (1 2 e.1 #S(NEST[] :LIST (function 2 (3))) (a b c))."
  (labels ((rec (r res)
	     (if (null r)
		 (nreverse res)
		 (let ((h (car r)))
		   (cond
		     ((or (symbolp h) (numberp h)) (rec (cdr r) (cons h res)))
		     ((listp h) (rec (cdr r) (cons (parse-result h) res)))
		     ((nest[]-p h)
		      (let ((l (nest[]-list h)))
			(if (or (null l) (not (refal-function-name-p (car l))))
			    nil
			    (let ((call (make-call :name (car l)
						   :args (parse-result (cdr l)))))
			      (rec (cdr r) (cons call res))))))
		     (t nil))))))
    (if (null result)
	(values nil t)
	(let ((res (rec result nil)))
	  (values res (not (null res)))))))

(assert (equal (multiple-value-list (parse-result '()))
	       '(() t)))
(assert (equal (multiple-value-list (parse-result '(1 2 e.1 (3 4) 5)))
	       '((1 2 e.1 (3 4) 5) t)))
;;(assert (equal (multiple-value-list (parse-result '(1 #S(NEST[] :LIST (function 2 (3))) 4)))
;;		'((1 #S(CALL :NAME FUNCTION :ARGS (2 (3))) 4) T)))
(assert (equalp (multiple-value-list (parse-result `(1 ,(make-nest[]-boa '(function 2 (3))) 4)))
		`((1 ,(make-CALL :NAME 'FUNCTION :ARGS '(2 (3))) 4) T)))

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
	  (cons pattern presults)
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

(assert (equalp (parse-function (nest-brackets '(ITAL-ENGL ((E.W) [ TRANS (E.W) [ TABLE ] ]))
					       '[ '] #'make-nest[]-boa))
		`(ITAL-ENGL ((E.W) ,(make-CALL :NAME 'TRANS :ARGS `((E.W) ,(make-CALL :NAME 'TABLE :ARGS NIL)))))))

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
	(if accepted
	    (parse-program r)
	    nil))
      nil))

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

(defun refal-eval (program view &key (c (make-counter)) (no-op #'default-no-function))
  "Input: a not yet parsed refal PROGRAM and a VIEW field.
Output: the result, when applying the VIEW field to the first function in PROGRAM."
  (let ((pprogram (parse-program* program)))
    (if (or (null pprogram) (not (listp view)))
	'parsing-error
	(catch 'refal-eval-error
	  (let ((first-function (caar program)))
	    (eval-view pprogram nil (make-call :name first-function :args view) :c c :no-op no-op))))))

;; Idea: A "lazy-evaluating" Refal, which can bind variables to lists with calls in them. That way calls could appear in a pattern, and a program could modify its meaning. Something like:
;;   Func-a { 0 s.1 = s.1; s.2 s.1 = <Func-a <- s.2 1> <+ s.1 1>> };
;;   Func-b { <Func-a s.1 s.2> = <+ s.1 s.2> }
;;   $ENTRY Go { =  <Func-b <Func-a 2 3>>  }
;; That way Func-b could accelerate calls to Func-a, like compiler-macros in lisp.

(defun eval-view (f b v &key c no-op)
  "F(unctions), B(indings), V(iew)."
  ;;(prind "eval-view" v)
  (let ((c-value (funcall c)))
    (when (<= c-value 0)
      (throw 'refal-eval-error 'overrun)))
  (cond
    ((null v) nil)
    ((or (svarp v) (tvarp v))
     (list (cdr (assoc v b))))
    ((evarp v)
     (cdr (assoc v b)))
    ((or (symbolp v) (numberp v))
     (list v))
    ((listp v)
     (list (loop for i in v
	      for ie = (eval-view f b i :c c :no-op no-op)
	      append ie)))
    ((call-p v)
     (let* ((n (call-name v))
	    (a (car (eval-view f b (call-args v) :c c :no-op no-op))))
       (car (eval-call f n a :c c :no-op no-op))))
    (t (error "unknown type"))))

(defun eval-call-userdef (f n a &key c no-op)
  "Evaluate the call of user-defined function N with arguments A by looking up the function in F(unctions)."
  ;;(prind "eval-call-userdef" n a)
  (let* ((clauses (assoc n f)))
    (if (null clauses)
	'unknown-function-error
	(let ((clauses (cdr clauses)))
	  ;; find the first matching clause
	  (loop for (pattern . result) in clauses do
	     ;;(prind "trying" pattern)
	       (let ((b (patmat a pattern)))
		 (when (not (eq b 'fail))
		   (return-from eval-call-userdef
		     (eval-view f b result :c c :no-op no-op)))))
	  (throw 'refal-eval-error 'recognition-error)))))

(defun numeric-list-p (a)
  (and (listp a)
       (loop for i in a always (numberp i))))

(defun eval-call-builtin (f n a &key c no-op)
  (declare (ignore f c no-op))
  (case n
    ((+) (if (numeric-list-p a) (list (list (apply #'+ a))) (throw 'refal-eval-error 'numeric-error)))
    ((-) (if (and (numeric-list-p a) (not (null a))) (list (list (apply #'- a))) (throw 'refal-eval-error 'numeric-error)))
    ((*) (if (numeric-list-p a) (list (list (apply #'* a))) (throw 'refal-eval-error 'numeric-error)))
    ((/) (if (and (numeric-list-p a)
		  (not (null a))
		  (loop for i in (cdr a) always (not (= i 0))))
	     (list (list (apply #'/ a)))
	     (throw 'refal-eval-error 'numeric-error)))
    (t 'unknown-function-error)))

(defun eval-call (f n a &key c no-op)
  "Evaluate the call of function N with arguments A by evaluating built-in functions or user-defined F(unctions), and, if both don't know N, call no-op."
  ;;(print (list "f" f "n" n "a" a))
  (let ((r (eval-call-builtin f n a :c c :no-op no-op)))
    (if (eq r 'unknown-function-error)
	(let ((r (eval-call-userdef f n a :c c :no-op no-op)))
	  (if (eq r 'unknown-function-error)
	      (funcall no-op f n a)
	      r))
	r)))

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
