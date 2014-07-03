(load "~/quicklisp/setup.lisp")
(ql:quickload :dlist)
(use-package :dlist)

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
  (let ((s (symbol-name symbol)))
    (and (>= (length s) 2)
	 (eq (elt s 0) #\S)
	 (eq (elt s 1) #\.))))

(declaim (inline tvarp))
(defun tvarp (symbol)
  (let ((s (symbol-name symbol)))
    (and (>= (length s) 2)
	 (eq (elt s 0) #\T)
	 (eq (elt s 1) #\.))))

(declaim (inline evarp))
(defun evarp (symbol)
  (let ((s (symbol-name symbol)))
    (and (>= (length s) 2)
	 (eq (elt s 0) #\E)
	 (eq (elt s 1) #\.))))

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
(defun bind-closed (var val closed &key (test #'equal))
  "Return an object representing the variables and their values bound in CLOSED, and the variable named VAR bound to VAL.
Return 'FAIL if the variable VAR is already bound to another value than VAL.
Return the unmodified CLOSED if VAR is bound to a value equal to VAL (under equality test TEST)."
  (declare (type symbol var)
	   (type list closed))
  ;; NOTE: this function must not modify CLOSED, since OPEN-VAR depends on an unmodified CLOSED.
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
