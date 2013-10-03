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

(defun var-type (var vars)
  (destructuring-bind (svars tvars evars) vars
    ;; TODO: check that SVARS, TVARS, and EVARS are non-overlapping sets, i.e. (var-type 'a '((a) (a) nil)) should assert an error.
    (cond
      ((find var svars) 'svar)
      ((find var tvars) 'tvar)
      ((find var evars) 'evar)
      (t nil))))

(defun bind-closed (var val closed &key (test #'equal))
  "Return an object representing the variables and their values bound in CLOSED, and the variable named VAR bound to VAL.
Return 'FAIL if the variable VAR is already bound to another value than VAL.
Return the unmodified CLOSED if VAR is bound to a value equal to VAL (under equality test TEST)."
  ;; NOTE: this function must not modify CLOSED, since OPEN-VAR depends on an unmodified CLOSED.
  (let ((acons (assoc var closed)))
    (if (null acons)
	(acons var val closed)
	(if (funcall test val (cdr acons))
	    closed
	    'fail))))

(defmacro and-fail-last (&rest forms)
  "If one of the FORMS but the last fails, return 'FAIL, otherwise the last form(which can be nil)."
  ;; TODO: add &key :fail-symbol, which customizes the fail symbol
  (labels ((rec (forms last)
	     (if (null forms)
		 last
		 `(if ,(car forms)
		     ,(rec (cdr forms) last)
		     'fail))))
    (assert (not (null forms)))
    (rec (butlast forms) (car (last forms)))))

(defun dlist->list* (dlist)
  "Deeply convert the dlist DLIST to a list."
  (do ((cur (dlist-last dlist) (prev cur)) (l nil)) ((eq cur nil) l)
    (push (let ((d (data cur)))
	    (if (dlistp d)
		(dlist->list* d)
		d))
	  l)))

(defun list->dlist (l)
  "Deeply convert list L to a dlist."
  (if (null l)
      nil
      (let ((content (loop for e in l collect
			  (if (listp e)
			      (list->dlist e)
			      e))))
	(apply #'dlist content))))

(defun dcons->list (dcons-start dcons-stop &key (iterate #'prev))
  "Iteratively push the elements between DCONS-STOP and DCONS-START onto a newly constructed list and substitute DCONS-STOP with (funcall DIRECTION DCONS-STOP) after each iteration.
If an element is a DLIST, it is converted into a list before pushing it.
This function returns a list with at least one element."
  (do ((cur dcons-stop (funcall iterate cur)) (l nil))
      ((eq cur dcons-start) (push (let ((d (data cur))) (if (dlistp d) (dlist->list* d) d)) l))
    (push (let ((d (data cur))) (if (dlistp d) (dlist->list* d) d)) l)))

(declaim (inline new-open-region))
(defun new-open-region (exp-l exp-r pat-l pat-r)
  "Make a new open region determined by EXP-L, EXP-R, PAT-L, PAT-R."
  (let ((region (list exp-l exp-r pat-l pat-r)))
    region))

(declaim (inline extract-open-region))
(defun extract-open-region (region)
  (destructuring-bind (exp-l exp-r pat-l pat-r) region
    (values exp-l exp-r pat-l pat-r)))

(defun close-var-exp-null (pat-l pat-r vars closed)
  "Bind the pattern described by PAT-L and PAT-R to a NIL-expression (therefore, the only possible binding pattern is a list of e-variables, which are all bound to NIL).
If any closed variables in the pattern have conflicting bindings in CLOSED, return 'FAIL.
This function returns NIL as the open variables, because there can't be open variables in an NIL-expression."
  (flet ((pat-data ()
	   (data pat-l)))
    ;;(prind pat-l) (prind pat-r)
    (let ((pat (pat-data)))
      ;;(prind pat)
      (and-fail-last (symbolp pat) (eq (var-type pat vars) 'evar)
		     (let ((closed (bind-closed pat nil closed)))
		       (and-fail-last
			(not (eq 'fail closed))
			(if (eq pat-l pat-r)
			    (values closed nil)
			    (close-var-exp-null (next pat-l) pat-r vars closed))))))))

(let ((pat-1 (list->dlist '(e.1)))
      (pat-2 (list->dlist '(e.1 e.2)))
      (pat-3 (list->dlist '(e.1 e.2 ())))
      (vars-1 '(() () (e.1 e.2)))
      (vars-2 '((e.2) () (e.1)))
      (vars-3 '(() (e.2) (e.1))))
  (assert (equal '((e.1 . nil)) (close-var-exp-null (dlist-first pat-1) (dlist-last pat-1) vars-1 nil)))
  (assert (equal '((e.2 . nil) (e.1 . nil)) (close-var-exp-null (dlist-first pat-2) (dlist-last pat-2) vars-1 nil)))
  (assert (equal 'fail (close-var-exp-null (dlist-first pat-3) (dlist-last pat-3) vars-1 nil)))
  (assert (equal 'fail (close-var-exp-null (dlist-first pat-2) (dlist-last pat-2) vars-2 nil)))
  (assert (equal 'fail (close-var-exp-null (dlist-first pat-2) (dlist-last pat-2) vars-3 nil))))

(defun close-var-notnull (exp-l exp-r pat-l pat-r vars closed open-l open-r from-r)
  "Bind closed variables in the pattern delimited by the pointers PAT-L and PAT-R to the expression delimited by EXP-L and EXP-R.
Binding works by recursively moving EXP-L and PAT-L right and EXP-R and PAT-R left until an open variable is found (and binding all found closed variables), or until PAT-L and PAT-R are equal.
FROM-R determines whether the -L or -R variables are moved towards the middle of the dlist.
If a found closed variable is already bound with a different value in CLOSED, or the type of the variable is not compatible with the element in EXP, 'FAIL is returned.
If a symbol or number in PAT is not matched with the same symbol or number in EXP, 'FAIL is returned.
VARS is needed for VAR-TYPE.
OPEN-L and OPEN-R are the open regions on the left and on the right side of the current point we're looking at (the point is determined by EXP-L, EXP-R, PAT-L, and PAT-R)."
  (declare (type dcons exp-l exp-r pat-l pat-r)
	   (type list vars closed open-l open-r)
	   (type (or null t) from-r)
	   (optimize (debug 3)))
  (macrolet ((recurse (closed)
	       (let ((closed-s (gensym)))
		 `(let ((,closed-s ,closed))
		    (if (eq pat-l pat-r)
			(values ,closed-s open-l open-r)
			(if from-r
			    (close-var-notnull exp-l (prev exp-r) pat-l (prev pat-r) vars ,closed-s open-l open-r from-r)
			    (close-var-notnull (next exp-l) exp-r (next pat-l) pat-r vars ,closed-s open-l open-r from-r)))))))
    (flet ((pat-data ()
	     (if from-r
		 (data pat-r)
		 (data pat-l)))
	   (exp-data ()
	     (if from-r
		 (data exp-r)
		 (data exp-l))))
      ;;(prind from-r) (prind exp-l) (prind exp-r) (prind pat-l) (prind pat-r)
      (let ((pat (pat-data))
	    (exp (exp-data)))
	;;(prind pat exp)
	(etypecase pat
	  (list (assert (null pat)) (and-fail-last (null exp) (recurse closed)))
	  (number (and-fail-last (eq pat exp) (recurse closed)))
	  (symbol ;;(prind (var-type pat vars))
		  (case (var-type pat vars)
		    ((svar) (and-fail-last (or (and (symbolp exp) (not (null exp))) (numberp exp))
					   (recurse (bind-closed pat exp closed))))
		    ((tvar) (and-fail-last (or (symbolp exp) (numberp exp) (dlistp exp))
					   (recurse (bind-closed pat (if (dlistp exp) (dlist->list* exp) exp) closed))))
		    ((evar) (if (eq pat-l pat-r)
				;; there is no more unbound variable after the variable named by PAT, therefore bind PAT to the compound or term determined by EXP-L and EXP-R.
				(values (let* ((val (dcons->list exp-l exp-r)))
					  (bind-closed pat val closed))
					(nconc open-l open-r))
				(if from-r
				    ;; check right end, or return
				    (values closed (nconc open-l
							  (list (new-open-region exp-l exp-r pat-l pat-r))
							  open-r))
				    (close-var-notnull exp-l exp-r pat-l pat-r vars closed open-l open-r t))))
		    (t (and-fail-last (eq pat exp) (recurse closed)))))
	  (dlist (multiple-value-bind (closed open)
		     (if (null exp)
			 (close-var-exp-null (dlist-first pat) (dlist-last pat) vars closed)
			 (and-fail-last (dlistp exp) (close-var-notnull (dlist-first exp) (dlist-last exp) (dlist-first pat) (dlist-last pat) vars closed nil nil nil)))
		   (and-fail-last (not (eq closed 'fail))
				  (let ((open-l (if from-r open-l (nconc open-l open)))
					(open-r (if from-r (nconc open open-r) open-r)))
				    (recurse closed))))))))))

(defun close-var-dlist (exp pat vars closed)
  (declare (type (or null dlist) exp pat)
	   (type list vars closed))
  (if (null pat)
      (values (and-fail-last (null exp) closed) nil)
      (if (null exp)
	  (close-var-exp-null (dlist-first pat) (dlist-last pat) vars closed)
	  (close-var-notnull (dlist-first exp) (dlist-last exp) (dlist-first pat) (dlist-last pat) vars closed nil nil nil))))

(defun close-var (exp pat vars closed)
  (let ((exp-dlist (list->dlist exp))
	(pat-dlist (list->dlist pat)))
    (close-var-dlist exp-dlist pat-dlist vars closed)))

(let ((exp-1 '(1 2 3))
      (exp-2 '(1 nil 3))
      (exp-3 '((1) (2) (3)))
      (pat-1 '(a b c))
      (pat-2 '(a 2 c))
      (pat-3 '(a 3 c))
      (pat-4 '((a) (b) (c)))
      (pat-5 '((a) b (c)))
      (vars-s '((a b c) nil nil))
      (vars-t '(nil (a b c) nil))
      (vars-e '((a c) nil (b))))
  (assert (equal '((c . 3) (b . 2) (a . 1)) (close-var exp-1 pat-1 vars-s nil)))
  (assert (equal '((c . 3) (b . 2) (a . 1)) (close-var exp-1 pat-1 vars-t nil)))
  (assert (equal '((c . 3) (a . 1)) (close-var exp-1 pat-2 vars-s nil)))
  (assert (equal '((c . 3) (a . 1)) (close-var exp-1 pat-2 vars-t nil)))
  (assert (equal 'fail (close-var exp-1 pat-3 vars-s nil)))
  (assert (equal 'fail (close-var exp-1 pat-3 vars-t nil)))
  (assert (equal '((b . (2)) (c . 3) (a . 1)) (close-var exp-1 pat-1 vars-e nil)))
  (assert (equal 'fail (close-var exp-2 pat-1 vars-s nil)))
  (assert (equal '((c . 3) (b . nil) (a . 1)) (close-var exp-2 pat-1 vars-t nil)))
  (assert (equal '((c . 3) (b . 2) (a . 1)) (close-var exp-3 pat-4 vars-s nil)))
  (assert (equal '((c . 3) (b . 2) (a . 1)) (close-var exp-3 pat-4 vars-t nil)))
  (assert (equal 'fail (close-var exp-3 pat-5 vars-s nil)))
  (assert (equal '((c . 3) (b . (2)) (a . 1)) (close-var exp-3 pat-5 vars-t nil)))
  (assert (equal '((b . ((2))) (c . 3) (a . 1)) (close-var exp-3 pat-5 vars-e nil)))
  (assert (equal 'fail (close-var exp-1 pat-4 vars-e nil)))
  (assert (equal '((s.2 . 7) (t.1 . (8)) (e.1 . (2 3)) (s.1 . 1))
		 (close-var '((1 2 3) (4) 5 6 (7) (8))
			    '((s.1 e.1) e.2 e.3 (s.2) t.1)
			    '((s.1 s.2) (t.1) (e.1 e.2 e.3))
			    nil))))

(defun open-var (vars closed open)
  "OPEN contains regions of the expression and the pattern.
Iteratively extend the first e-variable in OPEN (which is at the top, because OPEN was returned by CLOSE-VAR) and call CLOSE-VAR on the remaining regions of expression and pattern.
If this fails, try an extension of the first e-variable, or fail if all extensions were tried."
  (declare (type list vars closed open))
  (if (null open)
      closed
      (multiple-value-bind (exp-l exp-r pat-l pat-r)
	  (extract-open-region (car open))
	(let* ((pat (data pat-l))
	       (pat-l-next (next pat-l)))
	  (flet ((try (val exp-l-next)
		   ;;(prind pat val closed)
		   (let* ((closed (bind-closed pat val closed)))
		     (and-fail-last
		      (not (eq closed 'fail))
		      (multiple-value-bind (closed open-1)
			  (if (null exp-l-next)
			      (close-var-exp-null pat-l-next pat-r vars closed)
			      (close-var-notnull exp-l-next exp-r pat-l-next pat-r vars closed nil nil nil))
			;;(prind closed)
			(and-fail-last
			 (not (eq closed 'fail))
			 (if (null open-1)
			     (open-var vars closed (cdr open))
			     (open-var vars closed (append open-1 (cdr open))))))))))
	    (assert (eq 'evar (var-type pat vars)))
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

(defun patmat (vars exp pat &key (closed nil))
  "Return a variable binding compatible with VARS, EXP, PAT, and CLOSED.
EXP: expression, PAT: pattern, VARS: (svars tvars evars), CLOSED: ((a . value) (b . 1))."
  (multiple-value-bind (closed open)
      (close-var exp pat vars closed)
    ;;(prind closed)
    (and-fail-last (not (eq 'fail closed))
		   (open-var vars closed open))))

(assert (equal (patmat '((s.1 s.2) (t.1) (e.1 e.2 e.3 e.4 e.5 e.6 e.7))
		       '((1 2 3) 4 ())
		       '((s.1 e.1 e.2) e.3 e.4 (e.5)))
	       '((e.4 . (4)) (e.3 . nil) (e.2 . (2 3)) (e.1 . nil) (e.5 . nil) (s.1 . 1))))
(assert (equal 'fail (patmat '(() () ()) '(b) '(a))))
(assert (equal '((a . b)) (patmat '((a) () ()) '(b) '(a))))
(assert (equal (patmat '(() () (e.1 e.2 e.3 e.4 e.5 e.6 e.7))
		       '(a (x) (x y) (x y z) (a b c) (a b x (x y) (x y z)) a b c)
		       '(e.1 (e.2 z) e.3 (e.4 (e.5 e.2 e.6)) e.7))
	       '((e.6 . (z)) (e.5 . nil) (e.7 . (a b c)) (e.4 . (a b x (x y))) (e.3 . ((a b c))) (e.2 . (x y)) (e.1 . (a (x) (x y))))))
(assert (equal (patmat '(() () (e.1 e.2 e.3 e.4 e.5 e.6 e.7))
		       '(a (x) (x y) (x y z) (a b c) (a b x (x y) (x y z)))
		       '(e.1 (e.2 z) e.3 (e.4 (e.5 e.2 e.6))))
	       '((e.6 . (z)) (e.5 . nil) (e.3 . ((a b c))) (e.2 . (x y)) (e.1 . (a (x) (x y))) (e.4 . (a b x (x y))))))
(assert (equal (patmat '(() () (e.1 e.2 e.3 e.4 e.5 e.6 e.7))
		       '(c b a (a b x (x y) (x y z)) (a b c) (x y z) (x y) (x) a)
		       '(e.7 (e.4 (e.5 e.2 e.6)) e.3 (e.2 z) e.1))
	       '((e.1 . ((x y) (x) a)) (e.3 . ((a b c))) (e.6 . (z)) (e.2 . (x y)) (e.5 . nil) (e.4 . (a b x (x y))) (e.7 . (c b a)))))
