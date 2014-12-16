
(defun variable-p (a)
  "Return T if A is a variable, i.e. a symbol starting with ?."
  (and (symbolp a)
       (eq (elt (symbol-name a) 0) #\?)))

(defun atom-p (a)
  (or (and (symbolp a) (not (variable-p a)))
      (numberp a)))

(defstruct compound
  name
  pars)

(defstruct pro-list
  head  ; a list of terms
  tail  ; a term
)  ; if tail is non-nil, then the head must be non-nil, too (however, tail=nil must be allowed to facilitate computation)

(defun print-hash-table (ht)
  (format t "#<HASH-TABLE :TEST ~A :COUNT ~A~%" (hash-table-test ht) (hash-table-count ht))
  (loop for key being the hash-keys of ht do
       (format t "~A ~A~%" key (gethash key ht)))
  (format t ">~&"))

(defun print-hash-table* (ht)
  (if (hash-table-p ht)
      (print-hash-table ht)
      (print ht)))

(defun setf-hash-or-fail (ht k v)
  (print (list "ht" ht "k" k "v" v))
  (multiple-value-bind (vv present-p) (gethash k ht)
    (if present-p
	(if (equal vv v)
	    ht
	    nil)
	(progn
	  (setf (gethash k ht) v)
	  ht))))

(defun unify-pro-list (l1 l2 vars)
  (labels ((rec (h1 t1 h2 t2)
;;	     (print (list "rec" h1 t1 h2 t2))
;;	     (print "vars")
;;	     (print-hash-table* vars)
	     (if (null h1)
		 (if (null h2)
		     (unify t1 t2 vars) ; remember: t1,t2 are terms, no lists
		     ; unify t1 with [h2|t2]
		     (cond
		       ((pro-list-p t1)
			(setf vars (unify (car (pro-list-head t1)) (car h2) vars))
			(rec nil (make-pro-list :head (cdr (pro-list-head t1)) :tail (pro-list-tail t1)) h2 t2))
		       ((variable-p t1) (setf-hash-or-fail vars t1 (make-pro-list :head h2 :tail t2)))
		       (t nil)))
		 (if (null h2) ; h1 is not null
		     (rec h2 t2 h1 t1) ; unify t2 with [h1|t2]
		     (progn ; h1 and h2 are not nil
		       (setf vars (unify (car h1) (car h2) vars))
		       (rec (cdr h1) t1 (cdr h2) t2))))))
    (let ((h1 (pro-list-head l1)) ; a list of terms
	  (t1 (pro-list-tail l1)) ; a term
	  (h2 (pro-list-head l2)) ; a list of terms
	  (t2 (pro-list-tail l2))); a term
      (cond
	((and (null h1) (null t1) (null h2) (null t2)) vars)
	((or (and (not (null t1)) (null h1)) (and (not (null t2)) (null h2))) 'error)
	(t (rec h1 t1 h2 t2))))))

; this function and unify-pro-list have a bug: they overwrite variable assignments in the hash-table. rather use alists or plists.
(defun unify (terma termb &optional (vars (make-hash-table :test 'eq)))
  "Returns VARS which binds variables to terms. On error 'ERROR is returned, and if unifying fails, NIL is returned."
  (print (list terma termb))
  (print-hash-table* vars)
  ;  (print (list (variable-p terma) (variable-p termb) (compound-p terma) (compound-p termb) (pro-list-p terma) (pro-list-p termb)))
  (cond ((variable-p terma) (setf-hash-or-fail vars terma termb))
	((atom-p terma) (if (variable-p termb)
			    (setf-hash-or-fail vars termb terma)
			    (and (eq terma termb) vars)))
	((and (compound-p terma) (atom-p (compound-name terma)))
	 (cond ((variable-p termb) (setf-hash-or-fail vars termb terma))
	       ((and (compound-p termb) (atom-p (compound-name termb)))
		;(print (list terma termb))
		(and (eq (compound-name terma) (compound-name termb))
		     (if (loop
			    for a in (compound-pars terma)
			    for b in (compound-pars termb)
			    always (let ((u (unify a b vars))) u))
			 vars
			 nil)))
	       (t 'error)))
	;; list unification missing
	((pro-list-p terma)
	 (cond ((variable-p termb) (setf-hash-or-fail vars termb terma))
	       ((pro-list-p termb)
		(unify-pro-list terma termb vars))
	       (t 'error)))
	(t 'error)))

(defun make-hash-table-from-assoc (a &rest rest)
  (let ((ht (apply #'make-hash-table rest)))
    (loop for v in a do (setf (gethash (car v) ht) (cdr v)))
    ht))

(defun unify-test (terma termb vars)
  (let ((vars-unify (unify terma termb)))
;;    (print (list "vars-unify" vars-unify "vars" vars))
    (if (not (equalp vars vars-unify))
	(progn
	  (print "vars")
	  (if (hash-table-p vars)
	      (print-hash-table vars)
	      (print vars))
	  (print "vars-unify")
	  (if (hash-table-p vars-unify)
	      (print-hash-table vars-unify)
	      (print vars-unify))
	  (error (format nil "unify-test failed for terma:~A termb:~A"
			 terma termb))))))

(unify-test '?X '?Y (make-hash-table-from-assoc '((?X . ?Y)) :test 'eq))
(unify-test 'X '?Y (make-hash-table-from-assoc '((?Y . X)) :test 'eq))
(unify-test 'X 'Y nil)
(unify-test 1 1 (make-hash-table-from-assoc nil :test 'eq))
(unify-test (make-compound :name 'x :pars '(1 2))
	    (make-compound :name 'x :pars '(?X 2))
	    (make-hash-table-from-assoc '((?X . 1)) :test 'eq))
(unify-test (make-compound :name 'x :pars '(1 2))
	    (make-compound :name 'x :pars '(2 2))
	    nil)
(unify-test (make-compound :name 'x :pars '(1 2))
	    (make-compound :name 'x :pars '(1 2))
	    (make-hash-table-from-assoc nil :test 'eq))
(unify-test (make-compound :name '?x :pars '(1 2))
	    (make-compound :name 'x :pars '(1 2))
	    'error)
(unify-test (make-compound :name 'x :pars '(1 2))
	    (make-compound :name '?x :pars '(1 2))
	    'error)
(unify-test (make-pro-list :head '(1) :tail nil)
	    (make-pro-list :head '(?H) :tail '?T)
	    (make-hash-table-from-assoc '((?H . 1) (?T . nil)) :test 'eq))
(unify-test (make-pro-list :head '(?H) :tail '?T)
	    (make-pro-list :head '(1) :tail nil)
	    (make-hash-table-from-assoc '((?H . 1) (?T . nil)) :test 'eq))
(unify-test (make-pro-list :head '(1 ?B) :tail nil)
	    (make-pro-list :head '(?A 2) :tail nil)
	    (make-hash-table-from-assoc '((?A . 1) (?B . 2)) :test 'eq))
(unify-test (make-pro-list :head nil :tail '?A)
	    (make-pro-list :head '(?H) :tail '?T)
	    'error)
(unify-test (make-pro-list :head '(?H) :tail '?T)
	    (make-pro-list :head nil :tail '?A)
	    'error)
(unify-test (make-pro-list :head nil :tail nil)
	    (make-pro-list :head nil :tail nil)
	    (make-hash-table-from-assoc nil :test 'eq))
(unify-test (make-pro-list :head (list (make-pro-list :head '(?A) :tail nil)) :tail nil)
	    (make-pro-list :head (list (make-pro-list :head '(1) :tail nil)) :tail nil)
	    (make-hash-table-from-assoc '((?A . 1)) :test 'eq))
(unify-test (make-pro-list :head (list (make-pro-list :head '(1) :tail nil)) :tail nil)
	    (make-pro-list :head (list (make-pro-list :head '(?A) :tail nil)) :tail nil)
	    (make-hash-table-from-assoc '((?A . 1)) :test 'eq))
; the following (2?) test(s?) make sbcl to take all cpu
;(unify-test (make-pro-list :head '(?A) :tail (make-pro-list :head '(?B) :tail nil))
;	    (make-pro-list :head '(1 2) :tail nil)
;	    (make-hash-table-from-assoc '((?A . 1) (?B . 2)) :test 'eq))
;(unify-test (make-pro-list :head '(1 2) :tail nil)
;	    (make-pro-list :head '(?A) :tail (make-pro-list :head '(?B) :tail nil))
;	    (make-hash-table-from-assoc '((?A . 1) (?B . 2)) :test 'eq))
(unify-test (make-compound :name 'x :pars '(?A ?A))
	    (make-compound :name 'x :pars '(1 2))
	    nil)
(unify-test (make-compound :name 'x :pars '(?A ?A))
	    (make-compound :name 'x 
			   :pars (list (make-compound :name 'y :pars nil)
				       (make-compound :name 'y :pars '(1))))
	    nil)
;; missing: more testing of incompatible variable assignments unifying to NIL

(defun rule-from-cons (c)
  (make-rule :head (car c) :body (cdr c)))

(defun insert (consrule &optional (rules (make-hash-table :test 'equal)))
  "Inserts the CONSRULE (CONS HEAD BODY) into RULES. Appends to rules."
  ; appends to rules without checking for duplicate rules
  (let* ((head (car consrule))
	 (rulekey (cons (compound-name head) (length (compound-pars head))) ))
    (multiple-value-bind (v p) (gethash rulekey rules)
      (if p
	  (setf (gethash rulekey rules) (cons consrule v))
	  (setf (gethash rulekey rules) (cons consrule v)))))
  rules)

;(print-hash-table* (insert (cons (make-compound :name 'x :pars '(1 2)) 2)))
(defparameter rrr (insert (cons (make-compound :name 'x :pars '(1 2)) t)))

(defun copy-hash-table (ht)
  (let ((c (make-hash-table :test (hash-table-test ht) :size (hash-table-size ht) :rehash-size (hash-table-rehash-size ht) :rehash-threshold (hash-table-rehash-threshold ht))))
    (maphash (lambda (k v) (setf (gethash k c) v)) ht)
    c))

(defun ask-unify (query rules &optional (vars (make-hash-table :test 'eq)))
  "Queries RULES for QUERY, which is a COMPOUND. Returns 'ERROR on erroneous
query, NIL on query failure, or variable assignments using a hash-table or T on
success."
  (let ((qname (compound-name query))
	(qarity (length (compound-pars query))))
    (multiple-value-bind (qrules qpresent) (gethash (cons qname qarity) rules)
      (if qpresent
	  (loop for rule in qrules do
	       (print (list "rule" rule))
	       (let* ((head (car rule))
		      (body (cdr rule)) ; body is a list of statements
		      (u (unify query head vars)))
		 (when u
		   ;; query unifies with rule using variables: check body
		   (let ((cvars (copy-hash-table vars)))
		     ;; when do i need to copy-hash-table?
		     ;; loop over body statements
		     (loop for sment in body do
			  (print (list "sment" sment))
			  (let (ask sment rules cvars)
		   ;; query doesn't unify: do nothing (continue with the loop)
	       )
	  'error)))) ; undefined procedure

; man(hubert).
; man(hugo).
; woman(jill).
; woman(jane).
; parent(hugo, jill).
; parent(hugo, hubert).
; parent(jill, jane).
; parent(hubert, jane).
; father(X) :- parent(X, C), man(X).
; father(X).

;; findall/3, bagof/3, setof/3

;; (defun ask () 
;; should branch to ask-unify, ask-assert, ask-is, ask-=, ask-\=, ask-and, ask-or, ask-not, etc.
;; example: if a compound has :name and, then ask-and must be called