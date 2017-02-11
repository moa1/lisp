(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-custom-hash-table)
(use-package :cl-custom-hash-table)
(ql:quickload :dlist2)
(ql:quickload :alexandria)
(use-package :alexandria)

(defstruct multitree-type
  ;; Takes the multitree, a key, and a value. Destructively add the value or change the value stored below the key in the multitree. Returns the modified multitree and a boolean indicating if the key was changed (and not added).
  (set-elt nil :type (function (t t t) (values t (member t nil))))
  ;; Takes the multitree and the element whose subtree is to be returned. Returns the subtree (a multitree) and a value indicating if the element was present.
  (select-elt nil :type (function (t t) (values t (member t nil)))))

(defun multitree-select-path (multitree-type multitree path)
  "Walk through the MULTITREE (which must be of type MULTITREE-TYPE) recursively by selecting the car of PATH.
Returns as values the subtree found and a value indicating if all elements of PATH were found in MULTITREE."
  (if (null path)
      multitree
      (let* ((e (car path))
	     (select (multitree-type-select-elt multitree-type)))
	(multiple-value-bind (subtree present-p)
	    (funcall select multitree e)
	  (if present-p
	      (multitree-select-path multitree-type subtree (cdr path))
	      (values nil nil))))))

(defun multitree-set-elt-hash-table (multitree key value)
  (multiple-value-bind (old-value present)
      (gethash key multitree)
    (declare (ignore old-value))
    (setf (gethash key multitree) value)
    (values multitree present)))

(let* ((smt0 (make-hash-table))
       (smt1 (multitree-set-elt-hash-table smt0 'a 1))
       (smt2 (multitree-set-elt-hash-table smt1 'b 2))
       (mt0 (make-hash-table))
       (mt1 (multitree-set-elt-hash-table mt0 'a smt2))
       (mt2 (multitree-set-elt-hash-table mt1 'b 3))
       (mt-type (make-multitree-type :set-elt #'multitree-set-elt-hash-table
				     :select-elt (lambda (mt e) (gethash e mt)))))
  (assert (eq 3 (multitree-select-path mt-type mt2 '(b))))
  (assert (eq 2 (multitree-select-path mt-type mt2 '(a b))))
  (assert (eq 1 (multitree-select-path mt-type mt2 '(a a)))))

;;;; Property lists, i.e. a list of alternating SYMBOL and VALUE.
;;;; Example '(a 1 b 2 c (a list))

(defun plist-cdr (symbol plist)
  "Return the cdr of plist PLIST whose car is SYMBOL and whose cadr is the value of SYMBOL.
Return values NIL if SYMBOL is not found in plist PLIST, or if PLIST has an odd length."
  (labels ((rec (plist)
	     (if (null plist)
		 nil
		 (if (eq symbol (car plist))
		     plist
		     (rec (cddr plist))))))
    (rec plist)))

(defun plist-get (symbol plist)
  "Return the value associated with symbol SYMBOL in plist PLIST, or NIL if SYMBOL is not in PLIST.
Returns NIL if plist has an odd length."
  (cadr (plist-cdr symbol plist)))

(defun plist-has (symbol plist)
  "Return whether plist PLIST has symbol SYMBOL."
  (not (null (plist-cdr symbol plist))))

(defun plist-set (symbol val plist)
  "Return the plist PLIST that results from setting the symbol SYMBOL in PLIST to value VAL (or extending PLIST with the symbol SYMBOL and value VAL if SYMBOL didn't exist in PLIST before)."
  (let ((old-cdr (plist-cdr symbol plist)))
    (if (null old-cdr)
	(cons symbol (cons val plist))	
	(progn
	  (setf (cadr old-cdr) val)
	  plist))))

(defun alist-to-plist (alist)
  "Return the plist that is equivalent to alist ALIST."
  ;; NOTE: could return an plist that has multiple entries with the same symbol. (maybe FIXME: raise an error in that case.)
  (let ((plist nil))
    (loop for (a . b) in alist do
	 (setf plist (cons b (cons a plist))))
    (nreverse plist)))

(defun plist-to-alist (plist)
  "Return the alist that is equivalent to plist PLIST."
  (let ((alist nil))
    (loop for a on plist by #'cddr do
	 (setf alist (cons (cons (car a) (cadr a)) alist)))
    (nreverse alist)))

