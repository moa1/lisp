;; Using B yields "undefined variable: BC", unless BC is a lexical variable in the application of the symbol macro B.
(let ((bc 0)) ;this BC is not used in applications of B (except in GETBC)
  (define-symbol-macro b (incf bc)) ;this BC does not refer to the LET-BC bound above (unless I would use the symbol macro B in the forms of above LET.
  (defun getbc ()
    bc))

;; (defun error1 ()
;;   b) ;this expands to (INCF BC), but there is neither a global nor a lexical BC defined here.

(defun a0 ()
  (let ((ac 0)) ;LET1-AC
    (values
     (symbol-macrolet ((a (incf ac))) ;CLHS on SYMBOL-MACROLET: "The expansion of a symbol macro is subject to further macro expansion in the same lexical environment as the symbol macro invocation".
       (let ((ac 5)) ;LET2-AC
	 a)) ;this expands to (INCF AC), where AC refers to LET2-AC.
     ac))) ;returns (VALUES 6 0)

(defun a1 ()
  (let ((ac 0))
    (symbol-macrolet ((a (incf ac)))
      (flet ((hello (a b) ;the symbol macro A is shadowed by the FLET lambda list.
	     (+ a b)))
	(values (hello 1 2) ac))))) ;returns (VALUES 3 0)

(defun a2 ()
  (let ((ac 0))
    (symbol-macrolet ((a (incf ac)))
      (let ((bc 0))
	b ;this uses the global (macro) symbol B (modifying the lexical BC).
	(values a ac b bc)))))

(defun a3 ()
  (let ((ac 0))
    (values
     (symbol-macrolet ((a (incf ac)))
       (tagbody ;the symbol macro A is shadowed by TAGBODY tags
	  (go a)
	a))
     ac)))

(defun a4 ()
  (let ((ac 0))
    (values
     (symbol-macrolet ((a (incf ac)))
       (block a ;the block name A is not a symbol, but a BLOCK name, so it is independent of the symbol macro A.
	 (return-from a a))) ;the second A is the symbol macro.
     ac)))

(defun a5 ()
  (let ((ac 0))
    (values
     (symbol-macrolet ((a (incf ac)))
       ;; SBCL warns that using an INTEGER as a catch-throw tag tends to be unportable because tags are compared using EQ.
       (catch a ;this is the symbol macro A.
	 (throw ac 1)))
     ac)))

;; Error that "The variable A is unbound" since in (LOAD-TIME-VALUE A), A is evaluated in a null lexical environment, and thus A is undefined.
;; (defun error2 ()
;;   (let ((ac 0))
;;     (values
;;      (symbol-macrolet ((a (incf ac)))
;;        (load-time-value a))
;;      ac)))

(defun a6 ()
  (let ((ac 0))
    (values
     (symbol-macrolet ((a (incf ac)))
       (list
	(symbol-macrolet ((a 55))
	  a)
	a))
     ac))) ;returns (VALUES (55 1) 1)

(defun a7 ()
  (let ((a 0)
        (b 0))
    (symbol-macrolet ((a b))
      (tagbody
         (go a)
       a ;tags are in a different namespace than variables and symbol macros.
         (incf a) ;this is the symbol macro A, and thus replaced by B.
       b
         (incf b)))
    (values a b))) ;returns (VALUES 0 2)

(defun a8 ()
  (let ((ac 0))
    (symbol-macrolet ((a (incf ac)))
      (tagbody
	 (go a)
       a)) ;this is a tag, not the symbol macro A. CLHS on TAGBODY: "The determination of which elements of the body are tags and which are statements is made prior to any macro expansion of that element."
    ac)) ;returns 0

;; (defun error3 ()
;;   (let ((ac 0))
;;     (macrolet ((a () 'a))
;;       (tagbody
;; 	 (go a)
;;        (a))) ;this is a statement, not a tag, and thus expands to variable A, which is not defined. CLHS on TAGBODY: "The determination of which elements of the body are tags and which are statements is made prior to any macro expansion of that element. If a statement is a macro form and its macro expansion is an atom, that atom is treated as a statement, not a tag."
;;     ac))

(defun a9 ()
  (let ((ac 0))
    (values
     (symbol-macrolet ((a (incf ac)))
       (macrolet ((a () 'a))
	 (tagbody
	    (a)))) ;this is expanded to A, then interpreted not as a tag, but a symbol, and thus expanded to (INCF AC), and TAGBODY always returns NIL.
     ac))) ;returns (VALUES NIL 1)

(defun a10 ()
  (symbol-macrolet ((a (b)))
    (labels ((b () 1)
	     (a ()
	       (b) ;this is a function call and thus is not affected by the symbol macro A (so execution doesn't loop forever).
	       a)) ;this is expanded to (B) and thus calls #'B.
      (a))))

(defun b1 ()
  (let ((ac 0) (bc 0))
    ;; on SBCL, the following forms results in "undefined variable: LOCAL-B", but running B1 twice (and skipping the error that occurs on the first run) means that LOCAL-B is defined as a global symbol macro.
    ;; on CLISP, compiles (defining a global symbol macro) and doesn't error on any run.
    (define-symbol-macro local-b (incf bc))
    (list
     (symbol-macrolet ((a (incf ac)))
       local-b
       (list a ac local-b bc))
     (list local-b bc))))

(defun b1-2 ()
  (let ((bc 0))
    (values local-b bc))) ;uses the global (macro) symbol LOCAL-B.

(defun b1-3 ()
  (values
   (let ((local-b 0)) ;a possibly global (macro) symbol LOCAL-B is shadowed by LET.
     local-b
     local-b
     local-b)
   (flet ((hello (local-b) ;a possibly global (macro) symbol LOCAL-B is shadowed by the lambda list of an FLET binding.
	    local-b
	    local-b
	    local-b))
     (hello 0))
   ((lambda (local-b) ;a possibly global (macro) symbol LOCAL-B is shadowed by the lambda list of a LAMBDA.
      local-b
      local-b
      local-b) 0)))

(defun c1 ()
  (values
   (symbol-macrolet ((a :a)
		     (b a))
     b) ;since an application of a symbol macro is simply replaced by its expansion form, and then subject to further macro expansion, the order of bindings in SYMBOL-MACROLET does not matter.
   (symbol-macrolet ((b a)
		     (a :a))
     b))) ;returns (VALUES :A :A)
