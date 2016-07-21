;; This works
(let ((a 1))
  (defmacro m3g ()
    `(list ,(incf a) 4)))

(print (list (m3g) (m3g)))

;; I wonder how the following behaves on CLISP or ECL.
;; This fails on SBCL 1.1.13, saying that variable A is not defined. I think it should work, however, since CLHS MACROLET says "The macro-expansion functions defined by macrolet are defined in the lexical environment in which the macrolet form appears." and CLHS DEFMACRO says "The macro function is defined in the same lexical environment in which the defmacro form appears." Maybe that is why it does not work: (CLHS MACROLET) "Declarations and macrolet and symbol-macrolet definitions affect the local macro definitions in a macrolet, but the consequences are undefined if the local macro definitions reference any local variable or function bindings that are visible in that lexical environment." and SBCL 1.1.13 chooses to disallow access to lexically accessible variables.
(let ((a 1))
  (macrolet ((m3l ()
	       `(list ,(incf a) 4)))
    (print (m3l) (m3l))))
