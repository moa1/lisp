;;; tests for functions and macro functions.

;; CLHS on MACROLET says "The macro-expansion functions defined by macrolet are defined in the lexical environment in which the macrolet form appears." and doesn't say that the macro-expansion functions are defined in the definitions of MACROLET. Therefore in the following function, LASTGUY2 is not defined in the BODY of LASTGUY1.
;; (defun macrolet-error1 ()
;;   (macrolet ((lastguy2 (cons)
;; 	       (declare (ignore cons))
;; 	       :cons2)
;; 	     (lastguy1 (cons)
;; 	       (declare (ignore cons))
;; 	       (lastguy2 cons)))
;;     (let ((cons :cons))
;;       (lastguy1 cons))))

(defun macrolet2 ()
  (macrolet ((lastguy2 (cons)
	       (declare (ignore cons))
	       :cons2))
    (macrolet ((lastguy1 (cons)
		 (declare (ignore cons))
		 (lastguy2 cons)))
      (let ((cons :cons))
	(lastguy1 cons)))))
