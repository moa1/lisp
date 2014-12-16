(defun test (x y)
  (+ x y))

(define-compiler-macro test (&whole form x y &environment env)
  (print (list "form" form "x" x "y" y "env" env))
  `(1+ ,y))
;  form)

(defun bla (x)
  (print (test 1 x))
  (let ((y 3))
    (print (test x y))))

(bla 5)
