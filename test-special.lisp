(let ((a 1))
  (defun fun ()
    (format t "a:~A~%" a))) ;this is the lexically bound A, not the special one

(let ((a 1))
  (defun fun2 ()
    (declare (special a))
    (format t "a:~A~%" a))) ;this is the special bound A, not the lexical one

(defun test-special ()
  (progv '(a) '(2)
    (fun)
    (fun2)))

#|
CL-USER> (test-special)
a:1
a:2
NIL
|#
