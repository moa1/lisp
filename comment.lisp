;; TODO: IDEA: implement a common lisp reader that comments out forms after it, like in #(let ((a ... )) ...). The "#" must comment out the following form.


(defun bla1 ()
  1)

#|

asdfasdfasdfasdfasdfas
fdas
df
sadf
asdf
sadf

|#

(defun bla2 ()
  2)
