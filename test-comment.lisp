;; The following idea already exists in Common Lisp: #+c(FORM) comments out (FORM). (This works because C is not present in *FEATURES*.) IDEA: implement a common lisp reader that comments out forms after it, like in #(let ((a ... )) ...). The "#" must comment out the following form.

#+c(error "this is commented out")

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
