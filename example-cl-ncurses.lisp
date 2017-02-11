;; This doesn't seem to work: there is an "undefined alien function" error.

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ncurses)


(load "/home/toni/quicklisp/dists/quicklisp/software/cl-ncurses_0.1.4/tests/scroll-test.lisp")

;;(setf cl-ncurses::*max* 1000000)

(cl-ncurses::scroll-test)
