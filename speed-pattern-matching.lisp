(load "~/quicklisp/setup.lisp")

(ql:quickload :optima)
(ql:quickload :metabang-bind)
(ql:quickload :utils)

(defstruct str
  a b c)

(defun bind-optima1 (s)
  (optima:match s ((str a b c) (list a b c))))

(defun bind-optima2 (s)
  (optima:match s ((str- a b c) (list a b c))))

(defun bind-metabang (s)
  (metabang.bind:bind (((:structure str- a b c)
			s))
    (list a b c)))

(defun speed-optima1-vs-optima2 ()
  (let ((s (make-str :a 1 :b 2 :c 3)))
    (utils:timediff (bind-optima1 s) (bind-optima2 s) :maxtime 2 :showtimes t)))

(defun speed-optima1-vs-metabang ()
  (let ((s (make-str :a 1 :b 2 :c 3)))
    (utils:timediff (bind-optima1 s) (bind-metabang s) :maxtime 2 :showtimes t)))

;;;; recursive patterns
;; not possible:
;; (metabang.bind:bind (((a b (c d))
;; 		         (list 1 2 (3 4))))
;;   (+ a b c d))
;; works:
;; (optima:match (list 1 2 (list 3 4))
;;   ((list a b (list c d)) (+ a b c d)))
;; also works:
;; (optima:match (make-str :a 1 :b 2 :c (list 3 4))
;;   ((str a b (c (list c d))) (+ a b c d)))
;; so recursive matching should be possible wherever the BNF-form says PATTERN* in the optima README.md.
;;;; multiple possible patterns:
;; not possible in metabang.bind.
;; (optima:match (make-str :a 1 :b 2 :c 3)
;;   ((str a b (c (list c d))) (+ a b c d))
;;   ((str a b c) (+ a b c)))

;; apparently the patterns for the 3 slots a,b,c here go like this (SLOT-NAME PATTERN*) == (SLOT-NAME variable-pattern) == (SLOT-NAME variable SYMBOL). See "slot ::=" under section "#### STRUCTURE" and "variable-pattern" under section "Pattern Language" in optima's README.md.
(defun multiple-patterns-1 (s)
  (optima:match s
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 0 c))
     (list a b c))))

(defun multiple-patterns-2 (s)
  (optima:ematch s
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 1 c))
     (list a b c))
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 0 c))
     (list a b c))))

(defun multiple-patterns-4 (s)
  (optima:ematch s
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 3 c))
     (list a b c))
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 2 c))
     (list a b c))
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 1 c))
     (list a b c))
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 0 c))
     (list a b c))))

(defun time-multiple-patterns ()
  (let ((s (make-str :a '(1 2 (3 4) 5 6 (7)) :b '((7) :x :y (3 4) 2 1) :c 0)))
    (values (float (utils:timesec (lambda () (multiple-patterns-1 s))))
	    (float (utils:timesec (lambda () (multiple-patterns-2 s))))
	    (float (utils:timesec (lambda () (multiple-patterns-4 s)))))))

(defun multiple-patterns-2-2 (s)
  (optima:match s
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 1 c))
     (list a b c)))
  (optima:ematch s
    ((str (a '(1 2 (3 4) 5 6 (7)) a) (b '((7) :x :y (3 4) 2 1) b) (c 0 c))
     (list a b c))))

(defun time-multiple-patterns-2 ()
  (let ((s (make-str :a '(1 2 (3 4) 5 6 (7)) :b '((7) :x :y (3 4) 2 1) :c 0)))
    (values (float (utils:timesec (lambda () (multiple-patterns-2 s))))
	    (float (utils:timesec (lambda () (multiple-patterns-2-2 s)))))))
