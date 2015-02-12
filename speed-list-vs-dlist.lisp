(load "~/quicklisp/setup.lisp")
(ql:quickload :utils)
(ql:quickload :dlist2)
(use-package :utils)
(use-package :dlist2)

(defun make-l (n l)
  (loop for i below n collect (apply #'list (loop for i below (random l) collect i))))

(defun copy-list* (l)
  (copy-list l))

(defun test-list (l)
  (let ((c (copy-list* l)))
    (apply #'append c)))

(defun copy-dlist* (l)
  (mapcar #'dlist l))

(defun test-dlist (l)
  (let ((c (copy-dlist* l)))
    (apply #'dlist-nconc c)))

(defun time-list (n l)
  (let ((list (make-l n l)))
    (- (nth-value 1 (timesec (lambda () (test-list list))))
       (nth-value 1 (timesec (lambda () (copy-list* list)))))))

(defun time-dlist (n l)
  (let ((list (make-l n l)))
    (- (nth-value 1 (timesec (lambda () (test-dlist list))))
       (nth-value 1 (timesec (lambda () (copy-dlist* list)))))))

;; On purasuchikku, the point where time-dlist becomes faster is between (time-dlist 100 20) and (time-dlist 100 40). For shorter lists, test-list is faster.
