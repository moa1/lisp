(load "~/quicklisp/setup.lisp")
(ql:quickload 'cl-actors)
(use-package 'cl-actors)

;; cl-actors has the big disadvantage that there is no automatic garbage collection for unreachable actors. This means that running (test-actor 1000 10000) fails, due to not enough memory, but then also (test-actor 1 10000) fails because there is not enough memory anymore.

;; (defactor fact ((temp 1)) (n cust)
;;   (if (equal 1 n)
;;       (progn
;; 	(send cust (* temp 1))
;; 	(setf temp 1)
;; 	next)
;;       (progn
;; 	(setf temp (* n temp))
;; 	(send self (- n 1) cust)
;; 	next)))

;;(defparameter p (printer))
;;(defparameter f (fact))
;;;; doesn't work for some reason:
;;(send f 3 p)
;;;; doesn't print for some reason:
;;(send p "hello")

(defparameter *start* 0)
(defparameter *stop* 0)

(defactor ring-member (cw) (msg n)
  (if (> n 0)
      (send cw msg (1- n))
      (setf *stop* (float (/ (- (get-internal-real-time) *start*) internal-time-units-per-second))))
  next)

(defun curry (f &rest args)
  (lambda (&rest rem)
    (apply f (append rem args))))

(defun test-ring (m n)
  (let* ((f (ring-member :cw nil))
	 (cw f))
    (loop for i below m do
	 (let ((this (ring-member :cw cw)))
	   (setf cw this)))
    ;; (setf (behavior f) (behav () (msg n)
    ;; 			 (if (/= n 0)
    ;; 			     (send f msg (1- n)))))
    (setf (cl-actors::behavior f) (labels ((me (msg n &key self (next #'me next-supplied-p))
					     (if next-supplied-p
						 (setf next (curry next :self self)))
					     (if (> n 0)
						 (send cw msg (1- n)))
					     next))
				    #'me))
    (setf *start* (get-internal-real-time))
    (send cw nil n)))

;;;; testing on purasuchikku:
;;;; run (test-ring 1 10000), wait until CPU done, (print *stop*)
;; 2.232
;;;; run (test-ring 500 10000), wait until CPU done, (print *stop*)
;; 2.621

;;;; testing on Bobo:
;; CL-USER> (test-ring 1 10000)
;; CL-USER> *stop*
;; 1.718
;; CL-USER> (test-ring 1000 10000)
;; CL-USER> *stop*
;; 2.171
;; CL-USER> (test-ring 5000 10000)
;; CL-USER> *stop*
;; 7.47
;; CL-USER> (test-ring 1 10000)
;; CL-USER> *stop*
;; 8.417

(defactor spreader (parent (r1 nil)) (msg)
  (case (car msg)
    ((spread) (let ((i (cdr msg)))
		(if (> i 0)
		    (progn
		      (let ((child-1 (spreader :parent self))
			    (child-2 (spreader :parent self)))
			(send child-1 (cons 'spread (1- i)))
			(send child-2 (cons 'spread (1- i))))
		      next)
		    (send parent (cons 'receive 1)))))
    ((receive) (let ((i (cdr msg)))
		 (if (null r1)
		     (progn
		       (setf r1 i)
		       next)
		     (send parent (cons 'receive (+ r1 i))))))))

(defparameter *spreader-time* nil)

(defactor spreader-starter (start) (msg)
  (case (car msg)
    ((spread) (progn
		(setf start (get-internal-real-time))
		(let ((root (spreader :parent self)))
		  (send root (cons 'spread (cdr msg)))
		  next)))
    ((receive) (setf *spreader-time* (list (cdr msg)
					   (float (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))))

(defun spreader-test (i)
  (let ((sp (spreader-starter)))
    (send sp (cons 'spread i))))

;;;; testing on pc1400:
;; CL-USER> (spreader-test 7)
;; CL-USER> *spreader-time*
;; (128 0.957)
;; CL-USER> (spreader-test 8)
;; CL-USER> *spreader-time*
;; (256 1.931)
;; CL-USER> (spreader-test 9)
;; CL-USER> *spreader-time*
;; (512 3.888)

;;;; testing on Bobo:
;; CL-USER> (spreader-test 7)
;; CL-USER> *spreader-time*
;; (128 1.09)
;; CL-USER> (spreader-test 8)
;; CL-USER> *spreader-time*
;; (256 1.827)
;; CL-USER> (spreader-test 9)
;; CL-USER> *spreader-time*
;; (512 3.614)
;; CL-USER> (spreader-test 10)
;; CL-USER> *spreader-time*
;; (1024 7.272)
;; CL-USER> (spreader-test 11)
;; CL-USER> *spreader-time*
;; (2048 13.465)
;; CL-USER> (spreader-test 12)
;; CL-USER> *spreader-time*
;; (4096 28.98)
