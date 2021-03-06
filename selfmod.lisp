(asdf:oos 'asdf:load-op :utils)
(use-package :utils)

(defparameter size (expt 2 8))

(defparameter w1 (make-array size :element-type 'fixnum))
(defparameter w2 (make-array size :element-type 'fixnum))

(defparameter worlds (cons w1 w2))

(defun sic-signed-masker (length)
  (declare (type fixnum length)
	   (optimize (speed 3) (safety 0)))
  (let* ((length/2 (floor length 2))
	 (maxval (1- (floor length 2)))
	 (minval (- (floor length 2))))
    (lambda (n)
      (declare (type fixnum n))
;;       (format t "~A~%" n)
;;       (format t "~A ~A ~A ~A res:~A~%"
;; 	      length/2
;; 	      (+ n length/2)
;; 	      length
;; 	      (mod (+ n length/2) length)
;; 	      (- (mod (+ n length/2) length) length/2))
      (if (or (> n maxval) (< n minval))
	  (let ((res (- (mod (+ n length/2) length) length/2)))
	    (the fixnum res))
	  n))))

(defun sic-signed (n sics ip)
  (declare (type fixnum n ip)
	   (type (simple-array fixnum *) sics))
  (let ((sic-mask (sic-signed-masker (length sics))))
    (do* ((i 0 (1+ i))
	  (len (length sics)))
	 ((>= i n))
      (let* ((a_ ip)
	     (b_ (mod (1+ ip) len))
	     (c_ (mod (+ 2 ip) len))
	     (a (elt sics a_))
	     (b (elt sics b_))
	     (c (elt sics c_))
	     (r_ (- a b))
	     (r (funcall sic-mask r_)))
	;;(format t "~A elt:~A a_:~A r:~A~%" n (elt sics a_) a_ r)
	(setf (elt sics (+ a (floor len 2))) r)
	(if (< a b)
	    (setf ip (funcall sic-mask (+ c ip)))
	    (setf ip (funcall sic-mask (+ 3 ip))))))
    ip))

(defun sic-masker (length)
  (lambda (x) (mod x length)))

(defun sic (n sics ip &key verbose verbose-sics)
  (declare (type fixnum n ip)
	   (type (simple-array fixnum *) sics))
  (do* ((i 0 (1+ i))
	(len (length sics))
	(len/2 (floor len 2)))
       ((>= i n))
    (let* ((mask (sic-masker len))
	   (a_ (elt sics ip))
	   (b_ (elt sics (funcall mask (1+ ip))))
	   (c_ (elt sics (funcall mask (+ 2 ip))))
	   (a (elt sics a_))
	   (b (elt sics b_))
	   (c (elt sics c_))
	   (as (if (>= a len/2) (- a len) a))
	   (bs (if (>= b len/2) (- b len) b))
	   (rs (- as bs))
	   (r (funcall mask rs)))
      (when verbose (prind ip a_ b_ c_ a b c as rs r))
      (when verbose-sics (prind sics))
      (setf (elt sics a_) r)
      (if (< rs 0)
	  (setf ip c)
	  (setf ip (funcall mask (+ 3 ip))))))
  (values sics ip))


;; (setq myself '(+ 5 5))

;; (setq poss '(5 (+ 0 0) (- 0 0) 3))

;; (defun choose (poss)
;;   (nth (random (length poss)) poss))

;; (defun mutate (code)
;;   (if (< (random 1.0) 0.1)
;;       (if (< (random 2) 1)
;; 	  (choose poss))
;;       (if (consp code)
;; 	  (cons (mutate (car code)) (mutate (cdr code)))
;; 	  code)))

