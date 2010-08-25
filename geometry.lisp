(defmacro define-constant (name value &optional doc)
  (signal "use the define-constant macro of alexandria instead")
  `(defparameter ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; put these into utils.lisp

(defun compare (element list &key (test #'equal))
  "Compare ELEMENT with all elements in LIST using TEST. Return NIL when the
first comparison yields NIL, non-NIL otherwise."
  (if (consp list)
      (if (position-if (lambda (x) (funcall (complement test) element x)) list)
	  nil
	  t)
      t))

;; vector stuff

;; (defstruct (vector3d (:constructor make-vector3d (x y z)))
;;   (x 0 :type rational)
;;   (y 0 :type rational)
;;   (z 0 :type rational))

;;(defstruct (vector2d (:constructor make-vector2d (x y)))
;;  (x 0 :type rational)
;;  (y 0 :type rational))


;; vector3d      ---ct--- 98.44
;; vector3d-     -------- 93.44
;; vector3d-p    -f------ 91.77
;; vector3d-x    -f------ 91.77
;; vector3d-y    -f------ 91.77
;; vector3d-z    -f------ 91.77
;; copy-vector3d -f------ 80.41
;; make-vector3d -f------ 80.41

(deftype vector3d ()
  `(array rational (3)))

(deftype vector2d ()
  `(array rational (2)))

(defun make-vector3d (&optional (x 0) (y 0) (z 0))
  (make-array 3 :element-type 'rational :initial-contents (list x y z)))

(defun make-vector2d (&optional (x 0) (y 0))
  (make-array 2 :element-type 'rational :initial-contents (list x y)))

(defstruct (line (:constructor make-line (origin direction)))
  origin
  direction)

(defun assert-line (line)
  (let* ((dir (line-direction line))
	 (dirdim (length dir)))
    (assert (= dirdim (length (line-origin line))))
    (assert (loop for i below dirdim thereis (/= 0 (aref dir i))))))

(defstruct (plane (:constructor make-plane (origin normal)))
  (origin nil :type vector3d)
  (normal nil :type vector3d))

(defun assert-plane (plane)
  (assert (/= 0 (vector3d-scalar (plane-normal plane) (plane-normal plane)))))

(define-constant origin3d (make-vector3d 0 0 0))
(define-constant unit3d-x (make-vector3d 1 0 0))
(define-constant unit3d-y (make-vector3d 0 1 0))
(define-constant unit3d-z (make-vector3d 0 0 1))

(define-constant origin2d (make-vector2d 0 0))
(define-constant unit2d-x (make-vector2d 1 0))
(define-constant unit2d-y (make-vector2d 0 1))

(defmacro define-2d-and-3d (suffix parameters &body body)
  (let ((name2d (intern (concatenate 'string "VECTOR2D-" (string suffix))))
	(name3d (intern (concatenate 'string "VECTOR3D-" (string suffix)))))
    `(progn
       (defun ,name2d (,@parameters)
	 ,@body)
       (defun ,name3d (,@parameters)
	 ,@body))))

(define-2d-and-3d x (instance)
  (aref instance 0))

(define-2d-and-3d y (instance)
  (aref instance 1))

(defun vector3d-z (instance)
  (aref instance 2))

(defun arrays-equal-dimensions (&rest arrays)
  (if (consp arrays)
	(compare (array-dimensions (car arrays))
		 (cdr arrays)
		 :test (lambda (a b)
			 (equal a (array-dimensions b))))
	t))

(defun map-array (function &rest arrays)
  (assert (apply #'arrays-equal-dimensions arrays))
  (apply #'map 'vector function (car arrays) (cdr arrays)))

(defmacro define-map-array (suffix parameters function)
  `(define-2d-and-3d ,suffix (,@parameters &rest vectors) 
     (apply #'map-array ,function vectors)))

(define-map-array add nil #'+)

(define-map-array sub nil #'-)

(define-map-array invert nil #'-)

(define-map-array scale (scale) (lambda (x) (* scale x)))

(define-2d-and-3d scalar (a b)
  (reduce #'+ (map 'list #'* a b)))

(define-2d-and-3d colinear-scale (a b)
  (assert (arrays-equal-dimensions a b))
  (let (factor)
    (loop for i below (length a)
       do
	 (let ((val-a (aref a i))
	       (val-b (aref b i)))
	     (if (= val-b 0)
		 (if (not (= val-a 0))
		     (progn
		       (setf factor nil)
		       (return nil)))
		 (if (null factor)
		     (setf factor (/ val-a val-b))
		     (if (/= factor (/ val-a val-b))
			 (progn 
			   (setf factor nil)
			   (return nil)))))))
    factor))
  
(defun vector3d-cross (a b)
  (let ((u (vector3d-x a))
	(v (vector3d-y a))
	(w (vector3d-z a))
	(x (vector3d-x b))
	(y (vector3d-y b))
	(z (vector3d-z b)))
    (make-vector3d (- (* v z) (* w y))
		   (- (* w x) (* u z))
		   (- (* u y) (* v x)))))

(defun vector2d-perpendicular (a)
  "Rotates a 90 degrees counterclockwise in a right-hand coordinate system."
  (make-vector2d (vector2d-y a)
		 (- (vector2d-x a))))

(defun vector3d-intersect-line-plane (line plane)
  (assert-plane plane)
  (let* ((diff (vector3d-sub (plane-origin plane) (line-origin line)))
	 (line-fraction (vector3d-scalar diff (plane-normal plane)))
	 (intersection (vector3d-add (line-origin line)
				     (vector3d-scale line-fraction
						     (line-direction line)))))
    (values intersection line-fraction)))

(defun vector3d-perpendicular-any (a)
  (if (null (vector3d-colinear-scale a unit3d-x))
      (vector3d-cross a unit3d-x)
      (vector3d-cross a unit3d-y)))

;; up to here all functions from geometry_math.c line 193, where Matrices start
