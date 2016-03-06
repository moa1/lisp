;;;; ROUND-ROBIN

(defstruct round-robin
  (vector #() :type vector) ;the elements of the round-robin list, must be one longer than the size. Otherwise an empty round-robin cannot be distinguished from a completely full one.
  (front 0 :type fixnum) ;the index of the most-recently added element to the front
  (back 0 :type fixnum)) ;the index of the next element to be added to the back

(defun round-robin-length (rr)
  (with-slots (vector front back) rr
    (if (<= front back)
	(- back front)
	(+ (- (length vector) front) back))))

(defun round-robin-last (rr &optional (n 0))
  (declare (type round-robin rr)
	   (type (and unsigned-byte fixnum) n))
  "Get the Nth-back element of round-robin RR."
  (assert (and (<= 0 n) (< n (round-robin-length rr))) () "(ROUND-ROBIN-LENGTH RR) is ~S, but N is ~S" (round-robin-length rr) n)
  (let* ((vector (round-robin-vector rr))
	 (length (length vector))
	 (front (round-robin-front rr))
	 (back (round-robin-back rr))
	 (i (cond ((<= front back) (- back 1 n))
		  ((< n back) (- back 1 n))
		  (t (- length 1 n (- back))))))
    (aref vector i)))

;; TODO: add (SETF (ROUND-ROBIN-LAST RR) X)

;; TODO: add (ROUND-ROBIN-FIRST RR &OPTIONAL (N 0)), which does the same as (ROUND-ROBIN-LAST RR), but from the front.

(defun round-robin-push-back (rr elt)
  (declare (optimize (debug 3)))
  (let* ((vector (round-robin-vector rr))
	 (length (length vector))
	 (front (round-robin-front rr))
	 (back (round-robin-back rr)))
    (setf (aref vector
		(prog1 back
		  (setf back (mod (1+ back) length))
		  (when (= back front)
		    (setf (round-robin-front rr) (mod (1+ front) length)))
		  (setf (round-robin-back rr) back)))
	  elt))
  rr)

;; TODO: add (ROUND-ROBIN-PUSH-FRONT RR ELT), which pushes to the front, i.e. decrements FRONT by 1.

;; TODO: add #'(ROUND-ROBIN-POP-BACK RR) and (ROUND-ROBIN-POP-FRONT RR), which pops off from the back, i.e. decrements BACK by 1, or from the front, i.e. increments FRONT by 1.

(defun make-new-round-robin (size &key (element-type t) (initial-element nil initial-element-p) (initial-contents nil initial-contents-p))
  (assert (not (and (eq t initial-element-p) (eq t initial-contents-p))) () "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS")
  (cond
    ((eq t initial-element-p)
     (make-round-robin :vector (make-array (1+ size) :element-type element-type :initial-element initial-element :adjustable nil :fill-pointer nil)
		       :front 0
		       :back 0))
    ((eq t initial-contents-p)
     (assert (<= (length initial-contents) size) () "Length of INITIAL-CONTENTS must be smaller than SIZE")
     (let ((v (make-array (1+ size) :element-type element-type :adjustable nil :fill-pointer nil)))
       (let ((back (do ((i 0 (1+ i)) (c initial-contents (cdr c))) ((null c) i)
		     (setf (aref v i) (car c)))))
	 (make-round-robin :vector v
			   :front 0
			   :back back))))
    (t
     (make-round-robin :vector (make-array (1+ size) :element-type element-type :adjustable nil :fill-pointer nil)
		       :front 0
		       :back 0))))

(let ((rr (make-new-round-robin 3)))
  (round-robin-push-back rr 0)
  (assert (= 0 (round-robin-last rr)))
  (assert (= 1 (round-robin-length rr)))
  (round-robin-push-back rr 1)
  (assert (= 1 (round-robin-last rr)))
  (assert (= 2 (round-robin-length rr)))
  (round-robin-push-back rr 2)
  (assert (= 2 (round-robin-last rr)))
  (assert (= 3 (round-robin-length rr)))
  (round-robin-push-back rr 3)
  (assert (= 3 (round-robin-last rr)))
  (assert (= 3 (round-robin-length rr)))
  (round-robin-push-back rr 4)
  (assert (= 4 (round-robin-last rr)))
  (assert (= 3 (round-robin-length rr)))
  (assert (equal '(4 3 2) (loop for n below 3 collect (round-robin-last rr n)))))

(defmethod print-object ((rr round-robin) stream)
  (print-unreadable-object (rr stream :type t :identity t)
    (let* ((length (round-robin-length rr)))
      (when (> length 0)
	(format stream "~S" (round-robin-last rr (1- length)))
	(do ((i (- length 2) (1- i))) ((< i 0))
	  (format stream " ~S" (round-robin-last rr i)))))))
