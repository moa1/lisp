(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(use-package :alexandria)

;; The coordinate system we use here is that of computer graphics, i.e. (0 0) is the pixel in the top left corner, and (W H) is the bottom right corner of the screen with width W and height H.

(defun y-rational (y0 xw y1)
  "Given a line's start and end point, return the list of all y-coordinates of pixels which are below that line.
The line's start point is (0 Y0); its end point is ((+ 0 XW) Y1). Y0 must be a rational number in the interval (-1; 0], XW any positive integer in [1;Inf), Y1 must be a rational number in [Y0; XW].
Returns a sequence S of length (1+ XW) of integers. The integer (elt S X) is the y-coordinate of the lowest pixel that is at x-coordinate X and below the line.
Note: The above limits of Y0, XW, and Y1 should include all possible lines in the arc between the (horizontal) line (0 0) to (X 0) and the (steepest) diagonal (0 0) to (X X). (So that an algorithm that wants to draw the pixels below that line would need to scan all (integer) x coordinates between 0 and X and draw the correct y coordinate.) This also includes the lines with non-integer X0 and X1, because for those values you can prove that the lines with equivalent integer X0 and X1 exist and still fulfill above limits."
  (assert (and (< -1 y0) (<= y0 0)))
  (assert (and (>= xw 1) (integerp xw)))
  (assert (and (<= y0 y1) (<= y1 xw)))
  (let ((steepness (/ (- y1 y0) xw)))
    (loop for x upto xw collect
	 (ceiling (+ y0 (* steepness x))))))

(defun q16.16p (a)
  "Returns true if A is a Q16.16 fixed-point rational, false otherwise."
  ;; TODO: check that the integer part of A is representable as a signed 16-bit integer.
  (= (expt 2 16) (lcm (denominator a) (expt 2 16))))

(defun round-q16.16 (a)
  "Round real A to Q16.16 precision."
  (/ (round (* a (expt 2 16))) (expt 2 16)))

(defun floor-q16.16 (a)
  "Round to highest rational representable in Q16.16 precision not higher than real A.
Note that this is different from the division operator in C, which rounds always towards 0 (also for negative numbers)."
  (/ (floor (* a (expt 2 16))) (expt 2 16)))

;; Q16.16 fixed-point math:
;; (let ((x (/ 5063265 (expt 2 16)))
;;       (y (/ -6338360 (expt 2 16))))
;;   (/ x y)) should according to libfixmath google code issue "div" be -52352.
(defun /-q16.16 (x y)
  "For two Q16.16 fixed-point rationals, return the rounded result of (/x y) as Q16.16 rational."
  (round-q16.16 (/ x y)))
(assert (= (/ -52352 (expt 2 16)) (/-q16.16 (/ 5063265 (expt 2 16)) (/ -6338360 (expt 2 16)))))

(defun y-q16.16 (y0 xw y1)
  "Same as function Y-RATIONAL, except that Y0 and Y1 must be rationals representable as Q16.16-bit fractionals."
  (assert (and (< -1 y0) (<= y0 0) (q16.16p y0)))
  (assert (and (>= xw 1) (integerp xw)))
  (assert (and (<= y0 y1) (<= y1 xw) (q16.16p y1)))
  (let ((steepness (floor-q16.16 (/ (- y1 y0) xw))))
    (loop for x upto xw collect
	 (ceiling (+ y0 (* steepness x))))))

;; I now want to check if there are configurations where Y-RATIONAL gives a different result from Y-Q16.16 for the _same input_. This can happen when the STEEPNESS in both functions is sufficiently different. The higher XW, the more likely is that there is a configuration of the input that the result of both functions differs. Do I only need to check the case where the difference between the STEEPNESS values is maximal? The difference between the two functions will be that in function Y-RATIONAL, the value (+ y0 (* steepness x)) is barely above an integer value, and in function Y-Q16.16 it is barely below. I think I also need to take into account Y0, because although Y0 in (ceiling (+ y0 (* steepness x))) is the same for both functions (due to same input), the sum (+ y0 (* steepness x)) can be different (barely above and below an integer value in both functions, respectively), depending on Y0.
;; Indeed, there is already an example where the two functions have different results with just 3 pixels wide:
;; CL-USER> (y-rational (/ 0 65536) 2 (/ 1 65536))
;; (0 1 1)
;; CL-USER> (y-q16.16 (/ 0 65536) 2 (/ 1 65536))
;; (0 0 0)

;; Maybe i should use Q0.32 for STEEPNESS? But then the polygon scanning needs more instructions.


;;;; General line-scanners

(defmacro prind (&rest args)
  "Print args"
  (with-gensyms (i)
    `(progn
       (format t "~&")
       ,@(loop for a in args collect
	      (if (eq a T)
		  `(format t "~&")
		  `(progn
		     (format t "~A:" ,(format nil "~A" a))
		     (dolist (,i (multiple-value-list ,a))
		       (prin1 ,i)
		       (princ " "))))))))

(defun collect-line (line-function x1 y1 x2 y2)
  (let ((points nil))
    (funcall line-function x1 y1 x2 y2 :function (lambda (x y) (push (list x y) points)))
    (nreverse points)))

(defun line-rational (x1 y1 x2 y2 &key (function (lambda (x y) (print (list x y)))))
  "Draw a line from (X1,Y1) to (X2,Y2), where X2-X1 must be greater or equal to Y2-Y1.
Example: (line-rational 0 0 10 1)"
  ;; TODO: handle the case where (< (abs xdiff) (abs ydiff)) (by swapping x1,x2 with y1,y2 and calling FUNCTION with reverse X,Y.).
  ;; TODO: make (collect-rational 2 3 12 5) return the equal result as (reverse (collect-rational 12 5 2 3)), also see below.
  (declare (optimize (debug 3)))
  (let* ((x x1)
	 (y y1)
	 (xdiff (- x2 x1))
	 (ydiff (- y2 y1))
	 (d0 (abs ydiff))
	 (f0 0) ;TODO:I think this value is initialized wrong, because (collect-rational 2 3 12 5) != (reverse (collect-rational 12 5 2 3)).
	 (f1 (abs xdiff))
	 (dx (signum xdiff))
	 (dy (signum ydiff)))
    ;; the fraction F0/F1 is the implicit rational describing the y-coordinate of the X-coordinate we're at right now. It increases by YDIFF/XDIFF for every 1-increment of X we're making.
    (loop for i from 0 upto (1- (ceiling (abs xdiff))) do
       ;;(prind x y f0 f1)
	 (funcall function (floor x) (floor y))
	 (incf x dx)
	 (incf f0 d0)
	 (loop while (>= f0 f1) do ; if the rational F0/F1 is > 1, increment Y and decrease F0/F1.
	   (decf f0 f1)
	      (incf y dy)))
    (funcall function (floor x2) (floor y2))))

;; TODO: doesn't work yet...
;;   (loop for i below 1000 do
;;        (assert (let* ((x1 (- (random 201) 100))
;; 		      (y1 (- (random 201) 100))
;; 		      (x2 (- (random 201) 100))
;; 		      (y2 (- (random 201) 100))
;; 		      (a (collect-line #'line-rational x1 y1 x2 y2))
;; 		      (b (collect-line #'line-rational x2 y2 x1 y1)))
;; 		 (prind x1 y1 x2 y2)
;; 		 (and (equal a (reverse b))))))

(defun line-rational-subpixel (x1 y1 x2 y2 &key (function (lambda (x y) (print (list x y)))))
  "Draw a line from (X1,Y1) to (X2,Y2), where X2-X1 must be greater or equal to Y2-Y1. DOES NOT WORK YET.
Example: (line-rational-subpixel 0 0.9 10 2)"
  ;; TODO: make it possible to call (line-rational-subpixel (0 0 10 -10)).
  (declare (optimize (debug 3)))
  (let ((xdiff (- x2 x1))
	(ydiff (- y2 y1)))
    (multiple-value-bind (x1i x1f) (floor x1)
      (multiple-value-bind (y1i y1f) (floor y1)
	;; We want to start at an integer x-coordinate, so we need to start F0/F1 at a value which is equal to (* X1F Y1F). Later we need to increment F0/F1 by YDIFF/XDIFF.
	;;(prind x1f y1f xdiff ydiff)
	(let* ((x x1i) ;we want to paint the pixels whose left upper corner is in the area (in this case: line) to be drawn.
	       (y y1i)
	       (f0 (+ (* y1f xdiff) (* (- x1f) ydiff))) ;this might not be representable as float... :( for example, (floating:floating t (* 0.29 9.03) t t 16)==+2.9E631C is not equal to (floating:floating t (* 29/100 903/100) t t 16)==+2.9E632, which would be the exact value.
	       (f1 xdiff))
	  ;; the fraction F0/F1 is the implicit rational describing the y-coordinate of the X-coordinate we're at right now. It increases by YDIFF/XDIFF for every 1-increment of X we're making.
	  ;;(format t "f0:~A==~10,100/floating:floating/~%" (floating:floating-hex-c-readable f0) f0)
	  ;;(assert (= (float f0) (+ (* (float y1f) (float xdiff)) (* (- (float x1f)) (float ydiff)))))
	  (loop for i below (1+ xdiff) do
	     (prind x y f0 f1)
	       (funcall function x y)
	       (incf x)
	       (incf f0 ydiff)
	       (when (>= f0 f1) ; if the rational f0/f1 is > 1, increment y and decrease f0/f1.
		 (decf f0 f1)
		 (incf y))))))))

;; tests for line-rational-subpixel
(flet ((collect-rational (x1 y1 x2 y2)
	 (collect-line #'line-rational x1 y1 x2 y2))
       (collect-rational-subpixel (x1 y1 x2 y2)
	 (collect-line #'line-rational-subpixel x1 y1 x2 y2)))
  ;; the line (line-rational-subpixel 0.9 0.9 10.5 10.5) must be equal to (line-rational-subpixel 0.5 0.5 10.5 10.5) and equal to (line-rational 0 0 10 10).
  (assert (let ((a (collect-rational-subpixel 0.9 0.9 10.5 10.5))
		(b (collect-rational-subpixel 0.5 0.5 10.5 10.5))
		(c (collect-rational 0 0 10 10)))
	    (and (equal a b) (equal a c))))
  ;; the line (line-rational-subpixel 0.5 0.5 10.5 5.5) must be equal to (line-rational 0 0 10 5).
  (assert (equal (collect-rational-subpixel 0.5 0.5 10.5 5.5) (collect-rational 0 0 10 5)))
  ;; the line (line-rational-subpixel 0 0.1 10 1.1) must(or "should"? due to F0 not being representable by a float) be equal to (line-rational-subpixel 0 1/10 10 11/10).
  (assert (equal (collect-rational-subpixel 0 0.1 10 1.1) (collect-rational-subpixel 0 1/10 10 11/10)))
  ;; the line (line-rational-subpixel 0 0.9 10 1.9) must(or "should"? due to F0 not being representable by a float) be equal to (line-rational-subpixel 0 9/10 10 19/10).
  (assert (equal (collect-rational-subpixel 0 0.9 10 1.9) (collect-rational-subpixel 0 9/10 10 19/10)))
  ;; the lines must(or "should"? due to F0 not being representable by a float) be equal.
  (assert (equal (collect-rational-subpixel 0.5 0 2 3) (collect-rational-subpixel 0.75 0.5 2 3))))

;; construction of a case which gives different results between passing rationals to line-rational-subpixel and floats: note that the rational version gives "(1 1)" and the float version "(1 0)" for the second pixel.
(let* ((x1 79/100) (x1-f (float x1))
       (y1 58/100) (y1-f (float y1))
       (x2 132/100) (x2-f (float x2))
       (y2 164/100) (y2-f (float y2)))
  (line-rational-subpixel x1 y1 x2 y2)
  (line-rational-subpixel x1-f y1-f x2-f y2-f))

(defun line-bresenham (x1 y1 x2 y2 &key (function (lambda (x y) (print (list x y)))))
  "Doesn't work with subpixel coordinates.
And handles only the 1st quadrant."
  ;; This algorithm is taken from wikipedia's article called something like "line drawing (Bresenham)".
  ;; When doing (line-bresenham 2 3 12 5) you see that the first 3 pixels are at y=3, the next 5 at y=4, and the last 3 at y=5.
  (declare (type fixnum x1 y1 x2 y2))
  (let* ((dx (- x2 x1))
	 (dy (- y2 y1))
	 (d (- (* 2 dy) dx))
	 (y y1)
	 (dh (- (* 2 dy) (* 2 dx)))
	 (dl (* 2 dy)))
    (declare (type fixnum dx dy d y dh dl))
    (funcall function x1 y1)
    (loop for x fixnum from (1+ x1) upto x2 do
	 (if (> d 0)
	     (progn
	       (incf y)
	       (funcall function x y)
	       (incf d dh))
	     (progn
	       (funcall function x y)
	       (incf d dl))))))

(defun line-steepness-q1 (x1 y1 x2 y2 &key (function (lambda (x y) (print (list x y)))))
  "Step one pixel at a time and increment the other coordinate by the steepness.
Has the disadvantage that it is only an approximate solution because the steepness may not be representable as a float. (However, this function should be exact when given rationals.)
This function handles only the 1st quadrant: where X2 =/= X1 and |X2 - X1| >= |Y2 - Y1|."
  ;; The following is only valid for cases where X2 > X1: The problem of line drawing can be described as follows: shift a square of width 1 and height 1 (not rotated, but aligned with the coordinate axes) along the line, so that its middle is always on the line. All pixels, whose left upper corner is now covered by a shifted square should be drawn. In the case that a square covers more than one pixel, only the uppermost and leftmost pixel shall be drawn. The first pixel needs to be drawn separately: the pixel in which the point lies must be drawn.
  ;; If X2 < X1, then we must handle the last pixel (i.e. the one where the left upper corner of the square is at (X2, Y2)) specially.
  (multiple-value-bind (x1i x1f) (floor x1)
    (let* ((xdiff (- x2 x1))
	   (ydiff (- y2 y1))
	   (steepness (/ ydiff xdiff)))
      (declare (type rational steepness))
      ;;(prind xdiff ydiff steepness)
      (if (> xdiff 0)
	  (let ((y (+ y1 (* (- 1 x1f) steepness))))
	    (declare (type rational y))
	    (funcall function x1i (floor y1))
	    (loop for x fixnum from (1+ x1i) upto (floor x2) do
	       ;;(prind x y (float y))
		 (funcall function x (round y))
		 (incf y steepness)))
	  (let ((y (- y1 (* x1f steepness))))
	    (loop for x from x1i downto (1+ (floor x2)) do
	       ;;(prind x y (float y))
		 (funcall function x (round y))
		 (decf y steepness))
	    (funcall function (floor x2) (floor y2)))))))
		     
;; (apply #'line-steepness-q1 (mapcar #'rationalize '(0.5 0 10.5 -2)))

(defun line-steepness (x1 y1 x2 y2 &key (function (lambda (x y) (print (list x y)))))
  "Step one pixel at a time and increment the other coordinate by the steepness.
Has the disadvantage that it is only an approximate solution because the steepness may not be representable as a float. (However, this function should be exact when given rationals.)"
  ;; handle all the other 4 quadrants.
  (cond
    ((and (= x1 x2) (= y1 y2)) (funcall function (floor x1) (floor y1)))
    ((>= (abs (- x2 x1)) (abs (- y2 y1)))
     (line-steepness-q1 x1 y1 x2 y2 :function function))
    ((<= (abs (- x2 x1)) (abs (- y2 y1)))
     (line-steepness-q1 y1 x1 y2 x2 :function (lambda (x y) (funcall function y x))))))

;; fails for (x1 0) (y1 0) (x2 4) (y2 2): bresenham returns '((0 0) (1 0) (2 1) (3 1) (4 2)), and steepness returns '((0 0) (1 0) (2 1) (3 2) (4 2)), which I think is nicer, due to symmetry. (Is there a bug in bresenham?)
(loop for i below 1 do
     (let* ((x1 (rational (random 1)))
	    (y1 (rational (random 1)))
	    (x2 (+ x1 (random 10)))
	    (y2 (+ y1 (random (- x2 x1 -1))))
	    (c-b (collect-line #'line-bresenham x1 y1 x2 y2))
	    (c-s (collect-line #'line-steepness x1 y1 x2 y2)))
       (prind x1 y1 x2 y2)
       (prind c-b)
       (prind c-s)
       (assert (equal c-b c-s)
	       nil "x1:~S y1:~S x2:~S y2:~S" x1 y1 x2 y2)
       ))
