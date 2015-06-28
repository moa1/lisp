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
