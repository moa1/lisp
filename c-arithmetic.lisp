;;;; functions that try to emulate operations so that their results are equal to those from C.

(defun ash0 (i c)
  "What does #'ASH do? (particularly for negative I)"
  (declare (type integer i c))
  (concatenate 'string
	       (format nil "i:~A(~B)" i i)
	       (if (< c 0)
		   (format nil " >> c:~A" (- c))
		   (format nil " << c:~A" c))
	       (format nil " == ~A(~B)~%" (ash i c) (ash i c))
	       "[if I is negative, j=(-I+1), k=j<<c, put a minus before k, return. If I is positive, shift, return.]"))

;; Use ~/arithmetic/mult.c to display the result of multiplication and shift in C.

(defun c-shlr (i c)
  "Emulation of the shift in C: i << c (where i,c are signed integers).
Examples:
toni@eckplatz2:~/arithmetic$ ./shift 128 -1
as:-128(0b10000000) bs:  -1(0b11111111) cs: -64(0b11000000)
au: 128(0b10000000) bu:  -1(0b11111111) cu:  64(0b01000000)"
  (if (< i 0)
      (- (ash (- i) c))
      (ash i c)))

(defun logand0 (a b)
  "What does #'LOGAND do? (particularly for negative A or B)"
  (declare (type integer a b))
  (concatenate 'string
	       (format nil "a:~4A(~9B) b:~4A(~9B)" a a b b)
	       (format nil " == ~4A(~9B)~%" (logand a b) (logand a b))
	       "[if A is negative, write it as 2's complement. Likewise for B. Take the AND of the two numbers. (This means it works the same way as & in C.)"))

(defun c-and (a b)
  "Emulation of the and in C: a & b (where a,b are signed integers).
Examples:
toni@eckplatz2:~/arithmetic$ ./and -20 -123
as: -20(0b11101100) bs:-123(0b10000101) cs:-124(0b10000100)
au: 236(0b11101100) bu: 133(0b10000101) cu: 132(0b10000100)"
  (logand a b))
