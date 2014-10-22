(asdf:oos 'asdf:load-op 'utils)

;;(defstruct
;;   
;;
;;(defun make-sic (l)
;;  (loop repeat l collect (gensym)))
;;
;;(defmacro sic ()
;;go

(defun rssb (mem steps)
  "See Wikipedia article 'One instruction set computer', paragraph 'Reverse subtract and skip if borrow' (rssb). MEM must be a simple array that contains the memory values, steps is the number of steps that are to be executed.
Note that the instruction pointer is memory-mapped to memory position 0, and the accumulator is memory-mapped to memory position 1."
  (let ((n (array-dimension mem 0)))
    (symbol-macrolet ((ip (svref mem 0))
		      (acc (svref mem 1)))
      (flet ((rssb ()
	       (let* ((m (svref mem ip))
		      (v (svref mem m))
		      (d (- v acc)))
		 (print mem)
		 (print (list "ip" ip "m" m "v" v "d" d))
		 (when (< d 0)
		   (incf ip 1))
		 (setf (svref mem m) d)
		 (setf acc d))))
	(dotimes (step steps)
	  (rssb)
	  (incf ip 1))
	(setf (svref mem 0) ip)
	(setf (svref mem 1) acc)
	mem))))

(defun rssb-wikipedia-example (&key (y-val 2) (z-val 1))
  (let ((temp 31)
	(x 30)
	(y 29)
	(z 28))
    (let* ((prog (list
		  0 0
		  temp temp temp
		  x x
		  y
		  temp temp x
		  temp temp temp
		  z x))
	   (prog-1 (append prog (loop for i below (- 32 (length prog)) collect 0)))
	   (mem (make-array 32 :element-type 'integer :initial-contents prog-1)))
      (setf (svref mem y) y-val)
      (setf (svref mem z) z-val)
      (setf (svref mem temp) 15)
      (setf (svref mem x) 7)
      (rssb mem 15)
      (svref mem x))))
