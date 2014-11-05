(load "~/quicklisp/setup.lisp")
(asdf:oos 'asdf:load-op 'utils)
(load "~/lisp/multitree.lisp")

(define-custom-hash-table-constructor make-lsxhash-equalp-hash-table
    :test equalp :hash-function lsxhash)

(defun rssb (mem steps)
  "See Wikipedia article 'One instruction set computer', paragraph 'Reverse subtract and skip if borrow' (rssb). MEM must be a simple array that contains the memory values, steps is the number of steps that are to be executed.
Note that the instruction pointer is memory-mapped to memory position 0, and the accumulator is memory-mapped to memory position 1.
This function doesn't handle programs that have or create writes outside the memory."
  (symbol-macrolet ((ip (svref mem 0))
		    (acc (svref mem 1)))
    (flet ((rssb ()
	     (let* ((m (svref mem ip))
		    (v (svref mem m))
		    (d (- v acc)))
;;	       (print mem)
;;	       (print (list "ip" ip "m" m "v" v "d" d))
	       (when (< d 0)
		 (incf ip 1))
	       (setf (svref mem m) d)
	       (setf acc d))))
      (dotimes (step steps)
	(rssb)
	(incf ip 1))
      mem)))

(defun rssb-mod (mem steps)
  "See Wikipedia article 'One instruction set computer', paragraph 'Reverse subtract and skip if borrow' (rssb). MEM must be a simple array that contains the memory values, steps is the number of steps that are to be executed.
Note that the instruction pointer is memory-mapped to memory position 0, and the accumulator is memory-mapped to memory position 1.
This function never lets the value of any memory position exceed the range [0;MEM size). It does this by taking the modulus of the value and the memory size and only storing the result. Note that the initial values of MEM may not exceed the range [0;MEM size)."
  ;; TODO: check that all initial values of MEM are inside [0;mem size).
  (let ((n (array-dimension mem 0)))
    (symbol-macrolet ((ip (svref mem 0))
		      (acc (svref mem 1)))
      (labels ((inc-ip ()
		 (let ((i ip))
		   (setf ip (mod (1+ i) n))))
	       (rssb ()
		 (let* ((m (svref mem ip))
			(v (svref mem m))
			(d (- v acc)))
;;		   (print mem)
;;		   (print (list "ip" ip "m" m "v" v "d" d))
		   (when (< d 0)
		     (inc-ip))
		   (let ((d (mod d n)))
		     (setf (svref mem m) d)
		     (setf acc d)))))
	(dotimes (step steps)
	  (rssb)
	  (inc-ip))
	mem))))

(defun rssb-mod-2 (mem steps)
  "See Wikipedia article 'One instruction set computer', paragraph 'Reverse subtract and skip if borrow' (rssb). MEM must be a simple array that contains the memory values, steps is the number of steps that are to be executed.
Note that the instruction pointer is memory-mapped to memory position 0, and the accumulator is memory-mapped to memory position 1.
This function handles writes(reads) outside the memory by taking the modulus of the memory addess and the memory size and then writing to(reading from) the resulting address."
  (let ((n (array-dimension mem 0)))
    (symbol-macrolet ((ip (svref mem 0))
		      (acc (svref mem 1)))
      (flet ((rssb ()
	       (let* ((m (mod (svref mem (mod ip n)) n))
		      (v (svref mem m))
		      (d (- v acc)))
		 ;;		   (print mem)
		 ;;		   (print (list "ip" ip "m" m "v" v "d" d))
		 (when (< d 0)
		   (incf ip))
		 (setf (svref mem m) d)
		 (setf acc d))))
	(dotimes (step steps)
	  (rssb)
	  (incf ip))
	mem))))

(defun rssb-2power (mem steps)
  "See Wikipedia article 'One instruction set computer', paragraph 'Reverse subtract and skip if borrow' (rssb). MEM must be a simple array that contains the memory values, steps is the number of steps that are to be executed.
Note that the instruction pointer is memory-mapped to memory position 0, and the accumulator is memory-mapped to memory position 1.
This function handles writes outside the memory by never letting the number in any memory position fall outside the range [0;MEM size). For this to work, the memory size must be a power of 2."
  ;; TODO: check that MEM size is a power of 2.
  ;; TODO: check that all initial values of MEM are inside [0;MEM size).
  (let ((n (1- (array-dimension mem 0))))
    (symbol-macrolet ((ip (svref mem 0))
		      (acc (svref mem 1)))
      (labels ((inc-ip ()
		 (let ((i ip))
		   (setf ip (logand (1+ i) n))))
	       (rssb ()
		 (let* ((m (svref mem ip))
			(v (svref mem m))
			(d (- v acc)))
;;		   (print mem)
;;		   (print (list "ip" ip "m" m "v" v "d" d))
		   (when (< d 0)
		     (inc-ip))
		   (let ((d (logand d n)))
		     (setf (svref mem m) d)
		     (setf acc d)))))
	(dotimes (step steps)
	  (rssb)
	  (inc-ip))
	mem))))

(defun rssb-2power-2 (mem steps)
  "See Wikipedia article 'One instruction set computer', paragraph 'Reverse subtract and skip if borrow' (rssb). MEM must be a simple array that contains the memory values, steps is the number of steps that are to be executed.
Note that the instruction pointer is memory-mapped to memory position 0, and the accumulator is memory-mapped to memory position 1.
This function handles writes(reads) outside the memory by taking the logical AND of the memory addess and the memory size and then writing to(reading from) the resulting address."
  ;; TODO: check that MEM size is a power of 2.
  (let ((n (1- (array-dimension mem 0))))
    (symbol-macrolet ((ip (svref mem 0))
		      (acc (svref mem 1)))
      (flet ((rssb ()
	       (let* ((m (logand (svref mem (logand ip n)) n))
		      (v (svref mem m))
		      (d (- v acc)))
;;		 (print mem)
;;		 (print (list "ip" ip "m" m "v" v "d" d))
		 (when (< d 0)
		   (incf ip))
		 (setf (svref mem m) d)
		 (setf acc d))))
	(dotimes (step steps)
	  (rssb)
	  (incf ip))
	mem))))

(defun rssb-wikipedia-example (&key (rssb-function #'rssb) (y-val 2) (z-val 1))
  "The return value should be equal to (- Y Z)."
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
      (funcall rssb-function mem 15)
      (svref mem x))))

;;CL-USER> (rssb-wikipedia-example :y-val 1 :z-val 2 :rssb-function #'rssb-2power)
;;31
;;CL-USER> (rssb-wikipedia-example :y-val 1 :z-val 2 :rssb-function #'rssb-2power-2)
;;-1
;;CL-USER> (rssb-wikipedia-example :y-val 1 :z-val 2 :rssb-function #'rssb-mod)
;;31
;;CL-USER> (rssb-wikipedia-example :y-val 1 :z-val 2 :rssb-function #'rssb-mod-2)
;;-1

(defun rssb-example-random-mem (mem-size rssb-function steps)
  (let ((mem (make-array mem-size :element-type 'integer :initial-element 0)))
    (loop for i below mem-size do
	 (setf (svref mem i) (random mem-size)))
    (funcall rssb-function mem steps)))

(defun rssb-compare-speeds (rssb-function-1 rssb-function-2 mem-size steps
			    &key (init-random nil))
  (let ((mem (make-array mem-size :element-type 'integer)))
    ;; let MEM be filled with all zeroes, so that the same code-paths are taken.
    (when init-random
      (loop for i below mem-size do (setf (svref mem i) (random mem-size))))
    (let ((mem-2 (copy-array mem)))
      ;; utils:timediff sucks b/c it doesn't do repeatable timing.
      ;;      (utils:timediff (funcall rssb-function-1 mem steps)
      ;;		      (funcall rssb-function-2 mem-2 steps)
      ;;		      :maxtime 2 :showtimes t))))
      (flet ((measure-rssb (rssb-function mem)
	       (let ((f (lambda () (funcall rssb-function mem steps))))
		 (multiple-value-list (timesec f)))))
	
	(append
	 (measure-rssb rssb-function-1 mem)
	 (measure-rssb rssb-function-2 mem-2))))))

;;(rssb-compare-speeds #'rssb-2power #'rssb-2power-2 32 10000)
;;(81/128000 6.328125d-4 128 33/64000 5.15625d-4 128)
;;(rssb-compare-speeds #'rssb-2power #'rssb-2power-2 32 100000 :init-random t)
;;(51/8000 0.006375d0 8 41/1000 0.041d0 2)
;;(rssb-compare-speeds #'rssb-mod #'rssb-mod-2 32 10000)
;;(79/64000 0.001234375d0 64 73/64000 0.001140625d0 64)
;;(rssb-compare-speeds #'rssb-mod #'rssb-mod-2 32 10000 :init-random t)
;;(43/32000 0.00134375d0 64 11/1600 0.006875d0 8)

(defun enumerate-mem (mem-size function)
  "Assume a memory size of MEM-SIZE and call FUNCTION with all possible memory assignments, one after the other. The passed memory assignment may not be modified by FUNCTION."
  ;; test this function with (enumerate-mem 3 #'print)
  (let ((mem (make-array mem-size :element-type 'integer)))
    (labels ((fill (fill-position fill-value)
;;	       (print (list "fill-position" fill-position "fill-value" fill-value))
	       (if (>= fill-position mem-size)
		   (funcall function mem) ;memory filled, so call.
		   (if (>= fill-value (1- mem-size))
		       (progn
			 (setf (svref mem fill-position) fill-value)
			 (fill (1+ fill-position) 0))
		       (progn
			 (setf (svref mem fill-position) fill-value)
			 (fill (1+ fill-position) 0)
			 (fill fill-position (1+ fill-value)))))))
      (fill 0 0))))

(defun systematic-mapping-to-hash-table (mem-size rssb-function)
  "Return a hash-table that contains all possible memory configurations and their 1-step result obtained after passing the configuratio to RSSB-FUNCTION."
  (let ((mem-cache (make-lsxhash-equalp-hash-table)))
    (labels ((fill (mem)
	       (let* ((mem-1 (funcall rssb-function (copy-array mem) 1))
		      (mem-0 (copy-array mem)))
		 (multiple-value-bind (value present) (gethash mem-0 mem-cache)
		   (declare (ignore value))
		   (if (not present)
		     (progn
		       (setf (gethash mem-0 mem-cache) mem-1)
		       (fill mem-1))
		     nil)))))
      (enumerate-mem mem-size #'fill)
      mem-cache)))

(defun rssb-memory-to-rssb-index (mem)
  "Convert memory configuration MEM to a single number."
  (let ((size (array-dimension mem 0))
	(rssb-index 0))
    (loop for i below size do
	 (incf rssb-index (* (svref mem i) (expt size i))))
    rssb-index))

(defun rssb-index-to-rssb-memory (rssb-index mem-size)
  "Convert the single number RSSB-INDEX, which is an rssb-index, to a memory configuration with MEM-SIZE being the number of elements of the memory configuration."
  (let ((mem (make-array mem-size)))
    (loop for i from (1- mem-size) downto 0
       for pow = (expt mem-size i) do
	 (let ((whole (floor (/ rssb-index pow)))
	       (rem (mod rssb-index pow)))
	   (setf (svref mem i) whole)
	   (setf rssb-index rem)))
    mem))

(defun systematic-mapping-to-array (mem-size rssb-function)
  "Return an array that contains all possible memory configurations and their 1-step result obtained after passing the configuratio to RSSB-FUNCTION."
  (let* ((possibilities (expt mem-size mem-size))
	 (mem-cache (make-array possibilities :initial-element -1)))
    (labels ((fill (mem-0-index mem-0)
	       (let* ((mem-1 (funcall rssb-function mem-0 1))
		      (mem-1-index (rssb-memory-to-rssb-index mem-1)))
		 ;; Note that (eq mem-0 mem-1)
		 (let ((value (svref mem-cache mem-0-index)))
		   (if (= -1 value)
		     (progn
		       (setf (svref mem-cache mem-0-index) mem-1-index)
		       (fill mem-1-index mem-1))
		     nil)))))
      (loop for index below possibilities do
	   (let ((mem-0 (rssb-index-to-rssb-memory index mem-size)))
	     (fill index mem-0)))
      mem-cache)))

;; (systematic-mapping-to-array 2 #'rssb-mod)
;; #(1 2 2 0)
;;
;; (let ((ht (systematic-mapping 2 #'rssb-mod)))
;;   (maphash (lambda (k v) (print (list "k" k "v" v))) ht)
;;   ht)
;; ("k" #(0 0) "v" #(1 0)) 
;; ("k" #(1 0) "v" #(0 1)) 
;; ("k" #(0 1) "v" #(0 1)) 
;; ("k" #(1 1) "v" #(0 0)) 
;; #<HASH-TABLE :TEST EQUALP :COUNT 4 {13B39A81}>
