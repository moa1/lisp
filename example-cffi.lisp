(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :cffi)

(defmacro prind (&rest args)
  "Print args"
  ;; TODO: modify the pretty print dispatch table so that it prints representations readable by #'READ. (especially modify the table so that printing a float respects *print-base*.)
  (let ((i (gensym "I")))
    `(let ((*print-pretty* t)
	   (*print-right-margin* most-positive-fixnum))
       ,@(loop for a in args collect
	      (if (eq a T)
		  `(format t "~%")
		  `(progn
		     (format t "~A:" ,(format nil "~A" a))
		     (dolist (,i (multiple-value-list ,a))
		       (prin1 ,i)
		       (princ " ")))))
       (format t "~%"))))

(defparameter *org-max-off-length* 10000 "The maximal number of instructions an organism may have.")
(defparameter *org-off-genes-end* -1 "The end marker in OFF-GENES")

(cffi:defcstruct (org :class org) ;"foreign org"
  (id :int32)
  (genes :int32 :count 10000) ;genes of the organism
  (code :pointer) ;compiled code
  (code-length :int32)
  (code-jmp-table :pointer :count 10000) ;table with pointers by program instruction index
  (ip0-label :pointer) ;label to IP 0
  (mrk-label-table :pointer :count 10000) ;table with label by marker index
  (jmp-length :int32) ;number of JMP instructions
  (jmp-addr-table :pointer :count 10000) ;table with address by jump index
  (jmp-to-mrk-table :int32 :count 10000) ;table with indices to MRK-LABEL-TABLE by jmp index
  (min-ins :int32) ;minimal instruction number
  (max-ins :int32) ;maximal instruction number
  (ip :int32)
  (wait :int32)
  (iters :int32) ;TODO: remove this because ITERS is stored in V2
  (x :int32)
  (y :int32)
  (energy :int32)
  (age :int32)
  (off-org (:pointer (:struct org)))
  (off-genes :int32 :count 10000) ;genes of next offspring
  (off-code :pointer) ;instructions of next offspring
  (off-length :int32)
  (as :int32)
  (bs :int32)
  (an :int32)
  (bn :int32)
  (genesx :int32) ;index into GENES, the remaining genes to be read
  )


(defclass lisporg ()
  (id
   genes ;genes of the organism
   code ;compiled code
   code-length
   code-jmp-table ;table with pointers by program instruction index
   ip0-label ;label to IP 0
   mrk-label-table ;table with label by marker index
   jmp-length ;number of JMP instructions
   jmp-addr-table ;table with address by jump index
   jmp-to-mrk-table ;table with indices to MRK-LABEL-TABLE by jmp index
   min-ins ;minimal instruction number
   max-ins ;maximal instruction number
   ip
   wait
   iters
   x
   y
   energy
   age
   off-org
   off-genes ;genes of next offspring
   off-code ;instructions of next offspring
   off-length
   as
   bs
   an
   bn
   genesx ;rest of the genes to be read
   ))

(defmethod print-object ((object lisporg) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id
		 genes ;genes of the organism
		 code ;compiled code
		 code-length
		 code-jmp-table ;table with pointers by program instruction index
		 ip0-label ;label to IP 0
		 mrk-label-table ;table with label by marker index
		 jmp-length ;number of JMP instructions
		 jmp-addr-table ;table with address by jump index
		 jmp-to-mrk-table ;table with indices to MRK-LABEL-TABLE by jmp index
		 min-ins ;minimal instruction number
		 max-ins ;maximal instruction number
		 ip
		 wait
		 iters
		 x
		 y
		 energy
		 age
		 off-org
		 off-genes ;genes of next offspring
		 off-code ;instructions of next offspring
		 off-length
		 as
		 bs
		 an
		 bn
		 genesx ;rest of the genes to be read
		 )
	object
      (format stream "id:~S genes:~S code:~S code-length:~S code-jmp-table:? ip0-label:~S mrk-label-table:? jmp-length:~S jmp-addr-table:? jmp-to-mrk-table:? min-ins:~S max-ins:~S ip:~S wait:~S iters:~S x:~S y:~S energy:~S age:~S off-org:? off-genes:~S off-code:~S off-length:~S as:~S bs:~S an:~S bn:~S genesx:~S"
	      id
	      genes ;genes of the organism
	      code ;compiled code
	      code-length
	      ;;code-jmp-table ;table with pointers by program instruction index	
	      ip0-label ;label to IP 0
	      ;;mrk-label-table ;table with label by marker index
	      jmp-length
	      ;;jmp-addr-table ;table with address by jump index
	      ;;jmp-to-mrk-table ;table with indices to MRK-LABEL-TABLE by jmp index
	      min-ins ;minimal instruction number
	      max-ins ;maximal instruction number
	      ip
	      wait
	      iters
	      x
	      y
	      energy
	      age
	      off-genes ;genes of next offspring
	      off-code ;instructions of next offspring
	      off-length
	      as
	      bs
	      an
	      bn
	      genesx))))

(defun copy-to-foreign-array (array-pointer array-type array-values &optional (end-marker *org-off-genes-end*))
  (declare (optimize (debug 3)))
  (if (typep array-values 'list)
      (do* ((rest array-values (cdr rest)) (v (car rest) (car rest)) (i 0 (1+ i)))
	   ((null rest) (setf (cffi:mem-aref array-pointer array-type i) end-marker))
	;;(prind i v)
	(setf (cffi:mem-aref array-pointer array-type i) v))
      (let ((array-length (array-dimension array-values 0)))
	(do* ((i 0 (1+ i)))
	     ((>= i array-length) (setf (cffi:mem-aref array-pointer array-type i) end-marker))
	  ;;(prind i (aref array-values i))
	  (setf (cffi:mem-aref array-pointer array-type i) (aref array-values i))))))
	 
(defun copy-from-foreign-array (array-pointer array-type array-length &optional (end-marker *org-off-genes-end*))
  (let ((array (make-array array-length :fill-pointer t)))
    (loop for i from 0 below array-length do
	 (let ((e (cffi:mem-aref array-pointer array-type i)))
	   ;;(prind i array-length e)
	   (if (equal e end-marker)
	       (progn
		 (setf (fill-pointer array) i)
		 (return-from nil))
	       (setf (aref array i) e))))
    array))

(defmethod cffi:translate-into-foreign-memory ((value list) (eql org) pointer)
  ;;(prind value)
  (do ((v value (cddr v))) ((null v))
    (cond
      ((eq (car v) 'genes)
       (let ((value (cadr v)))
	 (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'genes) :int32 value)))
      ((eq (car v) 'code-jmp-table)
       (let ((value (cadr v)))
	 (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'code-jmp-table) :pointer value (cffi:null-pointer))))
      ((eq (car v) 'mrk-label-table)
       (let ((value (cadr v)))
	 (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'mrk-label-table) :pointer value (cffi:null-pointer))))
      ((eq (car v) 'jmp-addr-table)
       (let ((value (cadr v)))
	 (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'jmp-addr-table) :pointer value (cffi:null-pointer))))
      ((eq (car v) 'jmp-to-mrk-table)
       (let ((value (cadr v)))
	 (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'jmp-to-mrk-table) :int32 value -1)))
      ((eq (car v) 'off-genes)
       (let ((value (cadr v)))
	 (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'off-genes) :int32 value 0)))
      (t
       (setf (cffi:foreign-slot-value pointer '(:struct org) (car v)) (cadr v))))))

(defmethod cffi:translate-into-foreign-memory ((value lisporg) (eql org) pointer)
  ;;(prind value)
  (loop for slot in '(id
		      genes ;genes of the organism
		      code ;compiled code
		      code-length
		      code-jmp-table ;table with pointers by program instruction index
		      ip0-label ;label to IP 0
		      mrk-label-table ;table with label by marker index
		      jmp-length ;number of JMP instructions
		      jmp-addr-table ;table with address by jump index
		      jmp-to-mrk-table ;table with indices to MRK-LABEL-TABLE by jmp index
		      min-ins ;minimal instruction number
		      max-ins ;maximal instruction number
		      ip
		      wait
		      iters
		      x
		      y
		      energy
		      age
		      off-org
		      off-genes ;genes of next offspring
		      off-code ;instructions of next offspring
		      off-length
		      as
		      bs
		      an
		      bn
		      genesx ;rest of the genes to be read
		      ) do
       (let ((slot-value (slot-value value slot)))
       (cond
	 ((eq slot 'genes)
	  (let ((value slot-value))
	    (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'genes) :int32 value 0)))
	 ((eq slot 'code-jmp-table)
	  (let ((value slot-value))
	    (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'code-jmp-table) :pointer value (cffi:null-pointer))))
	 ((eq slot 'mrk-label-table)
	  (let ((value slot-value))
	    (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'mrk-label-table) :pointer value (cffi:null-pointer))))
	 ((eq slot 'jmp-addr-table)
	  (let ((value slot-value))
	    (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'jmp-addr-table) :pointer value (cffi:null-pointer))))
	 ((eq slot 'jmp-to-mrk-table)
	  (let ((value slot-value))
	    (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'jmp-to-mrk-table) :int32 value -1)))
	 ((eq slot 'off-genes)
	  (let ((value slot-value))
	    (copy-to-foreign-array (cffi:foreign-slot-pointer pointer '(:struct org) 'off-genes) :int32 value 0)))
	 (t
	  (setf (cffi:foreign-slot-value pointer '(:struct org) slot) slot-value))))))

(defmethod cffi:translate-from-foreign (value (eql org))
  ;;(prind value)
  (let ((org (make-instance 'lisporg)))
    (loop for slot in '(id
			genes ;genes of the organism
			code ;compiled code
			code-length
			code-jmp-table ;table with pointers by program instruction index
			ip0-label ;label to IP 0
			mrk-label-table ;table with label by marker index
			jmp-length ;number of JMP instructions
			jmp-addr-table ;table with address by jump index
			jmp-to-mrk-table ;table with indices to MRK-LABEL-TABLE by jmp index
			min-ins ;minimal instruction number
			max-ins ;maximal instruction number
			ip
			wait
			iters
			x
			y
			energy
			age	
			off-org
			off-genes ;genes of next offspring
			off-code ;instructions of next offspring
			off-length
			as
			bs
			an
			bn
			genesx ;rest of the genes to be read
			) do
	 ;;(prind slot)
	 (cond
	   ((eq slot 'genes)
	    (let ((array (cffi:foreign-slot-value value '(:struct org) 'genes)))
	      (setf (slot-value org 'genes) (copy-from-foreign-array array :int32 *org-max-off-length*))))
	   ((eq slot 'code-jmp-table)
	    (let ((array (cffi:foreign-slot-value value '(:struct org) 'code-jmp-table)))
	      (setf (slot-value org 'code-jmp-table) (copy-from-foreign-array array :pointer *org-max-off-length* (cffi:null-pointer)))))
	   ((eq slot 'mrk-label-table)
	    (let ((array (cffi:foreign-slot-value value '(:struct org) 'mrk-label-table)))
	      (setf (slot-value org 'mrk-label-table) (copy-from-foreign-array array :pointer *org-max-off-length* (cffi:null-pointer)))))
	   ((eq slot 'jmp-addr-table)
	    (let ((array (cffi:foreign-slot-value value '(:struct org) 'jmp-addr-table)))
	      (setf (slot-value org 'jmp-addr-table) (copy-from-foreign-array array :pointer *org-max-off-length* (cffi:null-pointer)))))
	   ((eq slot 'jmp-to-mrk-table)
	    (let ((array (cffi:foreign-slot-value value '(:struct org) 'jmp-to-mrk-table)))
	      (setf (slot-value org 'jmp-to-mrk-table) (copy-from-foreign-array array :int32 *org-max-off-length* -1))))
	   ((eq slot 'off-genes)
	    (let ((array (cffi:foreign-slot-value value '(:struct org) 'off-genes)))
	      (setf (slot-value org 'off-genes) (copy-from-foreign-array array :int32 *org-max-off-length* 0))))
	   (t
	    ;;(prind slot (cffi:foreign-slot-value value '(:struct org) slot))
	    (setf (slot-value org slot) (cffi:foreign-slot-value value '(:struct org) slot)))))
    org))

(defun test-alloc-and-free-convert-to-foreign ()
  (declare (optimize (debug 3)))
  (labels ((alloc-orgap ()
	     (multiple-value-bind (org org-alloc-params)
		 (cffi:convert-to-foreign `(ID -1 GENES #()
					       ;;CODE ,(cffi:null-pointer)
					       CODE-LENGTH 0 code-jmp-table #() ip0-label ,(cffi:null-pointer) mrk-label-table #() jmp-length 0 jmp-addr-table #() jmp-to-mrk-table #() min-ins 0 max-ins 0 IP 0 WAIT 0 X -1 Y -1 ENERGY -1 AGE -1 OFF-ORG ,(cffi:null-pointer) OFF-CODE ,(cffi:null-pointer) OFF-GENES #() OFF-LENGTH 0 AS 0 BS 0 AN 0 BN 0 GENESX 0) '(:struct org))
	       (cons org org-alloc-params)))
	   (free-orgap (orgap)
	     (cffi:free-converted-object (car orgap) '(:struct org) (cdr orgap))))
    (let ((l nil))
      (loop for i below 8000 do
	   (push (alloc-orgap) l))
      (loop for orgap in l do
	   (free-orgap orgap)))))


#|
(defun free-org (orgap)
  (declare (optimize (debug 3)))
  (let* ((off-org-alloc-params (orgap-off-org-alloc-params orgap))
	 (org-alloc-params (orgap-org-alloc-params orgap))
	 (org (orgap-org orgap))
	 (off-org (cffi:foreign-slot-value org '(:struct org) 'off-org)))
    (cffi:free-converted-object org '(:pointer (:struct org)) org-alloc-params)
    (cffi:free-converted-object off-org '(:pointer (:struct org)) off-org-alloc-params)))
|#
