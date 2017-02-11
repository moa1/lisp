
(defun test-i (aa)
  (declare (optimize (speed 3) (safety 0)))
  (iterate (for i in-vector aa) (sum i)))

(defun test-l (aa)
  (declare (optimize (speed 3) (safety 0)))
  (loop for i across aa sum i))

;;(macroexpand '(iterate (for i in-vector aa) (sum i))
;; (LET* ((#:SEQUENCE346 NIL)
;;        (#:LIMIT347 NIL)
;;        (I NIL)
;;        (#:INDEX345 NIL)
;;        (#:RESULT344 0))
;;   (BLOCK NIL
;;     (TAGBODY
;;       (PROGN
;;        (SETQ #:SEQUENCE346 AA)
;;        (SETQ #:LIMIT347 (LENGTH #:SEQUENCE346))
;;        (SETQ #:INDEX345 -1))
;;      LOOP-TOP-NIL
;;       (PROGN
;;        (SETQ #:INDEX345 (+ #:INDEX345 1))
;;        (IF (>= #:INDEX345 #:LIMIT347) (GO LOOP-END-NIL))
;;        (SETQ I (AREF #:SEQUENCE346 #:INDEX345))
;;        (SETQ #:RESULT344 (+ #:RESULT344 I)))
;;       (PROGN)
;;       (GO LOOP-TOP-NIL)
;;      LOOP-END-NIL
;;       (PROGN))
;;     #:RESULT344))

(defun test-e (aa)
  (declare (optimize (speed 3) (safety 0)))
  (LET* ((SEQUENCE330 aa)
	 (LIMIT331 (LENGTH SEQUENCE330))
	 (I 0)
	 (INDEX329 -1)
	 (RESULT328 0))
    (declare (type fixnum limit331 index329))
    (declare (type number result328 i))
;;                             (all with speed 3) (min cps) for (test-* aaad)
;;                                                (test-l) ;2440
;;                                                (test-l) ;2500 (safety 0)
;;                                                (test-i) ;1200
;;                                                (test-i) ;1630 (safety 0)
;;    (declare (type (SIMPLE-ARRAY number 1) sequence330)) ;6000
;;                                                         ;1600 (no decl)
;;    (declare (type (array number 1) sequence330))        ;600
;;    (declare (type (vector number *) sequence330))       ;600
;;    (declare (type vector sequence330))                  ;2300
;;    (declare (type (vector * *) sequence330))            ;2300
;;    (declare (type (vector t *) sequence330))            ;600
;;    (declare (type (vector number) sequence330))         ;600
    (declare (type vector sequence330))                  ;2440 (safety 0)
    (BLOCK NIL
      (TAGBODY
       LOOP-TOP-NIL
	 (PROGN
	   (SETQ INDEX329 (+ INDEX329 1))
	   (IF (>= INDEX329 LIMIT331) (GO LOOP-END-NIL))
	   (SETQ I (AREF SEQUENCE330 INDEX329))
	   (SETQ RESULT328 (+ RESULT328 I)))
	 (PROGN)
	 (GO LOOP-TOP-NIL)
       LOOP-END-NIL
	 (PROGN))
      RESULT328)))


(defparameter aa (make-array '(10000)))
(defparameter aaad (make-array '(10000) :adjustable t))
(defparameter aas (make-string 10000)) ;same error for test-l and test-e
(defparameter aan (make-array 10000)) ;same error for test-l and test-e
(setf (elt aan 9990) "X")


(defun measure ()
  (dolist (f '(test-i test-e test-l))
    (let* ((cps (multiple-value-list 
		 (timecps (10 :stats t :time 1) (funcall f aaad))))
	   (mincps (cadr cps)))
      (prind f mincps cps))))
