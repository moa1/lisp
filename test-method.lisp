(declaim (optimize (debug 3)))

(defclass mother ()
  ())

(defclass father ()
  ())

(defclass child (mother father)
  ())

(defclass grandchild (child)
  ())

;;;; SPECIFICITY

(defmethod fun ((a mother))
  (format t "mother~%"))

(defmethod fun ((a father))
  (format t "father~%"))

(defmethod fun ((a child))
  (format t "child~%"))

(defmethod fun ((a grandchild))
  (format t "grandchild~%"))

(defmethod fun (a)
  (format t "fallback~%"))

(defun test-specificity ()
  (format t "(FUN (MAKE-INSTANCE 'MOTHER)): ")
  (fun (make-instance 'mother))
  (format t "(FUN (MAKE-INSTANCE 'FATHER)): ")
  (fun (make-instance 'father))
  (format t "(FUN (MAKE-INSTANCE 'CHILD)): ")
  (fun (make-instance 'child))
  (format t "(FUN (MAKE-INSTANCE 'GRANDCHILD)): ")
  (fun (make-instance 'grandchild))
  (format t "(FUN 1): ")
  (fun 1))
#|
CL-USER> (test-specificity)
(FUN (MAKE-INSTANCE 'MOTHER)): mother
(FUN (MAKE-INSTANCE 'FATHER)): father
(FUN (MAKE-INSTANCE 'CHILD)): child
(FUN (MAKE-INSTANCE 'GRANDCHILD)): grandchild
(FUN 1): fallback
NIL
|#

;;;; AROUND

(defmethod met :around ((a mother))
  (format t "around MOTHER before a:~S~%" a)
  (call-next-method a)
  (format t "around MOTHER after~%")
  '(around mother))

(defmethod met :before ((a mother))
  (format t "before MOTHER a:~S~%" a)
  '(before mother))

(defmethod met :after ((a mother))
  (format t "after MOTHER a:~S~%" a)
  '(after mother))

(defmethod met ((a mother))
  (format t "primary MOTHER a:~S~%" a)
  '(primary mother))

(defmethod met :around ((a father))
  (format t "around FATHER before a:~S~%" a)
  (call-next-method a)
  (format t "around FATHER after~%")
  '(around father))

(defmethod met :before ((a father))
  (format t "before FATHER a:~S~%" a)
  '(before father))

(defmethod met :after ((a father))
  (format t "after FATHER a:~S~%" a)
  '(after father))

(defmethod met ((a father))
  (format t "primary FATHER a:~S~%" a)
  '(primary father))

(defmethod met :around ((a child))
  (format t "around CHILD before a:~S~%" a)
  (call-next-method a)
  (format t "around CHILD after~%")
  '(around child))

(defmethod met :before ((a child))
  (format t "before CHILD a:~S~%" a)
  '(before child))

(defmethod met :after ((a child))
  (format t "after CHILD a:~S~%" a)
  '(after child))

(defmethod met ((a child))
  (format t "primary CHILD a:~S~%" a)
  '(primary child))

(defmethod met :around ((a grandchild))
  (format t "around GRANDCHILD before a:~S~%" a)
  (call-next-method a)
  (format t "around GRANDCHILD after~%")
  '(around grandchild))

(defmethod met :before ((a grandchild))
  (format t "before GRANDCHILD a:~S~%" a)
  '(before grandchild))

(defmethod met :after ((a grandchild))
  (format t "after GRANDCHILD a:~S~%" a)
  '(after grandchild))

(defmethod met ((a grandchild))
  (format t "primary GRANDCHILD a:~S~%" a)
  '(primary grandchild))

(defmethod met :around (a)
  (format t "around FALLBACK before a:~S~%" a)
  (call-next-method a)
  (format t "around FALLBACK after~%")
  '(around fallback))

(defmethod met :before (a)
  (format t "before FALLBACK a:~S~%" a)
  '(before fallback))

(defmethod met :after (a)
  (format t "after FALLBACK a:~S~%" a)
  '(after fallback))

(defmethod met (a)
  (format t "primary FALLBACK a:~S~%" a)
  '(primary fallback))

;;;; MULTIPLE INHERITANCE

(defun test-multiple-inheritance ()
  (format t "(MET (MAKE-INSTANCE 'MOTHER))~%")
  (format t "~W~%~%" (met (make-instance 'mother)))
  (format t "(MET (MAKE-INSTANCE 'CHILD))~%")
  (format t "~W~%~%" (met (make-instance 'child)))
  (format t "(MET (MAKE-INSTANCE 'GRANDCHILD))~%")
  (format t "~W~%~%" (met (make-instance 'grandchild))))

#|
CL-USER> (test-multiple-inheritance)
(MET (MAKE-INSTANCE 'MOTHER))
around MOTHER before a:#<MOTHER {AB00F11}>
around FALLBACK before a:#<MOTHER {AB00F11}>
before MOTHER a:#<MOTHER {AB00F11}>
before FALLBACK a:#<MOTHER {AB00F11}>
primary MOTHER a:#<MOTHER {AB00F11}>
after FALLBACK a:#<MOTHER {AB00F11}>
after MOTHER a:#<MOTHER {AB00F11}>
around FALLBACK after
around MOTHER after
(AROUND MOTHER)

(MET (MAKE-INSTANCE 'CHILD))
around CHILD before a:#<CHILD {AB7D541}>
around MOTHER before a:#<CHILD {AB7D541}>
around FATHER before a:#<CHILD {AB7D541}>
around FALLBACK before a:#<CHILD {AB7D541}>
before CHILD a:#<CHILD {AB7D541}>
before MOTHER a:#<CHILD {AB7D541}>
before FATHER a:#<CHILD {AB7D541}>
before FALLBACK a:#<CHILD {AB7D541}>
primary CHILD a:#<CHILD {AB7D541}>
after FALLBACK a:#<CHILD {AB7D541}>
after FATHER a:#<CHILD {AB7D541}>
after MOTHER a:#<CHILD {AB7D541}>
after CHILD a:#<CHILD {AB7D541}>
around FALLBACK after
around FATHER after
around MOTHER after
around CHILD after
(AROUND CHILD)

(MET (MAKE-INSTANCE 'GRANDCHILD))
around GRANDCHILD before a:#<GRANDCHILD {AB7EAA9}>
around CHILD before a:#<GRANDCHILD {AB7EAA9}>
around MOTHER before a:#<GRANDCHILD {AB7EAA9}>
around FATHER before a:#<GRANDCHILD {AB7EAA9}>
around FALLBACK before a:#<GRANDCHILD {AB7EAA9}>
before GRANDCHILD a:#<GRANDCHILD {AB7EAA9}>
before CHILD a:#<GRANDCHILD {AB7EAA9}>
before MOTHER a:#<GRANDCHILD {AB7EAA9}>
before FATHER a:#<GRANDCHILD {AB7EAA9}>
before FALLBACK a:#<GRANDCHILD {AB7EAA9}>
primary GRANDCHILD a:#<GRANDCHILD {AB7EAA9}>
after FALLBACK a:#<GRANDCHILD {AB7EAA9}>
after FATHER a:#<GRANDCHILD {AB7EAA9}>
after MOTHER a:#<GRANDCHILD {AB7EAA9}>
after CHILD a:#<GRANDCHILD {AB7EAA9}>
after GRANDCHILD a:#<GRANDCHILD {AB7EAA9}>
around FALLBACK after
around FATHER after
around MOTHER after
around CHILD after
around GRANDCHILD after
(AROUND GRANDCHILD)

NIL
|#
