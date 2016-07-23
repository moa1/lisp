(load "~/quicklisp/setup.lisp")
(ql:quickload :squirl)

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

;; copied from PLANET.LISP in package :SQUIRL.

(squirl:defbody planetary-body)

(defun make-world ()
  (let* ((planet (prog1 (squirl:make-body :angular-velocity 0.3 :actor :not-grabbable :shapes (list (squirl:make-circle 70 :restitution 1 :friction 0.8))) (squirl:reset-shape-id-counter)))
	 (world (squirl:make-world :iterations 20))
	 (box (make-planetary-body :mass 1 :position (squirl:vec 0 -100) :velocity (squirl:vec 0 0) :shapes (list (squirl:make-poly (list (squirl:vec -10 -10) (squirl:vec -10 10) (squirl:vec 10 10) (squirl:vec 10 -10)) :friction 0.7 :restitution 1)))))
    (squirl:world-add-body world box)
    (squirl:world-add-body world planet)
    (values world planet box)))

(defmethod squirl:body-update-velocity ((body planetary-body) gravity damping dt)
  (declare (ignore gravity))
  ;;(prind "squirl:body-update-velocity" damping dt)
  (let* ((position (squirl:body-position body))
         (gravity (squirl:vec* position (/ -50000 (squirl:vec. position position)))))
    (call-next-method body gravity damping dt)))

(defun update-world (world planet box dt)
  (squirl:world-step world dt)
  (squirl:body-update-position planet dt)
  ;;(loop for body across (squirl:world-bodies world) do (prind body (squirl:body-position body) (squirl:body-velocity body)))
  (prind (squirl:body-position box) (squirl:body-velocity box))
  )

(defun test ()
  (multiple-value-bind (world planet box) (make-world)
    (update-world world planet box .01d0)))
