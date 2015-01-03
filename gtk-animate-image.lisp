(load "~/quicklisp/setup.lisp")
(ql:quickload 'cl-cffi-gtk)
(ql:quickload 'alexandria)

;; (defpackage :gtk-tutorial
;;   (:use :gtk :gdk :gdk-pixbuf :gobject
;;    :glib :gio :pango :cairo :common-lisp))
;; (in-package :gtk-tutorial)

(use-package '(:gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo))
(use-package 'alexandria)
;; (use-package 'gtk)
;; (use-package 'gdk)
;; (use-package 'gdk-pixbuf)
;; (use-package 'gobject)
;; (use-package 'glib)
;; (use-package 'gio)
;; (use-package 'pango)
;; (use-package 'cairo)

(defparameter *a* (gdk-pixbuf-new-from-file "/usr/share/pixmaps/xterm-color_48x48.xpm"))

(defun get-pixbuf-pixel (pixbuf x y)
  "Return the color stored in image PIXBUF at horizontal position X and vertical position Y."
  ;; Adapted from the source code at developer.gnome.org: 'Example 1. put_pixel() example'.
  (let ((n-channels (gdk-pixbuf-get-n-channels pixbuf))
	(colorspace (gdk-pixbuf-get-colorspace pixbuf))
	(bps (gdk-pixbuf-get-bits-per-sample pixbuf))
	(width (gdk-pixbuf-get-width pixbuf))
	(height (gdk-pixbuf-get-height pixbuf))
	(rowstride (gdk-pixbuf-get-rowstride pixbuf))
	(pixels (gdk-pixbuf-get-pixels pixbuf)))
    (assert (eq colorspace :rgb))
    (assert (= bps 8))
    (assert (> n-channels 0))
    (assert (<= 0 x (1- width)))
    (assert (<= 0 y (1- height)))
    (let ((offset (+ (* y rowstride) (* x n-channels)))
	  (result nil))
      ;; loop from last value to first to avoid having to reverse the result
      (loop for i from (+ offset n-channels -1) downto offset do
	   (setf result (cons (sb-sys:sap-ref-8 pixels i) result)))
      result)))

;; test reading a pixel
(let ((a (gdk-pixbuf-new :rgb nil 8 10 10)))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 0 0 0))))
(let ((a (gdk-pixbuf-new :rgb t 8 10 10)))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 0 0 0 0))))

(defun (setf get-pixbuf-pixel) (new-values pixbuf x y)
  "Replace the color stored in image PIXBUF at horizontal position X and vertical position Y with the values in the list NEW-VALUES."
  ;; Adapted from the source code at developer.gnome.org: 'Example 1. put_pixel() example'.
  (let ((n-channels (gdk-pixbuf-get-n-channels pixbuf))
	(colorspace (gdk-pixbuf-get-colorspace pixbuf))
	(bps (gdk-pixbuf-get-bits-per-sample pixbuf))
	(width (gdk-pixbuf-get-width pixbuf))
	(height (gdk-pixbuf-get-height pixbuf))
	(rowstride (gdk-pixbuf-get-rowstride pixbuf))
	(pixels (gdk-pixbuf-get-pixels pixbuf)))
    (assert (eq colorspace :rgb))
    (assert (= bps 8))
    (assert (<= 0 x (1- width)))
    (assert (<= 0 y (1- height)))
    (assert (<= (length new-values) n-channels))
    (let ((offset (+ (* y rowstride) (* x n-channels))))
      (loop
	 for new-value in new-values
	 for i from offset below (+ offset n-channels) do
	   (setf (sb-sys:sap-ref-8 pixels i) new-value))))
  new-values)

;; test setting a pixel
(let ((a (gdk-pixbuf-new :rgb nil 8 10 10)))
  (setf (get-pixbuf-pixel a 5 5) '(1 2 3))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 1 2 3))))
(let ((a (gdk-pixbuf-new :rgb t 8 10 10)))
  (setf (get-pixbuf-pixel a 5 5) '(1 2 3 4))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 1 2 3 4))))

(defun print-pixbuf-rectangle (pixbuf x0 y0 x1 y1)
  "This function is slow, use PIXBUF-ITERATE-RECTANGLE instead."
  (loop for y from y0 below y1 do
       (print (loop for x from x0 below x1 collect
		   (get-pixbuf-pixel pixbuf x y)))))

(defmacro pixbuf-iterate-rectangle (pixbuf x0 y0 x1 y1 x-symbol y-symbol set-current-pixel-form-symbol &body body)
  "Iterate through the pixels of a rectangle in PIXBUF and execute BODY for each pixel.
The pixels of the rectangle are specified by the left-upper coordinate (X0 Y0) and the right-lower coordinate (X1 Y1), where pixels with x-coordinate equal to X1 or y-coordinate Y1 are not included in the rectangle.
If non-NIL, the coordinates of the current pixel are bound to X-SYMBOL and Y-SYMBOL.
If non-NIL, SET-CURRENT-PIXEL-FORM-SYMBOL is the symbol of a form which can be called with 3 or 4 arguments, each of type (UNSIGNED-BYTE 8), and will set the current pixel to the color specified by the 3 parameters (R G B) or the 4 parameters (R G B ALPHA). When PIXBUF has 3 channels, but the form is called with 4 arguments, ALPHA will be ignored. When PIXBUF has 4 channels, but the form is called with 3 arguments, ALPHA will not be changed."
  (assert (symbolp x-symbol))
  (assert (symbolp y-symbol))
  (assert (symbolp set-current-pixel-form-symbol))
  (when (null x-symbol)
    (setf x-symbol (gensym)))
  (when (null y-symbol)
    (setf y-symbol (gensym)))
  (when (null set-current-pixel-form-symbol)
    (setf set-current-pixel-form-symbol (gensym)))
  (with-unique-names (p rx0 ry0 rx1 ry1
			width height
			n-channels rowstride pixels
			r g b a
			rval gval bval aval
			offset)
    `(let* ((,p ,pixbuf)
	    (,rx0 ,x0) (,ry0 ,y0) (,rx1 ,x1) (,ry1 ,y1)
	    (,width (gdk-pixbuf-get-width ,p))
	    (,height (gdk-pixbuf-get-height ,p))
	    (,n-channels (gdk-pixbuf-get-n-channels ,p))
	    (,rowstride (gdk-pixbuf-get-rowstride ,p))
	    (,pixels (gdk-pixbuf-get-pixels ,p)))
       (declare (type fixnum ,rx0 ,ry0 ,rx1 ,ry1 ,width ,height ,n-channels ,rowstride))
       (assert (eq :rgb (gdk-pixbuf-get-colorspace ,p)))
       (assert (= 8 (gdk-pixbuf-get-bits-per-sample ,p)))
       (assert (and (<= 0 ,rx0) (<= ,rx1 ,width)))
       (assert (and (<= 0 ,ry0) (<= ,ry1 ,height)))
       (assert (<= 3 ,n-channels 4))
       ;; This case form duplicates code in all invocations of this macro, which could really be avoided if the user specified the number of channels at compile time. In case the passed pixbuf has a different number of channels, we would have to raise an error. Maybe also allow the user to specify that the number of channels is unknown, in wich case the ecase would be kept to differentiate between the possible number of channels.
       (ecase ,n-channels
	 ((3)
	  (macrolet ((,set-current-pixel-form-symbol (,r ,g ,b &optional (,a nil))
		       (declare (ignore ,a))
		       (let ((,rval (gensym)) (,gval (gensym)) (,bval (gensym)))
			 `(let ((,,rval ,,r) (,,gval ,,g) (,,bval ,,b))
			    (declare (type (unsigned-byte 8) ,,rval ,,gval ,,bval))
			    (setf (sb-sys:sap-ref-8 ,',pixels ,',offset) ,,rval)
			    (setf (sb-sys:sap-ref-8 ,',pixels (1+ ,',offset)) ,,gval)
			    (setf (sb-sys:sap-ref-8 ,',pixels (+ 2 ,',offset)) ,,bval)))))
	    (loop for ,y-symbol of-type fixnum from ,ry0 below ,ry1 do
		 (let ((,offset (+ (* ,y-symbol ,rowstride) (* ,rx0 ,n-channels))))
		   (loop for ,x-symbol of-type fixnum from ,rx0 below ,rx1 do
			,@body
			(incf ,offset ,n-channels))))))
	 ((4)
	  (macrolet ((,set-current-pixel-form-symbol (,r ,g ,b &optional (,a nil))
		       (let ((,rval (gensym)) (,gval (gensym)) (,bval (gensym)) (,aval (gensym)))
			 (declare (ignorable ,aval))
			 `(let ((,,rval ,,r) (,,gval ,,g) (,,bval ,,b)
				,@(when ,a `((,,aval ,,a))))
			    (declare (type (unsigned-byte 8) ,,rval ,,gval ,,bval ,@(when ,a `(,,aval))))
			    (setf (sb-sys:sap-ref-8 ,',pixels ,',offset) ,,rval)
			    (setf (sb-sys:sap-ref-8 ,',pixels (1+ ,',offset)) ,,gval)
			    (setf (sb-sys:sap-ref-8 ,',pixels (+ 2 ,',offset)) ,,bval)
			    ,@(when ,a
				    `((setf (sb-sys:sap-ref-8 ,',pixels (+ 3 ,',offset)) ,,aval)))))))
	    (loop for ,y-symbol of-type fixnum from ,ry0 below ,ry1 do
		 (let ((,offset (+ (* ,y-symbol ,rowstride) (* ,rx0 ,n-channels))))
		   (loop for ,x-symbol of-type fixnum from ,rx0 below ,rx1 do
			,@body
			(incf ,offset ,n-channels))))))))))

;; test setting a pixel using pixbuf-iterate-rectangle: test the 4 cases for combinations of (n-channels being 3 or 4) and calling setpixel with (3 or 4 arguments).
(let ((a (gdk-pixbuf-new :rgb nil 8 10 10)))
  (pixbuf-iterate-rectangle a 5 5 6 6 x y setpixel (setpixel 1 2 3))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 1 2 3))))
(let ((a (gdk-pixbuf-new :rgb nil 8 10 10)))
  (pixbuf-iterate-rectangle a 5 5 6 6 x y setpixel (setpixel 1 2 3 4))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 1 2 3))))
(let ((a (gdk-pixbuf-new :rgb t 8 10 10)))
  (setf (get-pixbuf-pixel a 5 5) (list 5 5 5 5))
  (pixbuf-iterate-rectangle a 5 5 6 6 x y setpixel (setpixel 1 2 3))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 1 2 3 5))))
(let ((a (gdk-pixbuf-new :rgb t 8 10 10)))
  (setf (get-pixbuf-pixel a 5 5) (list 5 5 5 5))
  (pixbuf-iterate-rectangle a 5 5 6 6 x y setpixel (setpixel 1 2 3 4))
  (assert (equal (get-pixbuf-pixel a 5 5) (list 1 2 3 4))))

;; I would be content with getting to run the normal (blocking) within-main-loop. Right now it raises the exception division by zero when run in SLIME, but not when run from SBCL in a terminal.
(defun example-simple-window ()
  (let* (
	 (width 800)
	 (height 600)
	 (pixbuf (gdk-pixbuf-new :rgb nil 8 width height))
	 ;;(pixbuf (gdk-pixbuf-new-from-file "/usr/share/pixmaps/xterm-color_48x48.xpm"))
	 (frame (make-instance 'gtk-frame :shadow-type :in))
	 )
    (sb-int:with-float-traps-masked (:divide-by-zero) ;;otherwise execution in SLIME raises an division-by-zero error, see https://bugs.launchpad.net/sbcl/+bug/1038874
      (within-main-loop
	(flet ((modify-image1 ()
		 (loop for i below 10 do
		      (setf (get-pixbuf-pixel pixbuf (random 48) (random 48))
			    (list (random 256) (random 256) (random 256))))
		 (gtk:gtk-widget-queue-draw frame)
		 t)
	       (modify-image ()
		 (time
		  (let ((r (random 256)) (g (random 256)) (b (random 256)))
		    (pixbuf-iterate-rectangle pixbuf 0 0 width height x y set-pixel
		      (set-pixel (mod (+ r x) 256) (mod (+ g y) 256) b))))
		 (gtk:gtk-widget-queue-draw frame)
		 t)
	       (modify-image2 ()
		 (time
		  (let ((r (random 256)) (g (random 256)) (b (random 256)))
		    (loop for y below height do
			 (loop for x below width do
			      (setf (get-pixbuf-pixel pixbuf x y)
				    (list (mod (+ r x) 256) (mod (+ g y) 256) b))))))
		 (gtk:gtk-widget-queue-draw frame)
		 t))
	  (let ((window (gtk-window-new :toplevel)) ;; Create a toplevel window.
		;;(timeout (gdk-threads-add-timeout (ceiling 1000/25) #'modify-image)))
		(timeout (gdk-threads-add-timeout 1000 #'modify-image)))
	    ;; Signal handler for the window to handle the signal "destroy".
	    (g-signal-connect window "destroy"
			      (lambda (widget)
				(declare (ignore widget))
				(g-source-remove timeout)
				(leave-gtk-main)))
	    (let* ((image (gtk-image-new-from-pixbuf pixbuf)))
	      (gtk-container-add frame image)
	      (gtk-container-add window frame)
	      ;; Show the window.
	      (gtk-widget-show-all window))))))
    (print (bordeaux-threads:all-threads))
    (loop for i below 5 do
	 (sleep 1)
	 (print i)
	 ;; (loop for i below 10 do
	 ;;      (setf (get-pixbuf-pixel pixbuf (random 48) (random 48))
	 ;; 	    (list (random 256) (random 256) (random 256) 255)))
	 ;; (gtk:gtk-widget-queue-draw frame)
	 )
    (join-gtk-main)))

;; sbcl --eval '(progn (load "gtk-animate-image.lisp") (example-simple-window))'

;;(example-simple-window)

;;;; http://www.gtkforums.com/viewtopic.php?f=3&t=54863&p=69869&hilit=main+thread#p69869 suggests doing the following:
;; static gpointer do_something (Data *data) {
;;      g_idle_add ((GSourceFunc)update_gui, data);   // <--- update the GUI...

;;      // Start time-consuming operation...
;;      // ...the time-consuming operation ends,

;;      g_idle_add ((GSourceFunc)update_gui, data);   // <--- update the GUI...
;; }
(defun example-idle-add ()
  (let* (
	 (width 800)
	 (height 600)
	 (pixbuf (gdk-pixbuf-new :rgb nil 8 width height))
	 ;;(pixbuf (gdk-pixbuf-new-from-file "/usr/share/pixmaps/xterm-color_48x48.xpm"))
	 (frame (make-instance 'gtk-frame :shadow-type :in))
	 )
    (flet ((modify-image ()
	     (gtk:gtk-widget-queue-draw frame)
	     nil))
      (sb-int:with-float-traps-masked (:divide-by-zero) ;;otherwise execution in SLIME raises an division-by-zero error, see https://bugs.launchpad.net/sbcl/+bug/1038874
	(within-main-loop
	  (let ((window (gtk-window-new :toplevel)) ;; Create a toplevel window.
		)
	    ;; Signal handler for the window to handle the signal "destroy".
	    (g-signal-connect window "destroy"
			      (lambda (widget)
				(declare (ignore widget))
				(leave-gtk-main)))
	    (let* ((image (gtk-image-new-from-pixbuf pixbuf)))
	      (gtk-container-add frame image)
	      (gtk-container-add window frame)
	      ;; Show the window.
	      (gtk-widget-show-all window)))))
      (print (bordeaux-threads:all-threads))
      (let ((n-frames 100)
	    (start (get-internal-real-time)))
	(loop for i below n-frames do
	     (time
	      (let ((r (random 256)) (g (random 256)) (b (random 256)))
		(pixbuf-iterate-rectangle pixbuf 0 0 width height x y set-pixel
		  (set-pixel (mod (+ r x) 256) (mod (+ g y) 256) b))))
	     (print i)
	     (g-idle-add #'modify-image) ; this doesn't guarantee that the pixbuf gets drawn completely, it might get drawn while the pixbuf is modified for the next frame already. Maybe two alternating pixbufs (and a lock) would be better, so that one gets drawn while the other is being modified.
	     )
	(format t "average frames per second: ~A~%" (float (/ n-frames (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
      (join-gtk-main))))

(defmacro with-gtk-main-loop-ensure-destroyed (window-symbol gtk-main-body &body main-loop-body)
  (with-unique-names (gtk-thread)
    `(LET ((,GTK-THREAD NIL)
	   (,window-symbol (GTK-WINDOW-NEW :TOPLEVEL)))
       (SB-INT:WITH-FLOAT-TRAPS-MASKED (:DIVIDE-BY-ZERO)
	 (unwind-protect
	      (WITHIN-MAIN-LOOP
		(G-SIGNAL-CONNECT ,WINDOW-symbol "destroy"
				  (LAMBDA (WIDGET)
				    (DECLARE (IGNORE WIDGET))
				    (LEAVE-GTK-MAIN)
				    (gtk-widget-destroy ,WINDOW-symbol)
				    ))
		,gtk-main-body
		(GTK-WIDGET-SHOW-ALL WINDOW)
		(SETF ,GTK-THREAD (BORDEAUX-THREADS:CURRENT-THREAD))
		)
	   (gtk-widget-destroy ,WINDOW-symbol) ;for some reason, repeated calls to "(example-unwind-protect-gtk-main)" hang when this line is removed.
	   ))
       (UNWIND-PROTECT
	    (progn
	      ,@main-loop-body)
	 (PRINT "before loop")
	 (LOOP WHILE (NOT ,GTK-THREAD) do (print "loop"))
	 ;;(sleep 1)
	 (PRINT "after loop")
	 ;;; the following lines are the approaches I tried (and which didn't work) to let repeated calls to "(example-unwind-protect-gtk-main)" not hang, but let the gtk thread exit gracefully if there is an error in MAIN-LOOP-BODY.
	 ;; (HANDLER-CASE
	 ;;     (BORDEAUX-THREADS:DESTROY-THREAD GTK-THREAD)
	 ;;   (SB-THREAD:INTERRUPT-THREAD-ERROR NIL))
	 ;;(bordeaux-threads:interrupt-thread GTK-THREAD (lambda () (leave-gtk-main)))
	 ;;(bordeaux-threads:interrupt-thread GTK-THREAD (lambda () (gtk-main-quit)))
	 ;;(g-signal-emit WINDOW "destroy")
	 ;;; the following does work, does need ,WINDOW-symbol however. (So I cannot use a GTK-MAIN-BODY which uses a different setup than done in this macro, which would be possible if I wouldn't have to access ,WINDOW-symbol in the main body's unwind-protect.)
	 (g-idle-add (lambda () (g-signal-emit ,WINDOW-symbol "destroy")))
	 (PRINT "before sleep")
	 ;;(loop while (bordeaux-threads:thread-alive-p GTK-THREAD) do (print "sleep"))
	 ;;(sleep 1)
	 (loop while (bordeaux-threads:thread-alive-p ,GTK-THREAD) do (print "sleep"))
	 (PRINT "after sleep"))
       )
    )
  )

(defun example-unwind-protect-gtk-main ()
  (with-gtk-main-loop-ensure-destroyed window
      (print window)
    (print "main loop")
    (error "hello")
    nil))

;; I wrote the macro "with-gtk-main-loop-ensure-destroyed" by tweaking the below function until it worked (successive calls to "(example-unwind-protect-gtk-main*)" didn't hang the main loop), and then wrote the macro to allow custom GTK-MAIN-BODY and MAIN-LOOP-BODY.
(defun example-unwind-protect-gtk-main* ()
  (LET ((GTK-THREAD NIL))
    (LET ((WINDOW (GTK-WINDOW-NEW :TOPLEVEL)))
      (SB-INT:WITH-FLOAT-TRAPS-MASKED (:DIVIDE-BY-ZERO)
	(unwind-protect
	     (WITHIN-MAIN-LOOP
	       (G-SIGNAL-CONNECT WINDOW "destroy"
				 (LAMBDA (WIDGET)
				   (DECLARE (IGNORE WIDGET))
				   (LEAVE-GTK-MAIN)
				   (gtk-widget-destroy WINDOW)))
	       (GTK-WIDGET-SHOW-ALL WINDOW)
	       (SETF GTK-THREAD (BORDEAUX-THREADS:CURRENT-THREAD))
	       )
	  (gtk-widget-destroy WINDOW)
	  ))
      (UNWIND-PROTECT (PROGN (error "hello"))
	(PRINT "before loop")
	(LOOP WHILE (NOT GTK-THREAD) do (print "loop"))
	;;(sleep 1)
	(PRINT "after loop")
	;;      (HANDLER-CASE
	;;	  (BORDEAUX-THREADS:DESTROY-THREAD GTK-THREAD)
	;;(bordeaux-threads:interrupt-thread GTK-THREAD (lambda () (leave-gtk-main)))
	;;(bordeaux-threads:interrupt-thread GTK-THREAD (lambda () (gtk-main-quit)))
	;;(g-signal-emit WINDOW "destroy")
					;      (g-idle-add (lambda () (g-signal-emit WINDOW "destroy"))); TODO: set :priority to highest possible
	;;	(SB-THREAD:INTERRUPT-THREAD-ERROR NIL))
	(g-idle-add (lambda () (g-signal-emit WINDOW "destroy")))
	(PRINT "before sleep")
	;;(loop while (bordeaux-threads:thread-alive-p GTK-THREAD) do (print "sleep"))
	;;(sleep 1)
	(loop while (bordeaux-threads:thread-alive-p GTK-THREAD) do (print "sleep"))
	(PRINT "after sleep"))
      )
    )
  )

;; did this work at any time?
;; (defun example-unwind-protect-gtk-main ()
;;   (let ((gtk-thread nil)
;; 	(main-thread nil))
;;     (sb-int:with-float-traps-masked (:divide-by-zero) ;;otherwise execution in SLIME raises an division-by-zero error, see https://bugs.launchpad.net/sbcl/+bug/1038874
;;       (within-main-loop
;; 	(setf gtk-thread (bordeaux-threads:current-thread))
;; 	(let ((window (gtk-window-new :toplevel)))
;; 	  ;; Signal handler for the window to handle the signal "destroy".
;; 	  (g-signal-connect window "destroy"
;; 			    (lambda (widget)
;; 			      (declare (ignore widget))
;; 			      (leave-gtk-main)))
;; 	  (gtk-widget-show-all window)
;; 	  )))
;;     (setf main-thread (bordeaux-threads:current-thread))
;;     (unwind-protect
;; 	 ;;(sleep 3)
;; 	 (/ 1 0)
;;       (loop while (not gtk-thread))
;;       (print (list "gtk-thread" gtk-thread))
;;       (print (list "all threads" (bordeaux-threads:all-threads)))
;;       (print (list "alive-p" (bordeaux-threads:thread-alive-p gtk-thread)))
;;       (handler-case
;; 	  (bordeaux-threads:destroy-thread gtk-thread)
;; 	(sb-thread:interrupt-thread-error ())) ;if gtk-thread was already dead (when we call destroy-thread on it), SBCL raises this error
;;       (sleep 1) ;; give gtk-thread some time to exit (because SBCL apparently destroys threads by letting them execute sb-ext:quit/sb-ext:exit).
;;       (print (list "alive-p" (bordeaux-threads:thread-alive-p gtk-thread)))
;;       (print (list "all threads" (bordeaux-threads:all-threads)))
;;       )
;;     ))


;;(ql:quickload 'cl-cffi-gtk-demo-gtk)
;;(gtk-demo:main)
