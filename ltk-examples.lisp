(load "~/quicklisp/setup.lisp")
(ql:quickload 'ltk)
(use-package 'ltk)

(defun scribble ()
  (with-ltk ()
;;    (let* ((canvas (make-instance 'canvas))
    (let* ((canvas (make-canvas nil
				:width 800 :height 200
				:xscroll t :yscroll t))
	   (down nil)
	   (button (make-instance 'button
				  :master canvas
				  :text "button"
				  :command (lambda ()
					     (print "button")
					     (print (window-width canvas))
					     (print (screen-mouse-x)))))
	   (last nil))
      (pack canvas)
      (bind canvas "<ButtonPress-1>"
	    (lambda (evt)
	      (setf down t)
	      (create-oval canvas
			   (- (event-x evt) 10) (- (event-y evt) 10)
			   (+ (event-x evt) 10) (+ (event-y evt) 10))
	      (setf last (create-line* canvas
				       0 0 (event-x evt) (event-y evt)))
	      (create-window canvas
			     (event-x evt)
			     (event-y evt)
			     button)))
      (bind canvas "<ButtonRelease-1>" (lambda (evt)
					 (declare (ignore evt))
					 (setf down nil)))
      (bind canvas "<Motion>"
	    (lambda (evt)
	      (when down
		;(scrollregion canvas (event-x evt) (event-y evt)
					;	      (+ (event-x evt) 100) (+ (event-y evt) 100))
		(itemconfigure canvas last "arrow" "both")
		(set-coords canvas last (list 0 0 (event-x evt) (event-y evt)))
		(create-oval canvas
			     (- (event-x evt) 10) (- (event-y evt) 10)
			     (+ (event-x evt) 10) (+ (event-y evt) 10))))))))

(defun canvastest1()
  (with-ltk (:serve-event t)
    (let* ((sc (make-instance 'scrolled-canvas))
	   (c (canvas sc))
;;	   (line (create-line c (list 100 100 400 50 700 150)))
;;	   (polygon (create-polygon c (list 50 150 250 160 250
;;					    300 50 330 )))
	   (text (create-text c 260 250 "Canvas test"))
	   (image-1 (make-image))
	   (image (create-image c 0 0 :image image-1)))
      (image-load image-1 "~/test2.ppm")
      (pack sc :expand 1 :fill :both)
      ;;(pack sc)
      (scrollregion c 0 0 800 800)
      (loop do
	   (itemconfigure c text "text"
			  (format nil "x: ~A y:~A"
				  (screen-mouse-x sc) (screen-mouse-y sc)))
	   (let* ((w 100)
		  (h 200)
		  (b (random 256))
		  (m (/ 255. (+ b w h)))
		  (data (loop for y below h collect
		  	    (loop for x below w collect
		  		 (list b 255 (floor (* (+ b x y) m)))))))
	     (image-setpixel image-1 data 0 0 w h))
;	   (loop for i below 1000 do
;		(setf i (1+ i)))
	   ;; or, instead of computing something, call (sleep .01)
	   )))
  )

(defun canvastest2 ()
  (start-wish)
  (let* ((sc (make-instance 'scrolled-canvas))
	 (c (canvas sc))
	 (line (create-line c (list 100 100 400 50 700 150)))
	 (polygon (create-polygon c (list 50 150 250 160 250
					  300 50 330 )))
	 (text (create-text c 260 250 "Canvas test")))
    (pack sc :expand 1 :fill :both)
    ;;(pack sc)
    (scrollregion c 0 0 800 800)
    ;;(sleep 1)
    ;; (loop while (ltk::with-ltk-handlers ()
    ;; 		  (itemconfigure c text "text"
    ;; 				 (format nil "x: ~A y:~A"
    ;; 					 (screen-mouse-x sc) (screen-mouse-y sc)))
    ;; 		  (ltk::main-iteration :blocking nil)
    ;; 		  (sleep .01)
    ;; 		  ))
    (mainloop :serve-event t)
    (loop do
	 (itemconfigure c text "text"
			(format nil "x: ~A y:~A"
				(screen-mouse-x sc) (screen-mouse-y sc)))
	 (loop for i below 1000000 do
	      (setf i (1+ i)))
	 )
    ))
