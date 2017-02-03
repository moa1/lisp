(load "~/quicklisp/setup.lisp")
(ql:quickload :squirl)
(ql:quickload :sdl2)

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
  (let* ((planet (prog1 (squirl:make-body :angular-velocity 0.3 :actor :not-grabbable :shapes (list (squirl:make-circle 100 :restitution 1 :friction 0.8))) (squirl:reset-shape-id-counter)))
	 (world (squirl:make-world :iterations 20)))
    (squirl:world-add-body world planet)
    (loop for i below 120 do
	 (let* ((radius (+ 150 (random 200)))
		(angle (random (* 2 pi)))
		(x (+ 0 (* (cos angle) radius)))
		(y (+ 0 (* (sin angle) radius)))
		(position (squirl:vec x y))
		(box (make-planetary-body :mass 1 :position position :velocity (squirl:vec 0 0) :shapes (list (squirl:make-poly (list (squirl:vec -7 -7) (squirl:vec -7 7) (squirl:vec 7 7) (squirl:vec 7 -7)) :friction 0.8 :restitution 1)))))
	   (squirl:world-add-body world box)))
    (values world)))

(defmethod squirl:body-update-velocity ((body planetary-body) gravity damping dt)
  (declare (ignore gravity))
  ;;(prind "squirl:body-update-velocity" damping dt)
  (let* ((position (squirl:body-position body))
         (gravity (squirl:vec* position (/ -50000 (squirl:vec. position position)))))
    (call-next-method body gravity damping dt)))

;;; visualization

(defmethod draw-shape ((shape squirl:circle) renderer offset-x offset-y)
  (sdl2-ffi.functions::sdl-set-render-draw-color renderer 128 128 96 255)
  (let* ((radius (squirl:circle-radius shape))
	 (center (squirl:circle-transformed-center shape))
	 (x0 (squirl:vec-x center))
	 (y0 (squirl:vec-y center))
	 (step .2))
    (loop for a below (* pi 2) by step do
	 (let* ((x1 (+ x0 (* (cos a) radius)))
		(y1 (+ y0 (* (sin a) radius)))
		(x2 (+ x0 (* (cos (+ step a)) radius)))
		(y2 (+ y0 (* (sin (+ step a)) radius))))
	   (sdl2-ffi.functions::sdl-render-draw-line renderer (round (+ offset-x x1)) (round (+ offset-y y1)) (round (+ offset-x x2)) (round (+ offset-y y2)))))))

(defmethod draw-shape ((shape squirl:poly) renderer offset-x offset-y)
  (sdl2-ffi.functions::sdl-set-render-draw-color renderer 255 255 255 255)
  (let* ((vertices (squirl:poly-transformed-vertices shape))
	 (nvertices (length vertices)))
    (do ((i 0 (1+ i))) ((>= i nvertices))
      (let* ((v1 (aref vertices i))
	     (v2 (aref vertices (mod (1+ i) nvertices)))
	     (x1 (squirl:vec-x v1))
	     (y1 (squirl:vec-y v1))
	     (x2 (squirl:vec-x v2))
	     (y2 (squirl:vec-y v2)))
	(sdl2-ffi.functions::sdl-render-draw-line renderer (round (+ offset-x x1)) (round (+ offset-y y1)) (round (+ offset-x x2)) (round (+ offset-y y2)))))))

(defun software-render-texture (&key (frames -1) (win-x 212) (win-y 0) (win-w 800) (win-h 400))
  "Software renderer example, drawing a texture on the screen.
See SDL-wiki/MigrationGuide.html#If_your_game_just_wants_to_get_fully-rendered_frames_to_the_screen."
  (declare (optimize (debug 3)))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :x win-x :y win-y :w win-w :h win-h :flags '(:shown))
      ;; basic window/gl setup
      (format t "Setting up window: ~A (size:~A).~%" win (multiple-value-list (sdl2:get-window-size win)))
      (finish-output)

      (let* ((tex-w win-w) ;texture size; can be different from window size above.
	     (tex-h win-h)
	     (wrend (sdl2:create-renderer win -1 '(:software :targettexture)))
	     (tex (sdl2:create-texture wrend :ARGB8888 :streaming tex-w tex-h))
	     (sur (sdl2:create-rgb-surface tex-w tex-h 32 :r-mask #x00ff0000 :g-mask #x0000ff00 :b-mask #x000000ff :a-mask #xff000000))
	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
	     (first-frame-time (get-internal-real-time))
	     (num-frames 0)
	     (offset-x (/ tex-w 2))
	     (offset-y (/ tex-h 2))
	     (mouse-x (/ tex-w 2))
	     (mouse-y (/ tex-h 2))
	     (cursor nil)
	     (display-random-state (make-random-state t))
	     (ticks 3200)
	     (world (make-world))) ;; init world
	(declare (type fixnum num-frames))

	;; main loop
	(format t "Beginning main loop.~%")
	(finish-output)

	;;(sb-sprof:reset)
	;;(sb-sprof:start-profiling :mode :cpu) ;will profile all threads.

	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   (let ((scancode (sdl2:scancode-value keysym))
		 ;;(sym (sdl2:sym-value keysym))
		 ;;(mod-value (sdl2:mod-value keysym))
		 )
	     ;;(format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)
	     ))

	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))

	  (:mousebuttondown
	   ()
	   (multiple-value-bind (x y buttons) (sdl2:mouse-state)
	     (cond
	       ((= buttons 1)
		;;(format t "Mouse motion abs(rel): ~a, ~a~%Mouse buttons: ~a~%" x y buttons)
		(setf mouse-x (/ (* x tex-w) win-w) mouse-y (/ (* y tex-h) win-h))))))

	  (:idle
	   ()
	   (sdl2:update-texture tex (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels) :width (* 4 tex-w))
	   ;;TODO: call SDL_RenderClear(sdlRenderer);
	   (sdl2:render-copy wrend tex)
	   (let ((dt .01d0))
	     (squirl:world-step world dt)
	     (squirl:body-update-position (aref (squirl:world-bodies world) 0) dt)
	     (loop for body across (squirl:world-bodies world) do
		  (let ((firstshape (car (squirl:body-shapes body))))
		    (draw-shape firstshape wrend offset-x offset-y)))
	     (loop for body across (squirl:world-bodies world) do
		  (when (<= (random 10) 0)
		    (let* ((force (squirl:body-force body))
			   (x (squirl:vec-x force))
			   (y (squirl:vec-y force))
			   (magnitude 150.0)
			   (magnitude/2 (/ magnitude 2))
			   (xn (+ (* x 0.9) (- (random magnitude) magnitude/2)))
			   (yn (+ (* y 0.9) (- (random magnitude) magnitude/2))))
		      (setf (squirl:body-force body) (squirl:vec xn yn))))))
	   (sdl2:render-present wrend))

	  (:quit () t))

	;;(sb-sprof:stop-profiling)
	;;(sb-sprof:report)

	(format t "End of main loop.~%")
	(format t "Average frames per second: ~A.~%" (float (/ num-frames (/ (- (get-internal-real-time) first-frame-time) internal-time-units-per-second))))
	(finish-output)
	))))
