;; taken from basic.lisp from cl-sdl2, but removed the opengl requirement.

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :sdl2)

;;(in-package :sdl2-examples)
;;(require :sdl2)


(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown))
      (let ((controllers ())
	    (haptic ()))

	;; basic window/gl setup
	(format t "Setting up window: ~A.~%" win)
	(finish-output)

	(format t "Opening game controllers.~%")
	(finish-output)
	;; open any game controllers
	(loop for i from 0 upto (- (sdl2:joystick-count) 1)
	   do (when (sdl2:game-controller-p i)
		(format t "Found gamecontroller: ~a~%"
			(sdl2:game-controller-name-for-index i))
		(let* ((gc (sdl2:game-controller-open i))
		       (joy (sdl2:game-controller-get-joystick gc)))
		  (setf controllers (acons i gc controllers))
		  (when (sdl2:joystick-is-haptic-p joy)
		    (let ((h (sdl2:haptic-open-from-joystick joy)))
		      (setf haptic (acons i h haptic))
		      (sdl2:rumble-init h))))))

	;; main loop
	(format t "Beginning main loop.~%")
	(finish-output)
	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   (let ((scancode (sdl2:scancode-value keysym))
		 (sym (sdl2:sym-value keysym))
		 (mod-value (sdl2:mod-value keysym)))
	     (cond
	       ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
	       ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
	       ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
	     (format t "Key sym: ~a, code: ~a, mod: ~a~%"
		     sym
		     scancode
		     mod-value)))

	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))

	  (:mousemotion
	   (:x x :y y :xrel xrel :yrel yrel :state state)
	   (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
		   x xrel y yrel state))

	  (:controlleraxismotion
	   (:which controller-id :axis axis-id :value value)
	   (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
		   controller-id axis-id value))

	  (:controllerbuttondown
	   (:which controller-id)
	   (let ((h (cdr (assoc controller-id haptic))))
	     (when h
	       (sdl2:rumble-play h 1.0 100))))

	  (:idle
	   ())

	  (:quit () t))

	(format t "Closing opened game controllers.~%")
	(finish-output)
	;; close any game controllers that were opened
	;; as well as any haptics
	(loop for (i . controller) in controllers
	   do (progn
		(sdl2:game-controller-close controller)
		(sdl2:haptic-close (cdr (assoc i haptic)))))))))

(defun software-render-pixels ()
  "Software renderer example, drawing single pixels on the screen."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown))
      ;; basic window/gl setup
      (format t "Setting up window: ~A.~%" win)
      (finish-output)

      ;;(let ((wrend (sdl2:create-renderer win -1 '(:software :targettexture))))
      (let ((wrend (sdl2:create-renderer win -1 '(:software))))

	(format t "Window renderer: ~A~%" wrend)
	(finish-output)

	(sdl2-ffi.functions:sdl-set-render-draw-color wrend 255 0 0 255)

	;; main loop
	(format t "Beginning main loop.~%")
	(finish-output)
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))

	  (:mousemotion
	   (:x x :y y :xrel xrel :yrel yrel :state state)
	   (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
		   x xrel y yrel state))

	  (:idle
	   ()
	   ;;(sdl2-ffi.functions:sdl-set-render-draw-color wrend (random 256) (random 256) (random 256) (random 256))
	   ;;(sdl2-ffi.functions:sdl-render-draw-point wrend (random 800) (random 600))
	   (sdl2-ffi.functions:sdl-render-draw-point wrend (random 800) (random 600))
	   (sdl2:render-present wrend)
	   )

	  (:quit () t))

	(format t "End of main loop.~%")
	(finish-output)
	))))

;; (defun software-render-texture ()
;;   "Software renderer example, drawing a texture on the screen.
;; See SDL-wiki/MigrationGuide.html#If_your_game_just_wants_to_get_fully-rendered_frames_to_the_screen.
;; "
;;   (sdl2:with-init (:everything)
;;     (format t "Using SDL Library Version: ~D.~D.~D~%"
;;             sdl2-ffi:+sdl-major-version+
;;             sdl2-ffi:+sdl-minor-version+
;;             sdl2-ffi:+sdl-patchlevel+)
;;     (finish-output)

;;     (sdl2:with-window (win :flags '(:shown))
;;       ;; basic window/gl setup
;;       (format t "Setting up window: ~A.~%" win)
;;       (finish-output)

;;       (let* ((wrend (sdl2:create-renderer win -1 '(:software :targettexture)))
;; 	     (tex (sdl2:create-texture wrend :ARGB8888 :streaming 800 600))
;; 	     (sur (sdl2:create-rgb-surface 800 600 32
;; 					   :r-mask #x00ff0000
;; 					   :g-mask #x0000ff00
;; 					   :b-mask #x000000ff
;; 					   :a-mask #xff000000))
;; 	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
;; 	     (pix (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels)))

;; ;; get sur->pixels using (cffi:foreign-slot-value sur ...).

;; 	(format t "Window renderer: ~A~%" wrend)
;; 	(format t "Texture: ~A~%" tex)
;; 	(format t "Surface: ~A~%" sur)
;; 	(format t "Pixels: ~A~%" pix)
;; 	(finish-output)

;; 	(sdl2-ffi.functions:sdl-set-render-draw-color wrend 255 0 0 255)

;; 	;; main loop
;; 	(format t "Beginning main loop.~%")
;; 	(finish-output)
;; 	(sdl2:with-event-loop (:method :poll)
;; 	  (:keyup
;; 	   (:keysym keysym)
;; 	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
;; 	     (sdl2:push-event :quit)))

;; 	  (:mousemotion
;; 	   (:x x :y y :xrel xrel :yrel yrel :state state)
;; 	   (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
;; 		   x xrel y yrel state))

;; 	  (:idle
;; 	   ()
;; 	   (setf (cffi:mem-aref pix :uint32 (+ (random 800) (* (random 600) 800))) (random (expt 2 32)))
;; 	   (sdl2:update-texture tex pix)
;; 	   ;;TODO: call SDL_RenderClear(sdlRenderer);
;; 	   (sdl2:render-copy wrend tex)
;; 	   (sdl2:render-present wrend)
;; 	   )

;; 	  (:quit () t))

;; 	(format t "End of main loop.~%")
;; 	(finish-output)
;; 	))))

;; (let* ((sur (sdl2:create-rgb-surface 800 600 32
;; 				     :r-mask #x00ff0000
;; 				     :g-mask #x0000ff00
;; 				     :b-mask #x000000ff
;; 				     :a-mask #xff000000))
;;        (pix (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels)))
;;   (cffi:mem-aref pix :UINT32 1))
