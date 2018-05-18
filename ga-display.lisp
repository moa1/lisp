;; display of ga.lisp.

(load "~/lisp/ga.lisp")

(ql:quickload :sdl2)


(defun sdl-lock-surface (surface)
  "Lock SURFACE for directly accessing the pixels."
  (plus-c:c-fun sdl2-ffi::sdl-lock-surface surface))

(defun sdl-unlock-surface (surface)
  "Unlock SURFACE for directly accessing the pixels."
  (plus-c:c-fun sdl2-ffi::sdl-unlock-surface surface))

(defun sdl-surface-get-w (surface)
  "Return the width of SURFACE."
  (plus-c:c-ref surface SDL2-FFI:SDL-SURFACE :w))

(defun sdl-surface-get-h (surface)
  "Return the height of SURFACE."
  (plus-c:c-ref surface SDL2-FFI:SDL-SURFACE :h))

(defun sdl-surface-get-pitch (surface)
  "Return the pitch of SURFACE."
  (plus-c:c-ref surface SDL2-FFI:SDL-SURFACE :pitch))

(defun sdl-surface-get-pixels (surface)
  "Return the pitch of SURFACE."
  (plus-c:c-ref surface SDL2-FFI:SDL-SURFACE :pixels))

(defun sdl-surface-format-rgba-p (surface)
  "Return T if SURFACE has the standard 32-bit RGBA format."
  (let ((format (plus-c:c-ref surface SDL2-FFI:SDL-SURFACE :format)))
    ;; TODO: check (sdl-pixel-format-get-field format :format) to be SDL_PIXELFORMAT_ARGB8888 instead. This should be faster and simpler.
    (macrolet ((sdl-pixel-format-get-field (pixel-format field)
		 `(plus-c:c-ref ,pixel-format sdl2-ffi:sdl-pixel-format ,field)))
      (and (= 32 (sdl-pixel-format-get-field format :bits-per-pixel))
	   (= #x00ff0000 (sdl-pixel-format-get-field format :rmask))
	   (= #x0000ff00 (sdl-pixel-format-get-field format :gmask))
	   (= #x000000ff (sdl-pixel-format-get-field format :bmask))
	   (= #xff000000 (sdl-pixel-format-get-field format :amask))))))

(declaim (inline color-to-argb8888))
(defun color-to-argb8888 (a r g b)
  (declare (type (integer 0 255) a r g b))
  (+ (ash a 24) (ash r 16) (ash g 8) (ash b 0)))

(defmacro with-direct-pixel-access-raw (surface pixels-symbol pitch-symbol &body body)
  "Prepare the SURFACE for direct pixel access.
PIXELS-SYMBOL must be a symbol and will be bound to a pointer to the pixels buffer.
PITCH-SYMBOL must be a symbol and will be bound to the pitch in bytes."
  (let ((sur (gensym "SUR")))
    `(let* ((,sur ,surface)
	    (,pixels-symbol (plus-c:c-ref ,sur SDL2-FFI:SDL-SURFACE :pixels))
	    (,pitch-symbol (sdl-surface-get-pitch ,sur)))
       (declare (type fixnum ,pitch-symbol))
       (assert (sdl-surface-format-rgba-p ,sur))
       (sdl-lock-surface ,sur)
       (unwind-protect (progn ,@body) (sdl-unlock-surface ,sur)))))

(defmacro with-safe-pixel-access (surface set-pixel-symbol &body body)
  "Prepare SURFACE for setting individual pixels on it using SET-PIXEL.
The pixel format of SURFACE must be ARGB8888.
SET-PIXEL-SYMBOL must be a symbol (say, SET-PIXEL) and will be set to a function that draws to the surface. SET-PIXEL must be called like this: (SET-PIXEL X Y COLOR), where COLOR must be a color in the ARGB8888 format. SET-PIXEL will check X and Y for bounds of the surface."
  (let ((pixels (gensym "PIXELS")) (pitch (gensym "PITCH")) (pitch-uint32 (gensym "PITCH-UINT32")) (last-x (gensym "LAST-X")) (last-y (gensym "LAST-Y")))
    `(with-direct-pixel-access-raw ,surface ,pixels ,pitch
       (assert (sdl-surface-format-rgba-p ,surface))
       (let ((,pitch-uint32 (ash ,pitch -2)) ;pitch is in pixels, but we need it in :uint32.
	     (,last-x (1- (sdl-surface-get-w ,surface)))
	     (,last-y (1- (sdl-surface-get-h ,surface))))
	 (declare (type fixnum ,pitch-uint32 ,last-x ,last-y))
	 (flet ((,set-pixel-symbol (x y color)
		  (declare (optimize (speed 3) (safety 3) (debug 0) (compilation-speed 0))
			   (type fixnum x y) (type (integer 0 4294967295) color))
		  (assert (<= 0 x ,last-x))
		  (assert (<= 0 y ,last-y))
		  (let ((index (+ (the fixnum (* y ,pitch-uint32)) x)))
		    (declare (type fixnum index))
		    (setf (cffi:mem-aref ,pixels :uint32 index) color))))
	   ,@body)))))


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

      (let* ((tex-w (array-dimension *world* 0)) ;texture size; can be different from window size above.
	     (tex-h (array-dimension *world* 1))
	     (wrend (sdl2:create-renderer win -1 '(:software :targettexture)))
	     (tex (sdl2:create-texture wrend :ARGB8888 :streaming tex-w tex-h))
	     (sur (sdl2:create-rgb-surface tex-w tex-h 32 :r-mask #x00ff0000 :g-mask #x0000ff00 :b-mask #x000000ff :a-mask #xff000000))
	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
	     (first-frame-time (get-internal-real-time))
	     (num-frames 0)
	     (display-random-state (make-random-state t))
	     (ticks 12800)
	     (display-mode :energy)
	     (edit-distance-cacher (make-edit-distance-cacher )))
	(declare (type fixnum num-frames))

	(format t "Window renderer: ~A~%" wrend)
	(format t "Texture: ~A~%" tex)
	(format t "Surface: ~A~%" sur)
	(finish-output)

	;; main loop
	(format t "Beginning main loop.~%")
	(when (= 0 (hash-table-count *orgs*))
	  (error "No organisms"))
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
	     (cond
	       ((sdl2:scancode= scancode :scancode-f1)
		(setf display-mode :energy))
	       ((sdl2:scancode= scancode :scancode-f2)
		(setf display-mode :edit-distance))
	       ((sdl2:scancode= scancode :scancode-o) ;overview
		(setf *cursor* nil))
	       ((sdl2:scancode= scancode :scancode-f) ;fittest organism
		(setf *cursor* (argmax-hash-table *orgs* #'compute-fitness)))
	       ((sdl2:scancode= scancode :scancode-d) ;display world
		(setf *display-world* (not *display-world*)))
	       ((sdl2:scancode= scancode :scancode-s) ;statistics
		(setf *print-statistics* (not *print-statistics*)))
	       ((sdl2:scancode= scancode :scancode-1)
		(setf ticks 200))
	       ((sdl2:scancode= scancode :scancode-2)
		(setf ticks 400))
	       ((sdl2:scancode= scancode :scancode-3)
		(setf ticks 800))
	       ((sdl2:scancode= scancode :scancode-4)
		(setf ticks 1600))
	       ((sdl2:scancode= scancode :scancode-5)
		(setf ticks 3200))
	       ((sdl2:scancode= scancode :scancode-6)
		(setf ticks 6400))
	       ((sdl2:scancode= scancode :scancode-7)
		(setf ticks 12800))
	       ((sdl2:scancode= scancode :scancode-8)
		(setf ticks 25600))
	       ((sdl2:scancode= scancode :scancode-9)
		(setf ticks 51200))
	       ((sdl2:scancode= scancode :scancode-0)
		(setf ticks 102400)))
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
		(let* ((mouse-x (/ (* x tex-w) win-w))
		       (mouse-y (/ (* y tex-h) win-h))
		       (x (floor mouse-x)) (y (floor mouse-y))
		       (org (nearest-org x y *orgs*)))
		  (setf *cursor* org))))))

	  (:idle
	   ()
	   (if (or (and (>= frames 0) (>= num-frames frames)) (= 0 (hash-table-count *orgs*)))
	       (sdl2:push-quit-event)
	       (progn
		 (idleloop (+ *world-tick* ticks))
		 (when *display-world*
		   (with-safe-pixel-access sur set-pixel
		     (let* ((w (sdl-surface-get-w sur))
			    (h (sdl-surface-get-h sur)))
		       (loop for y below h do
			    (loop for x below w do
				 (let* ((c (min 255 (ash (floor (aref *world* x y 0)) -2)))
					(color (cond ((>= c 0) (color-to-argb8888 255 c c c))
						     (t (color-to-argb8888 255 0 0 255)))))
				   (set-pixel x y color))))
		       (loop for sun in *world-sun* do
			    (multiple-value-bind (x y) (funcall (nature-position-function sun) sun *world-tick*)
			      (let ((c (+ 128 (random 128 display-random-state))))
				(set-pixel (mod (round x) w) (mod (round y) h) (color-to-argb8888 255 c c 0))
				(set-pixel (mod (round (1+ x)) w) (mod (round y) h) (color-to-argb8888 255 c c 0))
				(set-pixel (mod (round x) w) (mod (round (1+ y)) h) (color-to-argb8888 255 c c 0))
				(set-pixel (mod (round (1+ x)) w) (mod (round (1+ y)) h) (color-to-argb8888 255 c c 0)))))
		       (loop for org being the hash-values of *orgs* do
			    (let* ((x (floor (orgap-x org)))
				   (y (floor (orgap-y org))))
			      (ecase display-mode
				((:energy)
				 (set-pixel x y
					    (let* ((e (max 0 (get-energy (orgap-energy org) 0)))
						   (c (min (ash e -1) 255)))
					      (if (= (orgcont-age org) 0)
						  (color-to-argb8888 255 255 0 0)
						  (color-to-argb8888 255 0 c c)))))
				((:edit-distance)
				 (when *cursor*
				   (set-pixel x y
					      (let* ((ed (funcall edit-distance-cacher *cursor* org))
						     (c (floor (* 255 ed))))
						(if (= ed 1.0)
						    (color-to-argb8888 255 255 0 0)
						    (color-to-argb8888 255 0 c c)))))))))))
		   (sdl2:update-texture tex (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels) :width (* 4 tex-w))
		   ;;TODO: call SDL_RenderClear(sdlRenderer);
		   (sdl2:render-copy wrend tex)
		   (sdl2-ffi.functions::sdl-set-render-draw-color wrend 255 (random 256 display-random-state) (random 256 display-random-state) 255)
		   (cond
		     ((and (not (null *cursor*)) (> (get-energy (orgap-energy *cursor*) 0) 0))
		      (print-orgcont *cursor*)
		      (let* ((x (floor (orgap-x *cursor*))) (y (floor (orgap-y *cursor*)))
			     (x1 (min (1- win-w) (max 0 (* x (/ win-w tex-w)))))
			     (y1 (min (1- win-h) (max 0 (* y (/ win-h tex-h)))))
			     (x2 (min (1- win-w) (max 0 (* (1+ x) (/ win-w tex-w)))))
			     (y2 (min (1- win-h) (max 0 (* (1+ y) (/ win-h tex-h))))))
			(sdl2-ffi.functions::sdl-render-draw-line wrend x1 y1 x2 y1)
			(sdl2-ffi.functions::sdl-render-draw-line wrend x2 y1 x2 y2)
			(sdl2-ffi.functions::sdl-render-draw-line wrend x2 y2 x1 y2)
			(sdl2-ffi.functions::sdl-render-draw-line wrend x1 y2 x1 y1)
			(sdl2-ffi.functions::sdl-render-draw-line wrend (1- x1) (1- y1) (1+ x2) (1- y1))
			(sdl2-ffi.functions::sdl-render-draw-line wrend (1+ x2) (1- y1) (1+ x2) (1+ y2))
			(sdl2-ffi.functions::sdl-render-draw-line wrend (1+ x2) (1+ y2) (1- x1) (1+ y2))
			(sdl2-ffi.functions::sdl-render-draw-line wrend (1- x1) (1+ y2) (1- x1) (1- y1))
			))
		     ((not (null *cursor*))
		      (setf *cursor* (nearest-org-genes (orgap-genes *cursor*) *orgs* :exclude-orgs (list *cursor*)))))
		   (sdl2:render-present wrend))
		 (incf num-frames)))
	   )

	  (:quit () t))

	;;(sb-sprof:stop-profiling)
	;;(sb-sprof:report)

	(format t "End of main loop.~%")
	(format t "Average frames per second: ~A.~%" (float (/ num-frames (/ (- (get-internal-real-time) first-frame-time) internal-time-units-per-second))))
	(finish-output)
	))))
