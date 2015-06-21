;;;; Speed-test different methods of drawing safely (without being able to violate memory bounds) onto a surface.

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :sdl2)
(ql:quickload :alexandria)
(use-package :alexandria)

#|
Example calls:
(software-render-texture :local-function)
(software-render-texture :list)
(software-render-texture :array)
|#

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
  (with-unique-names (sur)
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
  (with-unique-names (pixels pitch pitch-uint32 last-x last-y)
    `(with-direct-pixel-access-raw ,surface ,pixels ,pitch
       (assert (sdl-surface-format-rgba-p ,surface))
       (let ((,pitch-uint32 (ash ,pitch -2)) ;pitch is in pixels, but we need it in :uint32.
	     (,last-x (1- (sdl-surface-get-w ,surface)))
	     (,last-y (1- (sdl-surface-get-h ,surface))))
	 (declare (type fixnum ,pitch-uint32))
	 (flet ((,set-pixel-symbol (x y color)
		  (declare (type fixnum x y) (type (integer 0 4294967295) color))
		  (assert (<= 0 x ,last-x))
		  (assert (<= 0 y ,last-y))
		  (let ((index (+ (the fixnum (* y ,pitch-uint32)) x)))
		    (declare (type fixnum index))
		    (setf (cffi:mem-aref ,pixels :uint32 index) color))))
	   ,@body)))))

(defun render-pixels-list (surface pixels)
  "Draw PIXELS, a list of lists (X Y COLOR) onto the SDL SURFACE.
NOTE: Unfortunately, this function is too slow to use it in interactive graphics."
  (declare (type list pixels))
  (with-direct-pixel-access-raw surface buffer pitch
    (let ((pitch-uint32 (ash pitch -2)) ;pitch is in pixels, but we need it in :uint32.
	  (last-x (1- (sdl-surface-get-w surface)))
	  (last-y (1- (sdl-surface-get-h surface))))
      (declare (type fixnum pitch-uint32))
      (flet ((set-pixel (x y color)
	       (declare (type fixnum x y) (type (integer 0 4294967295) color))
	       (let ((index (+ (the fixnum (* y pitch-uint32)) x)))
		 (declare (type fixnum index))
		 (setf (cffi:mem-aref buffer :uint32 index) color))))
	(loop for (x y color) in pixels do
	     (assert (<= 0 x last-x))
	     (assert (<= 0 y last-y))
	     (set-pixel x y color))))))

(defun render-array (texture array x0 y0 texture-w texture-h)
  ;; TODO: Get TEXTURE-W and TEXTURE-H from calling SDL_QueryTexture. This doesn't work: (plus-c:c-fun sdl2-ffi::sdl-query-texture tex format access w h).
  "Render ARRAY at position X0, Y0 onto TEXTURE.
ARRAY must be a 2-dimensional array with the elements being the (32-bit ARGB) colors at the respective position.
TEXTURE must be a SDL-texture with flag :STREAMING.
X0 and Y0 must be integers.
NOTE: Unfortunately, this function is too slow to use it in interactive graphics."
  (let* ((h (array-dimension array 0))
	 (w (array-dimension array 1))
	 (surface (sdl2:create-rgb-surface w
					   h
					   32
					   :r-mask #x00ff0000
					   :g-mask #x0000ff00
					   :b-mask #x000000ff
					   :a-mask #xff000000)))
    (declare (type fixnum w h))
    (with-direct-pixel-access-raw surface pixels pitch
      (let ((pitch-uint32 (ash pitch -2))) ;pitch is in pixels, but we need it in :uint32.
	(declare (type fixnum pitch-uint32))
	(let ((ai 0))
	  (declare (type fixnum ai))
	  (loop for y fixnum below h
	     for mi fixnum = (* y pitch-uint32) do
	       (loop for x fixnum below w do
		    (setf (cffi:mem-aref pixels :uint32 mi)
			  (row-major-aref array ai))
		    (incf mi 1)
		    (incf ai 1))))
	;; TODO: Do I have to SDL_LockTexture the texture before calling SDL_UpdateTexture? (To answer this I should probably check if there is a call to SDL_LockTexture in the source code of SDL_UpdateTexture.)
	(sdl2:update-texture texture pixels
			     ;;:rect (sdl2:make-rect x0 y0 w h)
			     :rect (sdl2:make-rect x0 y0
						   (- (min texture-w (+ x0 w)) x0)
						   (- (min texture-h (+ y0 h)) y0))
			     :width pitch)))))

(defun software-render-texture (render-mode)
  "Software renderer example, drawing a texture on the screen.
See SDL-wiki/MigrationGuide.html#If_your_game_just_wants_to_get_fully-rendered_frames_to_the_screen."
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

      (let* ((wrend (sdl2:create-renderer win -1 '(:software :targettexture)))
	     (tex (sdl2:create-texture wrend :ARGB8888 :streaming 800 600))
	     (sur (sdl2:create-rgb-surface 800 600 32
					   :r-mask #x00ff0000
					   :g-mask #x0000ff00
					   :b-mask #x000000ff
					   :a-mask #xff000000))
	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
	     (pix (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels))
	     (first-frame-time (get-internal-real-time))
	     (num-frames 0)
	     (last-frame-time (float (/ (get-internal-real-time) internal-time-units-per-second))))
	(declare (type fixnum num-frames))

	(format t "Window renderer: ~A~%" wrend)
	(format t "Texture: ~A~%" tex)
	(format t "Surface: ~A~%" sur)
	(finish-output)

	(sdl2-ffi.functions:sdl-set-render-draw-color wrend 255 0 0 255)

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

	  (:idle
	   ()
	   (ecase render-mode
	     ((:local-function)
	      (with-safe-pixel-access sur set-pixel
		(loop for y below 600 do
		     (loop for x below 800 do
			  (let ((c (color-to-argb8888 #xff (logand (+ x y num-frames) 255) (logand (- x y) 255) 127)))
			    (set-pixel x y c)))))
	      (sdl2:update-texture tex pix :width (* 4 800)))
	     ((:list)
	      (let ((l nil))
		(loop for y fixnum below 600 do
		     (loop for x fixnum below 800 do
			  (push (list x y (color-to-argb8888 #xff (logand (+ x y num-frames) 255) (logand (- x y) 255) 0)) l)))
		(render-pixels-list sur l)
		(sdl2:update-texture tex pix :width (* 4 800))))
	     ((:array)
	      (let ((array (make-array '(600 800) :element-type 'integer)))
		;; TODO: FIXME: filling the array is very slow.
		(loop for y fixnum below 600 do
		     (loop for x fixnum below 800 do
			  (let* ((color (color-to-argb8888 #xff (logand (+ x y num-frames) 255) (logand (- x y) 255) 255)))
			    (setf (aref array y x) color))))
		(render-array tex array 0 0 800 600))))
	   ;;TODO: call SDL_RenderClear(sdlRenderer);
	   (sdl2:render-copy wrend tex)
	   (sdl2:render-present wrend)
	   (let ((now (float (/ (get-internal-real-time) internal-time-units-per-second))))
	     (format t "fps: ~A~%" (float (/ 1 (- now last-frame-time))))
	     (setf last-frame-time now))
	   (incf num-frames)
	   )

	  (:quit () t))

	(format t "End of main loop.~%")
	(format t "Average frames per second: ~A.~%" (float (/ num-frames (/ (- (get-internal-real-time) first-frame-time) internal-time-units-per-second))))
	(finish-output)
	))))
