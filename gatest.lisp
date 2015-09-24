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

(defmacro prind (&rest args)
  "Print args"
  (with-gensyms (i)
    `(progn
       (format t "~&")
       ,@(loop for a in args collect
	      (if (eq a T)
		  `(format t "~&")
		  `(progn
		     (format t "~A:" ,(format nil "~A" a))
		     (dolist (,i (multiple-value-list ,a))
		       (prin1 ,i)
		       (princ " "))))))))

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
	 (declare (type fixnum ,pitch-uint32 ,last-x ,last-y))
	 (flet ((,set-pixel-symbol (x y color)
		  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
			   (type fixnum x y) (type (integer 0 4294967295) color))
		  (assert (<= 0 x ,last-x))
		  (assert (<= 0 y ,last-y))
		  (let ((index (+ (the fixnum (* y ,pitch-uint32)) x)))
		    (declare (type fixnum index))
		    (setf (cffi:mem-aref ,pixels :uint32 index) color))))
	   ,@body)))))

(defun sample (seq)
  (let ((l (length seq)))
    (elt seq (random l))))

(defstruct (org
	     (:constructor make-org (gen ip x y energy)))
  gen
  ip
  x
  y
  energy)

(defparameter *world* (make-array '(200 150) :initial-element 500))
(defparameter *orgs*
  (let* ((org-1 (let ((g '(eat copy-left goto0)))
		  (make-array (length g) :initial-contents g))))
    (loop for i below 500 collect (make-org org-1 0 (random (array-dimension *world* 0)) (random (array-dimension *world* 1)) 10))))

(defun idleloop (surface)
  (declare (optimize (debug 3)))
  (labels ((copy-gen (gen)
	     (let* ((new-gen (make-array (length gen))))
	       (loop for g below (length gen) do
		    (setf (aref new-gen g)
			  (if (<= (random 1000) 1)
			      (sample '(eat copy-left copy-right copy-up copy-down goto0))
			      (aref gen g))))
	       new-gen))
	   (eval-org (iters org)
	     (let* ((offspring nil))
	       (with-slots (gen ip off x y energy) org
		 (loop for i below iters do
		      (when (or (>= ip (length gen)) (<= energy 0))
			(return-from eval-org :kill))
		      (let* ((ins (aref gen ip)))
			(decf energy)
			(ecase ins
			  ((eat)
			   (incf energy (aref *world* x y))
			   (setf (aref *world* x y) 0))
			  ((copy-left)
			   (let ((gen-off (copy-gen gen))
				 (new-energy (floor (/ energy 2))))
			     (push (make-org gen-off 0 (mod (1- x) (array-dimension *world* 0)) y new-energy) offspring)
			     (decf energy new-energy)))
			  ((copy-right)
			   (let ((gen-off (copy-gen gen))
				 (new-energy (floor (/ energy 2))))
			     (push (make-org gen-off 0 (mod (1+ x) (array-dimension *world* 0)) y new-energy) offspring)
			     (decf energy new-energy)))
			  ((copy-up)
			   (let ((gen-off (copy-gen gen))
				 (new-energy (floor (/ energy 2))))
			     (push (make-org gen-off 0 x (mod (1- y) (array-dimension *world* 1)) new-energy) offspring)
			     (decf energy new-energy)))
			  ((copy-down)
			   (let ((gen-off (copy-gen gen))
				 (new-energy (floor (/ energy 2))))
			     (push (make-org gen-off 0 x (mod (1+ y) (array-dimension *world* 1)) new-energy) offspring)
			     (decf energy new-energy)))
			  ((goto0)
			   (setf ip -1)))
			(incf ip))))
	       (values :survive offspring))))
    (let ((new-org nil))
      (loop for org in *orgs* do
	   (multiple-value-bind (status offspring) (eval-org 10 org)
	     (cond
	       ((eq status :kill)) ;do not transfer org to new-org
	       ((eq status :survive)
		(setf new-org (nconc (list org) offspring new-org)))
	       (t (error "bla")))))
      (setf *orgs* new-org)))
  (prind (length *orgs*))
  (let ((max-world-energy 0)
	(min-world-energy most-positive-fixnum)
	(avg-world-energy 0)
	(num-rain 10000))
    (loop for i below num-rain do
	 (let* ((x (random (array-dimension *world* 0)))
		(y (random (array-dimension *world* 1)))
		(e (aref *world* x y)))
	   (setf max-world-energy (max max-world-energy e))
	   (setf min-world-energy (min min-world-energy e))
	   (incf avg-world-energy e)
	   (incf (aref *world* x y))))
    (setf avg-world-energy (float (/ avg-world-energy num-rain)))
    (prind max-world-energy min-world-energy avg-world-energy))
  (with-safe-pixel-access surface set-pixel
    (let* ((w (sdl-surface-get-w surface))
	   (h (sdl-surface-get-h surface)))
      (loop for y below h do
	   (loop for x below w do
		(let* ((c (min 255 (ash (aref *world* x y) -2)))
		       (color (color-to-argb8888 255 c c c)))
		  (set-pixel x y color))))
      (loop for org in *orgs* do
	   (let* ((x (org-x org))
		  (y (org-y org))
		  (e (min 255 (org-energy org))))
	     (set-pixel x y (color-to-argb8888 255 e 0 0)))))))


(defun software-render-texture (&key (total-frames -1))
  "Software renderer example, drawing a texture on the screen.
See SDL-wiki/MigrationGuide.html#If_your_game_just_wants_to_get_fully-rendered_frames_to_the_screen."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :x 1000 :y 0 :w 800 :h 600 :flags '(:shown))
      ;; basic window/gl setup
      (format t "Setting up window: ~A (size:~A).~%" win (multiple-value-list (sdl2:get-window-size win)))
      (finish-output)

      (let* ((tex-w (array-dimension *world* 0)) ;texture size; can be different from window size above.
	     (tex-h (array-dimension *world* 1))
	     (wrend (sdl2:create-renderer win -1 '(:software :targettexture)))
	     (tex (sdl2:create-texture wrend :ARGB8888 :streaming tex-w tex-h))
	     (sur (sdl2:create-rgb-surface tex-w tex-h 32
					   :r-mask #x00ff0000
					   :g-mask #x0000ff00
					   :b-mask #x000000ff
					   :a-mask #xff000000))
	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
	     (pix (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels))
	     (first-frame-time (get-internal-real-time))
	     (num-frames 0))
	(declare (type fixnum num-frames))

	(format t "Window renderer: ~A~%" wrend)
	(format t "Texture: ~A~%" tex)
	(format t "Surface: ~A~%" sur)
	(finish-output)

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
	   (when (or (and (>= total-frames 0) (>= num-frames total-frames))
		     (null *orgs*))    
	     (sdl2:push-quit-event))
	   (idleloop sur)
	   (sdl2:update-texture tex pix :width (* 4 tex-w))
	   ;;TODO: call SDL_RenderClear(sdlRenderer);
	   (sdl2:render-copy wrend tex)
	   (sdl2:render-present wrend)
	   (incf num-frames)
	   )

	  (:quit () t))

	(format t "End of main loop.~%")
	(format t "Average frames per second: ~A.~%" (float (/ num-frames (/ (- (get-internal-real-time) first-frame-time) internal-time-units-per-second))))
	(finish-output)
	))))
