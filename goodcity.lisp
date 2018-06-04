;; Max flow algorithm to model traffic of the streets in a city.

;; Instead of the max flow algorithm, which finds the optimal flows for each vertex in the street graph, we could try to solve the problem differently. We could try a "distributed" algorithm, that tries to select the street that should be taken according to the following: 1. if a car is in a street, it drives with its preferred speed along the street 2. if a car arrives at a junction it selects one of the other streets connected to this junction by applying function #'SELECT. #'SELECT receives a list of outgoing streets, where each outgoing street has the following parameters: 1. length of the street 2. capacity of the street (number of outgoing lanes) 3. current flow in the outgoing lane 4. minimal distance between the end of the outgoing street (i.e. the next junction if the car selects this outgoing street) and the target of the car. #'SELECT returns the street that the car should take. #'SELECT could also output a list of probabilities with which the car should take each street.

;; The Goldberg-Rao max-flow-algorithm talks about strongly connected components. This gave me the idea of letting the player define pedestrian areas, within which no car is allowed and all transportation inside is done by foot. In the max-flow algorithm the pedestrian area is considered as a single node, and the cut of the pedestrian area with the former street network defines the edges to and from this new pedestrian-area-node. In the late-game, this could reduce the computation time of the max-flow-algorithm.

;; There will be police cars, firetrucks, and ambulances racing through the city and if they cannot reach their destination in time it will have consequences for the player. For example a fire will start, the health of the citizens will decrease, or crime will increase in the unreached destination.

;; Each citizen will need one or many income sources (i.e. jobs), a market or supermarket where they can get food and other products from, a house where they sleep, once in a while (preferably on weekends) a recreational trip to a "green" area (distant via railway/airplane/autobahn or local parks). There there will be industrial areas (for products), food production areas (for food), commercial areas (for markets, supermarkets, and general stores), housing areas (for huts, tents, houses, skyscrapers where citizens can build and own or rent a flat), they will own money, have spouses and kids, there will be schools where kids go to every day. Paths across heavily car-frequented streets inreases the probability of an accident which will spawn a police car and ambulance. From time to time, there will be a demonstration, which will block traffic on some streets completely. From time to time, a very important person (VIP) will have to be transferred from point A to point B in the city (for example from the airport to a hotel in the city center), and the player must provide a routing through the streets with little crime, and traffic in these streets will be blocked for the duration of the travel. There may be demonstrations against the VIP, blocking further streets near the route. From time to time, there may be non-orderly demonstrations lasting longer than a day, blocking places (including intersections and their streets in the city center).

(load "~/quicklisp/setup.lisp")
(ql:quickload :sdl2)
(ql:quickload :cl-heap)

(declaim (optimize (debug 3)))

;;;; Game

(defun euclidean-distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defclass coordinate-mixin ()
  ((x :initarg :x :accessor x-of)
   (y :initarg :y :accessor y-of)))

(defclass event-mixin ()
  ((time :initarg :time :accessor time-of)))

(defgeneric time-of (event)
  (:documentation "Returns the time when the EVENT will (or did) take place."))

(defclass game ()
  ((junctions :initform nil :accessor junctions-of)
   (streets :initform nil :accessor streets-of)
   (traffic :initform nil :accessor traffic-of)
   (events :initform (make-instance 'cl-heap:fibonacci-heap :key #'time-of :sort-fun #'<) :reader events-of)))

(defclass junction (coordinate-mixin)
  ())

(defmethod print-object ((junction junction) stream)
  (print-unreadable-object (junction stream :type t :identity t)
    (format stream "x:~S y:~S" (x-of junction) (y-of junction))))

(defclass street ()
  ((start :initarg :start :accessor start-of)
   (end :initarg :end :accessor end-of)))

(defmethod length-of ((street street))
  (let ((start (start-of street))
	(end (end-of street)))
    (euclidean-distance (x-of start) (y-of start) (x-of end) (y-of end))))

(defmethod print-object ((street street) stream)
  (print-unreadable-object (street stream :type t :identity t)
    (format stream "start:~S end:~S" (start-of street) (end-of street))))

(defclass event-traffic-arrives (event-mixin)
  ((start-time :initarg :start-time :reader start-time-of)
   (traffic :initarg :traffic :accessor traffic-of)
   (street :initarg :street :accessor street-of)))

(defclass traffic ()
  ((speed :initarg :speed :reader speed-of)
   (next-event :accessor next-event-of)))

(defmethod print-object ((traffic traffic) stream)
  (print-unreadable-object (traffic stream :type t :identity t)
    (format stream "speed:~S" (speed-of traffic))
#|    (let ((event (next-event-of traffic)))
      (when (and event (typep event 'event-traffic-arrives))
	(format stream " street:~S" (street-of event))))|#))

(defmethod position-at-time ((traffic traffic) time)
  (let ((event (next-event-of traffic)))
    (unless (null event)
      (let* ((arrival-time (time-of event))
	     (street (street-of event))
	     (start (start-of street))
	     (end (end-of street))
	     (x1 (x-of start))
	     (y1 (y-of start))
	     (x2 (x-of end))
	     (y2 (y-of end))
	     (start-time (start-time-of event)))
	(assert (<= start-time arrival-time))
	(cond
	  ((<= time start-time) (values x1 y1))
	  ((>= time arrival-time) (values x2 y2))
	  (t
	   (let* ((total-time (- arrival-time start-time))
		  (total-time (if (= 0 total-time) 1 total-time))
		  (fraction (/ (- time start-time) total-time)))
	     (values (+ x1 (* (- x2 x1) fraction)) (+ y1 (* (- y2 y1) fraction))))))))))

(defun find-junction (game x y)
  (let ((junctions (junctions-of game)))
    (unless (null junctions)
      (let* ((min-junction (car junctions))
	     (min-dist (euclidean-distance (x-of min-junction) (y-of min-junction) x y)))
	(loop for junction in (cdr junctions) do
	     (let ((dist (euclidean-distance (x-of junction) (y-of junction) x y)))
	       (when (< dist min-dist)
		 (setf min-dist dist
		       min-junction junction))))
	(values min-junction min-dist)))))

(defun sample (sequence)
  (let* ((length (length sequence)))
    (if (= 0 length)
	nil
	(elt sequence (random length)))))

(defun outgoing-streets (game junction)
  (remove-if (lambda (street) (not (eql (start-of street) junction))) (streets-of game)))

(defmethod schedule-event (game event (traffic traffic))
  (setf (next-event-of traffic) event)
  (cl-heap:add-to-heap (events-of game) event))

(defmethod handle-event (game (event event-traffic-arrives))
  (let* ((now (time-of event))
	 (traffic (traffic-of event))
	 (street (street-of event))
	 (junction (end-of street))
	 (outgoing (outgoing-streets game junction)))
    (let* ((new-street (sample outgoing))
	   (arrival (+ now (/ (length-of new-street) (speed-of traffic))))
	   (new-event (make-instance 'event-traffic-arrives
				     :traffic traffic :street new-street :start-time now :time arrival)))
      (format t "now:~S traffic ~S travelled ~S going along ~S~%" now traffic street new-street)
      (schedule-event game new-event traffic))))

(defmethod process-events (game now)
  (do () ((let* ((event (cl-heap:peep-at-heap (events-of game))))
	    (or (null event) (> (time-of event) now))))
    (let ((event (cl-heap:pop-heap (events-of game))))
      (handle-event game event))))

;;;; SDL functions

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

(defvar *game* (make-instance 'game))

(defun software-render-texture (&key (window-x 212) (window-y 0) (window-width 800) (window-height 400))
  "Software renderer example, drawing a texture on the screen.
See SDL-wiki/MigrationGuide.html#If_your_game_just_wants_to_get_fully-rendered_frames_to_the_screen."
  (declare (optimize (debug 3)))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (sdl2:with-window (window :x window-x :y window-y :w window-width :h window-height :flags '(:shown))
      ;; basic window/gl setup
      (format t "Setting up window: ~A (size:~A).~%" window (multiple-value-list (sdl2:get-window-size window)))
      (finish-output)
      (let* ((texture-width window-width) ;texture size; can be different from window size above.
	     (texture-height window-height)
	     (renderer (sdl2:create-renderer window -1 '(:software :targettexture)))
	     (texture (sdl2:create-texture renderer :ARGB8888 :streaming texture-width texture-height))
	     (surface (sdl2:create-rgb-surface texture-width texture-height 32 :r-mask #x00ff0000 :g-mask #x0000ff00 :b-mask #x000000ff :a-mask #xff000000))
	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
	     (frames 0)
	     (mouse-buttons nil)
	     (game *game*)
	     (mouse-mode :create-junction)
	     (create-stack nil)
	     (first-frame-time (get-internal-run-time))
	     (last-drawing-time first-frame-time)
	     )
	(unwind-protect
	     (progn
	       (format t "Window renderer: ~A~%" renderer)
	       (format t "Texture: ~A~%" texture)
	       (format t "Surface: ~A~%" surface)
	       (finish-output)
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
		    (cond
		      ((sdl2:scancode= scancode :scancode-1)
		       (setf mouse-mode :create-junction))
		      ((sdl2:scancode= scancode :scancode-2)
		       (setf mouse-mode :create-street-start))
		      ((sdl2:scancode= scancode :scancode-3)
		       (setf mouse-mode :create-traffic))
		      )
		    (setf create-stack nil)
		    (format t "Mouse mode: ~S~%" mouse-mode)
		    ;;(format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)
		    ))
		 (:keyup
		  (:keysym keysym)
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		    (sdl2:push-event :quit)))
		 (:mousebuttondown
		  (:x x :y y :state state)
		  (declare (ignore x y))
		  ;;(format t ":mousebuttondown x:~S y:~S state:~S~%" x y state)
		  (setf mouse-buttons state))
		 (:mousebuttonup
		  (:x x :y y :state state)
		  (declare (ignore state))
		  ;;(format t ":mousebuttonup x:~S y:~S state:~S~%" x y state)
		  (format t "x:~S y:~S buttons(up):~S~%" x y mouse-buttons)
		  (let ((now (/ (get-internal-run-time) internal-time-units-per-second)))
		    (cond
		      ((and (<= 0 x (1- texture-width)) (<= 0 y (1- texture-height)) (= mouse-buttons 1))
		       (format t "x:~S y:~S mouse-mode:~S create-stack:~S~%" x y mouse-mode create-stack)
		       (ecase mouse-mode
			 ((:create-junction)
			  (let ((junction (make-instance 'junction :x x :y y)))
			    (push junction (junctions-of game))
			    (format t "created junction ~S~%" junction)))
			 ((:create-street-start)
			  (let ((start (find-junction game x y)))
			    (push start create-stack))
			  (setf mouse-mode :create-street-end))
			 ((:create-street-end)
			  (let ((start (first create-stack))
				(end (find-junction game x y)))
			    (unless (eql end start)
			      (let ((street (make-instance 'street :start start :end end)))
				(push street (streets-of game))
				(format t "created street ~S~%" street))
			      (let ((street (make-instance 'street :start end :end start)))
				(push street (streets-of game))
				(format t "created street ~S~%" street))
			      (setf mouse-mode :create-street-start
				    create-stack nil))))
			 ((:create-traffic)
			  (let ((junction (find-junction game x y)))
			    (unless (null junction)
			      (let* ((traffic (make-instance 'traffic :speed 5))
				     (street (sample (outgoing-streets game junction)))
				     (arrival (+ now (/ (length-of street) (speed-of traffic))))
				     (arrival-event (make-instance 'event-traffic-arrives :street street :traffic traffic :start-time now :time arrival)))
				(push traffic (traffic-of game))
				(schedule-event game arrival-event traffic)
				(format t "created traffic ~S~%" traffic)))))
			 ))))
		  (force-output))
		 (:idle
		  ()
		  (let* ((now (/ (get-internal-run-time) internal-time-units-per-second))
			 (elapsed-time (let* ((diff (- now last-drawing-time)))
					 (prog1 (if (= 0 diff) .0001 diff)
					   (setf last-drawing-time now)))))
		    (declare (ignore elapsed-time))
		    (process-events game now)
		    (with-safe-pixel-access surface set-pixel
		      (let* ((width (sdl-surface-get-w surface))
			     (height (sdl-surface-get-h surface)))
			(loop for y below height do
			     (loop for x below width do
				  (set-pixel x y (color-to-argb8888 255 96 255 96))))
			(loop for junction in (junctions-of game) do
			     (flet ((safe-set-pixel (x y color)
				      (when (and (>= x 0) (< x width) (>= y 0) (< y height))
					(set-pixel x y color))))
			       (let ((x (1- (x-of junction))) (y (y-of junction)))
				 (safe-set-pixel x y (color-to-argb8888 255 64 64 64)))
			       (let ((x (x-of junction)) (y (1- (y-of junction))))
				 (safe-set-pixel x y (color-to-argb8888 255 64 64 64)))
			       (let ((x (1+ (x-of junction))) (y (y-of junction)))
				 (safe-set-pixel x y (color-to-argb8888 255 64 64 64)))
			       (let ((x (x-of junction)) (y (1+ (y-of junction))))
				 (safe-set-pixel x y (color-to-argb8888 255 64 64 64)))
			       (set-pixel (x-of junction) (y-of junction) (color-to-argb8888 255 64 64 64))))
			(loop for traffic in (traffic-of game) do
			     (multiple-value-bind (x y) (position-at-time traffic now)
			       (unless (null x)
				 ;;(format t "traffic x:~S y:~S~%" x y)
				 (let ((x (floor x)) (y (floor y)))
				   (let ((x (1- x)) (y (1- y))) (when (and (<= 0 x (1- texture-width)) (<= 0 y (1- texture-height))) (set-pixel x y (color-to-argb8888 255 0 0 255))))
				   (let ((x (1+ x)) (y (1- y))) (when (and (<= 0 x (1- texture-width)) (<= 0 y (1- texture-height))) (set-pixel x y (color-to-argb8888 255 0 0 255))))
				   (let ((x (1- x)) (y (1+ y))) (when (and (<= 0 x (1- texture-width)) (<= 0 y (1- texture-height))) (set-pixel x y (color-to-argb8888 255 0 0 255))))
				   (let ((x (1+ x)) (y (1+ y))) (when (and (<= 0 x (1- texture-width)) (<= 0 y (1- texture-height))) (set-pixel x y (color-to-argb8888 255 0 0 255))))))))
			)
		      (sdl2:update-texture texture (plus-c:c-ref surface SDL2-FFI:SDL-SURFACE :pixels) :width (* 4 texture-width)))
		    ;;TODO: call SDL_RenderClear(sdlRenderer);
		    (sdl2:render-copy renderer texture)
		    (sdl2-ffi.functions::sdl-set-render-draw-color renderer 128 128 128 255)
		    (loop for street in (streets-of game) do
			 (let ((start (start-of street))
			       (end (end-of street)))
			   (sdl2-ffi.functions::sdl-render-draw-line renderer (x-of start) (y-of start) (x-of end) (y-of end))))
		    (sdl2:render-present renderer)
		    (incf frames)
		    (format t "frames:~S fps:~S events:~S~%" frames (float (/ frames (/ (- (get-internal-run-time) first-frame-time) internal-time-units-per-second))) (cl-heap:heap-size (events-of game)))
		    (force-output)
		    ))
		 (:quit () t))
	       ;;(sb-sprof:stop-profiling)
	       ;;(sb-sprof:report)
	       (format t "End of main loop.~%")
	       (format t "Average frames per second: ~A.~%" (float (/ frames (/ (- (get-internal-run-time) first-frame-time) internal-time-units-per-second))))
	       (finish-output)))
	(setf *game* game)))))
