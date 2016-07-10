;; I should let the organisms play games against each other. The games could be created randomly. A game has state (a fixed number of variables), and rules when and in what format it accepts inputs from the players, and what state these inputs change (input=function call). It also has a rule that describes what state the game has to be in so that player 1 wins, player 2 wins, etc. (or maybe an ordering of the players). The game should be fair, i.e. the game should work the same way if the order of players is permuted. There even could be games that have only one player, like puzzle-games (but how to generate them automatically?) A game could be implemented as a finite state machine.

(defparameter *default-random-state* (make-random-state nil)) ;save default random state using defparameter, this way it will only be evaluated once. Then, when we want to reset the state, we can copy *default-random-state* and use it as the new state.

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :sdl2)
(ql:quickload :alexandria)
(use-package :alexandria)

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
		  (declare (optimize (speed 3) (safety 3) (debug 0) (compilation-speed 0))
			   (type fixnum x y) (type (integer 0 4294967295) color))
		  (assert (<= 0 x ,last-x))
		  (assert (<= 0 y ,last-y))
		  (let ((index (+ (the fixnum (* y ,pitch-uint32)) x)))
		    (declare (type fixnum index))
		    (setf (cffi:mem-aref ,pixels :uint32 index) color))))
	   ,@body)))))

(defun reset-random-state ()
  "Set the random state to the default random state."
  (setf *random-state* (make-random-state *default-random-state*)))

(defun sample (seq)
  (let ((l (length seq)))
    (elt seq (random l))))

(defun arefd (array default &rest subscripts)
  (loop
     for i from 0
     for s in subscripts do
       (let ((d (1- (array-dimension array i))))
	 (if (not (<= 0 s d)) (return-from arefd default))))
  (apply #'aref array subscripts))

(defparameter *id* 0 "The identification number of the last organism made")

(defvar *world-max-energy* 4000)
(defun make-world (w h energy)
  (make-array (list w h) :initial-element energy))
(defparameter *default-world* (make-world 200 100 50))
(defstruct world-cloud
  x y xvel yvel edge drop-num drop-amount)
(defun make-clouds (rain-per-coordinate num-clouds fraction-covered drop-amount world-w world-h velocity)
  "RAIN-PER-COORDINATE is the average rain per world coordinate per iteration.
FRACTION-COVERED is the fraction of the whole world covered with clouds.
DROP-AMOUNT is the energy per drop."
  (let* ((cloud-edge (round (sqrt (/ (* world-w world-h fraction-covered) num-clouds))))
	 (total-rain (* world-w world-h rain-per-coordinate))
	 (rain-size (round total-rain num-clouds))
	 (rains0 (loop for i below num-clouds collect (random rain-size)))
	 (rains (let ((sum (apply #'+ rains0))) (loop for r in rains0 collect (round (* r (/ total-rain sum)))))))
    (format t "~&rains:~A~%" rains)
    (loop for r in rains collect
	 (let* ((drop-num (round (/ r drop-amount)))
		(cloud (make-world-cloud
			:x (random world-w)
			:y (random world-h)
			:xvel (- (random velocity) (/ velocity 2))
			:yvel (- (random velocity) (/ velocity 2))
			:drop-num (max drop-num 1)
			:drop-amount drop-amount
			:edge cloud-edge)))
	   (prind r cloud)
	   cloud))))
(defvar *world* (make-world 200 100 50))
(defvar *world-clouds* (make-clouds .025 3 .25 30 (array-dimension *world* 0) (array-dimension *world* 1) .1))

;; load organism implementation
(load "~/lisp/gatest-orgap-lisp.lisp")
;;(load "~/lisp/gatest-orgap-lightning.lisp")
      
(defstruct orgcont ;organism container
  orgap
  id
  age
  totage
  noffspring)

(defun copy-orgs (orgs)
  (mapcar (lambda (orgcont)
	    (with-slots (orgap id age totage noffspring) orgcont
	      (make-orgcont :orgap (copy-orgap orgap) :id id :age age :totage totage :noffspring noffspring)))
	  orgs))

(defun make-default-orgs (num energy &optional
				       (genes '(set-bs-nil eat in-energy-left-an in-energy-right-bn sub-from-an-bn mrk0 read-as read-next write-as cmp-as-bs jne0 set-an-1 set-bn-1 add-to-bn-an mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn split-cell-an wait-an walk-x-an walk-y-an goto0))
				       )
  (loop for i below num collect
       (let* ((orgap (make-orgap genes (random (array-dimension *world* 0)) (random (array-dimension *world* 1)) energy))
	      (orgcont (make-orgcont :orgap orgap :id (incf *id*) :age 0 :totage 0 :noffspring 0)))
	 (when (= 0 (orgap-code-length orgap))
	   (error "Organism ~A has code length 0" orgap))
	 orgcont)))
(defparameter *default-orgs* (make-default-orgs 50 500))
(defvar *orgs* (copy-orgs *default-orgs*))

(defun default-world (&key (reset-random-state t))
  (when reset-random-state
    (reset-random-state))
  (setf *world-max-energy* 4000)
  (setf *world* (copy-array *default-world*))
  (setf *orgs* (copy-orgs *default-orgs*))
  nil)

(defun world1 (&key (w 200) (h 100) (world-energy 50) (orgs 50) (org-energy 500) (reset-random-state t))
  (when reset-random-state
    (reset-random-state))
  (setf *world* (make-world w h world-energy))
  (setf *orgs* (make-default-orgs orgs org-energy))
  nil)

(defparameter *display-world* t)
(defparameter *world-iterations* 0)

(defun orgcont-noff/tage (orgcont)
  (let ((totage (orgcont-totage orgcont))
	(noffspring (orgcont-noffspring orgcont)))
    (when (> totage 0) (float (/ noffspring totage)))))

(defun print-orgcont (orgcont)
  (with-slots (orgap id age totage noffspring) orgcont
    (format t "genes:~A~%" (orgap-genes orgap))
    (format t "org id:~5A wait:~4A energy:~4A age:~3A/~A x:~3A y:~3A ip:~3A/~3A off(~2A)/tage:~4A~%"
	    id (orgap-wait orgap) (orgap-energy orgap) age totage (orgap-x orgap) (orgap-y orgap) (orgap-ip orgap) (orgap-code-length orgap)
	    noffspring (orgcont-noff/tage orgcont))))

(defun print-org-stats (orgs)
  (let ((max-org-energy 0)
	(avg-org-energy 0)
	(max-org-noff/tage 0)
	(length-orgs (length orgs)))
    (loop for org in orgs do
	 (let* ((orgap (orgcont-orgap org))
		(e (orgap-energy orgap)))
	   (setf max-org-energy (max max-org-energy e))
	   (incf avg-org-energy e)
	   (setf max-org-noff/tage (max max-org-noff/tage (let ((noff/tage (orgcont-noff/tage org))) (if noff/tage noff/tage 0))))))
    (setf avg-org-energy (float (/ avg-org-energy length-orgs)))
    (format t "~A (org num:~5A energy avg:~5A max:~5A noff/tage max:~4F)~%"
	    *world-iterations* length-orgs (round avg-org-energy) max-org-energy max-org-noff/tage)))

(defun idleloop (surface cursor)
  (declare (optimize (debug 3)))
  (incf *world-iterations*)
  (let ((next-loop-orgs nil))
    ;; TODO: FIXME: Make clouds and organisms conceptually to be event sources so that they have a wait time (i.e. iterations until their next event must be computed). then rewrite the following loop so that always the event source with the least wait time is computed next. Maybe use a heap tree to keep track of the minimum.
    (loop for org in *orgs* do
	 (when (eq org cursor)
	   (print-orgcont org))
	 (let ((orgap (orgcont-orgap org)))
	   (incf (orgcont-age org))
	   (incf (orgcont-totage org))
	   ;;(prind org)
	   (multiple-value-bind (status offspring) (eval-orgap 200 orgap)
	     (when offspring
	       (incf (orgcont-noffspring org) (length offspring))
	       (setf (orgcont-age org) 0))
	     (setf next-loop-orgs
		   (nconc (if (and (eq status :survive) (< (orgcont-age org) 200000))
			      (list org)
			      nil)
			  (loop for orgap in offspring collect (make-orgcont :orgap orgap :id (incf *id*) :age 0 :totage 0 :noffspring 0))
			  next-loop-orgs)))))
    (setf *orgs* next-loop-orgs)
    (let ((max-world-energy 0)
	  (min-world-energy most-positive-fixnum)
	  (avg-world-energy 0)
	  (total-rain 0))
      (loop for cloud in *world-clouds* do
	   (with-slots (x y xvel yvel edge drop-num drop-amount) cloud
	     (incf total-rain drop-num)
	     (loop for i below drop-num do
		  (let* ((rx (mod (+ (floor x) (random edge)) (array-dimension *world* 0)))
			 (ry (mod (+ (floor y) (random edge)) (array-dimension *world* 1)))
			 (e (aref *world* rx ry)))
		    (setf max-world-energy (max max-world-energy e))
		    (setf min-world-energy (min min-world-energy e))
		    (incf avg-world-energy e)
		    (let ((new-e (min *world-max-energy* (+ e drop-amount))))
		      (setf (aref *world* rx ry) new-e))))
	     (setf x (mod (+ x xvel) (array-dimension *world* 0)))
	     (setf y (mod (+ y yvel) (array-dimension *world* 1)))))
      (setf avg-world-energy (float (/ avg-world-energy total-rain)))
      ;;(prind length-orgs max-world-energy min-world-energy avg-world-energy max-org-energy avg-org-energy)
      )
    (when (and (> (length *orgs*) 0) (or (null cursor) (eq cursor :no-output)))
      (print-org-stats *orgs*)))
  (when *display-world*
    (with-safe-pixel-access surface set-pixel
      (let* ((w (sdl-surface-get-w surface))
	     (h (sdl-surface-get-h surface)))
	(loop for y below h do
	     (loop for x below w do
		  (let* ((c (min 255 (ash (aref *world* x y) -2)))
			 (color (color-to-argb8888 255 c c c)))
		    (set-pixel x y color))))
	(loop for org in *orgs* do
	     (let* ((orgap (orgcont-orgap org))
		    (x (orgap-x orgap))
		    (y (orgap-y orgap))
		    (e (min 255 (orgap-energy orgap))))
	       (if (= (orgcont-age org) 0)
		   (set-pixel x y (color-to-argb8888 255 255 0 0))
		   (set-pixel x y (color-to-argb8888 255 0 e (max 128 e))))))))))

(defun nearest-orgap (x y orgs)
  "Return the organism in ORGS which is nearest to coordinate (X, Y)."
  (let ((min-dist nil)
	(min-org))
    (loop for org in orgs do
	 (let* ((orgap (orgcont-orgap org))
		(org-x (orgap-x orgap))
		(org-y (orgap-y orgap))
		(diff-x (- x org-x))
		(diff-y (- y org-y))
		(dist (+ (* diff-x diff-x) (* diff-y diff-y))))
	   (when (or (null min-dist) (< dist min-dist))
	     (setf min-dist dist min-org org))))
    min-org))

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
	     (mouse-x (/ tex-w 2))
	     (mouse-y (/ tex-h 2))
	     (cursor nil)
	     (display-random-state (make-random-state t)))
	(declare (type fixnum num-frames))

	(format t "Window renderer: ~A~%" wrend)
	(format t "Texture: ~A~%" tex)
	(format t "Surface: ~A~%" sur)
	(finish-output)

	;; main loop
	(format t "Beginning main loop.~%")
	(when (= 0 (length *orgs*))
	  (format t "No organisms left! Starting with default organisms.~%")
	  (setf *orgs* (copy-orgs *default-orgs*)))
	(finish-output)

	;;(sb-sprof:reset)
	;;(sb-sprof:start-profiling :mode :cpu) ;will profile all threads.

	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   (let ((scancode (sdl2:scancode-value keysym))
		 (sym (sdl2:sym-value keysym))
		 (mod-value (sdl2:mod-value keysym)))
	     (cond
	       ((sdl2:scancode= scancode :scancode-o) ;overview
		(setf cursor nil))
	       ((sdl2:scancode= scancode :scancode-d) ;display world
		(setf *display-world* (not *display-world*))))
	     
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
		(format t "Mouse motion abs(rel): ~a, ~a~%Mouse buttons: ~a~%" x y buttons)
		(setf mouse-x (/ (* x tex-w) win-w) mouse-y (/ (* y tex-h) win-h))
		(let* ((x (floor mouse-x)) (y (floor mouse-y))
		       (org (nearest-orgap x y *orgs*)))
		  (setf cursor org))))))

	  (:idle
	   ()
	   (if (or (and (>= frames 0) (>= num-frames frames))
		   (null *orgs*))    
	       (sdl2:push-quit-event)
	       (progn
		 (idleloop sur cursor)
		 (when *display-world*
		   (sdl2:update-texture tex (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels) :width (* 4 tex-w))
		   ;;TODO: call SDL_RenderClear(sdlRenderer);
		   (sdl2:render-copy wrend tex)
		   (let ((*random-state* display-random-state))
		     (sdl2-ffi.functions::sdl-set-render-draw-color wrend 255 (random 256) (random 256) 255))
		   (cond
		     ((and (orgcont-p cursor) (> (orgap-energy (orgcont-orgap cursor)) 0))
		      (let* ((orgap (orgcont-orgap cursor))
			     (x (orgap-x orgap)) (y (orgap-y orgap))
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
			(sdl2-ffi.functions::sdl-render-draw-line wrend (1- x1) (1+ y2) (1- x1) (1- y1))))
		     (t (setf cursor nil)))
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

(defun without-graphics (&optional (skip-lines 1))
  (let ((*display-world* nil))
    (loop for frame from 0 do
	 (idleloop nil (if (= (mod frame skip-lines) 0) :no-output :no-output-really)))))

;; this one is pretty good, it can sustain 1300 organisms. It was evolved in about 3-4 hours, the clouds were changed about 3 times: (genes '(IN-ENERGY-LEFT-AN SUB-FROM-BN-AN WALK-X-BN EAT MRK0 READ-AS READ-NEXT WRITE-AS CMP-AS-BS JNE0 SET-AN-1 SETF-BN-MAX-AN-BN WALK-Y-AN ADD-TO-AN-BN MUL-TO-AN-BN EAT MUL-TO-AN-BN MUL-TO-AN-BN MUL-TO-AN-BN MUL-TO-BN-AN MUL-TO-AN-BN WALK-Y-AN SPLIT-CELL-AN ADD-TO-BN-AN SETF-AS-AN-GT0 EAT GOTO0 SIGN-AN)), or is there a bug because the organism with the highest energy has about 14.8 million energy, and the average energy is 600000. when initializing the world with organisms having the original (hand-written) genes and simulating it until it has reached a stable population of about 300-700 organisms, and then adding 50 evolved organisms, they out-compete the stable population in a short time. Also see the organisms commented out in #'MAKE-DEFAULT-ORGS.
