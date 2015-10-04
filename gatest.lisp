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
  ;; TODO: modify the pretty print dispatch table so that it prints representations readable by #'READ. (especially modify the table so that printing a float respects *print-base*.)
  (with-gensyms (i)
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

(defun arefd (array default &rest subscripts)
  (loop
     for i from 0
     for s in subscripts do
       (let ((d (1- (array-dimension array i))))
	 (if (not (<= 0 s d)) (return-from arefd default))))
  (apply #'aref array subscripts))

(defstruct (org
	     (:constructor make-org* (id genes code ip wait x y energy age off as bs an bn genesx)))
  id
  genes
  code
  ip
  wait
  x
  y
  energy
  age
  off
  as
  bs
  an
  bn
  genesx)

(defun compile-genes (genes &optional (default-code (make-array 0)))
  ;;(declare (optimize (debug 3)))
  (labels ((return-defaults ()
	     (return-from compile-genes (values default-code))))
    (multiple-value-bind (code markers)
	(let ((markers nil)
	      (code nil)
	      (code-ip 0))
	  (loop do
	       (labels ((next ()
			  (if (null genes)
			      (let* ((rev (nreverse code))
				     (new (make-array (length rev) :initial-contents rev)))
				(return (values new markers)))
			      (let ((a (car genes)))
				(setf genes (cdr genes))
				a))))
		 (let* ((g (next))
			(gs (string g)))
		   ;;(prind g code-ip)
		   (cond
		     ((eq (elt gs 0) #\M)
		      (let* ((rest (subseq gs 1))
			     (pos (assoc rest markers)))
			(when (null pos)
			  (let ((label (read-from-string rest)))
			    (when (not (typep label 'fixnum))
			      (return-defaults))
			    (push (cons label code-ip) markers)))))
		     (t
		      (push g code)
		      (incf code-ip)))))))
      ;;(prind code)
      ;;(prind markers)
      (let ((max-ip (1- (array-dimension code 0))))
	(do* ((ip 0 (1+ ip)))
	     ((> ip max-ip) nil)
	  (let ((ins (aref code ip)))
	    ;;(prind ip ins (array-dimension code 0))
	    (case ins
	      ((jne)
	       (incf ip)
	       (when (> ip max-ip)
		 (return-defaults))
	       (let* ((whole-label (string (arefd code 'X ip)))
		      (label (read-from-string (subseq whole-label 1)))
		      (marker (assoc label markers)))
		 (when (or (not (eq (elt whole-label 0) #\L)) (not (typep label 'fixnum)) (null marker))
		   ;;(prind ip ins whole-label label marker markers)
		   (return-defaults))
		 (setf (aref code ip) (cdr marker))))))))
      (values code))))
	     
(defparameter *id* 0)

(defun make-org (genes x y energy)
  (incf *id*)
  (multiple-value-bind (code) (compile-genes genes)
    (make-org* *id* genes code 0 0 x y energy 0 nil nil nil 0 0 genes)))

(defparameter *world-max-energy* 4000)
(defun make-world (w h energy)
  (make-array (list w h) :initial-element energy))
(defparameter *default-world* (make-world 200 150 50))
(defparameter *world* (copy-array *default-world*))
(defstruct world-cloud
  x y xvel yvel rain edge)
(defun make-clouds (world-rain num-clouds fraction-covered world-w world-h velocity)
  "WORLD-RAIN is the average rain per world coordinate per iteration.
FRACTION-COVERED is the fraction of the whole world covered with clouds."
  (let* ((cloud-edge (round (sqrt (/ (* world-w world-h fraction-covered) num-clouds))))
	 (total-rain (* world-w world-h world-rain))
	 (rain-size (round total-rain num-clouds))
	 (rains0 (loop for i below num-clouds collect (random rain-size)))
	 (rains (let ((sum (apply #'+ rains0))) (loop for r in rains0 collect (round (* r (/ total-rain sum)))))))
    (loop for r in rains collect
	 (make-world-cloud
	  :x (random world-w)
	  :y (random world-h)
	  :xvel (- (random velocity) (/ velocity 2))
	  :yvel (- (random velocity) (/ velocity 2))
	  :rain r
	  :edge cloud-edge))))
(defparameter *world-clouds* (make-clouds .1 3 .25 (array-dimension *world* 0) (array-dimension *world* 1) .1))

(defun make-default-orgs (num energy &optional (genes '(eat in-energy-left-an in-energy-right-bn sub-bn-an walk-an m0 read-as read-next write-as cmp-as-bs jne l0 off-up goto0)))
  (loop for i below num collect
       (make-org genes (random (array-dimension *world* 0)) (random (array-dimension *world* 1)) energy)))
(defparameter *default-orgs* (make-default-orgs 50 500))
(defparameter *orgs* *default-orgs*)

(defun copy-orgs (orgs)
  (mapcar #'copy-org orgs))

(defun default-world ()
  (setf *world-max-energy* 4000)
  (setf *world* (copy-array *default-world*))
  (setf *orgs* (copy-orgs *default-orgs*))
  nil)

(defun world1 (&key (w 200) (h 150) (world-energy 500) (orgs 500) (org-energy 10))
  (setf *world* (make-world w h world-energy))
  (setf *orgs* (make-default-orgs orgs org-energy))
  nil)

(defun idleloop (surface cursor)
  (declare (optimize (debug 3)))
  (let ((new-org nil)
	(max-org-energy 0)
	(avg-org-energy 0)
	(length-orgs (length *orgs*))
	(num-died 0)
	(num-new 0))
    (labels ((eval-org (iters org)
	       (with-slots (genes code ip wait x y energy age off as bs an bn genesx) org
		 (let* ((offspring nil)
			(max-ip (1- (array-dimension code 0)))
			(world-w (array-dimension *world* 0))
			(world-h (array-dimension *world* 1)))
		   (labels ((die ()
			      ;;(prind "dies" org)
			      (incf num-died)
			      (return-from eval-org (values :kill offspring)))
			    (cell-division (off-x off-y off-energy)
			      (setf off (nreverse off))
			      (setf off-energy (min energy (max (floor off-energy) 0)))
			      (setf off-x (mod off-x world-w))
			      (setf off-y (mod off-y world-h))
			      ;;(format t "cell-division x:~A y:~A energy:~A off:~A~%" off-x off-y off-energy off)
			      (push (make-org off off-x off-y off-energy) offspring)
			      (push (make-org off x y (- energy off-energy)) offspring)
			      (setf energy 0)
			      (incf num-new 1)
			      (return-from eval-org (values :kill offspring))))
		     (loop for i below iters do
			  (when (> wait 0)
			    (let ((skip (min wait (- iters i))))
			      (decf wait skip)
			      (incf i skip))
			    (when (> wait 0)
			      (return)))
			  (tagbody
			   next-ins
			     (when (or (> ip max-ip) (<= energy 0))
			       (die))
			     (when (<= (random 1000) 1)
			       (setf ip (random (length code))))
			     (let* ((ins (aref code ip)))
			       ;;(prind energy ip ins as bs off)
			       (decf energy)
			       (macrolet ((make-instructions (keyform &body cases)
					    (let* ((instructions-list (mapcar #'caar cases))
						   (n-labels 4)
						   (markers-list (loop for i below n-labels collect
								      (intern (format nil "M~A" i))))
						   (labels-list (loop for i below n-labels collect
								     (intern (format nil "L~A" i))))
						   (i-list (append instructions-list markers-list labels-list))
						   (instructions (make-array (length i-list) :initial-contents i-list))
						   (instructions-hash-table (make-hash-table :test 'eq))
						   (labels-cases (loop for label in labels-list collect
								      `((,label))))
						   (keyform-sym (gensym)))
					      (loop for ins in i-list do
						   (setf (gethash ins instructions-hash-table) t))
					      (let ((code
						     `(labels ((random-ins ()
								 (sample ,instructions))
							       (is-valid-ins (ins)
								 (multiple-value-bind (val present)
								     (gethash ins ,instructions-hash-table)
								   (declare (ignore val))
								   present)))
							(let ((,keyform-sym ,keyform))
							  (case ,keyform-sym
							    ,@cases
							    ,@labels-cases
							    ((end))
							    (t (assert (and (integerp ,keyform-sym) (>= ,keyform-sym 0))))))
							)))
						(prind code)
						(prind instructions)
						code))))
				 (make-instructions
				  ins
				  ((eat)
				   (incf energy (aref *world* x y))
				   (setf (aref *world* x y) 0)
				   (setf wait 1000))
				  ((set-as)
				   (incf ip)
				   (setf as (arefd code nil ip)))
				  ((set-bs)
				   (incf ip)
				   (setf bs (arefd code nil ip)))
				  ((read-as)
				   (setf as (if (<= (random 1000) 0) (random-ins) (car genesx))))
				  ((read-bs)
				   (setf bs (if (<= (random 1000) 0) (random-ins) (car genesx))))
				  ((read-next)
				   (setf genesx (cdr genesx)))
				  ((write-as)
				   (when (is-valid-ins as)
				     (push as off)))
				  ((write-bs)
				   (when (is-valid-ins bs)
				     (push bs off)))
				  ((cmp-as-bs)
				   (setf as (eq as bs)))
				  ((jne)
				   (incf ip)
				   (when (not as)
				     (let* ((jump-ip (arefd code nil ip)))
				       (setf ip jump-ip)
				       (if (<= 0 ip max-ip)
					   (go next-ins)
					   (die)))))
				  ((set-an-1)
				   (setf an 1))
				  ((set-bn-1)
				   (setf bn 1))
				  ((in-energy-left-an)
				   (let ((x (mod (- x (random 10)) world-w)))
				     (setf an (aref *world* x y))))
				  ((in-energy-right-an)
				   (let ((x (mod (+ x (random 10)) world-w)))
				     (setf an (aref *world* x y))))
				  ((in-energy-left-bn)
				   (let ((x (mod (- x (random 10)) world-w)))
				     (setf an (aref *world* x y))))
				  ((in-energy-right-bn)
				   (let ((x (mod (+ x (random 10)) world-w)))
				     (setf an (aref *world* x y))))
				  ((add-an-bn)
				   (setf an (+ an bn)))
				  ((sub-an-bn)
				   (setf an (- an bn)))
				  ((sub-bn-an)
				   (setf an (- bn an)))
				  ((sign-an)
				   (setf an (signum an)))
				  ((sign-bn)
				   (setf bn (signum bn)))
				  ((setf-as-an-gt0)
				   (setf as (> an 0)))
				  ((setf-as-an-ge0)
				   (setf as (>= an 0)))
				  ((off-left)
				   (cell-division (1- x) y (/ energy 2)))
				  ((off-right)
				   (cell-division (1+ x) y (/ energy 2)))
				  ((off-up)
				   (cell-division x (1- y) (/ energy 2)))
				  ((off-down)
				   (cell-division x (1+ y) (/ energy 2)))
				  ((goto0)
				   (setf ip 0)
				   (go next-ins))
				  ((walk-an)
				   (setf x (mod (+ x (signum an)) world-w))
				   (setf wait 1000))
				  ((walk-bn)
				   (setf x (mod (+ x (signum bn)) world-w))
				   (setf wait 1000)))))
			     (incf ip))))
		   (values :survive offspring)))))
      (loop for org in *orgs* do
	   (when (eq org cursor)
	     (format t "org id:~5A wait:~4A energy:~4A age:~2A x:~3A y:~3A~%" (org-id org) (org-wait org) (org-energy org) (org-age org) (org-x org) (org-y org))
	     (format t "genes:~A~%" (org-genes org)))
	   (let ((e (org-energy org)))
	     (setf max-org-energy (max max-org-energy e))
	     (incf avg-org-energy e))
	   (incf (org-age org))
	 ;;(prind org)
	   (multiple-value-bind (status offspring) (eval-org 30 org)
	     (setf new-org (nconc (if (and (eq status :survive) (< (org-age org) 100))
				      (list org)
				      nil)
				  offspring
				  new-org))))
      (setf avg-org-energy (float (/ avg-org-energy length-orgs)))
      (setf *orgs* new-org)
      (let ((max-world-energy 0)
	    (min-world-energy most-positive-fixnum)
	    (avg-world-energy 0)
	    (total-rain (apply #'+ (mapcar #'world-cloud-rain *world-clouds*))))
	(loop for cloud in *world-clouds* do
	     (with-slots (x y xvel yvel rain edge) cloud
	       (loop for i below rain do
		    (let* ((rx (mod (+ (floor x) (random edge)) (array-dimension *world* 0)))
			   (ry (mod (+ (floor y) (random edge)) (array-dimension *world* 1)))
			   (e (aref *world* rx ry)))
		      (setf max-world-energy (max max-world-energy e))
		      (setf min-world-energy (min min-world-energy e))
		      (incf avg-world-energy e)
		      (let ((new-e (min *world-max-energy* (+ e 1))))
			(setf (aref *world* rx ry) new-e))))
	       (setf x (mod (+ x xvel) (array-dimension *world* 0)))
	       (setf y (mod (+ y yvel) (array-dimension *world* 1)))))
	(setf avg-world-energy (float (/ avg-world-energy total-rain)))
	;;(prind length-orgs max-world-energy min-world-energy avg-world-energy max-org-energy avg-org-energy)
	(when (null cursor)
	  (format t "(world energy min:~4A avg:~4A max:~4A) (org num:~5A -:~3A +:~3A =:~3A energy avg:~5A max:~5A)~%"
		  min-world-energy (round avg-world-energy) max-world-energy length-orgs num-died num-new (- num-new num-died) (round avg-org-energy) max-org-energy))
	)))
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
	     (if (= (org-age org) 0)
		 (set-pixel x y (color-to-argb8888 255 255 0 0))
		 (set-pixel x y (color-to-argb8888 255 0 e (max 128 e)))))))))

(defun nearest-org (x y orgs)
  "Return the organism in ORGS which is nearest to coordinate (X, Y)."
  (let ((min-dist nil)
	(min-org))
    (loop for org in orgs do
	 (let* ((org-x (org-x org))
		(org-y (org-y org))
		(diff-x (- x org-x))
		(diff-y (- y org-y))
		(dist (+ (* diff-x diff-x) (* diff-y diff-y))))
	   (when (or (null min-dist) (< dist min-dist))
	     (setf min-dist dist min-org org))))
    min-org))

(defun software-render-texture (&key (total-frames -1) (win-x 388) (win-y 0) (win-w 800) (win-h 600))
  "Software renderer example, drawing a texture on the screen.
See SDL-wiki/MigrationGuide.html#If_your_game_just_wants_to_get_fully-rendered_frames_to_the_screen."
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
	     (sur (sdl2:create-rgb-surface tex-w tex-h 32
					   :r-mask #x00ff0000
					   :g-mask #x0000ff00
					   :b-mask #x000000ff
					   :a-mask #xff000000))
	     ;; see .../cl-autowrap-20141217-git/cl-plus-c.md: "We may access the various fields as follows:"
	     (pix (plus-c:c-ref sur SDL2-FFI:SDL-SURFACE :pixels))
	     (first-frame-time (get-internal-real-time))
	     (num-frames 0)
	     (mouse-x (/ tex-w 2))
	     (mouse-y (/ tex-h 2))
	     (cursor nil))
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
	       ((sdl2:scancode= scancode :scancode-w)
		(prind "w")))
	     
	     ;;(format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)
	     ))

	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))

	  (:mousemotion
	   (:x x :y y :xrel xrel :yrel yrel :state state)
	   ;;(format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%" x xrel y yrel state)
	   (setf mouse-x (/ (* x tex-w) win-w) mouse-y (/ (* y tex-h) win-h))
	   (let* ((x (floor mouse-x)) (y (floor mouse-y))
		  (org (nearest-org x y *orgs*)))
	     (setf cursor org)))

	  (:idle
	   ()
	   (if (or (and (>= total-frames 0) (>= num-frames total-frames))
		   (null *orgs*))    
	       (sdl2:push-quit-event)
	       (progn
		 (idleloop sur cursor)
		 (sdl2:update-texture tex pix :width (* 4 tex-w))
		 ;;TODO: call SDL_RenderClear(sdlRenderer);
		 (sdl2:render-copy wrend tex)
		 (sdl2-ffi.functions::sdl-set-render-draw-color wrend (random 256) (random 256) (random 256) 255)
		 (cond
		   ((and (org-p cursor) (> (org-energy cursor) 0))
		    (let* ((x (org-x cursor)) (y (org-y cursor))
			   (x1 (* x (/ win-w tex-w)))
			   (y1 (* y (/ win-h tex-h)))
			   (x2 (* (1+ x) (/ win-w tex-w)))
			   (y2 (* (1+ y) (/ win-h tex-h))))
		      (sdl2-ffi.functions::sdl-render-draw-line wrend x1 y1 x2 y1)
		      (sdl2-ffi.functions::sdl-render-draw-line wrend x2 y1 x2 y2)
		      (sdl2-ffi.functions::sdl-render-draw-line wrend x2 y2 x1 y2)
		      (sdl2-ffi.functions::sdl-render-draw-line wrend x1 y2 x1 y1)))
		   (t (setf cursor nil)))
		 (sdl2:render-present wrend)
		 (incf num-frames)))
	   )

	  (:quit () t))

	(format t "End of main loop.~%")
	(format t "Average frames per second: ~A.~%" (float (/ num-frames (/ (- (get-internal-real-time) first-frame-time) internal-time-units-per-second))))
	(finish-output)
	))))
