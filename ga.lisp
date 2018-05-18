;; TODO: make computation instructions have an energy cost of 0. To avoid piling up trash instructions, implement the utilization/pruning-waves described below. (The pruning phase should get rid of superfluous computation instructions.)

;; quicklisp packages interesting for persistence: clache, clsql, hu.dwim.perec, hyperluminal, manardb, marshal, rucksack, submarine. On zweihorn and on average, 13 new organisms are created per second. An approximation of the size of structure ORGAP is (+ (* 29 2) 4 4 4 4 4 4 4 4 4 4 (* 29 2) 4 2 2 2 4 4 (* 4 2) (* 4 2) 4) bytes == 194 bytes =~ 256 bytes. That means that one hour of logging all born organisms takes up (* 256 13 60 60) bytes == 11980800 bytes =~ 12 MB.

;; TODO (high priority): implent world persistence between different programming sessions. This should be implemented by separating the simulation part (of ga.lisp and gatest-orgap-lisp.lisp) into a simulation program that stores the world to a sqlite database from time to time, including the state of the random number generator. The sqlite database should also be committed to git. A different lisp program implements the graphical display (of ga.lisp) by reading the current state from the sqlite database and rendering it to the screen. Whenever the sqlite database format changes, the display program must be adapted.

;; IDEA (low priority): below idea of COS/SIN-waves for utilizing/pruning instructions could be used for communication between organisms. This could make communication without cost, i.e. with an energy-cost per communication-instruction of 0, not fill the genome with useless instructions. Die Grundlage der Idee ist, dass es in Bayern vor nicht allzu langer Zeit Raiffeisen(?)-Höfe gab, an denen u.a. Baumaterial, Saatgut und Abfälle in einem Kreislauf ausgetauscht wurden. Dabei war der Lager-Puffer der einzelnen Materialien groß (da die Höfe im Prinzip Lagerhäuser waren). Danach wurde der Kreislauf aufgegeben, und der Puffer wurde immer kleiner, da es immer weniger Lagerhöfe gab. Inzwischen gibt es (z.B. durch die Abfallwirtschaft München (AWM)) wieder größere Puffer durch mehr Lagerhöfe. Um zurück zu ga.lisp zu kommen, könnten verschiedene Kommunikationsinstruktionen implementiert werden, die sich voneinander unterscheiden. Z.B. könnte Instruktion A peer-to-peer-Kommuniktion (á la Internet-Protocol) implementieren und eine andere, redundante, Instruktion B könnte asynchrone Kommunikation (á la Email) implementieren. Die Utilization/Pruning-Phasenverschiebung zwischen Instruktionen A und B könnte sich um 90 Grad unterscheiden.

;; IDEA: If i remember correctly, the below idea of COS/SIN-waves utilizes/prunes two instructions A and B with an phase difference of 0. This means the two COS-waves (i.e. the utilization-wave) of A and B have their maximum at the same time and their minimum also at the same time. But there could be world environments where instruction A should be pruned at the same time that instruction B is utilized. Should we make the phase difference of A and B different from 90 degrees? Also, should we make the phase difference between utilizing and pruning of each individual instruction not 90 degrees, but an variable amound?

;; IDEA: assume there are two worlds, one fast (short utilizing/pruning period) and a slow one (long utilizing/pruning period), and each world has a constant number of organisms. Then, if an organism dies in world A, I could copy the organism from world B to world A, and, symmetrically, when an organism dies in world B, copy it to world A. This would let each organism find its own optimal period of utilizing/pruning wave period, because we can assume that if an organism dies in a world, then it possibly had the wrong instructions (this of course does not take into account that the environment could have been bad). I just have to try it out.

;; IDEA: to find the optimal period of the COS/SIN-waves for utilizing/pruning of instructions, we could run two worlds in parallel, one with a long period (slow world) and one with a short period (fast world). The long period should be an integer multiple of the short period. Each organism should be run in each of the two worlds at the same time, let's call the organism in the fast world organism A and the organism in the slow world organism B. We need to find a way to synchronize the two worlds from time to time. The goal is to find an optimal instruction utilizing/pruning period. (Maybe for a third world, which is run with an adapting period?) Or we could exchange organism A and B from time to time, for example when it dies. I first have to get a feeling (by trying out different periods, in a single world) for how different periods change the population genome distribution. (Intuitively, it should broaden the genomic diversity.)

;; IDEA: have a constant number of organisms in the world by inserting offspring into a priority queue, which is ordered by the amount of "love" (a different form of energy) that the mother(+father if there is sex) has. Each time an organism dies, the offspring with the most love is deleted from the priority queue and born into the world at the position the mother is currently at.

;; TODO: When a random instruction is to be inserted, try out not drawing the random instruction from an equal distributed distribution over the instructions, but from an distribution that is determined by a second genome. (The first genome (called array A) is composed of the instructions, just like it is now.) The second genome is composed of an array B[i] with floating point values, one for each instruction. Each element of the array B[i] encodes the probability that the instruction i is drawn with. It doesn't encode an equal distribution, but the probability P for an instruction i is: P(new_instruction_to_be_inserted=i) = (2^B[i])/SUM_P2. (P2[i] = 2^B[i], and SUM_P2 is the sum of P2[i] over all instructions i. The raising to the power of 2 ensures that SUM_P2 and P[i] are positive.) The floats B[i] in the array B are constant over the life-time of an organism, but each float B[i] changes when an offspring is born: it performs a random walk: B[i] = B_mother[i] + R, where R is a random number drawn from a gaussian distribution with mean 0 and standard deviation 1. The effect of this whole procedure should be that the currently favourable instructions to be inserted are conserved between mother and offspring: offspring of a mother that has a good second genome B are more likely to be fit for life, because the array B changes little between mother and offspring. Now, this whole procedure also will have a very bad effect: Instructions that happen to be unlikely to be inserted (i.e. those instructions i with a large negative B[i]) will remain unlikely for a long time, because a random walk is, well, random. To balance this bad effect, we will inverse the array B each, lets say 80 generations. This is done as follows. We re-define P(new_instruction_to_be_inserted=i) := (2^(B[i]*cos(g/80*pi)))/SUM_P2cos, where cos(g) is the cosine of g, g is the generation of the organism, and SUM_P2cos is the sum of 2^(B[i]*cos(g/80*pi)) over all instructions i. The generation g is constant over the life-time of an organism, starts with 0, i.e. the first organisms in the initial world have g=0, and their offsprinig all have generation 1: g(offspring) = g(mother) + 1. This cosine wave will have the effect that the second genome B of an organism will be not only be conserved if it is good, but also that the elements of B, B[i], will be under constant pressure to be near each other. This is because if an instruction i is much better for life under the current world-conditions, then it will receive a large positive B[i]. But if the B[j] for the other instructions j have a wildly different value than B[i], then the offspring of the organism 80 generations later will have an unfavourable second genome B, because the probability of instruction i will be very small (because B[i]*cos(g/80*pi) will be a large negative number). If the B[i] for each instruction i are similar to each other, then the effect of the cosine wave will be small. (If the B[i] all are equal for a mother and remain equal for its offspring, then the cosine wave will have no effect at all.) But if an instruction i is favourable under the current conditions for life, then B[i] will increase from mother to offspring. But, 80 generations later, the instructions i in the first genome A will be under pressure to be pruned from A, because their likelihood to be inserted will be small. (Maybe (on second thought: definitely) I should also make not only insertion, but also deletion dependent on B. The probability of an instruction i to be omitted (i.e. deleted) in the copying of A from mother to offspring could also happen dependent on generation g. If the current probability P(omit_i_in_the_copying) for an instruction i is constantly 1/1000, then make the new probability P(omit_i_in_the_copying)=(2^B[i]*sin(g/80*pi))/SUM_P2sin. Note that the only difference between P(new_instruction_to_be_inserted_i) and P(omit_i_in_the_copying) is the change from cos to sin.) This will allow two kinds of organisms: those that evolve slowly (because their B[i] are all similar to each other for instructions i), and those that adapt quickly to new conditions for life, but that are also prone to quickly changing conditions (because they have at least one instruction i that has a large positive B[i], whose large positive value will become a hindrance in a new condition for life that requires a different instruction j to be inserted with a higher probability, and if B[i]-B[j] >> 0, then the organisms' offspring will need many generations to make B[i] smaller or B[j] larger).

;; TODO: instead of storing an array of energies, I could let the organisms have energy whenever they want (using EAT), but only give them an amount proportional to the distance to the sun (or to the clouds). This would not require this huge *WORLD* array. However, I don't know whether this would lead to unlimited (expontential) growth, since there would be unlimited energy available. I could make the sun faster than organisms can walk, so that they cannot keep up with the suns movement.

;; DONE: use editdistance.lisp to implement when selecting an organism, all other organisms should be colored according to the edit-distance to that organisms' genes using a color ramp.

;; I should let the organisms play games against each other. The games could be created randomly. A game has state (a fixed number of variables), and rules when and in what format it accepts inputs from the players, and what state these inputs change (input=function call). It also has a rule that describes what state the game has to be in so that player 1 wins, player 2 wins, etc. (or maybe a ranking of the players). The game should be fair, i.e. the game should work the same way if the order of players is permuted. There even could be games that have only one player, like puzzle-games (but how to generate them automatically?) A game could be implemented as a finite state machine. I thought about this a little bit, and I think there are too many different games that could be implemented this way. (The order of the number of different games is probably the same as could be implemented using a programming language of the same length.)

(defvar *default-random-state* (make-random-state nil)) ;save default random state using DEFVAR, this way it will only be evaluated once. Then, when we want to reset the state, we can copy *DEFAULT-RANDOM-STATE* and use it as the new state.

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(ql:quickload :sdl2)
(ql:quickload :cl-heap)
(ql:quickload :mru-cache)

(declaim (optimize (debug 3)))

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

(defun random-gaussian-2 ()
  "Return two with mean 0 and standard deviation 1 normally distributed random v
ariables."
  (declare (optimize (speed 3) (compilation-speed 0) (debug 3) (safety 3) (space 0)))
  (flet ((xinit ()
           (the single-float (- (* 2.0 (random 1.0)) 1))))
    (do* ((x1 (xinit) (xinit))
          (x2 (xinit) (xinit))
          (w (+ (* x1 x1) (* x2 x2)) (+ (* x1 x1) (* x2 x2))))
         ((< w 1.0)
          (let* ((wlog (the single-float (log (the (single-float 0.0 *) w))))
                 (v (the single-float (sqrt (the (single-float 0.0 *) (/ (* -2.0 wlog) w))))))
	    (declare (type single-float wlog))
            (values (* x1 v) (* x2 v)))))))

(let ((temp nil))
  (defun random-gaussian ()
    (if temp
	(prog1 temp
	  (setf temp nil))
	(multiple-value-bind (a b) (random-gaussian-2)
	  (setf temp b)
	  a))))

(defun argmax-hash-table (hash-table function &key (exclude nil))
  (let ((best-score nil)
	(best-element nil))
    (loop for element being the hash-values of hash-table do
	 (unless (find element exclude)
	   (multiple-value-bind (score exclude-p) (funcall function element)
	     (unless exclude-p
	       (when (or (null best-score) (> score best-score))
		 (setf best-score score best-element element))))))
    best-element))

(defun max-hash-table (hash-table function &key (exclude nil))
  (let ((argmax (argmax-hash-table hash-table function :exclude exclude)))
    (when argmax
      (funcall function argmax))))

(defun argmin-hash-table (hash-table function &key (exclude nil))
  (let ((best-score nil)
	(best-element nil))
    (loop for element being the hash-values of hash-table do
	 (unless (find element exclude)
	   (multiple-value-bind (score exclude-p) (funcall function element)
	     (unless exclude-p
	       (when (or (null best-score) (< score best-score))
		 (setf best-score score best-element element))))))
    best-element))

(defun min-hash-table (hash-table function &key (exclude nil))
  (let ((argmin (argmin-hash-table hash-table function :exclude exclude)))
    (when argmin
      (funcall function argmin))))

(defun avg-hash-table (hash-table function &key (exclude nil))
  (let ((score-sum 0)
	(count-elements 0))
    (loop for element being the hash-values of hash-table do
	 (unless (find element exclude)
	   (multiple-value-bind (score exclude-p) (funcall function element)
	     (unless exclude-p
	       (incf score-sum score)
	       (incf count-elements)))))
    (if (>= count-elements 1)
	(/ score-sum count-elements)
	nil)))

(defun sum-hash-table (hash-table function &key (exclude nil))
  (let ((score-sum 0)
	(count-elements 0))
    (loop for element being the hash-values of hash-table do
	 (unless (find element exclude)
	   (multiple-value-bind (score exclude-p) (funcall function element)
	     (unless exclude-p
	       (incf score-sum score)
	       (incf count-elements)))))
    (values score-sum count-elements)))

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

(defvar *id* 0 "The identification number of the last organism made")
(defvar *orgs* nil)
(defvar *event-heap* nil)
(defvar *world* nil)
(defvar *world-sun* nil)
(defvar *world-clouds* nil)
(defvar *world-max-energy* 4000 "The energy at a coordinate in the world is truncated to this value")
(defvar *world-tick* 0 "The number of elapsed world ticks")
(defvar *orgap-min-wait* 200 "The minimum number of ticks between an organism's events")
(defvar *sun-drop-wait* 200 "The number of ticks between sun events")
(defvar *clouds-drop-wait* 200 "The number of ticks between cloud events")
;; display variables
(defvar *display-world* t)
(defvar *print-statistics* nil)
(defparameter *cursor* nil)

(defun world-set-barriers! (world num-barriers-horizontal barrier-width-horizontal num-barriers-vertical barrier-width-vertical)
  (let ((w (array-dimension world 0))
	(h (array-dimension world 1)))
    (loop for i below num-barriers-horizontal do
	 (let ((y (random h)) (x (random w)))
	   (loop for i below barrier-width-horizontal do
		(setf (aref world (mod (+ x i) w) y 0) -1))))
    (loop for i below num-barriers-vertical do
	 (let ((y (random h)) (x (random w)))
	   (loop for i below barrier-width-vertical do
		(setf (aref world x (mod (+ y i) h) 0) -1))))))

(defun make-world (w h initial-energy num-instructions initial-instructions)
  (declare (ignorable num-instructions initial-instructions))
  ;;(let ((world (make-array (list w h (+ 1 num-instructions)) :element-type 'fixnum)))
  (let ((world (make-array (list w h 1) :element-type 'fixnum)))
    (loop for x below w do
	 (loop for y below h do
	      (setf (aref world x y 0) initial-energy)
	      ;; (loop for i below num-instructions do
	      ;; 	   (setf (aref world x y (+ 1 i)) initial-instructions))
	      ))
    world))

(defclass nature-object ()
  ((position-function :initarg :position-function :accessor nature-position-function)
   (edge :initarg :edge :accessor natur-edge)
   (drop-wait :initarg :drop-wait :accessor nature-drop-wait)
   (drop-amount :initarg :drop-amount :accessor nature-drop-amount)
   (nexttick :initform 0 :initarg :nexttick :accessor nature-nexttick)
   (energy-drop-sum :initform 0 :initarg :energy-drop-sum :accessor nature-energy-drop-sum)
   (energy-lost-sum :initform 0 :initarg :energy-lost-sum :accessor nature-energy-lost-sum)))

(defclass sun (nature-object)
  ((energy-index :initform 0 :initarg :energy-index :accessor nature-energy-index)))

(defun make-sun (world-w world-h energy-per-coordinate-per-tick fraction-covered position-function)
  "ENERGY-PER-COORDINATE-PER-TICK is the average energy dropped per world coordinate per tick.
FRACTION-COVERED is the fraction of the whole world covered with sunlight.
VELOCITY is the speed, i.e. position change per tick."
  (let* ((edge (round (sqrt (* world-w world-h fraction-covered))))
	 (energy-per-tick (* world-w world-h energy-per-coordinate-per-tick))
	 (drop-wait *SUN-DROP-WAIT*)
	 (drop-amount (round (* energy-per-tick drop-wait))))
    (let* ((sun (make-instance 'sun
			       :position-function position-function
			       :edge edge
			       :drop-wait drop-wait
			       :drop-amount drop-amount
			       :energy-index 0)))
      (prind position-function drop-wait drop-amount edge)
      sun)))

(defclass cloud (nature-object)
  ((pos-x :initarg :pos-x :accessor cloud-pos-x)
   (pos-y :initarg :pos-y :accessor cloud-pos-y)
   (vel-x :initarg :vel-x :accessor cloud-vel-x)
   (vel-y :initarg :vel-y :accessor cloud-vel-y)
   (energy-index :initarg :energy-index :accessor nature-energy-index)))

(defun make-cloud (world-w world-h energy-index edge cloud-speed-per-tick position-function rain-per-coordinate-per-tick)
  (let* ((angle (random (* 2 pi)))
	 (energy-per-tick (* world-w world-h rain-per-coordinate-per-tick))
	 (drop-wait *CLOUDS-DROP-WAIT*)
	 (drop-amount (round (* energy-per-tick drop-wait))))
    (prind position-function drop-wait drop-amount edge energy-index)
    (make-instance 'cloud
		   :position-function position-function
		   :edge edge
		   :drop-wait drop-wait
		   :drop-amount drop-amount
		   :pos-x (random world-w)
		   :pos-y (random world-h)
		   :vel-x (* cloud-speed-per-tick (sin angle))
		   :vel-y (* cloud-speed-per-tick (cos angle))
		   :energy-index energy-index)))

#|
(defun make-energy (initial-energy-0)
  (let ((energy (make-array *num-energies* :element-type 'fixnum :initial-element 0)))
    (setf (aref energy 0) initial-energy-0)
    energy))
(defun get-energy (energy energy-index)
  (aref energy energy-index))
(defun (setf get-energy) (value energy energy-index)
  (setf (aref energy energy-index) value))
|#
;; the program is still slow with the following, which is equivalent to the situation in commit 0d9cf2 where an organism had only one energy value, so the slowness must come from somewhere else.
(defmacro make-energy (initial-energy-0)
  `,initial-energy-0)
(defmacro get-energy (energy energy-index)
  (declare (ignore energy-index))
  `,energy)
(define-setf-expander get-energy (energy energy-index &environment env)
  "Set the last element in a list to the given value."
  (declare (ignore energy-index env))
  (let ((store (gensym)))
    (values `()
	    `()
	    `(,store)
	    `(setf ,energy ,store)
	    `,energy)))


(defclass org ()
  (;; organism program start: contains the actually interpreted slots.
   (genes :initarg :genes :type list :accessor orgap-genes :documentation "genes of the organism")
   (code :initarg :code :type vector :accessor orgap-code :documentation "compiled code")
   (markers :initarg :markers :type alist :accessor orgap-markers :documentation "ALIST of IPs by marker number")
   (functions :initarg :functions :type alist :accessor orgap-functions :documentation "ALIST of IPs by function number")
   (ip :initform 0 :initarg :ip :type integer :accessor orgap-ip)
   (wait :initform 0 :initarg :wait :type integer :accessor orgap-wait)
   (angle :initarg :angle :type single-float :accessor orgap-angle)
   (target :initform nil :initarg :target :type (or null org) :accessor orgap-target)
   (targeter :initform nil :initarg :targeter :type (or null org) :accessor orgap-targeter)
   (x :initarg :x :type single-float :accessor orgap-x)
   (y :initarg :y :type single-float :accessor orgap-y)
   (total-energy :initarg :total-energy :type integer :accessor orgap-total-energy)
   (energy :initarg :energy :type integer :accessor orgap-energy)
   (skin :initform 1 :initarg :skin :type integer :accessor orgap-skin)
   (off-genes :initform nil :initarg :off-genes :type list :accessor orgap-off-genes)
   (off-length :initform 0 :initarg :off-length :type integer :accessor orgap-off-length)
   (as :initform nil :initarg :as :type symbol :accessor orgap-as)
   (bs :initform nil :initarg :bs :type symbol :accessor orgap-bs)
   (cs :initform nil :initarg :cs :type symbol :accessor orgap-cs)
   (an :initform 0 :initarg :an :type integer :accessor orgap-an)
   (bn :initform 0 :initarg :bn :type integer :accessor orgap-bn)
   (memory :initform (make-array 2 :initial-element 0) :initarg :memory :type vector :accessor orgap-memory)
   (stack :initform (make-array 2 :initial-element 0) :initarg :stack :type vector :accessor orgap-stack)
   (sp :initform 0 :initarg :sp :type integer :accessor orgap-sp)
   (genesx :initarg :genesx :type list :accessor orgap-genesx :documentation "rest of the genes to be read")
   ;; organism container start: contains management and statistics slots.
   (id :initform (incf *id*) :initarg :id :accessor orgcont-id)
   (lasttick :initform 0 :initarg :lasttick :accessor orgcont-lasttick)
   (nexttick :initform 0 :initarg :nexttick :accessor orgcont-nexttick)
   ;;statistics
   (age :initform 0 :initarg :age :accessor orgcont-age)
   (totage :initform 0 :initarg :totage :accessor orgcont-totage)
   (offspring-list :initform nil :initarg :offspring-list :accessor orgcont-offspring-list)
   (offspring-count :initform 0 :initarg :offspring-count :accessor orgcont-offspring-count)
   (offspring-energy-sum :initform 0 :initarg :offspring-energy-sum :accessor orgcont-offspring-energy-sum)
   (walk-sum :initform 0.0 :initarg :walk-sum :accessor orgcont-walk-sum)
   (walk-count :initform 0 :initarg :walk-count :accessor orgcont-walk-count)
   (energy-in-sum :initform (make-energy 0) :initarg :energy-in-sum :accessor orgcont-energy-in-sum :documentation "The total energy taken in, excluding initial energy of organisms spawned in the world.")
   (energy-out-sum :initform (make-energy 0) :initarg :energy-out-sum :accessor orgcont-energy-out-sum :documentation "The total energy spent voluntarily or involuntarily.")))

;; load edit-distance functions
(load "edit-distance.lisp")

;; load organism implementation
(load "~/lisp/gatest-orgap-lisp.lisp")
;;(load "~/lisp/gatest-orgap-lightning.lisp")
;;(defvar *num-energies* (+ 1 *num-instructions*) "The total number of different energies at each world coordinate")
(defvar *num-energies* 1 "The total number of different energies at each world coordinate")

(defun copy-orgcont (org)
  (with-slots (genes code markers functions ip wait angle target targeter x y total-energy energy skin off-genes off-length as bs cs an bn memory stack sp genesx id lasttick nexttick age totage offspring-list offspring-count offspring-energy-sum walk-sum walk-count energy-in-sum energy-out-sum) org
    (make-instance 'org :genes genes :code code :markers markers :functions functions :ip ip :wait wait :angle angle :target target :targeter targeter :x x :y y :total-energy total-energy :energy energy :skin skin :off-genes off-genes :off-length off-length :as as :bs bs :cs cs :an an :bn bn :memory memory :stack stack :sp sp :genesx genesx :id id :lasttick lasttick :nexttick nexttick :age age :totage totage :offspring-list offspring-list :offspring-count offspring-count :offspring-energy-sum offspring-energy-sum :walk-sum walk-sum :walk-count walk-count :energy-in-sum (alexandria:copy-array energy-in-sum) :energy-out-sum (alexandria:copy-array energy-out-sum))))

(defun orgs-add-org (org)
  (setf (gethash (orgcont-id org) *orgs*) org))

(defun orgs-del-org (org)
  (remhash (orgcont-id org) *orgs*))

(defun orgs-add-orgs (org-list)
  (loop for org in org-list do
       (orgs-add-org org)))

(defun make-default-orgs (num energy &optional
				       ;;(genes '(mrk0= set-bs-nil eat in-an-as-energy-left in-bn-as-energy-right sub-from-an-bn mrk1= read-as read-next write-as cmp-as-as-bs jne1= set-an-1 set-bn-1 add-to-bn-an mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn split-cell-an wait-an walk-an set-an-1 turn-cw-an set-as-nil set-bs-random cmp-as-as-bs jne0= jne0= jne0=))
				       ;;(genes '(mrk0= set-bs-nil eat in-an-as-energy-left in-bn-as-energy-right sub-from-an-bn mrk1= read-as read-bs cmp-as-as-bs jne1= read-next write-bs set-as-nil cmp-as-as-bs jne1=    set-an-1 set-bn-1 add-to-bn-an mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn mul-to-an-bn split-cell-an wait-an walk-an set-an-1 turn-cw-an set-as-nil set-bs-random cmp-as-as-bs jne0= jne0= jne0=))
				       ;;(genes '(MRK0= SUB-FROM-BN-AN SET-BS-RANDOM MRK1= READ-BS READ-NEXT WRITE-BS CMP-AS-AS-BS JNE1= SET-ANGLE-TO-BN SET-AN-1 IN-BN-AS-ENERGY-Y+ TURN-CW-BN ATTACK-TARGET SET-BN-1 SIGN-AN ADD-TO-BN-AN MUL-TO-AN-BN MUL-TO-AN-BN MUL-TO-AN-BN MUL-TO-AN-BN MUL-TO-AN-BN MUL-TO-AN-BN CMP-BS-GT-BN-AN MUL-TO-AN-BN MUL-TO-AN-BN SPLIT-CELL-AN WALK-AN WALK-AN SET-TARGET-NEAR SET-AS-NIL ATTACK-TARGET EAT WALK-BN EAT JNE0=))
				       ;;(genes '(MRK1= READ-BS READ-NEXT WRITE-BS IN-BN-AS-ENERGY-X+ CMP-AS-AS-BS IN-BN-AS-ENERGY-Y- ADD-TO-AN-BN SET-AN-1 SET-BN-1 ADD-TO-BN-AN ADD-TO-AN-BN ADD-TO-AN-BN SET-BN-MAX-AN-BN ADD-TO-AN-BN WALK-BN WALK-BN EAT JNE1= MRK0= TURN-CCW-AN WALK-AN EAT WALK-AN IN-BN-AS-ENERGY-Y- WALK-AN READ-AS WRITE-AS WALK-AN SET-AN-TO-ANGLE SPLIT-CELL-AN MUL-TO-BN-AN IN-BN-AS-ENERGY-LEFT SET-AN-MAX-AN-BN EAT IN-BN-AS-ENERGY-X+ JNE1= CMP-AS-GT-AN-BN MRK3= WAIT-AN TURN-CCW-AN))
				       ;;(genes '(MRK1= READ-BS READ-NEXT WRITE-BS TURN-CCW-BN IN-BN-AS-ENERGY-X+ CMP-AS-AS-BS EAT ADD-TO-AN-BN SET-AN-1 SET-BN-1 WRITE-AS ADD-TO-BN-AN ADD-TO-AN-BN ADD-TO-AN-BN SET-BN-MAX-AN-BN ADD-TO-AN-BN WALK-AN WALK-BN WALK-BN WALK-AN WALK-AN JNE1= MRK0= TURN-CCW-AN CMP-AS-AS-BS EAT WALK-AN IN-BN-AS-ENERGY-Y- WALK-AN WRITE-AS SET-ANGLE-TO-BN WALK-AN SET-AN-TO-ANGLE SET-BN-TO-ANGLE SPLIT-CELL-AN MUL-TO-BN-AN IN-BN-AS-ENERGY-LEFT SET-AN-MAX-AN-BN EAT IN-BN-AS-ENERGY-X+ TURN-CW-AN JNE1= SET-AN-1 CMP-AS-GT-AN-BN MRK3= SET-ANGLE-DOWN TURN-CCW-AN IN-BN-AS-ENERGY-LEFT))
				       ;;(genes '(MRK1= READ-BS READ-NEXT WRITE-BS CMP-AS-AS-BS EAT SET-AN-1 SET-BN-1 ADD-TO-BN-AN MUL-TO-BN-AN ADD-TO-BN-AN ADD-TO-AN-BN ADD-TO-AN-BN SET-BN-MAX-AN-BN WALK-AN WALK-BN WALK-BN WALK-AN WALK-AN TURN-CCW-BN WALK-BN TURN-CW-BN JNE1= IN-BN-AS-ENERGY-Y+ MRK0= WAIT-AN CMP-AS-AS-BS WRITE-BS WALK-AN IN-BN-AS-ENERGY-Y- WALK-AN SPLIT-CELL-BN WRITE-AS SET-AN-1 WALK-AN SET-AN-TO-ANGLE SPLIT-CELL-AN MUL-TO-BN-AN SET-AN-TO-BN EAT IN-BN-AS-ENERGY-X+ TURN-CW-AN JNE1= SET-AN-1 SET-ANGLE-DOWN SET-AS-RANDOM TURN-CCW-AN))
				       ;;(genes '(MRK1= READ-BS READ-NEXT WRITE-BS CMP-AS-AS-BS EAT SET-AN-1 SET-BN-1 ADD-TO-BN-AN ADD-TO-BN-AN ADD-TO-AN-BN ADD-TO-AN-BN SET-BN-MAX-AN-BN WALK-AN WALK-BN SET-BS-NIL WALK-BN WALK-AN TURN-CCW-BN WALK-BN TURN-CW-BN JNE1= IN-BN-AS-ENERGY-Y+ WAIT-AN CMP-AS-AS-BS WRITE-BS WALK-AN IN-BN-AS-ENERGY-Y- SPLIT-CELL-BN WRITE-AS IN-AN-AS-ENERGY-LEFT SET-AN-1 WALK-AN SET-AN-TO-ANGLE SPLIT-CELL-AN MUL-TO-BN-AN SUB-FROM-AN-BN EAT TURN-CW-AN IN-BN-AS-ENERGY-Y- JNE1= SET-AN-1 SET-AN-TO-ANGLE CMP-BS-GT-BN-AN SET-AS-RANDOM TURN-CCW-AN SET-AN-TO-ENERGY))
				       ;;(genes '(mrk0= set-an-1 set-bn-1 add-to-bn-an add-to-an-bn walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an walk-an mul-to-an-bn mul-to-bn-an mul-to-an-bn mul-to-bn-an mul-to-an-bn mul-to-bn-an wait-bn eat mrk1= read-as read-next write-as cmp-as-as-bs jne1= in-bn-as-energy-left split-cell-bn turn-cw-bn jmp0=))
				       ;;(genes '(EAT MRK0= SET-AN-1 SET-BN-1 ADD-TO-BN-AN ADD-TO-AN-BN MUL-TO-AN-BN WALK-AN WALK-AN WALK-AN SIGN-BN WALK-AN WALK-AN WALK-AN WALK-AN EAT WALK-AN MUL-TO-AN-BN SET-AN--1 MUL-TO-BN-AN MUL-TO-AN-BN MUL-TO-BN-AN EAT MRK1= READ-AS READ-NEXT WRITE-AS CMP-AS-AS-BS JNE1= IN-BN-AS-ENERGY-LEFT SPLIT-CELL-BN TURN-CW-BN JMP0= MRK1=))
				       (GENES '(EAT MRK0= SET-BN-1 ADD-TO-BN-AN ADD-TO-AN-BN MUL-TO-AN-BN WALK-AN WALK-AN WALK-AN  SIGN-BN WALK-AN WALK-AN WALK-AN WALK-AN SET-AN--1 MUL-TO-BN-AN MUL-TO-AN-BN EAT READ-AS READ-NEXT WRITE-AS CMP-AS-AS-BS JNE1= IN-BN-AS-ENERGY-LEFT SPLIT-CELL-BN IN-AN-AS-ENERGY-X+-CSP TURN-CW-BN JMP0= MRK1=))
				       )
  (loop for i below num collect
       (let ((x (random (array-dimension *world* 0)))
	     (y (random (array-dimension *world* 1))))
	 (loop until (>= (aref *world* x y 0) 0) do
	      (setf x (random (array-dimension *world* 0))
		    y (random (array-dimension *world* 1))))
	 (let* ((org (make-orgap genes x y (random (ceiling (* 2 pi 128))) energy)))
	   (setf (orgcont-nexttick org) *orgap-min-wait*)
	   (when (= 0 (orgap-code-length org))
	     (error "Organism ~A has code length 0" org))
	   org))))

(defmethod eventsource-nexttick ((eventsource org))
  (orgcont-nexttick eventsource))

(defmethod eventsource-nexttick ((eventsource nature-object))
  (nature-nexttick eventsource))

(defun sun-position-line (tick)
  (values (* tick .00001 .707)
	  (* tick .00001 .707)))

(defun sun-position-circle (sun tick)
  (declare (ignore sun))
  (values (+ (* 300 (cos (* tick .00000005))))
	  (+ (* 300 (sin (* tick .00000005))))))

(defun cloud-position-line (cloud tick)
  (with-slots (pos-x pos-y vel-x vel-y) cloud
    (values (+ pos-x (* tick vel-x))
	    (+ pos-y (* tick vel-y)))))

(defun set-default-world (&key (w 400) (h 200) (world-energy 0) (world-instructions 16) (orgs 250) (org-energy 4000) (reset-random-state t) (world-max-energy 4000) (energy-per-coordinate-per-tick .00001) (fraction-covered .01) (position-function #'sun-position-circle) (num-barriers-horizontal 5) (barrier-width-horizontal 40) (num-barriers-vertical 5) (barrier-width-vertical 30) (clouds-edge 1) (clouds-speed-per-tick 1) (clouds-position-function #'cloud-position-line) (clouds-rain-per-coordinate-per-tick .00001))
  (when reset-random-state
    (reset-random-state))
  (setf *id* 0)
  (setf *world-tick* 0)
  (setf *world-max-energy* world-max-energy)
  (setf *world* (make-world w h world-energy *num-instructions* world-instructions))
  (world-set-barriers! *world* num-barriers-horizontal barrier-width-horizontal num-barriers-vertical barrier-width-vertical)
  (setf *world-sun* (list (make-sun (array-dimension *world* 0) (array-dimension *world* 1) energy-per-coordinate-per-tick fraction-covered position-function)))
  ;;(setf *world-clouds* (loop for instruction-index below *num-instructions* collect (make-cloud (array-dimension *world* 0) (array-dimension *world* 1) (+ 1 instruction-index) clouds-edge clouds-speed-per-tick clouds-position-function clouds-rain-per-coordinate-per-tick)))
  ;;(setf *world-clouds* (list (make-cloud (array-dimension *world* 0) (array-dimension *world* 1) -1 clouds-edge clouds-speed-per-tick clouds-position-function clouds-rain-per-coordinate-per-tick)))
  (setf *world-clouds* nil)
  (let ((orgs (make-default-orgs orgs org-energy)))
    (setf *orgs* (make-hash-table))
    (orgs-add-orgs orgs)
    (setf *event-heap*
	  (let ((heap (make-instance 'cl-heap:fibonacci-heap :key #'eventsource-nexttick :sort-fun #'<)))
	    (cl-heap:add-all-to-heap heap orgs)
	    (cl-heap:add-all-to-heap heap *world-sun*)
	    (cl-heap:add-all-to-heap heap *world-clouds*)
	    heap)))
  (setf *cursor* nil)
  nil)
(when (null *orgs*)
  (set-default-world))

(defun print-orgap (org)
  (with-slots (ip genes off-genes as bs cs an bn stack) org
    (format t "orgap ip:~3A/~3A as:~20A bs:~20A cs:~3A an:~A bn:~A~%"
	    ip (length genes) as bs cs an bn)
    (format t "orgap stack:~S~%"
	    stack)
    (format t "~A length:~S hash:~S~%" genes (length genes) (mru-cache:lsxhash genes))))

(defun compute-fitness (org &optional (fitness-function #'orgcont-energy-out-sum))
  (if (> (get-energy (orgap-energy org) 0) 0)
      (+ (funcall fitness-function org) (apply #'+ (mapcar (lambda (org) (compute-fitness org fitness-function)) (orgcont-offspring-list org))))
      0))

(defun print-orgcont (org)
  (with-slots (wait energy x y angle id age totage offspring-count offspring-energy-sum walk-sum walk-count) org
    (format t "org id:~5A wait:~5A energy:~4A age:~3A/~A tage/off(~2A):~6F fitness(~3A,~3A):~A~%"
	    id
	    wait
	    energy
	    age
	    totage
	    offspring-count
	    (when (> offspring-count 0) (float (/ totage offspring-count)))
	    (compute-fitness org (constantly 1))
	    (compute-fitness org #'orgcont-offspring-count)
	    (compute-fitness org))
    (format t "org x:~3,2F y:~3,2F angle:~7,2E speed avg:~1,3F off-energy avg:~4A~%"
	    x
	    y
	    (float angle)
	    (when (> walk-count 0) (/ walk-sum walk-count))
	    (when (> offspring-count 0) (round offspring-energy-sum offspring-count))))
  (print-orgap org))

(defun compute-raw-edit-distance (org1-genes org2-genes)
  (declare (optimize (debug 3)))
  (let* ((score-fn (make-score-fn 0 -1))
	 (m (needleman-wunsch org1-genes org2-genes score-fn -1))
	 (score (best-alignment-score m))
	 (length (let ((l1 (length org1-genes))
		       (l2 (length org2-genes)))
		   (if (or (= l1 0) (= l2 0))
		       1.0
		       (sqrt (* l1 l2)))))
	 (scaled-score (/ score length)))
    scaled-score))

(defun compute-edit-distance (org1-genes org2-genes)
  (declare (optimize (debug 3)))
  (let* ((score-fn (make-score-fn 1 -1))
	 (m (needleman-wunsch org1-genes org2-genes score-fn -1))
	 (score (best-alignment-score m))
	 (length (length org1-genes))
	 (scaled-score (expt 2 (* (- (float score) length) 0.125)))
	 )
    scaled-score))

(let ((last nil))
  (defun calculate-average-edit-distance (orgs)
    (declare (optimize (debug 3)))
    (let* ((orgs (let ((a (make-array (hash-table-count orgs))))
		   (loop for v being the hash-value of orgs for i from 0 do (setf (aref a i) v))
		   a))
	   (n (length orgs))
	   (score-sum 0.0))
      (loop for i below (1- n) do
	 ;;(loop for j from (1+ i) below n do
	   (let* ((j (+ i (random (- n i))))
		  (org1-genes (orgap-genes (aref orgs i)))
		  (org2-genes (orgap-genes (aref orgs j)))
		  (scaled-score (compute-raw-edit-distance org1-genes org2-genes)))
	     (incf score-sum scaled-score)))
      (let ((now (/ score-sum n)))
	(setf last (if (null last)
		       now
		       (+ (* now 0.02) (* last 0.98))))
	last))))

(defun print-statistics ()
  (flet ((genome-length (org) (length (orgap-genes org))))
    (format t "genome min:~3A avg:~4,1F max:~3A edit-distance-score avg:~F~%" (min-hash-table *orgs* #'genome-length) (avg-hash-table *orgs* #'genome-length) (max-hash-table *orgs* #'genome-length) (calculate-average-edit-distance *orgs*)))
  (flet ((speed (org) (if (> (orgcont-walk-count org) 0) (/ (orgcont-walk-sum org) (orgcont-walk-count org)) (values nil t))))
    (format t "speed min:~1,3F avg:~1,3F max:~1,3F~%" (min-hash-table *orgs* #'speed) (avg-hash-table *orgs* #'speed) (max-hash-table *orgs* #'speed)))
  (flet ((compute-fitness-1 (org)
	   (compute-fitness org (constantly 1)))
	 (compute-fitness-offspring (org)
	   (compute-fitness org #'orgcont-offspring-count)))
    (format t "fitness min:~A avg:~A max:~A fitness-1 min:~A avg:~A max:~A fitness-offspring min:~A avg:~A max:~A~%" (min-hash-table *orgs* #'compute-fitness) (float (avg-hash-table *orgs* #'compute-fitness)) (max-hash-table *orgs* #'compute-fitness) (min-hash-table *orgs* #'compute-fitness-1) (float (avg-hash-table *orgs* #'compute-fitness-1)) (max-hash-table *orgs* #'compute-fitness-1) (min-hash-table *orgs* #'compute-fitness-offspring) (float (avg-hash-table *orgs* #'compute-fitness-offspring)) (max-hash-table *orgs* #'compute-fitness-offspring))))

(let ((last-nature-energy-used-sum 0)
      (last-nature-energy-drop-sum 0))
  (defun print-world-stats (ticks loop-start-real-time total-ins-count)
    (declare (optimize (debug 3)))
    (let* ((length-orgs (hash-table-count *orgs*))
	   (max-org-energy (max-hash-table *orgs* (lambda (org) (get-energy (orgap-energy org) 0))))
	   (avg-org-energy (avg-hash-table *orgs* (lambda (org) (get-energy (orgap-energy org) 0))))
	   (avg-org-tage/noff (avg-hash-table *orgs* (lambda (org) (if (> (orgcont-offspring-count org) 0) (float (/ (orgcont-totage org) (orgcont-offspring-count org))) (values nil t)))))
	   (max-totage (max-hash-table *orgs* #'orgcont-totage))
	   (nature-energy-drop-sum (apply #'+ (mapcar #'nature-energy-drop-sum (append *world-sun* *world-clouds*))))
	   (nature-energy-unused-sum (+ (loop for sun in *world-sun* sum (nature-energy-lost-sum sun))
					(loop for i below (apply #'* (array-dimensions *world*)) sum (row-major-aref *world* i))))
	   (nature-energy-used-sum (- nature-energy-drop-sum nature-energy-unused-sum)))
      (let* ((ins/s (round total-ins-count (max 0.0001 (/ (- (get-internal-real-time) loop-start-real-time) internal-time-units-per-second)))))
	(format t "i ~A+~A ~8Ains/s (org num:~4A energy avg:~5A max:~5A used:~4,3FA(li~4,2F) tage/noff avg:~6F totage max:~6A)~%"
		*world-tick* ticks ins/s length-orgs (if avg-org-energy (round avg-org-energy) nil) max-org-energy (float (/ nature-energy-used-sum nature-energy-drop-sum)) (let ((ds (- nature-energy-drop-sum last-nature-energy-drop-sum))) (if (= ds 0) nil (float (/ (- nature-energy-used-sum last-nature-energy-used-sum) ds)))) avg-org-tage/noff max-totage))
      (setf last-nature-energy-used-sum nature-energy-used-sum)
      (setf last-nature-energy-drop-sum nature-energy-drop-sum))))

(defmethod idleloop-event ((org org))
  (declare (optimize (debug 3)))
  (let* ((lasttick (orgcont-lasttick org))
	 (nexttick (orgcont-nexttick org))
	 (iters (- nexttick lasttick)))
    (incf (orgcont-age org) iters)
    (incf (orgcont-totage org) iters)
    ;;(prind (orgcont-id org) lasttick iters (get-energy (orgap-energy org) 0))
    (flet ((add-offspring (off-org)
	     (incf (orgcont-offspring-count org))
	     (setf (orgcont-age org) 0)
	     (push off-org (orgcont-offspring-list org))
	     ;; final initialization of OFF-ORG.
	     (setf (orgcont-lasttick off-org) nexttick)
	     (setf (orgcont-nexttick off-org) (+ nexttick (orgap-wait off-org) *orgap-min-wait*))
	     (cl-heap:add-to-heap *event-heap* off-org)
	     (orgs-add-org off-org)
	     off-org))
      (multiple-value-bind (status ins-count) (eval-orgap iters org #'add-offspring)
	(setf (orgcont-lasttick org) nexttick)
	(setf (orgcont-nexttick org) (+ nexttick (orgap-wait org) *orgap-min-wait*))
	;;(prind "nexttick" (orgcont-lasttick org) (orgcont-nexttick org))
	(cond
	  ((eq status :survive)
	   (cl-heap:add-to-heap *event-heap* org)
	   (orgs-add-org org))
	  (t
	   ;;(prind "kill" (orgcont-id org) status (orgcont-age org) (get-energy (orgap-energy org) 0))
	   (orgs-del-org org)))
	ins-count))))

(defun nature-event (nature-object position-function edge drop-wait drop-amount energy-index)
  (declare (optimize (debug 3)))
  (when (typep nature-object 'cloud)
    (setf energy-index (1+ (random *num-instructions*))))
  (let ((world-w (array-dimension *world* 0))
	(world-h (array-dimension *world* 1)))
    (multiple-value-bind (x y) (funcall position-function nature-object (nature-nexttick nature-object))
      (let* ((rx (mod (+ (floor x) (floor (* edge (random-gaussian)))) world-w))
	     (ry (mod (+ (floor y) (floor (* edge (random-gaussian)))) world-h))
	     (e (aref *world* rx ry energy-index)))
	(when (>= e 0)
	  (let* ((new-e (+ e drop-amount))
		 (actual-e (min *world-max-energy* new-e))
		 (lost-e (- new-e actual-e)))
	    (setf (aref *world* rx ry energy-index) actual-e)
	    (incf (nature-energy-drop-sum nature-object) drop-amount)
	    (incf (nature-energy-lost-sum nature-object) lost-e)))))
    (cl-heap:add-to-heap *event-heap* nature-object))
  drop-wait)

(defmethod idleloop-event ((sun sun))
  (with-slots (position-function edge drop-wait drop-amount energy-index) sun
    (incf (nature-nexttick sun) (nature-event sun position-function edge drop-wait drop-amount energy-index)))
  nil)

(defmethod idleloop-event ((cloud cloud))
  (with-slots (position-function edge drop-wait drop-amount energy-index) cloud
    (incf (nature-nexttick cloud) (nature-event cloud position-function edge drop-wait drop-amount energy-index)))
  nil)

(let ((lastloop nil))
  (defun idleloop (lasttick)
    (declare (optimize (debug 3)))
    (let ((loop-start-real-time (get-internal-real-time))
	  (total-ins-count 0))
      ;; sun, clouds and organisms conceptually are event sources that have an #'EVENTSOURCE-NEXTTICK.
      (loop until (let* ((org (cl-heap:peep-at-heap *event-heap*))) (or (null org) (> (eventsource-nexttick org) lasttick))) do
	 ;;(prind *world-tick* *orgs* *event-heap*)
	   (let* ((org (cl-heap:pop-heap *event-heap*)))
	     (let ((ins-count (idleloop-event org)))
	       (when (typep org 'org)
		 (incf total-ins-count ins-count)))))
      (print-world-stats (- lasttick *world-tick*) (if lastloop lastloop loop-start-real-time) total-ins-count))
    (setf lastloop (get-internal-real-time))
    (when *print-statistics*
      (print-statistics))
    (setf *world-tick* lasttick)))

(defun nearest-org-genes (genes orgs &key (exclude-orgs nil))
  "Return the organism in ORGS which has the most similar genes to GENES."
  (argmax-hash-table orgs
		     (lambda (org)
		       (let* ((org-genes (orgap-genes org))
			      (score (compute-edit-distance genes org-genes)))
			 (if (= score 1)
			     (return-from nearest-org-genes org)
			     score)))
		     :exclude exclude-orgs))

(defun nearest-org (x y orgs &key (exclude-orgs nil))
  "Return the organism in ORGS which is nearest to coordinate (X, Y)."
  (argmin-hash-table orgs
		     (lambda (org)
		       (let* ((org-x (orgap-x org))
			      (org-y (orgap-y org))
			      (diff-x (- x org-x))
			      (diff-y (- y org-y))
			      (dist (+ (* diff-x diff-x) (* diff-y diff-y))))
			 dist))
		     :exclude exclude-orgs))

(defun make-edit-distance-cacher ()
  (declare (optimize (debug 3)))
  (mru-cache:make-function-cacher (lambda (org1 org2) (compute-edit-distance (orgap-genes org1) (orgap-genes org2))) 100000 :make-hash-table-fn #'mru-cache:make-sxhash-equal-hash-table))

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
			      (let ((c (+ 128 (random 128))))
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
		   (let ((*random-state* display-random-state))
		     (sdl2-ffi.functions::sdl-set-render-draw-color wrend 255 (random 256) (random 256) 255))
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

(defun without-graphics (&optional (total-ticks nil))
  "Simulate the world for TOTAL-TICKS ticks, or until pressing CTRL-C, if TOTAL-TICKS is NIL."
  (let ((quit-without-graphics nil)
	(*display-world* nil))
    (restart-bind ((quit-without-graphics (lambda (&optional v)
					    (declare (ignore v))
					    (setf quit-without-graphics t)
					    (continue))
		     :report-function
		     (lambda (stream)
		       (format stream "Quit #'WITHOUT-GRAPHICS after finishing the current loop."))))
      (let ((initial-tick *world-tick*)
	    (ticks (if total-ticks total-ticks 1000000)))
	(loop until (if total-ticks
			(>= *world-tick* (+ initial-tick total-ticks))
			quit-without-graphics)
	   do
	     (idleloop (+ *world-tick* ticks))
	     (print-orgcont (argmax-hash-table *orgs* #'compute-fitness)))))))

;;(software-render-texture)
