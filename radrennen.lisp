

(load "game-nnet.lisp")

;;;; UTIL

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

;;;; GAME RULES

;; Taken from ALEXANDRIA
(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
	       (when subtree
		 (if (consp subtree)
		     (progn
		       (traverse (car subtree))
		       (traverse (cdr subtree)))
		     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun shuffle-new-stack ()
  (let ((s (append (flatten (loop for i from 39 upto 50 collect (list i i i i i i i)))
		   (list :headwind :headwind :headwind :headwind :headwind)
		   (list :spurt :spurt :spurt :spurt :spurt)
		   (list :ascent :ascent :ascent :ascent :ascent))))
    (flet ((pop-random ()
	     (let* ((i (random (length s)))
		    (card (nth i s)))
	       (setf (nth i s) (car s))
	       (pop s)
	       card)))
      (let ((shuffled nil))
	(loop until (null s) do
	     (push (pop-random) shuffled))
	shuffled))))

(defvar *print-game-events* nil "When non-NIL, print game events.")

(deftype card ()
  '(member 39 40 41 42 43 44 45 46 47 48 49 50 :headwind :spurt :ascent))

(defclass game ()
  ((stack :initarg :stack :initform (shuffle-new-stack) :accessor game-stack :type list :documentation "the game's remaining card stack")
   (players :initarg :players :accessor game-players :type (vector list) :documentation "the player's hand cards")
   (leader :initarg :leader :initform 0 :accessor game-leader :type fixnum :documentation "the index of the leading player")
   (speed :initarg :speed :initform 0 :accessor game-speed :type fixnum :documentation "the leader's speed")
   (ascentp :initarg :ascentp :initform nil :accessor game-ascentp :type boolean :documentation "whether :ASCENT was played on the leader")
   (penalties :initarg :penalties :accessor game-penalties :type (vector fixnum) :documentation "the accumulated penalty minutes")
   ;; the following slots provide the ai with information of the game state, and are in the GAME class because I don't want to start another class.
   (remaining :initarg :remaining :initform (make-array 15 :element-type 'real :initial-element 1.0) :accessor game-remaining :type (vector real 15) :documentation "The fraction of remaining cards in the game stack")
   ))

(defun card-index (card)
  "Return the CARD's index, which is an integer in [0;14]."
  (cond
    ((integerp card) (- card 39))
    ((eq card :headwind) 12)
    ((eq card :spurt) 13)
    ((eq card :ascent) 14)
    (t (error "unknown card ~S" card))))

(defun draw-card! (game player)
  (assert (not (null (game-stack game))))
  (let ((card (car (game-stack game))))
    (setf (game-stack game) (cdr (game-stack game)))
    (push card (elt (game-players game) player))
    (when (null (game-stack game))
      (throw :game-ends t)) ;caught in #'PLAYER-TURN in #'GAME-MAKE-PLAYER-TURN
    ;; update remaining
    (let ((index (card-index card)))
      (decf (aref (game-remaining game) index)
	    (cond
	      ((integerp card) 1/7)
	      (t 1/5))))))

(defun make-new-game (edition num-players starting-coins)
  (declare (ignore edition starting-coins))
  (let ((game (make-instance 'game
			     :players (make-array num-players :element-type 'list :initial-element nil)
			     :penalties (make-array num-players :element-type 'list :initial-element 0))))
    (loop for p below num-players do
	 (loop for i below 6 do
	      (draw-card! game p)))
    game))

(defmethod print-object ((game game) stream)
  (print-unreadable-object (game stream)
    (format stream "Stack: (~A) ~A~%" (length (game-stack game)) (game-stack game))
    (format stream "Player ~A leads with speed ~A~A~%" (game-leader game) (game-speed game) (if (game-ascentp game) " on ascent" ""))
    (loop for p below (length (game-players game)) do
	 (format stream "PLAYER ~A:~A(~A penalty) ~A~%"
		 p
		 (if (= p (game-leader game)) "*" " ")
		 (elt (game-penalties game) p)
		 (elt (game-players game) p)))))

(defun print-game (game)
  (print game))

(defun game-over-p (game)
  "Return the player-number of the winner if the game is over, NIL otherwise."
  (let ((num-players-with-cards nil))
    (loop for player from 0 for hand across (game-players game) do
	 (when (find-if #'integerp hand)
	   (push player num-players-with-cards)))
    (cond ((null (game-stack game))
	   (game-leader game))
	  ((<= (length num-players-with-cards) 1)
	   (game-leader game))
	  (t
	   nil))))

(defun player-won-p (game player-number)
  (= (game-over-p game) player-number))

(defmacro do1* (varlist endlist &body body)
  `(do* (,@(loop for (symbol update) in varlist collect `(,symbol ,update ,update)))
	,endlist
     ,@body))

(defun game-make-player-turn (draw-headwind select-speed-card select-ascent draw-ascent)
  "This function implements the state changes in the game that arise from a player taking its turn.
The functions UPDATER-BELIEFS, SELECTOR-CARDTARGET, SELECTOR-GUESS receive the current game state and must return the step that the player takes.
Return a function that is given a GAME instance, a TURN number, a PLAYER-NUMBER, and an AI-UPDATERS list and lets the player with PLAYER-NUMBER take its turn.
AI-UPDATERS is the list of updaters of all players, as returned by this function (#'GAME-MAKE-PLAYER-TURN).
Note that #'MAKE-NNET-AI-PLAYER and this function are separated because this function does not contain any ai-player specific code, but can also be used for a human player."
  (declare (optimize (debug 3)))
  (labels ((player-turn (game turn player-number ai-updaters)
	     (declare (ignore turn))
	     (catch :game-ends
	       ;; after one game round of players taking turns, remove ascent.
	       (when (= (game-leader game) player-number)
		 (setf (game-speed game) 0)
		 (when (game-ascentp game)
		   (setf (game-ascentp game) nil)))
	       (let* ((num-players (length (game-players game)))
		      (hand (elt (game-players game) player-number)))
		 ;; count all headwinds in hand, and draw up to one more cards than the player had before.
		 (multiple-value-bind (hand num-headwinds)
		     (let ((num-headwinds (count :headwind hand))
			   (hand (remove :headwind hand)))
		       (setf (elt (game-players game) player-number) hand)
		       (loop for i below (1+ num-headwinds) do
			    (when (funcall draw-headwind player-number game num-headwinds hand)
			      (when *print-game-events*
				(format t "Player ~A draws a card.~%" player-number))
			      (draw-card! game player-number))
			    (let ((hand (elt (game-players game) player-number)))
			      (when (find :headwind hand)
				(incf num-headwinds)
				(setf hand (remove :headwind hand)))
			      (setf (elt (game-players game) player-number) hand)))
		       (values hand num-headwinds))
		   (when *print-game-events*
		     (format t "Player ~S has ~Ahand ~S~%" player-number (if (> num-headwinds 0) (format nil "~A headwinds and " num-headwinds) "") hand))
		   (assert (null (remove :headwind hand :test-not #'eql)))
		   (when (null (remove :ascent (remove :spurt hand)))
		     (return-from player-turn t))
		   ;; select played card(s)
		   (let* ((played nil)
			  (hand (do1* ((selected (funcall select-speed-card player-number game hand played))
				       (old-hand hand)
				       (hand (remove selected hand :count 1)))
				    ((progn
				       (assert (not (equal old-hand hand)) () "player ~S doesn't have card ~S, but wants to play it~%old-hand:~S hand:~S" player-number selected old-hand hand)
				       (or (null hand) (integerp selected)))
				     (progn (push selected played) hand))
				  ;; TODO: FIXME: allow drawing another card for a non-integerp played card.
				  (assert (not (eq selected :ascent)))
				  (push selected played))))
		     ;; set current player's hand in GAME.
		     (setf (elt (game-players game) player-number) hand)
		     ;; compute current player's speed.
		     (let ((speed (* num-headwinds -2)))
		       (loop for card in played do
			    (cond ((eq card :spurt) (incf speed 2))
				  ((numberp card) (incf speed card))
				  (t (error "unknown card ~S" card))))
		       (when *print-game-events*
			 (format t "Player ~S plays ~S (speed ~S) and has hand ~S~%" player-number played speed hand))
		       ;; let players decide whether to play :ASCENT on this player's speed.
		       (let ((ascent
			      (and (not (game-ascentp game))
				   (dotimes (round (* 2 num-players) nil)
				     (let* ((player (mod (+ player-number round) num-players))
					    (select-ascent (car (elt ai-updaters player)))
					    (ascent (funcall select-ascent player game player-number round speed)))
				       (when ascent
					 (let ((draw-ascent (cadr (elt ai-updaters player))))
					   (when (funcall draw-ascent player game player-number speed)
					     (when *print-game-events*
					       (format t "Player ~A draws a card.~%" player))
					     (draw-card! game player)
					     (return player)))))))))
			 (setf played (nreverse played)) ;the number card is last
			 (setf (elt (game-players game) player-number) hand)
			 (when (and *print-game-events* ascent)
			   (format t "Player ~S gains ascent from player ~S~%" player-number ascent))
			 ;; remove :ASCENT card from ascent player's hand.
			 (when ascent
			   (assert (not (null (remove :ascent (elt (game-players game) ascent) :test-not #'eql :count 1))))
			   (setf (elt (game-players game) ascent) (remove :ascent (elt (game-players game) ascent) :count 1))
			   (setf played (cons :ascent played))
			   (setf speed (min speed 50)))
			 ;; update game status and penalties.
			 (cond
			   ((<= (elt (game-penalties game) player-number) 0)
			    (cond
			      ((and (game-ascentp game) (> speed (game-speed game)))
			       nil)
			      ((and (not (game-ascentp game)) (> speed (game-speed game)))
			       (setf (game-speed game) speed)
			       (setf (game-ascentp game) (if ascent t nil))
			       (setf (game-leader game) player-number))
			      ((and (game-ascentp game) (<= speed (game-speed game)))
			       (incf (elt (game-penalties game) player-number) (- (game-speed game) speed)))
			      ((and (not (game-ascentp game)) (<= speed (game-speed game)))
			       (let ((delta (- (game-speed game) speed)))
				 (when (> delta 2)
				   (incf (elt (game-penalties game) player-number) delta))))))
			   ((> (elt (game-penalties game) player-number) 0)
			    (let ((delta (- (game-speed game) speed)))
			      (incf (elt (game-penalties game) player-number) delta))))
			 ;; penalty may not be negative
			 (when (< (elt (game-penalties game) player-number) 0)
			   (setf (elt (game-penalties game) player-number) 0)))))))
	       (when *print-game-events*
		 (print-game game))
	       t)))
    (values #'player-turn (list select-ascent draw-ascent))))

;;;; AI FUNCTIONS

(load "game-nnet.lisp")

(defstruct ai
  (genes-draw-headwind nil :type nnet)
  (genes-select-speed-card nil :type nnet)
  (genes-select-ascent nil :type nnet)
  (genes-draw-ascent nil :type nnet))
  
(defun make-nnet-ai-player (edition num-players ai)
  "This function makes an nnet AI player, and must be implemented for every different ai type and game."
  (declare (ignore edition)
	   (optimize (debug 3)))
  (let* ((speed-size (1+ (- 60 29)))
	 (input-size-game (+ 15 ;remaining cards for each type of card
			     15 ;the player's hand cards
			     num-players ;the leading player
			     speed-size ;the leader's speed
			     1 ;whether ascent was played on the leader
			     (* num-players 20) ;the accumulated penalty minutes
			     1 ;remaining cards in the stack in total
			     )))
    (labels ((copy-game-to-input (input player game hand start-index)
	       (let ((num-players (length (game-players game))))
		 ;; remaining cards for each type of card
		 (copy-array-to-array (game-remaining game) input 0 start-index)
		 ;; the player's hand cards
		 (loop for card in hand do
		      (let ((index (card-index card)))
			(incf (aref input (+ start-index 15 index)) 1/7)))
		 (loop for i from 12 below 15 do
		      (setf (aref input (+ start-index 15 i)) (* 7/5 (aref input (+ start-index 15 i)))))
		 ;; the leading player
		 (let* ((leader (game-leader game))
			(leader-relative (mod (- leader player) num-players)))
		   (setf (aref input (+ start-index 15 15 leader-relative)) 1.0))
		 ;; the leader's speed
		 (let ((speed (game-speed game)))
		   (setf (aref input (+ start-index 15 15 num-players (- speed 29))) 1.0))
		 ;; whether ascent was played on the leader
		 (setf (aref input (+ start-index 15 15 num-players (1+ (- 60 29)))) (if (game-ascentp game) 1.0 0.0))
		 ;; the accumulated penalty minutes
		 (loop for p below num-players do
		      (let ((p-relative (mod (- p player) num-players)))
			(setf (aref input (+ start-index 15 15 num-players (1+ (- 60 29)) 1
					     (* p-relative 20)
					     (let ((penalty (elt (game-penalties game) p)))
					       (cond
						 ((< penalty 19) penalty)
						 (t 19)))))
			      1.0)))
		 ;; remaining cards in the stack in total
		 (setf (aref input (+ start-index 15 15 num-players (1+ (- 60 29)) 1 (* num-players 20)))
		       (/ (apply #'+ (loop for x across (game-remaining game) collect x)) 15))))
	     (draw-headwind (player game num-headwinds hand)
	       "return a boolean, indicating whether to draw a card from the stack or not."
	       (let ((input (make-array (+ 1 input-size-game 5) :element-type 'single-float :initial-element 0.0)))
		 (setf (aref input 0) 1.0) ;bias
		 (copy-game-to-input input player game hand 1)
		 (setf (aref input (+ 1 input-size-game (1- num-headwinds))) 1.0)
		 (let ((output (eval-nnet (ai-genes-draw-headwind ai) input)))
		   (assert (= (length output) 1))
		   (> (aref output 0) 0.5))))
	     (select-speed-card (player game hand played)
	       "return a card from HAND that is to be played together with the already PLAYED cards. If a number card is played, no other cards may be selected."
	       (let ((input (make-array (+ 1 input-size-game 1) :element-type 'single-float :initial-element 0.0)))
		 (setf (aref input 0) 1.0) ;bias
		 (copy-game-to-input input player game hand 1)
		 (let ((num-spurts (count :spurt played)))
		   (setf (aref input (+ 1 input-size-game)) (/ (float num-spurts) 5)))
		 (let ((output (eval-nnet (ai-genes-select-speed-card ai) input))
		       (output2 (make-array 13 :element-type 'single-float :initial-element 0.0)))
		   (assert (= (length output) 13))
		   ;; remove missing cards from possible list
		   (loop for card in hand do
			(cond
			  ((numberp card) (let ((index (card-index card)))
					    (setf (aref output2 index) (aref output index))))
			  ((eq card :spurt) (let ((index 12))
					      (setf (aref output2 index) (aref output index))))))
		   (let ((selected-index (maximal-index output2)))
		     (cond
		       ((= selected-index 12) :spurt)
		       (t (+ selected-index 39)))))))
	     (select-ascent (player game current-player round speed)
	       "return a boolean, indicating whether PLAYER should play :ASCENT on SPEED"
	       (let* ((hand (elt (game-players game) player))
		      (has-ascent (find :ascent hand)))
		 (when (not has-ascent)
		   (return-from select-ascent nil))
		 (let ((input (make-array (+ 1 input-size-game num-players (* num-players 2) speed-size) :element-type 'single-float :initial-element 0.0)))
		   (setf (aref input 0) 1.0) ;bias
		   (copy-game-to-input input player game hand 1)
		   (let ((relative-current-player (mod (- current-player player) num-players)))
		     (setf (aref input (+ 1 input-size-game relative-current-player)) 1.0))
		   (assert (<= 0 round (1- (* num-players 2))))
		   (setf (aref input (+ 1 input-size-game num-players round)) 1.0)
		   (setf (aref input (+ 1 input-size-game num-players (* num-players 2) (- speed 29))) 1.0)
		   (let ((output (eval-nnet (ai-genes-select-ascent ai) input)))
		     (assert (= (length output) 1))
		     (> (aref output 0) 0.5)))))
	     (draw-ascent (player game current-player speed)
	       "return a boolean, indicating whether to draw a card from the stack or not."
	       (let ((input (make-array (+ 1 input-size-game num-players speed-size) :element-type 'single-float :initial-element 0.0))
		     (hand (elt (game-players game) player)))
		 (setf (aref input 0) 1.0) ;bias
		 (copy-game-to-input input player game hand 1)
		 (let ((relative-current-player (mod (- current-player player) num-players)))
		   (setf (aref input (+ 1 input-size-game relative-current-player)) 1.0))
		 (setf (aref input (+ 1 input-size-game num-players (- speed 29))) 1.0)
		 (let ((output (eval-nnet (ai-genes-draw-ascent ai) input)))
		   (assert (= (length output) 1))
		   (> (aref output 0) 0.5)))))
      (game-make-player-turn #'draw-headwind #'select-speed-card #'select-ascent #'draw-ascent))))

(defun make-new-ai (edition num-players)
  "NUM-PLAYERS is the number of players in a game."
  (declare (ignore edition))
  (let* ((speed-size (1+ (- 60 29)))
	 (input-size-game (+ 15 ;remaining cards for each type of card
			     15 ;the player's hand cards
			     num-players ;the leading player
			     speed-size ;the leader's speed
			     1 ;whether ascent was played on the leader
			     (* num-players 20) ;the accumulated penalty minutes
			     1 ;remaining cards in the stack in total
			     ))
	 (genes-draw-headwind (make-new-nnet (list (+ 1 input-size-game 5) (+ 1 input-size-game 5) 1)))
	 (genes-select-speed-card (make-new-nnet (list (+ 1 input-size-game 1) (+ 1 input-size-game 1) (+ 1 input-size-game 1) 13)))
	 (genes-select-ascent (make-new-nnet (list (+ 1 input-size-game num-players (* num-players 2) speed-size) (+ 1 input-size-game) 1)))
	 (genes-draw-ascent (make-new-nnet (list (+ 1 input-size-game num-players speed-size) (+ 1 input-size-game) 1))))
    (make-ai :genes-draw-headwind genes-draw-headwind :genes-select-speed-card genes-select-speed-card :genes-select-ascent genes-select-ascent :genes-draw-ascent genes-draw-ascent)))

(defun make-ai-offspring (parent1 parent2)
  (make-ai :genes-draw-headwind (make-nnet-offspring (ai-genes-draw-headwind parent1) (ai-genes-draw-headwind parent2))
	   :genes-select-speed-card (make-nnet-offspring (ai-genes-select-speed-card parent1) (ai-genes-select-speed-card parent2))
	   :genes-select-ascent (make-nnet-offspring (ai-genes-select-ascent parent1) (ai-genes-select-ascent parent2))
	   :genes-draw-ascent (make-nnet-offspring (ai-genes-draw-ascent parent1) (ai-genes-draw-ascent parent2))))

(load "game-training.lisp")

;;(train nil 4 30000 :fraction-keep 0.9 :games-per-iteration 15)
#|
(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(let ((*print-game-events* t)
      (scores (alexandria:copy-array *last-scores*)))
  (setf scores (sort scores #'score-better))
  (loop for score across scores for i from 0 do
       (format t "~3D. ~S~%" i (print-score score nil)))
  (let ((winners (loop for i below 1 collect
		      (run-game nil (mapcar (lambda (x) (elt scores x)) '(7 14 14 14))))))
    (mapcar (lambda (player-num) (cons player-num (count player-num winners)))
	    '(0 1 2 3))))
|#
