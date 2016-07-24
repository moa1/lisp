;; DONE: when an ai plays :KING, there is no entry in the history that shows the cards that each swapping player had. Maybe instead, I should have a nnet that is updated when the player plays :PRIEST or :KING (and in the case of :KING, also the target's nnet is updated), and the nnet receives the other player's player number (i.e. TARGET for PLAYER, and PLAYER for TARGET) and the knowledge about that player's card; the output of the nnet is kept in an array for each player (that means the array stores the ai's knowledge about the other players), and is fed into the SELECT-CARD, SELECT-TARGET, SELECT-GUESS nnets instead of the current HISTORY array. This might help the ais to represent knowledge about other players' hand, which they currently suck at. Hmm... Maybe update each player's belief of the other players' hands after each player, and feed it the played card, the belief-array of the targeted player, and the belief-array of the playing player, and interpret the output as the updated belief-arrays of the two players (targeted and playing player). That way the ais might learn that if a player plays :KING on somebody, probably that somebody has :PRINCESS.
;; maybe TODO (only maybe because I'm assigning the players in the next round randomly so the first-player advantage should even out): make the last winner be the dealer for the next round.

(load "game-nnet.lisp")

#|
1 princess: if you play/drop her, you lose.
1 duchess: must play her if you have the king or prince.
1 king: exchange your card with another player.
2 princes: a player (may be self) must drop his/her card, and redraw another card.
2 abigails: you are protected in the next round.
2 barons: compare with another player, the one with the lower card loses.
2 priests: look at the card of a player.
5 watchwomen: if you guess the card of another player, he/she loses. may not guess watchwoman.

one card is put aside. each of the 4 players gets one card. then, for each player: the player draws a second card, and plays one of them. this continues until only one player remains, or no cards are left to draw. in the latter case the player with the highest card gets a heart, and must shuffle next round (so that the player next in clockwise order is the next first player). the first player to get 3 hearts wins.
|#

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

(defvar *print-game-events* nil "When non-NIL, print game events.")

(deftype card ()
  '(member :watchwoman :priest :baron :abigail :prince :king :duchess :princess))

(defun shuffle-new-stack ()
  (let ((s (copy-list '(:princess :duchess :king :prince :prince :abigail :abigail :baron :baron :priest :priest :watchwoman :watchwoman :watchwoman :watchwoman :watchwoman))))
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

(defclass game ()
  ((players :initarg :players :accessor game-players :type (simple-array symbol 4))
   (protected :initarg :protected :initform (make-array 4 :element-type 'boolean :initial-contents '(nil nil nil nil)) :accessor game-protected :type (simple-array boolean 4))
   (stack :initarg :stack :accessor game-stack :type list)
   (lastcard :initarg :lastcard :accessor game-lastcard :type symbol)
   ;; the following slots provide the ai with information of the game state, and are in the GAME class because I don't want to start another class.
   (played :initarg :played :accessor game-played :initform (make-array 8 :element-type 'single-float :initial-element 0.0) :type (simple-array single-float 8) :documentation "For each card type, the fraction of cards that has been played.")
   (beliefs :initarg :belief :initform (make-array 4 :element-type 'simple-vector :initial-contents (loop for i below 4 collect (make-array 4 :element-type 'simple-vector :initial-contents (loop for i below 4 collect (make-array 8 :element-type 'single-float :initial-element 0.0 :adjustable nil))))) :accessor game-beliefs :type (simple-vector 4) :documentation "for each player, the belief-vector of the 8 cards for all other players")
   ))

(defmethod print-object ((game game) stream)
  (print-unreadable-object (game stream)
    (format stream "PLAYERS: ~S PROTECTED: ~S~%  STACK: ~S~%  LASTCARD: ~S PLAYED: ~S~%" (game-players game) (game-protected game) (game-stack game) (game-lastcard game) (game-played game)
	    ;;(map 'vector (lambda (x) (map 'vector (lambda (x) (map 'vector (lambda (x) (let ((step 0.02)) (* (round x step) step))) x)) x)) (game-beliefs game))
	    )))

(defun print-game (game)
  (format t "~S~%" game))

(defun make-game ()
  (let ((s (shuffle-new-stack)))
    (make-instance 'game
		   :lastcard (pop s)
		   :players (make-array 4 :element-type 'symbol :initial-contents (list (pop s) (pop s) (pop s) (pop s)))
		   :stack s)))

(defun card-power (card)
  "Return the power of CARD."
  (let ((pos (position card '(:watchwoman :priest :baron :abigail :prince :king :duchess :princess) :test #'eq)))
    (assert (not (null pos)) () "Invalid card ~S" card)
    (1+ pos)))

(defun card-from-power (power)
  "Return the CARD that has power POWER."
  (aref #(:watchwoman :priest :baron :abigail :prince :king :duchess :princess) (1- power)))

(defun card-count (card)
  "Return the number of cards of type CARD."
  (let ((counts #(5 2 2 2 2 1 1 1))
	(power (card-power card)))
    (aref counts (1- power))))

(defun lose (game player)
  (when *print-game-events*
    (format t "Player ~S loses~%" player))
  ;; update played card list
  (let ((card (aref (game-players game) player)))
    (incf (aref (game-played game) (1- (card-power card))) (/ 1.0 (card-count card))))
  (setf (aref (game-players game) player) nil))

(defun play (game current card &optional target guess)
  "Player CURRENT plays CARD against player TARGET.
When calling this function, (AREF (GAME-PLAYERS GAME) CURRENT) must be the other card CURRENT has.
Return the card player TARGET has, or NIL if unknown."
  (declare (optimize (debug 3))
	   (type game game)
	   (type (integer 0 4) current)
	   (type card card)
	   (type (or (integer 0 4) null) target)
	   (type (member :princess :duchess :king :prince :abigail :baron :priest nil) guess))
  (let ((p (game-players game))
	(ret nil))
    ;; check legality of turn
    (assert (or (null target) (not (null (aref p target)))) () "Player ~S may not target out-of-game player ~S" current target)
    (when (aref (game-protected game) current)
      (setf (aref (game-protected game) current) nil))
    (assert (or (null target) (null (aref (game-protected game) target))) () "Player ~S cannot attack protected player ~S" current target)
    ;; play the turn
    (ecase card
      ((:watchwoman) (assert (not (null guess))) (when (and (eq guess (aref p target)) (/= current target)) (lose game target)))
      ((:priest) (setf ret (aref p target)))
      ((:baron) (let ((current-power (card-power (aref p current))) (target-power (card-power (aref p target))))
		  (cond
		    ((> current-power target-power) (lose game target))
		    ((< current-power target-power) (lose game current)))))
      ((:abigail) (setf (aref (game-protected game) current) t))
      ((:prince)
       (assert (not (eq (aref p current) :duchess)) () "Player ~S may not play prince, must play duchess" current)
       (if (eq (aref p target) :princess)
	   (lose game target)
	   (let ((next (pop (game-stack game))))
	     ;; update played card list
	     (let ((card (aref p target)))
	       (incf (aref (game-played game) (1- (card-power card))) (/ 1.0 (card-count card))))
	     (setf (aref p target) (if (null next) (game-lastcard game) next)))))
      ((:king)
       (assert (not (eq (aref p current) :duchess)) () "Player ~S may not play king, must play duchess" current)
       (setf ret (aref p current)) ;we know our current card
       (psetf (aref p current) (aref p target) (aref p target) (aref p current)))
      ((:duchess))
      ((:princess) (lose game current)))
    (incf (aref (game-played game) (1- (card-power card))) (/ 1.0 (card-count card)))
    ret))

(defun forced-card (card1 card2)
  "If you are forced to play CARD1 or CARD2, return :CARD1, or :CARD2, respectively, or NIL if you are free to play any of the two."
  (let ((orig1 :card1))
    (when (eq card2 :duchess)
      (psetf card1 card2 card2 card1 orig1 :card2))
    (if (and (eq card1 :duchess) (or (eq card2 :king) (eq card2 :prince)))
	orig1
	nil)))

;;;; FUNCTIONS NEEDED FOR TRAINING

(defun make-new-game (edition num-players starting-coins)
  (declare (ignore edition num-players starting-coins))
  (make-game))

(defun player-won-p (game player-number)
  (or (and (null (game-stack game))
	   (let ((max-power (apply #'max (map 'list #'card-power (remove nil (game-players game)))))
		 (my-card (aref (game-players game) player-number)))
	     (and (not (null my-card)) (= (card-power my-card) max-power))))
      (let ((players (game-players game)))
	(every #'null (concatenate 'list (subseq players 0 player-number) (subseq players (1+ player-number)))))))

(defun game-over-p (game)
  "Return the player-number of the winner if the game is over, NIL otherwise."
  (let ((num-players (length (game-players game))))
    (loop for player-number below num-players do
	 (when (player-won-p game player-number)
	   (return-from game-over-p player-number)))
    nil))

(defun game-make-player-turn (updater-beliefs selector-cardtarget selector-guess)
  "This function implements the state changes in the game that arise from a player taking its turn.
The functions UPDATER-BELIEFS, SELECTOR-CARDTARGET, SELECTOR-GUESS receive the current game state and must return the step that the player takes.
Return a function that is given a GAME instance and a PLAYER-NUMBER, and lets the player with PLAYER-NUMBER take its turn.
Note that #'MAKE-NNET-AI-PLAYER and this function are separated because this function does not contain any ai-player specific code, but can also be used for a human player."
  (declare (optimize (debug 3)))
  (labels ((player-turn (game turn player-number ai-updaters)
	     "AI-UPDATERS is the list of updaters of all players, as returned by this function (#'GAME-MAKE-PLAYER-TURN)."
	     (let* ((players (game-players game))
		    (card1 (aref players player-number)))
	       (when (null card1) (return-from player-turn nil)) ;skip players that lost
	       (when *print-game-events*
		 (format t "TURN ~S, PLAYER ~S:~%" turn player-number)
	       (print-game game))
	       ;; unprotect current player
	       (when (aref (game-protected game) player-number)
		 (setf (aref (game-protected game) player-number) nil))
	       (let* ((card2 (pop (game-stack game))))
		 (multiple-value-bind (wanted-card wanted-target) (funcall selector-cardtarget game turn player-number card1 card2)
		   (let* ((forced (forced-card card1 card2))
			  (card-to-play (if forced forced wanted-card))
			  (play-card (ecase card-to-play ((:card1) card1) ((:card2) card2)))
			  (remaining-card (ecase card-to-play ((:card1) card2) ((:card2) card1)))
			  (target (when (position play-card #(:watchwoman :priest :baron :prince :king)) wanted-target))
			  (guess (when (eq play-card :watchwoman) (funcall selector-guess game turn player-number target))))
		 (setf (aref players player-number) remaining-card) ;this must be done before calling #'PLAY
		 (when *print-game-events*
		   (format t "Belief of player ~S:~S~%" player-number (aref (game-beliefs game) player-number))
		   (format t "Player ~S plays ~S (keeps ~S)" player-number play-card remaining-card)
		   (when target
		     (format t " against player ~S" target)
		     (when guess
		       (format t " and guesses card ~S" guess)))
		   (format t ".~%"))
		 (let* ((target-player-card (when target (aref players target)))
			(shown-card (play game player-number play-card target guess))
			(current-player-losing-card (when (null (aref players player-number)) remaining-card))
			(target-player-losing-card (when (and target (null (aref players target))) target-player-card)))
		   (when (and *print-game-events* shown-card)
		     (format t "and knows that she has ~S~%" shown-card))
		   ;; update beliefs
		   (if shown-card
		       (let ((target-belief (aref (aref (game-beliefs game) player-number) target)))
			 (loop for i below 8 do (setf (aref target-belief i) 0.0))
			 (setf (aref target-belief (1- (card-power shown-card))) 1.0))
		       (funcall updater-beliefs game turn player-number play-card player-number target current-player-losing-card target-player-losing-card))
		   ;; update beliefs of other ais
		   (loop for ai-updater across ai-updaters for ai-number from 0 do
			(when (/= ai-number player-number)
			  (funcall ai-updater game turn ai-number play-card player-number target current-player-losing-card target-player-losing-card))))))))
	     t))
    (values #'player-turn updater-beliefs)))

(defstruct ai
  (genes-belief nil :type nnet)
  (genes-cardtarget nil :type nnet))

(defun maximal-index (sequence)
  "Return two values: the index with the highest value in SEQUENCE and the highest value, or NIL if SEQUENCE is empty."
  (when (= 0 (length sequence))
    (return-from maximal-index nil))
  (let ((max-value (elt sequence 0))
	(max-index 0)
	(len (length sequence)))
    (loop
       for i from 1 below len do
	 (let ((e (elt sequence i)))
	   (when (> e max-value)
	     (setf max-value e max-index i))))
    (values max-index max-value)))

(defun copy-array-to-array (input output &optional (input-start-index 0) (output-start-index 0) (size (length input)))
  (loop for i below size do
       (setf (aref output (+ i output-start-index)) (aref input (+ i input-start-index)))))

(defun make-nnet-ai-player (edition num-players ai)
  "This function makes an nnet AI player, and must be implemented for every different ai type and game."
  (declare (ignore edition)
	   (optimize (debug 3)))
  (let* ((input-size-beliefs (* num-players 8))) ;the belief array about the current player ;the belief array about the target player ;the two belief arrays about the 2 other players
    (labels ((copy-belief-to-input (input start-index beliefs player-number)
	       (let ((belief (aref beliefs player-number)))
		 (loop for i below 8 do
		      (setf (aref input (+ start-index i)) (aref belief i)))))
	     (copy-beliefs-to-input (input start-index beliefs player-1 player-2 player-3 player-4)
	       (copy-belief-to-input input (+ start-index) beliefs player-1)
	       (copy-belief-to-input input (+ 8 start-index) beliefs player-2)
	       (copy-belief-to-input input (+ 8 8 start-index) beliefs player-3)
	       (copy-belief-to-input input (+ 8 8 8 start-index) beliefs player-4))
	     (updater-beliefs (game turn player-number card current-player target-player current-player-losing-card target-player-losing-card)
	       "Update the belief of player PLAYER-NUMBER when CURRENT-PLAYER plays CARD on TARGET-PLAYER."
	       (when (null target-player)
		 (setf target-player (car (remove current-player '(0 1 2 3)))))
	       (let ((input (make-array (+ 1
					   1 ;turn
					   8 ;the played card
					   8 ;the fraction of cards left
					   8 ;the remaining card of CURRENT-PLAYER if she lost this turn
					   8 ;the remaining card of TARGET-PLAYER if she lost this turn
					   input-size-beliefs ;in the order CURRENT-PLAYER, TARGET-PLAYER, other players
					   )
					:element-type 'single-float
					:initial-element 0.0)))
		 (setf (aref input 0) 1.0) ;bias
		 (assert (<= turn 16))
		 (setf (aref input 1) (float (/ turn 16)))
		 (setf (aref input (+ 1 1 (1- (card-power card)))) 1.0)
		 (copy-array-to-array (game-played game) input 0 (+ 1 1 8))
		 (when current-player-losing-card
		   (setf (aref input (+ 1 1 8 8 (1- (card-power current-player-losing-card)))) 1.0))
		 (when target-player-losing-card
		   (setf (aref input (+ 1 1 8 8 8 (1- (card-power target-player-losing-card)))) 1.0))
		 (let* ((beliefs (aref (game-beliefs game) player-number))
			(other-players (remove target-player (remove current-player '(0 1 2 3))))
			(other-player-1 (car other-players))
			(other-player-2 (cadr other-players)))
		   (copy-beliefs-to-input input (+ 1 1 8 8 8 8) beliefs current-player target-player other-player-1 other-player-2)
		   (let ((output (eval-nnet (ai-genes-belief ai) input)))
		     (copy-array-to-array output (aref beliefs current-player) 0 0 8)
		     (copy-array-to-array output (aref beliefs target-player) 8 0 8)
		     (copy-array-to-array output (aref beliefs other-player-1) 16 0 8)
		     (copy-array-to-array output (aref beliefs other-player-2) 24 0 8)
		     nil))))
	     (selector-cardtarget (game turn player-number card1 card2)
	       (let ((input (make-array (+ 1
					   1 ;turn
					   8 ;cards possible to play
					   8 8 8 ;beliefs about all other players
					   )
					:element-type 'single-float
					:initial-element 0.0))
		     (card1-index (1- (card-power card1)))
		     (card2-index (1- (card-power card2)))
		     (beliefs (aref (game-beliefs game) player-number)))
		 (setf (aref input 0) 1.0) ;bias
		 (assert (<= turn 16))
		 (setf (aref input 1) (float (/ turn 16)))
		 (let ((player-belief (make-array 8 :element-type 'single-float :initial-element 0.0)))
		   (setf (aref player-belief card1-index) 1.0)
		   (setf (aref player-belief card2-index) 1.0)
		   (setf (aref beliefs player-number) player-belief))
		 (copy-beliefs-to-input input (+ 1 1) beliefs
					player-number (mod (+ 1 player-number) 4) (mod (+ 2 player-number) 4) (mod (+ 3 player-number) 4))
		 (let ((output (eval-nnet (ai-genes-cardtarget ai) input)))
		   (values
		    (if (> (aref output card1-index) (aref output card2-index)) :card1 :card2) ;card to play
		    (flet ((get-target (target-offset-number)
			     (let ((player-number (mod (+ player-number 1 target-offset-number) 4)))
			       ;;(prind target-offset-number player-number (aref (game-protected game) player-number) (null (aref (game-players game) player-number)))
			       (if (or (aref (game-protected game) player-number) (null (aref (game-players game) player-number)))
				   -1.0
				   (aref output (+ 8 target-offset-number))))))
		      (let* ((other-players (make-array 3 :element-type 'single-float :initial-contents (list (get-target 0) (get-target 1) (get-target 2))))
			     (max-player (multiple-value-bind (i v) (maximal-index other-players)
					   (if (= v -1.0) 3 i)))
			     (target (mod (+ player-number 1 max-player) 4))) ;target player
			;;(prind player-number other-players target)
			target
			))))))
	     (selector-guess (game turn player-number target-player)
	       (declare (ignore turn))
	       (let* ((beliefs (aref (game-beliefs game) player-number))
		      (belief (aref beliefs target-player))
		      (belief-copy (make-array 8 :element-type 'single-float :initial-element 0.0)))
		 (copy-array-to-array belief belief-copy)
		 (setf (aref belief-copy 0) -1.0) ;may not guess watchwoman
		 (card-from-power (1+ (maximal-index belief-copy))))))
      (game-make-player-turn #'updater-beliefs #'selector-cardtarget #'selector-guess))))

(defun make-new-ai (edition num-players)
  "NUM-PLAYERS is the number of players in a game."
  (declare (ignore edition))
  (let* ((input-size-beliefs (* num-players 8)) ;the size of the (flattened) belief-array of a player
	 (genes-belief (make-new-nnet (list (+ 1 1 8 8 8 8 input-size-beliefs) (+ 1 64 input-size-beliefs) (+ 1 8 input-size-beliefs) input-size-beliefs)))
	 (genes-cardtarget (make-new-nnet (list (+ 1 1 input-size-beliefs) 50 16 (+ 8 3)))))
    (make-ai :genes-belief genes-belief :genes-cardtarget genes-cardtarget)))

(defun make-ai-offspring (parent1 parent2)
  (make-ai :genes-belief (make-nnet-offspring (ai-genes-belief parent1) (ai-genes-belief parent2))
	   :genes-cardtarget (make-nnet-offspring (ai-genes-cardtarget parent1) (ai-genes-cardtarget parent2))))

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
