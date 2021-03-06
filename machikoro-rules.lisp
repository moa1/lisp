(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)

;;;; AUXILIARY

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

(defun shuffle! (&rest sequences)
  "Destructively shuffle the elements of all SEQUENCES in the same manner."
  (when (not (null sequences))
    (let ((length (length (car sequences))))
      (assert (every (lambda (x) (= (length x) length)) (cdr sequences)) () "All sequences must have the same length")
      (loop for i from (1- length) downto 1 do
	   (let* ((j (random (1+ i))))
	     (loop for seq in sequences do
		  (rotatef (elt seq i) (elt seq j)))))))
  (apply #'values sequences))

(let ((a (loop for i below 100 collect i))
      (b (loop for i below 100 collect i)))
  (shuffle! a b)
  (assert (every #'= a b)))

;;;; CARD

(defclass card ()
  ((name :initform "" :initarg :name :accessor card-name :type string)
   (symbol :initform :undef :initarg :symbol :accessor card-symbol :type symbol)
   (cost :initform -1 :initarg :cost :accessor card-cost :type (and unsigned-byte fixnum))
   (description :initform "" :initarg :description :accessor card-description :type string)
   (edition :initform "" :initarg :edition :accessor card-edition :type string)))

(defclass normal-card (card)
  ((dice :initform 0 :initarg :dice :accessor card-dice :type list) ;list of (integer 1 12)
   (activation :initform :undef :initarg :activation :accessor card-activation :type (member :any :self))
   (color :initform :undef :initarg :color :accessor card-color :type symbol)))

(defclass large-card (card)
  ())

(defun make-normal-card (&rest rest)
  (apply #'make-instance 'normal-card rest))
(defun make-large-card (&rest rest)
  (apply #'make-instance 'large-card rest))

(load "/home/toni/lisp/machikoro-cards.lisp") ;generated by machikoro-parse-cards.py
(assert (null (find-if (lambda (x) (equal "" (card-name x))) +large-cards+)))
(assert (null (find-if (lambda (x) (equal "" (card-name x))) +normal-cards+)))

(defun find-card (card-name)
  (or (find-if (lambda (x) (equal card-name (card-name x))) +normal-cards+)
      (find-if (lambda (x) (equal card-name (card-name x))) +large-cards+)))

;;;; EDITION

(defstruct edition
  (name "" :type string)
  (normal-cards nil :type sequence) ;sequence of NORMAL-CARDs
  (large-cards nil :type sequence) ;sequence of LARGE-CARDs
  (number-normal-cards nil :type sequence) ;sequence of INTEGERs
  (map-card-function (constantly nil) :type (function (card) integer)) ;function mapping CARD to its index
  (map-number-function (constantly nil) :type (function (integer) card)) ;function mapping card number to CARD
  (starting-cards #() :type (array integer)) ;an array of size (+ (length NORMAL-CARDS) (length LARGE-CARDS)), with each element being the number of cards with that index
  (stack-size 0 :type (and unsigned-byte integer)))

(defun make-edition-real (name normal-cards normal-cards-total-number large-cards starting-cards stack-size)
  (let* ((nc-length (length normal-cards))
	 (lc-length (length large-cards))
	 (sc (make-array (+ nc-length lc-length) :initial-element 0)))
    (flet ((map-c-fn (card)
	     (declare (type card card))
	     (let ((number (if (typep card 'normal-card)
			       (position card normal-cards :test #'equalp)
			       (+ nc-length (position card large-cards :test #'equalp)))))
	       (assert (integerp number) () "Cannot find ~S in the normal-cards or large-cards of edition ~S" card name)
	       number))
	   (map-n-fn (number)
	     (declare (type integer number))
	     (assert (and (<= 0 number) (< number (+ nc-length lc-length))) () "Card number out ~S of range (0 ~S) in edition ~S" number (+ nc-length lc-length) name)
	     (if (< number nc-length)
		 (elt normal-cards number)
		 (elt large-cards (- number nc-length)))))
      (mapcar (lambda (card) (assert (typep card 'normal-card))) normal-cards)
      (mapcar (lambda (card) (assert (typep card 'large-card))) large-cards)
      (loop for card in starting-cards do
	   (incf (aref sc (map-c-fn card))))
      (mapcar (lambda (card) (assert (equal card (funcall #'map-n-fn (funcall #'map-c-fn card)))))
	      (append normal-cards large-cards))
      (make-edition :name name
		    :normal-cards normal-cards
		    :large-cards large-cards
		    :number-normal-cards normal-cards-total-number
		    :map-card-function #'map-c-fn
		    :map-number-function #'map-n-fn
		    :starting-cards sc
		    :stack-size stack-size))))

(let ((nc '(("Stadion" . 4) ("Fernsehsender" . 4) ("Bürohaus" . 4)
	    ("Bergwerk" . 6) ("Apfelplantage" . 6) ("Bäckerei" . 6) ("Mini-Markt" . 6) ("Molkerei" . 6) ("Möbelfabrik" . 6) ("Markthalle" . 6) ("Weizenfeld" . 6) ("Bauernhof" . 6) ("Wald" . 6) ("Familienrestaurant" . 6) ("Café" . 6)))
      (lc '("Bahnhof" "Einkaufszentrum" "Freizeitpark" "Funkturm"))
      (sc '("Weizenfeld" "Bäckerei")))
  (flet ((card-name-to-instance (name)
	   (or (find name +normal-cards+ :test 'equal :key #'card-name)
	       (find name +large-cards+ :test 'equal :key #'card-name))))
    (defconstant +base-edition+
      (make-edition-real "base-edition"
			 (mapcar #'card-name-to-instance (mapcar #'car nc))
			 (mapcar #'cdr nc)
			 (mapcar #'card-name-to-instance lc)
			 ;;TODO: FIXME: allow large card names here as well)
			 (mapcar #'card-name-to-instance sc)
			 12))
    (defconstant +base-edition-15+
      (make-edition-real "base-edition-15"
			 (mapcar #'card-name-to-instance (mapcar #'car nc))
			 (mapcar #'cdr nc)
			 (mapcar #'card-name-to-instance lc)
			 ;;TODO: FIXME: allow large card names here as well)
			 (mapcar #'card-name-to-instance sc)
			 15))))

;;;; GAME

(defstruct player
  (cards #() :type (array integer)) ;array of INTEGERs, each integer being the number of cards the player has with that card index (in the edition of the game currently played).
  (coins 3 :type integer)
  (turn-function (constantly nil) :type function))

(defstruct game
  (edition nil :type edition)
  (players #() :type (array player)) ;list of PLAYERs
  (stack #() :type (array integer)) ;array of INTEGERs, with the index of the array being the normal card number and the value equal to the number of available cards of that normal card type
  (deck nil :type list)) ;list of normal card numbers

(defvar *print-game-events* nil "When non-NIL, print game events.")

(defun deal-card! (game)
  "Deal one card from the deck of the GAME to the stack of the GAME.
Returns whether the type of the dealt card was already present in the stack or not."
  (let* ((stack (game-stack game))
	 (new-card-number (pop (game-deck game)))
	 (new-card-stack-number (aref stack new-card-number)))
    (incf (aref stack new-card-number))
    (when *print-game-events*
      (format t "CARD ~A IS DEALT FROM THE DECK.~%" (card-name (funcall (edition-map-number-function (game-edition game)) new-card-number))))
    (/= 0 new-card-stack-number)))

(defun make-new-game (edition num-players starting-coins)
  "Make a new game with EDITION, NUM-PLAYERS having STARTING-COINS each."
  (flet ((make-new-player (starting-coins)
	   (make-player :cards (alexandria:copy-array (edition-starting-cards edition)) :coins starting-coins)))
    (let* ((deck (shuffle! (alexandria:flatten (loop for i from 0 for n in (edition-number-normal-cards +base-edition+) collect (loop for x below n collect i)))))
	   (players-list (loop for i below num-players collect (make-new-player starting-coins)))
	   (players (make-array num-players :initial-contents players-list))
	   (number-normal-cards (length (edition-normal-cards edition)))
	   (stack-size (edition-stack-size edition))
	   (game (make-game :edition edition
			    :players players
			    :stack (make-array number-normal-cards :initial-element 0)
			    :deck deck)))
      (loop until (or (null (game-deck game)) (= stack-size 0)) do
	   (when (not (deal-card! game))
	     (decf stack-size)))
      game)))

(defun print-game (game)
  (declare (optimize (debug 3)))
  (labels ((print-stack (stack-array)
	     (let* ((edition (game-edition game))
		    (cards (loop for card-number from 0 for count across stack-array collect
				(funcall (edition-map-number-function edition) card-number)))
		    (normal-cards (remove-if (lambda (x) (not (typep x 'normal-card))) cards))
		    (large-cards (remove-if (lambda (x) (not (typep x 'large-card))) cards)))
	       (setf normal-cards (sort normal-cards #'< :key (lambda (x) (car (card-dice x)))))
	       (setf large-cards (sort large-cards #'< :key #'card-cost))
	       (flet ((card-count (card)
			(aref stack-array
			      (funcall (edition-map-card-function edition) card))))
		 (let ((printed-cards 0))
		   (loop for card in normal-cards do
			(let ((count (card-count card)))
			  (when (> count 0)
			    (when (and (/= printed-cards 0) (= (mod printed-cards 3) 0))
			      (format t "~%"))
			    (incf printed-cards)
			    (format t "  ~A~S:~S" (card-name card) (card-dice card) count)))))
		 (format t "~%")
		 (loop for card in large-cards do
		      (let ((count (card-count card)))
			(when (> count 0)
			  (format t " ~A" (card-name card)))))))))
    (format t "Game stack:~%")
    (print-stack (game-stack game))
    (loop
       for player-number from 0
       for player across (game-players game) do
	 (format t "~%Player ~S (~S coins):~%" player-number (player-coins player))
	 (print-stack (player-cards player)))
    (format t "~%")))

(defun game-card (game card-number)
  (funcall (edition-map-number-function (game-edition game)) card-number))

(defun buy-card! (game player-number card-number)
  (declare (optimize (debug 3)))
  (let* ((stack (game-stack game))
	 (player (aref (game-players game) player-number))
	 (card (game-card game card-number))
	 (cost (card-cost card))
	 (player-cards (player-cards player)))
    (when *print-game-events*
      (format t "===> PLAYER ~S BUYS ~S FOR ~S~%" player-number (card-name card) cost))
    (assert (or (typep card 'large-card) (> (aref stack card-number) 0)) () "No ~S card left in game card stack!" card)
    (assert (>= (player-coins player) cost) () "Player ~S needs ~S coins to buy card ~S, but has only ~S left." player-number cost card (player-coins player))
    (decf (player-coins player) cost)
    ;; TODO: check that the player may have only 1 of some cards.
    (let ((card (game-card game card-number)))
      (when (and (typep card 'normal-card) (eq (card-color card) :pink))
	(assert (= 0 (aref player-cards card-number)) () "Player ~S wants to buy the pink card ~S, but already has ~S of them." player-number card (aref player-cards card-number)))
      (when (typep card 'large-card)
	(assert (= 0 (aref player-cards card-number)) () "Player ~S wants to buy the large card ~S, but already has ~S of them." player-number card (aref player-cards card-number))))
    (incf (aref player-cards card-number))
    (when (typep card 'normal-card)
      (decf (aref stack card-number))
      (when (<= (aref stack card-number) 0)
	;; redeal cards until at least one more type of card is dealt.
	(loop do
	     (if (null (game-deck game))
		 (return)
		 (when (not (deal-card! game))
		   (return))))))))

(defun player-get-card-count (game player-or-player-number card-or-card-number)
  "Return the number of a given card a given player has.
PLAYER-OR-PLAYER-NUMBER must either be of type PLAYER or a player-number.
CARD-OR-CARD-NUMBER must either be of type NORMAL-CARD or LARGE-CARD or a card number."
  (let* ((player (if (typep player-or-player-number 'player)
		     player-or-player-number
		     (aref (game-players game) player-or-player-number)))
	 (cards (player-cards player))
	 (card-number (cond
			((typep card-or-card-number 'card)
			 (funcall (edition-map-card-function (game-edition game)) card-or-card-number))
			(t
			 card-or-card-number))))
    (aref cards card-number)))

(defun player-has-card (game player-or-player-number card-or-card-number)
  (> (player-get-card-count game player-or-player-number card-or-card-number) 0))

(defun player-won-p (game player-number)
  (and (player-has-card game player-number (find-card "Bahnhof"))
       (player-has-card game player-number (find-card "Einkaufszentrum"))
       (player-has-card game player-number (find-card "Freizeitpark"))
       (player-has-card game player-number (find-card "Funkturm"))))

(defun game-over-p (game)
  "Return the player-number of the winner if the game is over, NIL otherwise."
  (let ((num-players (length (game-players game))))
    (loop for player-number below num-players do
	 (when (player-won-p game player-number)
	   (return-from game-over-p player-number)))
    nil))

(defun game-eval-dice-roll! (game player-number roll)
  "Carry out the effects when the die/dice rolled (the sum) ROLL, and it is PLAYER-NUMBER's turn."
  (assert (< 0 roll 13))
  (let ((shoppingmall+1 (player-get-card-count game player-number (find-card "Einkaufszentrum")))
	(players (game-players game))
	(edition (game-edition game)))
    (labels ((activated-card-number (player-number card)
	       "If CARD is activated this turn, returns the number of CARDS the PLAYER-NUMBER has, 0 otherwise."
	       (if (find roll (card-dice card))
		   (let ((count (player-get-card-count game player-number card)))
		     (when (and *print-game-events* (> count 0))
		       (format t "PLAYER ~S ACTIVATES CARD ~S ~S TIMES.~%" player-number (card-name card) count))
		     count)
		   0))
	     (player-pay (from-player-number to-player-number coins)
	       (let ((coins2 (min (player-coins (aref players from-player-number)) coins)))
		 (when (and *print-game-events* (/= coins 0))
		   (if (/= coins2 coins)
		       (format t "PLAYER ~S OWES ~S COINS TO PLAYER ~S, BUT HAS ONLY ~S COINS~%" from-player-number coins to-player-number coins2)
		       (format t "PLAYER ~S PAYS ~S COINS TO PLAYER ~S~%" from-player-number coins2 to-player-number)))
		 (decf (player-coins (aref players from-player-number)) coins2)
		 (incf (player-coins (aref players to-player-number)) coins2)))
	     (bank-pay (to-player-number coins)
	       (when (and *print-game-events* (/= coins 0))
		 (format t "BANK PAYS ~S COINS TO PLAYER ~S~%" coins to-player-number))
	       (incf (player-coins (aref players to-player-number)) coins))
	     (player-count-card-symbol (player-number card-symbol)
	       (let ((count 0)
		     (player-cards (player-cards (aref players player-number))))
		 (loop for card-number below (length player-cards) do
		      (let ((card (funcall (edition-map-number-function edition) card-number)))
			(when (eq (card-symbol card) card-symbol)
			  (incf count (aref player-cards card-number)))))
		 count)))
      ;; Turns are activated in the following order: 1. :ROT
      (loop for i+ below (1- (length players)) do
	   (let ((player1-number (mod (+ player-number 1 i+) (length players))))
	     (player-pay player-number player1-number
			 (* (activated-card-number player1-number (find-card "Café")) (+ 1 shoppingmall+1)))
	     (player-pay player-number player1-number
			 (* (activated-card-number player1-number (find-card "Familienrestaurant")) (+ 2 shoppingmall+1)))))
      ;; Turns are activated in the following order: 2. :GRÜN
      (bank-pay player-number (* 1 (activated-card-number player-number (find-card "Bäckerei"))))
      (bank-pay player-number (* 3 (activated-card-number player-number (find-card "Mini-Markt"))))
      (bank-pay player-number (* 3 (activated-card-number player-number (find-card "Molkerei")) (player-count-card-symbol player-number :cow)))
      (bank-pay player-number (* 3 (activated-card-number player-number (find-card "Möbelfabrik")) (player-count-card-symbol player-number :gear)))
      (bank-pay player-number (* 2 (activated-card-number player-number (find-card "Markthalle")) (player-count-card-symbol player-number :grain)))
      ;; Turns are activated in the following order: 2. :BLAU
      (loop for player-number below (length players) do
	   (bank-pay player-number (* 1 (activated-card-number player-number (find-card "Weizenfeld"))))
	   (bank-pay player-number (* 1 (activated-card-number player-number (find-card "Bauernhof"))))
	   (bank-pay player-number (* 1 (activated-card-number player-number (find-card "Wald"))))
	   (bank-pay player-number (* 5 (activated-card-number player-number (find-card "Bergwerk"))))
	   (bank-pay player-number (* 3 (activated-card-number player-number (find-card "Apfelplantage")))))
      ;; Turns are activated in the following order: 3. :PINK
      (when (> (activated-card-number player-number (find-card "Bürohaus")) 0)
	;; TODO:
	;; #S(NORMAL-CARD
	;;    :NAME "Bürohaus"
	;;    :SYMBOL :TOWER
	;;    :DICE (6)
	;;    :COST 8
	;;    :ACTIVATION :SELF
	;;    :COLOR :PINK
	;;    :DESCRIPTION "Tausche 1 Karte mit einem Mitspieler deiner Wahl. Kein \"tower\" Unternehmen."
	;;    :EDITION "base")
	)
      (when (> (activated-card-number player-number (find-card "Fernsehsender")) 0)
	;; TODO: FIXME: There should be a mechanism of choosing a player that has to pay.
	(let ((player-number-max-coins -1) (max-coins -1))
	  (loop for i+ below (1- (length players)) do
	       (let* ((player1-number (mod (+ player-number 1 i+) (length players)))
		      (player-coins (player-coins (aref players player1-number))))
		 (when (> player-coins max-coins)
		   (setf max-coins player-coins
			 player-number-max-coins player1-number))))
	  (player-pay player-number-max-coins player-number 5)))
      (when (> (activated-card-number player-number (find-card "Stadion")) 0)
	(loop for i+ below (1- (length players)) do
	     (let* ((player1-number (mod (+ player-number 1 i+) (length players))))
	       (player-pay player1-number player-number 2))))
      )))

(defun game-make-player-turn (selector-dice12 selector-reroll selector-card)
  "This function implements the state changes in the game that arise from a player taking its turn.
The functions SELECTOR-DICE12, SELECTOR-REROLL, and SELECTOR-CARD receive the current game state and must return the step that the player takes.
Return a function that is given a GAME instance and a PLAYER-NUMBER, and lets the player with PLAYER-NUMBER take its turn.
Note that #'MAKE-NNET-AI-PLAYER and this function are separated because this function does not contain any ai-player specific code, but can also be used for a human player."
  (labels ((player-turn-once (game player-number)
	     (let* ((player (aref (game-players game) player-number))
		    (player-cards (player-cards player))
		    (stack (game-stack game))
		    (dice-12 (funcall selector-dice12 game player-number))
		    (roll1 (1+ (random 6)))
		    (roll2 (if (= dice-12 2) (1+ (random 6)) 0))
		    (roll-equal-p (= roll1 roll2)))
	       (when *print-game-events*
		 (format t "===> PLAYER ~S ROLLED ~S" player-number roll1)
		 (when (= dice-12 2) (format t "+~S=~S" roll2 (+ roll1 roll2))))
	       (when (and (player-has-card game player-number (find-card "Funkturm"))
			  (funcall selector-reroll game player-number roll1 roll2))
		 (when *print-game-events*
		   (format t ", BUT CHOOSES TO ROLL AGAIN: "))
		 (setf dice-12 (funcall selector-dice12 game player-number))
		 (setf roll1 (1+ (random 6))
		       roll2 (if (= dice-12 2) (1+ (random 6)) 0)
		       roll-equal-p (= roll1 roll2))
		 (when *print-game-events*
		   (format t "PLAYER ~S ROLLED ~S" player-number roll1)
		   (when (= dice-12 2) (format t "+~S=~S" roll2 (+ roll1 roll2)))))
	       (when *print-game-events*
		 (format t "~%"))
	       (game-eval-dice-roll! game player-number (+ roll1 roll2))
	       (let ((card-preferences (funcall selector-card game player-number))
		     (max-card-number -1)
		     (max-card-preference -1))
		 ;; Remove cards that we have already and cannot have duplicates of, or that are too expensive.
		 (when *print-game-events*
		   (format t "PLAYER ~S PREFERS" player-number)
		   (let* ((edition (game-edition game))
			  (cards (append (edition-normal-cards edition) (edition-large-cards edition)))
			  (preferences-list (map 'list (lambda (card price pref) (list card price pref))
						 (mapcar #'card-name cards)
						 (mapcar #'card-cost cards)
						 card-preferences)))
		     (setf preferences-list (sort preferences-list #'> :key #'third))
		     (loop for (name price pref) in preferences-list do (format t " ~A(~S coins):~5,3F" name price pref))
		     (format t "~%")))
		 ;;(prind card-preferences)
		 (loop for card-number below (length card-preferences) do
		      (let* ((card (game-card game card-number))
			     (cost (card-cost card)))
			;;(prind card-number (card-name card) cost)
			(cond
			  ((< (player-coins player) cost)
			   ;;(prind "too expensive" (player-coins player) cost)
			   )
			  ((and (typep card 'large-card) (> (aref player-cards card-number) 0))
			   ;;(prind "has large card already")
			   )
			  ((and (typep card 'normal-card) (eq (card-color card) :pink) (> (aref player-cards card-number) 0))
			   ;;(prind "has normal pink card already")
			   )
			  ((and (typep card 'normal-card) (< (aref stack card-number) 1))
			   ;;(prind "not on stack")
			   )
			  (t
			   (let ((card-preference (aref card-preferences card-number)))
			     (when (> card-preference max-card-preference)
			       (setf max-card-number card-number
				     max-card-preference card-preference)))))))
		 (when (/= -1 max-card-number)
		   (buy-card! game player-number max-card-number))
		 roll-equal-p)))
	   (player-turn (game turn player-number ai-updaters)
	     (declare (ignore turn ai-updaters))
	     (when (and (player-turn-once game player-number)
			(player-has-card game player-number (find-card "Freizeitpark")))
	       (player-turn-once game player-number))
	     t ;;this is used by #'RUN-GAME to determine if this turn was skipped or not
	     ))
    #'player-turn))

;; Example:
(let ((game (make-new-game +base-edition-15+ 2 12)))
  (print-game game)
  (game-eval-dice-roll! game 0 1)
  (buy-card! game 0 (funcall (edition-map-card-function (game-edition game)) (find-card "Café")))
  (print-game game)
  (assert (= 11 (player-coins (aref (game-players game) 0))))
  (assert (= 13 (player-coins (aref (game-players game) 1))))
  (game-eval-dice-roll! game 1 3)
  (print-game game)
  (assert (= 12 (player-coins (aref (game-players game) 0))))
  (assert (= 13 (player-coins (aref (game-players game) 1))))
  )
(let ((game (make-new-game +base-edition-15+ 2 12)))
  (print-game game)
  (buy-card! game 0 (funcall (edition-map-card-function (game-edition game)) (find-card "Stadion")))
  (print-game game)
  (game-eval-dice-roll! game 0 6)
  (assert (= 8 (player-coins (aref (game-players game) 0))))
  (assert (= 10 (player-coins (aref (game-players game) 1))))
  (print-game game)
  (buy-card! game 0 (funcall (edition-map-card-function (game-edition game)) (find-card "Fernsehsender")))
  (game-eval-dice-roll! game 0 6)
  (print-game game)
  (assert (= 8 (player-coins (aref (game-players game) 0))))
  (assert (= 3 (player-coins (aref (game-players game) 1))))
  )

;;(remove-if (lambda (x) (not (equal "base" (card-edition x)))) +normal-cards+)
;;(remove-if (lambda (x) (not (equal "base" (card-edition x)))) +large-cards+)

;;;; NNET
(load "game-nnet.lisp")

(defstruct ai
  (genes-dice12 nil :type nnet)
  (genes-reroll nil :type nnet)
  (genes-card nil :type nnet))

(defun make-new-ai (edition num-players)
  "NUM-PLAYERS is the number of players in a game."
  (let* ((num-normal-cards (length (edition-normal-cards edition)))
	 (num-cards (+ num-normal-cards (length (edition-large-cards edition))))
	 ;; TODO: remove the duplication of INPUT-SIZE here and in #'MAKE-NNET-AI-PLAYER.
	 (input-size (+ 1 num-normal-cards (* num-players (+ 1 num-cards)))) ;bias + cards on the stack + player cards + player coins
	 (genes-dice12 (make-new-nnet (list input-size 10 1)))
	 (genes-reroll (make-new-nnet (list (+ input-size 2) 10 1)))
	 (genes-card (make-new-nnet (list input-size 10 num-cards))))
    (make-ai :genes-dice12 genes-dice12 :genes-reroll genes-reroll :genes-card genes-card)))

(defun make-ai-offspring (parent1 parent2)
  (make-ai :genes-dice12 (make-nnet-offspring (ai-genes-dice12 parent1) (ai-genes-dice12 parent2))
	   :genes-reroll (make-nnet-offspring (ai-genes-reroll parent1) (ai-genes-reroll parent2))
	   :genes-card (make-nnet-offspring (ai-genes-card parent1) (ai-genes-card parent2))))

(defun make-nnet-ai-player (edition num-players ai)
  "This function makes an nnet AI player, and must be implemented for every different ai type and game."
  (let* ((num-normal-cards (length (edition-normal-cards edition)))
	 (num-cards (+ num-normal-cards (length (edition-large-cards edition))))
	 (input-size (+ 1 num-normal-cards (* num-players (+ 1 num-cards))))) ;bias + cards on the stack + player cards + player coins
    (labels ((copy-subseq (to-sequence to-start from-sequence from-start &optional (from-end (length from-sequence)))
	       "Overwrite a subsequence of TO-SEQUENCE with a subsequence from FROM-SEQUENCE.
FROM-START specifies the index where copying starts, FROM-END the index where copying ends, TO-START the index that is first copied to."
	       (declare (type sequence to-sequence from-sequence)
			(type (and unsigned-byte fixnum) to-start from-start from-end)
			(optimize (speed 3)))
	       (loop
		  for i from from-start below from-end
		  for j from to-start do
		    (setf (elt to-sequence j) (elt from-sequence i)))
	       to-sequence)
	     (copy-to-input (input start game player-number)
	       (let* ((players (game-players game))
		      (stack (game-stack game)))
		 (setf (aref input start) 1.0)
		 (incf start)
		 (copy-subseq input 1 (map '(simple-array single-float 1) #'float stack) 0)
		 (incf start (length stack))
		 (loop for i from 0 below (length players) do
		      (let* ((player1-number (mod (+ player-number i) (length players)))
			     (player (aref players player1-number)))
			(copy-subseq input start (map '(simple-array single-float 1) #'float (player-cards player)) 0)
			(incf start (length (player-cards player)))
			(setf (aref input start) (float (player-coins player)))
			(incf start)))
		 start))
	     (selector-dice12 (game player-number)
	       (let* ((input (make-array input-size :element-type 'single-float)))
		 (copy-to-input input 0 game player-number)
		 (let ((output (eval-nnet (ai-genes-dice12 ai) input)))
		   (if (< (elt output 0) 0.5) 1 2))))
	     (selector-reroll (game player-number roll1 roll2)
	       (let* ((input (make-array (+ input-size 2) :element-type 'single-float)))
		 (setf (aref input 0) (float roll1))
		 (setf (aref input 1) (float roll2))
		 (copy-to-input input 2 game player-number)
		 (let ((output (eval-nnet (ai-genes-reroll ai) input)))
		   (if (< (elt output 0) 0.5) nil t))))
	     (selector-card (game player-number)
	       (let* ((input (make-array input-size :element-type 'single-float)))
		 (copy-to-input input 0 game player-number)
		 (eval-nnet (ai-genes-card ai) input))))
      (game-make-player-turn #'selector-dice12 #'selector-reroll #'selector-card))))

;;;; TRAINING
(load "game-training.lisp")

;;(train +base-edition+ 4 30000)
#|
(let ((scores (alexandria:copy-array *last-scores*)))
  (setf scores (sort scores #'score-better))
  (loop for score across scores for i from 0 do
       (format t "~3D. ~S~%" i (print-score score nil)))
  (let ((winners (loop for i below 100 collect
		      (run-game +base-edition+ (mapcar (lambda (x) (elt scores x)) '(24 7 24 24))))))
    (mapcar (lambda (player-num) (cons player-num (count player-num winners)))
	    '(0 1 2 3))))
|#
