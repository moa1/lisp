(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)

;;;; Cards

(defstruct normal-card
  (name "" :type string)
  (symbol :undef :type symbol)
  (dice 0 :type list) ;list of (integer 1 12)
  (cost -1 :type (and unsigned-byte fixnum))
  (activation :undef :type (member :any :self))
  (color :undef :type symbol)
  (description "" :type string)
  (edition "" :type string))

(defstruct large-card
  (name "" :type string)
  (symbol :undef :type (eql :large))
  (cost -1 :type (and unsigned-byte fixnum))
  (description "" :type string)
  (edition "" :type string))

(load "/home/toni/lisp/machikoro-cards.lisp") ;generated by machikoro-parse-cards.py

(defun find-card (card-name)
  (or (find-if (lambda (x) (equal card-name (normal-card-name x))) +normal-cards+)
      (find-if (lambda (x) (equal card-name (large-card-name x))) +large-cards+)))

;;;; Editions

(defstruct edition
  (normal-cards nil :type sequence) ;sequence of NORMAL-CARDs
  (large-cards nil :type sequence) ;sequence of LARGE-CARDs
  (number-normal-cards nil :type sequence) ;sequence of INTEGERs
  (map-normal-card-function (constantly nil) :type (function (normal-card) integer)) ;function mapping NORMAL-CARD to its index
  (map-large-card-function (constantly nil) :type (function (large-card) integer)) ;function mapping LARGE-CARD to its index
  (map-number-function (constantly nil) :type (function (integer) (or normal-card large-card))) ;function mapping card number to NORMAL-CARD or LARGE-CARD
  (starting-cards #() :type (array integer))) ;an array of size (+ (length NORMAL-CARDS) (length LARGE-CARDS)), with each element being the number of cards with that index

(defmacro defedition (name normal-cards normal-cards-total-number large-cards starting-cards)
  (declare (type symbol name))
  (alexandria:once-only ((nc normal-cards)
			 (lc large-cards)
			 (nctot normal-cards-total-number)
			 starting-cards)
    (alexandria:with-gensyms (map-nc-fn map-lc-fn map-n-fn card number nc-length lc-length sc)
      `(let* ((,nc-length (length ,nc))
	      (,lc-length (length ,lc))
	      (,sc (make-array (+ ,nc-length ,lc-length) :initial-element 0)))
	 (flet ((,map-nc-fn (,card)
		  (declare (type normal-card ,card))
		  (let ((,number (position ,card ,nc :test #'equalp)))
		    (assert (integerp ,number) () "Cannot find ~S in the normal-cards of edition ~S" ,card ,name)
		    ,number))
		(,map-lc-fn (,card)
		  (declare (type large-card ,card))
		  (let ((,number (position ,card ,lc :test #'equalp)))
		    (assert (integerp ,number) () "Cannot find ~S in the large-cards of edition ~S" ,card ,name)
		    (+ ,nc-length ,number)))
		(,map-n-fn (,number)
		  (declare (type integer ,number))
		  (assert (and (<= 0 ,number) (< ,number (+ ,nc-length ,lc-length))) () "Card number out ~S of range (0 ~S) in edition ~S" ,number (+ ,nc-length ,lc-length) ,name)
		  (if (< ,number ,nc-length)
		      (elt ,nc ,number)
		      (elt ,lc (- ,number ,nc-length)))))
	   (mapcar (lambda (,card) (assert (typep ,card 'normal-card))) ,nc)
	   (mapcar (lambda (,card) (assert (typep ,card 'large-card))) ,lc)
	   (loop for ,card in ,starting-cards do
		(incf (aref ,sc (,map-nc-fn ,card))))
	   (defconstant ,name
	     (make-edition :normal-cards ,nc
			   :large-cards ,lc
			   :number-normal-cards ,nctot
			   :map-normal-card-function #',map-nc-fn
			   :map-large-card-function #',map-lc-fn
			   :map-number-function #',map-n-fn
			   :starting-cards ,sc)))))))

(let ((nc '(("Stadion" . 4) ("Fernsehsender" . 4) ("Bürohaus" . 4)
	    ("Bergwerk" . 6) ("Apfelplantage" . 6) ("Bäckerei" . 6) ("Mini-Markt" . 6) ("Molkerei" . 6) ("Möbelfabrik" . 6) ("Markthalle" . 6) ("Weizenfeld" . 6) ("Bauernhof" . 6) ("Wald" . 6) ("Familienrestaurant" . 6) ("Café" . 6)))
      (lc '("Bahnhof" "Einkaufszentrum" "Freizeitpark" "Funkturm"))
      (sc '("Weizenfeld" "Bäckerei")))
  (flet ((card-name-to-instance (name)
	   (or (find name +normal-cards+ :test 'equal :key #'normal-card-name)
	       (find name +large-cards+ :test 'equal :key #'large-card-name))))
    (defedition +base-edition+
	(mapcar #'card-name-to-instance (mapcar #'car nc))
      (mapcar #'cdr nc)
      (mapcar #'card-name-to-instance lc)
      ;;TODO: FIXME: allow large card names here as well)
      (mapcar #'card-name-to-instance sc))))

;;;; Game

(defstruct player
  (cards #() :type (array integer)) ;array of INTEGERs, each integer being the number of cards the player has with that card index (in the edition of the game currently played).
  (coins 3 :type integer)
  (turn-function (constantly nil) :type function))

(defstruct game
  (edition nil :type edition)
  (players #() :type (array player)) ;list of PLAYERs
  (stack #() :type (array integer)) ;array of INTEGERs, with the index of the array being the normal card number and the value equal to the number of available cards of that normal card type
  (deck nil :type list)) ;list of normal card numbers

(defun deal-card! (game)
  "Deal one card from the deck of the GAME to the stack of the GAME.
Returns whether the type of the dealt card was already present in the stack or not."
  (let* ((stack (game-stack game))
	 (new-card-number (pop (game-deck game)))
	 (new-card-stack-number (aref stack new-card-number)))
    (incf (aref stack new-card-number))
    (/= 0 new-card-stack-number)))

(defun make-new-game (edition stack-size num-players starting-coins)
  "Make a new game with EDITION, STACK-SIZE different cards in the starting stack, NUM-PLAYERS having STARTING-COINS each."
  (flet ((make-new-player (starting-coins)
	   (make-player :cards (alexandria:copy-array (edition-starting-cards edition)) :coins starting-coins))
	 (shuffle! (seq)
	   (let ((length (length seq)))
	     (loop for i below (1- length) do
		  (let* ((j (+ i 1 (random (- length i 1))))
			 (i-elt (elt seq i))
			 (j-elt (elt seq j)))
		    (setf (elt seq i) j-elt (elt seq j) i-elt))))
	   seq))
    (let* ((deck (shuffle! (alexandria:flatten (loop for i from 0 for n in (edition-number-normal-cards +base-edition+) collect (loop for x below n collect i)))))
	   (players-list (loop for i below num-players collect (make-new-player starting-coins)))
	   (players (make-array num-players :initial-contents players-list))
	   (number-normal-cards (length (edition-normal-cards edition)))
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
		    (normal-cards (remove-if (complement #'normal-card-p) cards))
		    (large-cards (remove-if (complement #'large-card-p) cards)))
	       (setf normal-cards (sort normal-cards #'< :key (lambda (x) (car (normal-card-dice x)))))
	       (setf large-cards (sort large-cards #'< :key #'large-card-cost))
	       (flet ((card-count (card)
			(aref stack-array
			      (if (typep card 'normal-card)
				  (funcall (edition-map-normal-card-function edition) card)
				  (funcall (edition-map-large-card-function edition) card)))))
		 (let ((printed-cards 0))
		   (loop for card in normal-cards do
			(let ((count (card-count card)))
			  (when (> count 0)
			    (when (and (/= printed-cards 0) (= (mod printed-cards 3) 0))
			      (format t "~%"))
			    (incf printed-cards)
			    (format t "  ~A~S:~S" (normal-card-name card) (normal-card-dice card) count)))))
		 (format t "  ")
		 (loop for card in large-cards do
		      (let ((count (card-count card)))
			(when (> count 0)
			  (format t " ~A" (large-card-name card)))))))))
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
	 (cost (if (typep card 'normal-card) (normal-card-cost card) (large-card-cost card)))
	 (player-cards (player-cards player)))
    (format t "===> PLAYER ~S BUYS ~S FOR ~S~%" player-number (normal-card-name card) cost)
    (assert (or (typep card 'large-card) (> (aref stack card-number) 0)) () "No ~S card left in game card stack!" card)
    (assert (>= (player-coins player) cost) () "Player ~S needs ~S coins to buy card ~S, but has only ~S left." player-number cost card (player-coins player))
    (decf (player-coins player) cost)
    ;; TODO: check that the player may have only 1 of some cards.
    (let ((card (game-card game card-number)))
      (when (and (typep card 'normal-card) (eq (normal-card-color card) :pink))
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
			((typep card-or-card-number 'normal-card)
			 (funcall (edition-map-normal-card-function (game-edition game)) card-or-card-number))
			((typep card-or-card-number 'large-card)
			 (funcall (edition-map-large-card-function (game-edition game)) card-or-card-number))
			(t
			 card-or-card-number))))
    (aref cards card-number)))

(defun player-has-card (game player-or-player-number card-or-card-number)
  (> (player-get-card-count game player-or-player-number card-or-card-number) 0))

(defun game-eval-dice-roll! (game player-number roll)
  "Carry out the effects when the die/dice rolled (the sum) ROLL, and it is PLAYER-NUMBER's turn."
  (assert (< 0 roll 13))
  (format t "===> PLAYER ~S ROLLED ~S~%" player-number roll)
  (let ((shoppingmall+1 (player-get-card-count game player-number (find-card "Einkaufszentrum")))
	(players (game-players game))
	(edition (game-edition game)))
    (labels ((activated-card-number (player-number card)
	       "If CARD is activated this turn, returns the number of CARDS the PLAYER-NUMBER has, 0 otherwise."
	       (if (find roll (normal-card-dice card))
		   (player-get-card-count game player-number card)
		   0))
	     (player-pay (from-player-number to-player-number coins)
	       (let ((coins (min (player-coins (aref players from-player-number)) coins)))
		 (decf (player-coins (aref players from-player-number)) coins)
		 (incf (player-coins (aref players to-player-number)) coins)))
	     (bank-pay (to-player-number coins)
	       (incf (player-coins (aref players to-player-number)) coins))
	     (player-count-card-symbol (player-number card-symbol)
	       (let ((count 0)
		     (player-cards (player-cards (aref players player-number))))
		 (loop for card-number below (length player-cards) do
		      (let ((card (funcall (edition-map-number-function edition) card-number)))
			(when (and (typep card 'normal-card) (eq (normal-card-symbol card) card-symbol))
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
      (when (player-has-card game player-number (find-card "Bürohaus"))
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
      (when (player-has-card game player-number (find-card "Fernsehsender"))
	;; TODO: FIXME: There should be a mechanism of choosing a player that has to pay.
	(let ((player-number-max-coins -1) (max-coins -1))
	  (loop for i+ below (1- (length players)) do
	       (let* ((player1-number (mod (+ player-number 1 i+) (length players)))
		      (player-coins (player-coins (aref players player1-number))))
		 (when (> player-coins max-coins)
		   (setf max-coins player-coins
			 player-number-max-coins player1-number))))
	  (player-pay player-number-max-coins player-number 5)))
      (when (player-has-card game player-number (find-card "Stadion"))
	(loop for i+ below (1- (length players)) do
	     (let* ((player1-number (mod (+ player-number 1 i+) (length players))))
	       (player-pay player1-number player-number 2))))
      )))

(defun make-ai-player-turn (selector-dice-12 selector-reroll selector-card)
  "Return a function that, when called, makes a turn for an ai-player."
  (labels ((player-turn-once (game player-number)
	     (let* ((player (aref (game-players game) player-number))
		    (player-cards (player-cards player))
		    (stack (game-stack game))
		    (dice-12 (funcall selector-dice-12 game))
		    (roll1 (1+ (random 6)))
		    (roll2 (if (= dice-12 2) (1+ (random 6)) 0))
		    (roll-equal-p (= roll1 roll2)))
	       (when (funcall selector-reroll game roll1 roll2)
		 (setf dice-12 (funcall selector-dice-12 game))
		 (setf roll1 (1+ (random 6))
		       roll2 (if (= dice-12 2) (1+ (random 6)) 0)
		       roll-equal-p (= roll1 roll2)))
	       (game-eval-dice-roll! game player-number (+ roll1 roll2))
	       (let ((card-preferences (funcall selector-card game))
		     (max-card-number -1)
		     (max-card-preference -1))
		 ;; Remove cards that we have already and cannot have duplicates of, or that are too expensive.
		 (loop for card-number below (length card-preferences) do
		      (let* ((card (game-card game card-number))
			     (cost (if (typep card 'normal-card) (normal-card-cost card) (large-card-cost card))))
			(cond
			  ((< (player-coins player) cost))
			  ((and (typep card 'large-card) (> (aref player-cards card-number) 0)))
			  ((and (typep card 'normal-card) (eq (normal-card-color card) :pink) (> (aref player-cards card-number) 0)))
			  ((and (typep card 'normal-card) (< (aref stack card-number) 1))))
			(let ((card-preference (aref card-preferences card-number)))
			  (when (> card-preference max-card-preference)
			    (setf max-card-number card-number
				  max-card-preference card-preference)))))
		 (buy-card! game player-number max-card-number)
		 roll-equal-p)))
	   (player-turn (game player-number)
	     (when (and (player-turn-once game player-number)
			(player-has-card game player-number (find-card "Freizeitpark")))
	       (player-turn-once game player-number))))
    #'player-turn))


;; Example:
(let ((game (make-new-game +base-edition+ 15 2 12)))
  (print-game game)
  (game-eval-dice-roll! game 0 1)
  (buy-card! game 0 (funcall (edition-map-normal-card-function (game-edition game)) (find-card "Café")))
  (print-game game)
  (assert (= 11 (player-coins (aref (game-players game) 0))))
  (assert (= 13 (player-coins (aref (game-players game) 1))))
  (game-eval-dice-roll! game 1 3)
  (print-game game)
  (assert (= 12 (player-coins (aref (game-players game) 0))))
  (assert (= 13 (player-coins (aref (game-players game) 1))))
  )
(let ((game (make-new-game +base-edition+ 15 2 12)))
  (print-game game)
  (buy-card! game 0 (funcall (edition-map-normal-card-function (game-edition game)) (find-card "Stadion")))
  (print-game game)
  (game-eval-dice-roll! game 0 6)
  (assert (= 8 (player-coins (aref (game-players game) 0))))
  (assert (= 10 (player-coins (aref (game-players game) 1))))
  (print-game game)
  (buy-card! game 0 (funcall (edition-map-normal-card-function (game-edition game)) (find-card "Fernsehsender")))
  (game-eval-dice-roll! game 0 6)
  (print-game game)
  (assert (= 8 (player-coins (aref (game-players game) 0))))
  (assert (= 3 (player-coins (aref (game-players game) 1))))
  )

;;(remove-if (lambda (x) (not (equal "base" (normal-card-edition x)))) +normal-cards+)
;;(remove-if (lambda (x) (not (equal "base" (large-card-edition x)))) +large-cards+)

#|
(defun game-loop (game)
  (loop do
       (
|#
