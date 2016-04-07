(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :cl-cffi-gtk)
(use-package '(:gtk :gdk :gdk-pixbuf :cairo :glib :gobject))

(load "/home/toni/lisp/machikoro-rules.lisp")

(defparameter *base-edition-card-to-image-filename*
  (mapcar (lambda (card)
	    (let ((filename (concatenate 'string "/home/toni/text/Machi Koro/images/machikoro-" (string-downcase (card-name card)) ".png")))
	      (cons card filename)))
	  (concatenate 'list (edition-normal-cards +base-edition+) (edition-large-cards +base-edition+))))

(defvar *base-edition-images* nil)

(defun load-edition-pixbufs (edition assoc-card-to-filename)
  (let* ((edition-cards (concatenate 'list (edition-normal-cards edition) (edition-large-cards edition)))
	 (pixbufs (make-array (length edition-cards))))
    (loop for i from 0 for card in edition-cards do
	 (let ((a (assoc card assoc-card-to-filename)))
	   (assert (not (null a)) () "Unknown card ~S with name ~A" card (card-name card))
	   (let ((filename (cdr a)))
	     (when (not (probe-file filename))
	       (setf filename "/home/toni/text/Machi Koro/images/machikoro-missing.png"))
	     (setf (aref pixbufs i)
		   (gdk-pixbuf-new-from-file filename)))))
    pixbufs))

(defvar *number-images* nil)

(defun load-number-pixbufs ()
  (let* ((num-numbers 27)
	 (pixbufs (make-array num-numbers)))
    (loop for number below num-numbers do
	 (let ((filename (format nil "/home/toni/text/Machi Koro/images/number-~S.png" number)))
	   (setf (aref pixbufs number)
		 (gdk-pixbuf-new-from-file filename))))
    pixbufs))
       
;;(defvar *background-width* 944) (defvar *background-height* 708)
;;(defvar *background-width* 1468) (defvar *background-height* 930)
(defvar *background-width* 1835) (defvar *background-height* 930)

(eval-when (:execute)
  (when (null *base-edition-images*)
    (setf *base-edition-images* (load-edition-pixbufs +base-edition+ *base-edition-card-to-image-filename*)))
  (when (null *number-images*)
    (setf *number-images* (load-number-pixbufs)))
  )

(defparameter *card-table-number-to-card* nil)

(defun draw-game (game frame area surface)
  (declare (optimize (debug 3)))
  (let* ((back-width *background-width*)
	 (back-height *background-height*)
	 (back-rect (make-gdk-rectangle :x 0 :y 0 :width back-width :height back-height))
	 (card-width 119)
	 (card-height 155)
	 (count-width 56)
	 (count-height 56)
	 (count-offset-x 80)
	 (count-offset-y 75)
	 (coins-width 100)
	 (coins-height 100))
    (labels ((card-pos-1 (player-number card-table-number)
	       ;; for 1024x768
	       ;; 944x708=(7*119+111)x(4*155+88)=(1+2*3=7)cards/player
	       ;; ABBBBCC
	       ;; AABBBCC
	       ;; AADDDCC
	       ;; AADDDDC
	       (assert (<= 0 card-table-number 6))
	       (ecase player-number
		 ((0) (values (+ 0 (* card-width (floor card-table-number 4)))
			      (- back-height card-height (* card-height (mod card-table-number 4)))))
		 ((1) (values (- (+ 10 (* card-width 4))
				 (* card-width (mod card-table-number 4)))
			      (+ 0 (* card-height (floor card-table-number 4)))))
		 ((2) (values (- (- back-width card-width) (* card-width (floor card-table-number 4)))
			      (+ 0 (* card-height (mod card-table-number 4)))))
		 ((3) (values (+ (- back-width (* card-width 5) 10)
				 (* card-width (mod card-table-number 4)))
			      (- (- back-height card-height)
				 (* card-height (floor card-table-number 4)))))))
	     (card-pos-2 (player-number card-table-number)
	       ;; for 1920x1080
	       ;; 1222x962=(10*119+32)x(6*155+32)=(1+2+4*3=15)cards/player
	       ;; ABBBBBBCCC
	       ;; AABBBBBCCC
	       ;; AAABBBBCCC
	       ;; AAADDDDCCC
	       ;; AAADDDDDCC
	       ;; AAADDDDDDC
	       (assert (<= 0 card-table-number 14))
	       (error "TODO"))
	     (card-pos-3 (player-number card-table-number)
	       ;; for 1920x1080
	       ;; 1468x930=((3*119+10)*4)x(6*155)=(3*6=18)cards/player
	       ;; AAABBBCCCDDD
	       ;; AAABBBCCCDDD
	       ;; AAABBBCCCDDD
	       ;; AAABBBCCCDDD
	       ;; AAABBBCCCDDD
	       ;; AAABBBCCCDDD
	       (let* ((corner-x (* player-number (+ 10 (* 3 card-width))))
		      (corner-y 0))
		 (values (+ corner-x (* card-width (mod card-table-number 3)))
			 (+ corner-y (* card-height (floor card-table-number 3))))))
	     (draw-image (xpos ypos width height image)
	       (let* ((image-width (gdk-pixbuf-get-width image))
		      (image-height (gdk-pixbuf-get-height image))
		      (k (min (/ width image-width) (/ height image-height)))
		      (unclipped-rect (make-gdk-rectangle :x xpos :y ypos :width width :height height))
		      (dest-rect (gdk-rectangle-intersect unclipped-rect back-rect)))
		 (gdk-pixbuf-composite image
				       frame
				       (gdk-rectangle-x dest-rect)
				       (gdk-rectangle-y dest-rect)
				       (gdk-rectangle-width dest-rect)
				       (gdk-rectangle-height dest-rect)
				       (coerce xpos 'double-float)
				       (coerce ypos 'double-float)
				       (coerce k 'double-float)
				       (coerce k 'double-float)
				       :bilinear 255)))
	     (draw-card (slot-number card-table-number card-number count)
	       (multiple-value-bind (xpos ypos) (card-pos-3 slot-number card-table-number)
		 (draw-image xpos ypos card-width card-height (aref *base-edition-images* card-number))
		 (draw-image (+ xpos count-offset-x) (+ ypos count-offset-y) count-width count-height (aref *number-images* count))))
	     (draw-stack (slot-number stack)
	       (let* ((edition (game-edition game))
		      (normal-cards-and-counts nil)
		      (large-cards-and-counts nil))
		 (loop for card-number below (length stack) do
		      (let ((count (aref stack card-number))
			    (card (funcall (edition-map-number-function edition) card-number)))
			(when (> count 0)
			  (if (typep card 'normal-card)
			      (push (list card count) normal-cards-and-counts)
			      (push (list card count) large-cards-and-counts)))))
		 (setf normal-cards-and-counts (sort normal-cards-and-counts #'<
						     :key (lambda (card-and-count) (car (card-dice (first card-and-count))))))
		 (setf large-cards-and-counts (sort large-cards-and-counts #'<
						    :key (lambda (card-and-count) (card-cost (first card-and-count)))))
		 (setf *card-table-number-to-card* (make-array 18 :initial-element nil))
		 (loop for (card count) in (append normal-cards-and-counts large-cards-and-counts) for card-table-number from 0 do
		      (draw-card slot-number card-table-number (funcall (edition-map-card-function edition) card) count)
		      (setf (aref *card-table-number-to-card* card-table-number) card) ;TODO: FIXME: this is hacky since it depends on the game-stack being drawn last.
		      ))))
      (gdk-pixbuf-fill frame #xaaaaaaff)
      (loop for player-number below 4 do
	   (let ((player (aref (game-players game) player-number)))
	     (draw-stack player-number (player-cards player))
	     (multiple-value-bind (xpos ypos) (card-pos-3 player-number 16)
	       (draw-image xpos ypos coins-width coins-height (aref *number-images* (min (player-coins player) (length *number-images*)))))))
      (let* ((game-stack (game-stack game))
	     (augmented-game-stack (make-array (+ (length (edition-normal-cards (game-edition game))) (length (edition-large-cards (game-edition game)))))))
	(loop for i below (length game-stack) do
	     (setf (aref augmented-game-stack i) (aref game-stack i)))
	(mapc (lambda (card) (incf (aref augmented-game-stack (funcall (edition-map-card-function (game-edition game)) card))))
	      (edition-large-cards (game-edition game)))
	(draw-stack 4 augmented-game-stack)))))

(defun coordinate-to-card (x y)
  "Return the card that is under the point X,Y."
  (let* ((card-width 119)
	 (card-height 155)
	 (slot-width (+ 10 (* card-width 3)))
	 (slot-number (floor x slot-width))
	 (slot-x (floor (- x (* slot-number slot-width)) card-width))
	 (slot-y (floor y card-height))
	 (card-table-number (+ slot-x (* slot-y 3))))
    (aref *card-table-number-to-card* card-table-number)))

(let ((frame-num 0)
      (frame nil)
      (area (make-instance 'gtk-drawing-area))
      (surface nil)
      (game nil)
      (turn nil)
      (ais nil)
      (next-player-number nil)
      (human-state nil))

  (defun load-pixbufs ()
    (setf frame (gdk-pixbuf-new :rgb nil 8 *background-width* *background-height*)))

  (defun setup-new-game (edition player-organism-list)
    (let ((num-players (length player-organism-list)))
      (assert (<= 1 num-players 4))
      (setf game (make-new-game edition num-players 3))
      (setf ais (make-array num-players :initial-contents
			    (map 'list (lambda (player-org)
					 (cond
					   ((eq player-org :human)
					    :human)
					   (t
					    (make-ai-player-from-organism edition num-players player-org))))
				 player-organism-list))))
    (setf turn 0)
    (setf next-player-number 0))

  (defun start-new-game ()
    (format t "===> STARTING NEW GAME~%")
    (let ((orgs (alexandria:copy-array *last-organisms*)))
      (setf orgs (sort orgs #'organism-better))
      (loop for org across orgs for i from 0 do
	   (format t "~3D. ~S~%" i (print-organism org nil)))
      (let ((player-organism-list (mapcar (lambda (x) (elt orgs x)) '(7 7 7 7))))
	(setf (elt player-organism-list 0) :human)
	(setup-new-game +base-edition+ player-organism-list)))
    (let ((res (run-turns)))
      (prog1 res
	(assert (eq res :human)))))

  (defun run-turns ()
    (declare (optimize (debug 3)))
    (loop do
	 (let ((ai (aref ais next-player-number)))
	   (cond
	     ((eq ai :human)
	      (setf human-state :dice)
	      (return-from run-turns :human))
	     (t
	      (let ((*print-game-events* t))
		(funcall ai game next-player-number))))
	   (when (player-won-p game next-player-number) ;checking whether the human player won must be done elsewhere
	     (format t "===> PLAYER ~S WON~%" next-player-number)
	     (return-from run-turns (start-new-game)))
	   (setf next-player-number (mod (1+ next-player-number) (length (game-players game))))
	   (when (= next-player-number 0)
	     (incf turn)))))

  (defun timeout2 ()
    (let* ((back-rect (make-gdk-rectangle :x 0 :y 0
					  :width *background-width*
					  :height *background-height*)))
      (draw-game game frame area surface)
      (let ((cr (cairo-create surface)))
	(gdk-cairo-set-source-pixbuf cr frame 0.0d0 0.0d0)
	(cairo-paint cr)
	(cairo-destroy cr))
      (gtk-widget-queue-draw-area area
				  0 0
				  (gdk-rectangle-width back-rect)
				  (gdk-rectangle-height back-rect)))
    (incf frame-num 1)
    t)

  (defun demo-pixbufs ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Demo Pixbufs"
                                   :resizable nil
                                   :default-width 944
				   :default-height 708
                                   ;;:default-width 1468
				   ;;:default-height 930
				   )))
        (setf frame-num 0)
        (g-signal-connect area "draw"
           (lambda (widget cr)
             (declare (ignore widget))
             (cairo-set-source-surface (pointer cr) surface 0.0d0 0.0d0)
             (cairo-paint (pointer cr))
             ;; We must destroy the Cairo Context
             (cairo-destroy (pointer cr))
             t))
        (g-signal-connect area "configure-event"
           (lambda (widget event)
             (declare (ignore event))
             (when surface
               (cairo-surface-destroy surface))
             (setq surface
                   (gdk-window-create-similar-surface
                                   (gtk-widget-window widget)
                                   :color
                                   (gtk-widget-get-allocated-width widget)
                                   (gtk-widget-get-allocated-height widget)))
             ;; Clear surface
             (let ((cr (cairo-create surface)))
               (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
               (cairo-paint cr)
               (cairo-destroy cr))
             t))

        ;; Load the background and the images.
	(format t "3~%")
        (load-pixbufs)

	(format t "2~%")
	(setf (gtk-widget-size-request window) (list *background-width* *background-height*))

	(format t "1~%")
        (gtk-container-add window area)
	
	(start-new-game)

	(prind surface)
	
	(g-signal-connect window "button-press-event"
			  (lambda (widget event)
			    (declare (ignore widget))
			    (let ((type (gdk-event-button-type event))
				  (button (gdk-event-button-button event))
				  (x (gdk-event-button-x event))
				  (y (gdk-event-button-y event)))
			      ;;(format t "BUTTON-PRESS-EVENT ~S type:~S button:~S x:~S y:~S~%" event type button x y)
			      (prind human-state type button x y)
			      (when (eq type :button-press)
				(cond
				  ((eq human-state :dice)
				   (let (dice-12)
				     (case button
				       ((2) (setf dice-12 1))
				       ((3) (setf dice-12 2)))
				     (when (member dice-12 '(1 2))
				       (let ((roll1 (1+ (random 6)))
					     (roll2 (if (= dice-12 2) (1+ (random 6)) 0)))
					 (format t "===> PLAYER ~S ROLLED ~S" next-player-number roll1)
					 (when (= dice-12 2) (format t "+~S=~S" roll2 (+ roll1 roll2)))
					 (format t "~%")
					 ;; TODO: possibility of rerolling when human has "Funkturm"
					 (format t "TODO: possibility of rerolling when human has \"Funkturm\"~%")
					 (game-eval-dice-roll! game next-player-number (+ roll1 roll2))
					 (setf human-state :card)))))
				  ((and (eq human-state :card) (eq button 1) (not (null (coordinate-to-card x y))))
				   (let* ((card (coordinate-to-card x y))
					  (card-number (funcall (edition-map-card-function (game-edition game)) card))
					  (cost (card-cost card))
					  (name (card-name card))
					  (player (aref (game-players game) next-player-number))
					  (player-cards (player-cards player)))
				     ;;(prind card-number (card-name card) cost)
				     (cond
				       ((< (player-coins player) cost)
					(format t "Card ~S, cost ~S, too expensive when you have ~S coins~%" name cost (player-coins player))
					)
				       ((and (typep card 'large-card) (> (aref player-cards card-number) 0))
					(format t "You already have large card ~S~%" name)
					)
				       ((and (typep card 'normal-card) (eq (card-color card) :pink) (> (aref player-cards card-number) 0))
					(format t "You have normal pink card ~S already~%" name)
					)
				       ((and (typep card 'normal-card) (< (aref (game-stack game) card-number) 1))
					(error "how could you click a card that is not on stack?")
					)
				       (t
					(format t "You buy card ~S for ~S coins~%" name cost)
					(buy-card! game next-player-number card-number))))
				   (when (player-won-p game next-player-number) ;checking whether the human player won must be done elsewhere
				     (format t "===> PLAYER ~S WON~%" next-player-number)
				     (start-new-game))
				   ;; TODO: have another turn if human has "Freizeitpark" and rolled equal dice.
				   (format t "TODO: have another turn if human has \"Freizeitpark\" and rolled equal dice.~%")
				   (setf human-state nil)
				   (setf next-player-number (mod (1+ next-player-number) (length (game-players game))))
				   (assert (eq (run-turns) :human))
				   ))))))

	(let ((timeout-id (g-timeout-add 100 #'timeout2)))
	  (g-signal-connect window "destroy"
			    (lambda (widget)
			      (declare (ignore widget))
			      (g-source-remove timeout-id)
			      (setf timeout-id 0)
			      (leave-gtk-main))))

	(format t "0~%")
	
        (gtk-widget-show-all window)))))

#|
(load "/home/toni/lisp/machikoro-gtk.lisp")
(train +base-edition+ 4 2000)
(demo-pixbufs)
|#
