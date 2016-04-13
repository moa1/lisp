;;;; SCORE

(defstruct score
  ;; TODO: add slot EDITION, and check that game-loop was passed the same edition.
  ;; The score of a player
  (id -1 :type integer)
  (ai nil)
  (played 0 :type (and unsigned-byte integer))
  (won 0 :type (and unsigned-byte integer))
  (lastrank 0 :type integer)
  (ranksum 0.0 :type single-float)
  (printpos -1 :type integer))

(defun string-score (object)
  (declare (optimize (debug 3)))
  (let ((played (score-played object)))
    (format nil "id:~5D (~3D,~A) ~4,2F%(~4D)"
	    (score-id object)
	    (score-lastrank object)
	    (if (> played 0) (format nil "~4,1F" (/ (score-ranksum object) played)) "  NA")
	    (if (> played 0) (/ (score-won object) played) 0)
	    (score-played object))))

(defun print-score (object stream)
  (format stream "#<~A ~A>" (type-of object) (string-score object)))

(defun print-scores (scores)
  (let* ((num-scores (length scores))
	 (scores-no (map 'list (lambda (x) x) (remove-if (lambda (x) (>= (score-printpos x) 0)) scores)))
	 (scores-yes (sort (remove-if (lambda (x) (< (score-printpos x) 0)) scores) #'< :key #'score-printpos))
	 (printpos-to-score (make-array num-scores))
	 (printed 0))
    (let ((last-score-yes-index (length scores-yes))
	  (next-score-yes-index 0))
      (loop for printpos below num-scores do
	   (setf (aref printpos-to-score printpos)
		 (if (and (< next-score-yes-index last-score-yes-index)
			  (= (score-printpos (aref scores-yes next-score-yes-index)) printpos))
		     (prog1 (aref scores-yes next-score-yes-index) (incf next-score-yes-index))
		     (prog1 (car scores-no) (setf (score-printpos (car scores-no)) printpos) (pop scores-no))))))
    (loop for printpos below num-scores do
	 (let ((score (aref printpos-to-score printpos)))
	   (print-score score t) (princ "  " t)
	   ;;(format t "~S  " score)
	   (incf printed)
	   (when (>= printed 3) (setf printed 0) (format t "~%"))))
    (format t "~&")))

(defun score-better (score1 score2)
  (let* ((p1 (score-played score1)) (p2 (score-played score2))
	 (w1 (score-won score1)) (w2 (score-won score2))
	 (r1 (if (> p1 0) (/ w1 p1) 0)) (r2 (if (> p2 0) (/ w2 p2) 0)))
    (if (= r1 r2)
	(> w1 w2)
	(> r1 r2))))

;;;; TRAINING

(defun run-game (edition scores &key (make-new-game #'make-new-game) (make-nnet-ai-player #'make-nnet-ai-player) (player-won-p #'player-won-p) (print-game #'print-game))
  (declare (optimize (debug 3)))
  (let* ((num-players (length scores))
	 (game (funcall make-new-game edition num-players 3))
	 (ais (make-array num-players :initial-contents (map 'list (lambda (player-score) (funcall make-nnet-ai-player edition num-players (score-ai player-score))) scores))))
    (block game
      (loop for turn from 0 do
	   (loop for player-number below num-players do
		(when *print-game-events*
		  (format t "TURN ~S:~%" turn)
		  (funcall print-game game))
		(funcall (aref ais player-number) game player-number)
		(when (funcall player-won-p game player-number)
		  (when *print-game-events*
		    (funcall print-game game)
		    (format t "===> PLAYER ~S WON~%" player-number))
		  (return-from game (values player-number turn))))))))

(defvar *next-score-id* 0)
(defvar *last-scores* nil "The scores from the last call to #'TRAIN")

(defun train (edition num-players iterations &key (initial-scores *last-scores*) (make-ai-offspring #'make-ai-offspring) (make-new-ai #'make-new-ai) (make-new-game #'make-new-game) (make-nnet-ai-player #'make-nnet-ai-player) (player-won-p #'player-won-p) (print-game #'print-game))
  (declare (optimize (debug 3)))
  (let* ((num-scores (* 10 num-players))
	 (num-keep (ceiling (* num-scores 0.7))))
    (labels ((make-new-score ()
	       (let* ((ai (funcall make-new-ai edition num-players)))
		 (make-score :id (incf *next-score-id*) :ai ai)))
	     (make-offspring (parent1 parent2)
	       (make-score :id (incf *next-score-id*)
			   :ai (funcall make-ai-offspring (score-ai parent1) (score-ai parent2)))))
      (let* ((scores (let* ((scores (make-array num-scores :initial-contents (loop for i below num-scores collect (make-new-score)))))
		       (setf (subseq scores 0) initial-scores)
		       scores)))
	(assert (and (/= num-keep num-scores) (/= 0 num-keep)))
	(do ((iteration iterations (1- iteration)))
	    ((= iteration 0))
	  (format t "iterations left: ~S~%" iteration)
	  ;; let random players play against each other.
	  (loop for i from (1- (length scores)) downto 1 do
	       (rotatef (aref scores i) (aref scores (random i))))
	  (let ((turns-sum 0)
		(num-games 0))
	    (loop for player1-index from 0 below num-scores by num-players do
	       ;;(format t "iteration:~S players:~S~%" iteration (loop for player-number from player1-index below (+ player1-index num-players) collect (aref scores player-number)))
		 (loop for player-number from player1-index below (+ player1-index num-players) do
		      (incf (score-played (aref scores player-number))))
		 (let* ((player-scores (loop for i below num-players collect (aref scores (+ player1-index i)))))
		   (multiple-value-bind (winner-index turns) (run-game edition player-scores :make-new-game make-new-game :make-nnet-ai-player make-nnet-ai-player :player-won-p player-won-p :print-game print-game)
		     (incf turns-sum turns) (incf num-games)
		     (let ((winner-score (elt player-scores winner-index)))
		       ;;(format t "player ~S won~%" (score-id winner-score))
		       (incf (score-won winner-score))))))
	    (format t "Average number of turns: ~F~%" (/ turns-sum num-games)))
	  (setf scores (sort scores #'score-better))
	  (loop for i below num-scores do
	       (let ((score (aref scores i)))
		 (setf (score-lastrank score) i)
		 (incf (score-ranksum score) i)))
	  (setf *last-scores* scores)
	  (print-scores scores)
	  (loop for i from num-keep below num-scores do
	       (let* ((parent1 (aref scores (random num-keep)))
		      (parent2 (aref scores (random num-keep)))
		      (offspring (make-offspring parent1 parent2)))
		 (setf (aref scores i) offspring))))))))

#|
(progn
  (ql:quickload :sb-sprof)
  (sb-sprof:reset)
  (sb-sprof:start-profiling :mode :cpu)
  (train +base-edition+ 4 10)
  (sb-sprof:stop-profiling)
  (sb-sprof:report))
|#
