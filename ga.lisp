;; TODO: Try to find conditions for altruism. Add a APO (apoptosis) instruction, which drops an organism's energy to the floor, and removes the organism. Then, change the OFFSPRING instruction, so that producing offspring requires continuous feeding for some time, so that the mother has to eat in-between. Then, experiment with different food availability amounts and try to find conditions for altruism, i.e. when organisms decide to go into apoptosis instead of trying to produce offspring.

;; TODO: How to enforce that the WAIT instruction is used: Let any organism only have 1 offspring, and after that is has to use WAIT to gain points required for another offspring. When is has that other offspring, X points are subtracted from its account. X depends on the (already computed) current average elapsed time T that an organism spends between having two offspring. If X==T, then the organisms that do WAIT will spend an additional T ticks between having two offfspring. If X==0.5*T, then the organisms that WAIT spend an additional 0.5*T ticks between having two offspring. I expect that if I let a newly-born organism have only 1 offspring, then they will all die out, because if no organism dies because its energy ran out, then the number of organisms will just stay constant (and besides they won't have enough time to evolve offspring that do WAIT). But there will be some organisms that wander too far from the sun, and therefore die. So I will have to make the number of free offspring larger than 1. What will that number be? It's probably a number between 0 and 2. If I make it too large, then the organisms that do WAIT will die out because they are outnumbered by fast-replicating organisms. If it is too small then the fast-replicating organisms will die out. I think I want both types of organisms in my world. I could determine the number by letting two worlds run in parallel, and perform binary search to determine the number of free offspring. Let world A have the number Ta and search for the bound from below, and world B have the number Tb and search for the above bound. If in world A the fast-replicating organisms die out, then Ta was too small, and this world has to be reset (to the state where they had not died out or, if that is too complicated to determine, to the starting state of the experiment) to Ta_new=(Tb+Ta_old)/2. If in world B the WAITing organisms die out, then Tb was too large and world B has to be reset, and Tb_new=(Ta+Tb_old)/2. If in world A the WAITing organisms die out then Ta was too large to begin with and the experiment should be re-run with a smaller Ta. But as I said I think the optimal number is between 0 and 2, so I should start with Ta=0.5 and Tb=2.0. (As a side-note, because an organism can only have an integer number of offspring, if T=1.5, then two options come to my mind: either determine the number of free offspring at the time of birth of an organism (half of the organisms have 1 free offspring, and the other half have 2. Then there is a problem if an organism dies before having any offspring. (In that case I could give another fast organism the one not spent free offspring) Or, after an organism has had already 1 free offspring, make the birth of the second offspring probabilistic, and only give it a probability of 0.25 for the first try, 0.125 for the second, 0.0625 for the third and so on, so that the integrated probability is 0.5. (Is that math correct?)) Does the optimal number T also depend on the environment? If plenty energy is available, then T might be smaller, because the fast organisms thrive in that environment. But if energy is scarce then WAITing organisms might have an advantage because they just WAIT between checking for adjacent energy, and T might be larger. It will be even more interesting when all organisms have a WAIT instruction but they are conditional and an organism computes whether to WAIT or not. (For this, the computation should be free, so implement that ADD-, SUB-, MUL-, DIV-, ENERGYNEARBYAVAILABLE-instructions are free.)

;; TODO: make the world have different walls. These could be implemented by interpreting negative world-energy-levels as walls, and a value of -1 could mean "fast walking speed", a value of -2 "medium walking speed", and a value of -3 "slow walking speed". The negative cells of the world then cannot store energy from the sun.

;; TODO: think about the Hox-gene (https://de.wikipedia.org/wiki/Hox-Gen) and how regulatory genes could be implemented in ga.lisp. For example, implement more than one instruction pointer (IP) per organism. The multiple instruction pointers should operate in parallel on the same genome (and share the same registers?). Having per-instruction-pointer instances of the GENESX register, but sharing the OFF-GENES register could allow mixing instructions from different parent organisms for sex. Another idea is that each instruction pointer could have a different CS gene, which would allow switching on and off the evaluation of different sets of instructions per instruction pointer, because the CS gene controls which conditional instructions are evaluated. The addition of another instruction pointer to an organism and its subsequent deletion could be initiated by an ADDIP and DELIP instruction. But at which instruction and with what initial per-IP-registers should IPs be added? If they are the same as those of the mother-IP, then the mother-IP and its daughter-IP will execute the same sequence of instructions. Hmm.. maybe not, since either the mother-IP or daughter-IP will execute an instruction that modifies the global state, and the other IP will thus operate on a different set of global state or registers.

;; TODO: make computation instructions have an energy cost of 0. To avoid piling up trash instructions, implement the utilization/pruning-waves described below. (The pruning phase should get rid of superfluous computation instructions.)

;; TODO: replace #'string-to-* and #'*-to-string by real serialization functions. quicklisp packages interesting for persistence: clache, clsql, hu.dwim.perec, hyperluminal, manardb, marshal, rucksack, submarine.

;; IDEA (low priority): below idea of COS/SIN-waves for utilizing/pruning instructions could be used for communication between organisms. This could make communication without cost, i.e. with an energy-cost per communication-instruction of 0, not fill the genome with useless instructions. Die Grundlage der Idee ist, dass es in Bayern vor nicht allzu langer Zeit Raiffeisen(?)-Höfe gab, an denen u.a. Baumaterial, Saatgut und Abfälle in einem Kreislauf ausgetauscht wurden. Dabei war der Lager-Puffer der einzelnen Materialien groß (da die Höfe im Prinzip Lagerhäuser waren). Danach wurde der Kreislauf aufgegeben, und der Puffer wurde immer kleiner, da es immer weniger Lagerhöfe gab. Inzwischen gibt es (z.B. durch die Abfallwirtschaft München (AWM)) wieder größere Puffer durch mehr Lagerhöfe. Um zurück zu ga.lisp zu kommen, könnten verschiedene Kommunikationsinstruktionen implementiert werden, die sich voneinander unterscheiden. Z.B. könnte Instruktion A peer-to-peer-Kommuniktion (á la Internet-Protocol) implementieren und eine andere, redundante, Instruktion B könnte asynchrone Kommunikation (á la Email) implementieren. Die Utilization/Pruning-Phasenverschiebung zwischen Instruktionen A und B könnte sich um 90 Grad unterscheiden.

;; IDEA: If i remember correctly, the below idea of COS/SIN-waves utilizes/prunes two instructions A and B with an phase difference of 0. This means the two COS-waves (i.e. the utilization-wave) of A and B have their maximum at the same time and their minimum also at the same time. But there could be world environments where instruction A should be pruned at the same time that instruction B is utilized. Should we make the phase difference of A and B different from 90 degrees? Also, should we make the phase difference between utilizing and pruning of each individual instruction not 90 degrees, but an variable amound?

;; IDEA: assume there are two worlds, one fast (short utilizing/pruning period) and a slow one (long utilizing/pruning period), and each world has a constant number of organisms. Then, if an organism dies in world A, I could copy the organism from world B to world A, and, symmetrically, when an organism dies in world B, copy it to world A. This would let each organism find its own optimal period of utilizing/pruning wave period, because we can assume that if an organism dies in a world, then it possibly had the wrong instructions (this of course does not take into account that the environment could have been bad). I just have to try it out.

;; IDEA: to find the optimal period of the COS/SIN-waves for utilizing/pruning of instructions, we could run two worlds in parallel, one with a long period (slow world) and one with a short period (fast world). The long period should be an integer multiple of the short period. Each organism should be run in each of the two worlds at the same time, let's call the organism in the fast world organism A and the organism in the slow world organism B. We need to find a way to synchronize the two worlds from time to time. The goal is to find an optimal instruction utilizing/pruning period. (Maybe for a third world, which is run with an adapting period?) Or we could exchange organism A and B from time to time, for example when it dies. I first have to get a feeling (by trying out different periods, in a single world) for how different periods change the population genome distribution. (Intuitively, it should broaden the genomic diversity.)

;; IDEA: have a constant number of organisms in the world by inserting offspring into a priority queue, which is ordered by the amount of "love" (a different form of energy) that the mother(+father if there is sex) has. Each time an organism dies, the offspring with the most love is deleted from the priority queue and born into the world at the position the mother is currently at.

;; TODO: When a random instruction is to be inserted, try out not drawing the random instruction from an equal distributed distribution over the instructions, but from an distribution that is determined by a second genome. (The first genome (called array A) is composed of the instructions, just like it is now.) The second genome is composed of an array B[i] with floating point values, one for each instruction. Each element of the array B[i] encodes the probability that the instruction i is drawn with. It doesn't encode an equal distribution, but the probability P for an instruction i is: P(new_instruction_to_be_inserted=i) = (2^B[i])/SUM_P2. (P2[i] = 2^B[i], and SUM_P2 is the sum of P2[i] over all instructions i. The raising to the power of 2 ensures that SUM_P2 and P[i] are positive.) The floats B[i] in the array B are constant over the life-time of an organism, but each float B[i] changes when an offspring is born: it performs a random walk: B[i] = B_mother[i] + R, where R is a random number drawn from a gaussian distribution with mean 0 and standard deviation 1. The effect of this whole procedure should be that the currently favourable instructions to be inserted are conserved between mother and offspring: offspring of a mother that has a good second genome B are more likely to be fit for life, because the array B changes little between mother and offspring. Now, this whole procedure also will have a very bad effect: Instructions that happen to be unlikely to be inserted (i.e. those instructions i with a large negative B[i]) will remain unlikely for a long time, because a random walk is, well, random. To balance this bad effect, we will inverse the array B each, lets say 80 generations. This is done as follows. We re-define P(new_instruction_to_be_inserted=i) := (2^(B[i]*cos(g/80*pi)))/SUM_P2cos, where cos(g) is the cosine of g, g is the generation of the organism, and SUM_P2cos is the sum of 2^(B[i]*cos(g/80*pi)) over all instructions i. The generation g is constant over the life-time of an organism, starts with 0, i.e. the first organisms in the initial world have g=0, and their offsprinig all have generation 1: g(offspring) = g(mother) + 1. This cosine wave will have the effect that the second genome B of an organism will be not only be conserved if it is good, but also that the elements of B, B[i], will be under constant pressure to be near each other. This is because if an instruction i is much better for life under the current world-conditions, then it will receive a large positive B[i]. But if the B[j] for the other instructions j have a wildly different value than B[i], then the offspring of the organism 80 generations later will have an unfavourable second genome B, because the probability of instruction i will be very small (because B[i]*cos(g/80*pi) will be a large negative number). If the B[i] for each instruction i are similar to each other, then the effect of the cosine wave will be small. (If the B[i] all are equal for a mother and remain equal for its offspring, then the cosine wave will have no effect at all.) But if an instruction i is favourable under the current conditions for life, then B[i] will increase from mother to offspring. But, 80 generations later, the instructions i in the first genome A will be under pressure to be pruned from A, because their likelihood to be inserted will be small. (Maybe (on second thought: definitely) I should also make not only insertion, but also deletion dependent on B. The probability of an instruction i to be omitted (i.e. deleted) in the copying of A from mother to offspring could also happen dependent on generation g. If the current probability P(omit_i_in_the_copying) for an instruction i is constantly 1/1000, then make the new probability P(omit_i_in_the_copying)=(2^B[i]*sin(g/80*pi))/SUM_P2sin. Note that the only difference between P(new_instruction_to_be_inserted_i) and P(omit_i_in_the_copying) is the change from cos to sin.) This will allow two kinds of organisms: those that evolve slowly (because their B[i] are all similar to each other for instructions i), and those that adapt quickly to new conditions for life, but that are also prone to quickly changing conditions (because they have at least one instruction i that has a large positive B[i], whose large positive value will become a hindrance in a new condition for life that requires a different instruction j to be inserted with a higher probability, and if B[i]-B[j] >> 0, then the organisms' offspring will need many generations to make B[i] smaller or B[j] larger).

;; TODO: instead of storing an array of energies, I could let the organisms have energy whenever they want (using EAT), but only give them an amount proportional to the distance to the sun (or to the clouds). This would not require this huge *WORLD* array. However, I don't know whether this would lead to unlimited (expontential) growth, since there would be unlimited energy available. I could make the sun faster than organisms can walk, so that they cannot keep up with the suns movement.

;; DONE: implent world persistence between different programming sessions. This should be implemented by separating the simulation part (of ga.lisp and gatest-orgap-lisp.lisp) into a simulation program that stores the world to a sqlite database from time to time, including the state of the random number generator. The sqlite database should also be committed to git. A different lisp program implements the graphical display (of ga.lisp) by reading the current state from the sqlite database and rendering it to the screen. Whenever the sqlite database format changes, the display program must be adapted.

;; DONE: use editdistance.lisp to implement when selecting an organism, all other organisms should be colored according to the edit-distance to that organisms' genes using a color ramp.

;; I should let the organisms play games against each other. The games could be created randomly. A game has state (a fixed number of variables), and rules when and in what format it accepts inputs from the players, and what state these inputs change (input=function call). It also has a rule that describes what state the game has to be in so that player 1 wins, player 2 wins, etc. (or maybe a ranking of the players). The game should be fair, i.e. the game should work the same way if the order of players is permuted. There even could be games that have only one player, like puzzle-games (but how to generate them automatically?) A game could be implemented as a finite state machine. I thought about this a little bit, and I think there are too many different games that could be implemented this way. (The order of the number of different games is probably the same as could be implemented using a programming language of the same length.)

(defvar *default-random-state* (make-random-state nil)) ;save default random state using DEFVAR, this way it will only be evaluated once. Then, when we want to reset the state, we can copy *DEFAULT-RANDOM-STATE* and use it as the new state.

(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(ql:quickload :cl-heap)
(ql:quickload :mru-cache)
(ql:quickload :clsql)
(ql:quickload :xorshift)

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

(defun reset-random-state ()
  "Set the random state to the default random state."
  (setf *random-state* (make-random-state *default-random-state*))
  ;; init random number generator to defined state
  (setf xorshift:*xorshift1024*-random-state* (xorshift:make-xorshift1024*-random-state)))

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
(defvar *default-database-name* "ga-world.sqlite3" "The name of the database where the world is kept persistent")
(defvar *database-store-world-wait* 100000000 "The number of ticks between storing the world to the database")
;;(defvar *database-store-world-wait* 1000000 "The number of ticks between storing the world to the database")
;; display variables
(defvar *display-world* t)
(defvar *print-statistics* nil)
(defparameter *cursor* nil)

(defclass database-store ()
  ((nexttick :initform 0 :initarg :nexttick :type integer :accessor database-store-nexttick :documentation "The next tick at which the world will be stored in the database.")
   (database-name :initform *default-database-name* :initarg :database-name :accessor database-store-database-name :documentation "The name of the database where the world will be stored.")))

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

(defun world-create-database-table ()
  "Create the table where the world is stored."
  (clsql:create-table (clsql:sql-expression :table 'world)
		      `((,(clsql:sql-expression :attribute 'tick) integer :not-null)
			(,(clsql:sql-expression :attribute 'x) integer :not-null)
			(,(clsql:sql-expression :attribute 'y) integer :not-null)
			(,(clsql:sql-expression :attribute 'energy) integer :not-null))
		      :constraints '("PRIMARY KEY (tick,x,y)")))

(defun store-world (tick world)
  "Store the world WORLD."
  (format t "World width ~A: " (array-dimension world 0))
  (loop for x below (array-dimension world 0) do
       (when (= 0 (mod x 10))
	 (format t "~A " x))
       (loop for y below (array-dimension world 1) do
	    (let ((energy (aref world x y 0)))
	      (clsql:insert-records :into (clsql:sql-expression :table 'world)
				    :attributes '(tick x y energy)
				    :values (list tick x y energy)))))
  (format t "~%"))

(defun restore-world (tick)
  "Restore the world stored for tick TICK and return it."
  (let* ((w (1+ (car (clsql:select (clsql:sql-operation 'max (clsql:sql-expression :attribute 'x))
				   :from (clsql:sql-expression :table 'world)
				   :where (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick) :flatp t))))
	 (h (1+ (car (clsql:select (clsql:sql-operation 'max (clsql:sql-expression :attribute 'y))
				   :from (clsql:sql-expression :table 'world)
				   :where (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick) :flatp t))))
	 (world (make-array (list w h 1) :element-type 'fixnum)))
    (format t "World width ~A: " w)
    (let ((db-world (clsql:select (clsql:sql-expression :attribute 'x) (clsql:sql-expression :attribute 'y) (clsql:sql-expression :attribute 'energy)
				  :from (clsql:sql-expression :table 'world)
				  :where (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick))))
      (loop for tuple in db-world do
	   (destructuring-bind (x y energy) tuple
	     (when (and (= y 0) (= 0 (mod x 10)))
	       (format t "~A " x))
	     (setf (aref world x y 0) energy))))
    (format t "~%")
    world))

(defclass nature-object ()
  ((id :initarg :id :accessor nature-id)
   (position-function :initarg :position-function :accessor nature-position-function)
   (edge :initarg :edge :accessor nature-edge)
   (drop-wait :initarg :drop-wait :accessor nature-drop-wait)
   (drop-amount :initarg :drop-amount :accessor nature-drop-amount)
   (nexttick :initform 0 :initarg :nexttick :accessor nature-nexttick)
   (energy-drop-sum :initform 0 :initarg :energy-drop-sum :accessor nature-energy-drop-sum)
   (energy-lost-sum :initform 0 :initarg :energy-lost-sum :accessor nature-energy-lost-sum)))

(defclass sun (nature-object)
  ((energy-index :initform 0 :initarg :energy-index :accessor nature-energy-index)))

(defun make-sun (id world-w world-h energy-per-coordinate-per-tick fraction-covered position-function)
  "ENERGY-PER-COORDINATE-PER-TICK is the average energy dropped per world coordinate per tick.
FRACTION-COVERED is the fraction of the whole world covered with sunlight.
VELOCITY is the speed, i.e. position change per tick."
  (let* ((edge (round (sqrt (* world-w world-h fraction-covered))))
	 (energy-per-tick (* world-w world-h energy-per-coordinate-per-tick))
	 (drop-wait *SUN-DROP-WAIT*)
	 (drop-amount (round (* energy-per-tick drop-wait))))
    (let* ((sun (make-instance 'sun
			       :id id
			       :position-function position-function
			       :edge edge
			       :drop-wait drop-wait
			       :drop-amount drop-amount
			       :energy-index 0)))
      (prind position-function drop-wait drop-amount edge)
      sun)))

(defun sun-create-database-table ()
  "Create the table where the world is stored."
  (clsql:create-table (clsql:sql-expression :table 'sun)
		      `((,(clsql:sql-expression :attribute 'tick) integer :not-null)
			(,(clsql:sql-expression :attribute 'id) integer :not-null)
			;;(,(clsql:sql-expression :attribute 'position-function) string :not-null) ;TODO FIXME
			(,(clsql:sql-expression :attribute 'edge) integer)
			(,(clsql:sql-expression :attribute 'drop-wait) integer)
			(,(clsql:sql-expression :attribute 'drop-amount) integer)
			(,(clsql:sql-expression :attribute 'nexttick) integer)
			(,(clsql:sql-expression :attribute 'energy-drop-sum) integer)
			(,(clsql:sql-expression :attribute 'energy-lost-sum) integer))
		      :constraints '("PRIMARY KEY (tick,id)")))

(defun store-sun (tick sun)
  "Store the sun SUN."
  (with-slots (id position-function edge drop-wait drop-amount nexttick energy-drop-sum energy-lost-sum energy-index) sun
    (clsql:insert-records :into (clsql:sql-expression :table 'sun)
			  :attributes '(tick id edge drop-wait drop-amount nexttick energy-drop-sum energy-lost-sum)
			  :values (list tick id edge drop-wait drop-amount nexttick energy-drop-sum energy-lost-sum))))

(defun restore-sun (tick id)
  "Restore the sun with id ID stored for tick TICK and return it."
  (let ((sun (make-instance 'sun :id id)))
    (loop for slot in '(#|position-function|# edge drop-wait drop-amount nexttick energy-drop-sum energy-lost-sum) do
	 (let ((value (car (clsql:select (clsql:sql-expression :attribute slot)
					 :from (clsql:sql-expression :table 'sun)
					 :where (clsql:sql-operation 'and (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick) (clsql:sql-operation '== (clsql:sql-expression :attribute 'id) id))
					 :flatp t))))
	   (setf (slot-value sun slot) value)))
    (setf (nature-position-function sun) #'sun-position-circle) ;TODO FIXME: load from database
    sun))

(defclass cloud (nature-object)
  ((pos-x :initarg :pos-x :accessor cloud-pos-x)
   (pos-y :initarg :pos-y :accessor cloud-pos-y)
   (vel-x :initarg :vel-x :accessor cloud-vel-x)
   (vel-y :initarg :vel-y :accessor cloud-vel-y)
   (energy-index :initarg :energy-index :accessor nature-energy-index)))

(defun make-cloud (id world-w world-h energy-index edge cloud-speed-per-tick position-function rain-per-coordinate-per-tick)
  (let* ((angle (random (* 2 pi)))
	 (energy-per-tick (* world-w world-h rain-per-coordinate-per-tick))
	 (drop-wait *CLOUDS-DROP-WAIT*)
	 (drop-amount (round (* energy-per-tick drop-wait))))
    (prind position-function drop-wait drop-amount edge energy-index)
    (make-instance 'cloud
		   :id id
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

(defun list-to-string (l)
  (write-to-string l))
(defun string-to-list (s)
  (read-from-string s))
(defun vector-to-string (v)
  (write-to-string v))
(defun string-to-vector (s)
  (read-from-string s))

(clsql:def-view-class org ()
  (;; organism program start: contains the actually interpreted slots.
   ;; TODO: replace #'READ-FROM-STRING and #'WRITE-TO-STRING with serialization package functions.
   (genes :initarg :genes :type list :accessor orgap-genes :documentation "genes of the organism" :db-kind :base :db-type "VARCHAR(65536)" :db-reader string-to-list :db-writer list-to-string)
   (code :initarg :code :type vector :accessor orgap-code :documentation "compiled code" :db-kind :virtual)
   (markers :initarg :markers :type alist :accessor orgap-markers :documentation "ALIST of IPs by marker number" :db-kind :virtual)
   (functions :initarg :functions :type alist :accessor orgap-functions :documentation "ALIST of IPs by function number" :db-kind :virtual)
   (ip :initform 0 :initarg :ip :type integer :accessor orgap-ip)
   (wait :initform 0 :initarg :wait :type integer :accessor orgap-wait)
   (angle :initarg :angle :type float :accessor orgap-angle)
   (target :initform nil :initarg :target :type (or null org) :accessor orgap-target :db-kind :virtual)
   (db-target :type integer :accessor org-db-target :db-kind :base)
   (targeter :initform nil :initarg :targeter :type (or null org) :accessor orgap-targeter :db-kind :virtual)
   (db-targeter :type integer :accessor org-db-targeter :db-kind :base)
   (x :initarg :x :type float :accessor orgap-x)
   (y :initarg :y :type float :accessor orgap-y)
   (total-energy :initarg :total-energy :type integer :accessor orgap-total-energy)
   (energy :initarg :energy :type integer :accessor orgap-energy)
   (skin :initform 1 :initarg :skin :type integer :accessor orgap-skin)
   (off-genes :initform nil :initarg :off-genes :type list :accessor orgap-off-genes :db-kind :base :db-type "VARCHAR(65536)" :db-reader string-to-list :db-writer list-to-string)
   (off-length :initform 0 :initarg :off-length :type integer :accessor orgap-off-length)
   (as :initform nil :initarg :as :type symbol :accessor orgap-as :column ras)
   (bs :initform nil :initarg :bs :type symbol :accessor orgap-bs :column rbs)
   (cs :initform nil :initarg :cs :type symbol :accessor orgap-cs :column rcs)
   (an :initform 0 :initarg :an :type integer :accessor orgap-an :column ran)
   (bn :initform 0 :initarg :bn :type integer :accessor orgap-bn :column rbn)
   (memory :initform (make-array 2 :initial-element 0) :initarg :memory :type vector :accessor orgap-memory :db-kind :base :db-type "VARCHAR(255)" :db-reader string-to-vector :db-writer vector-to-string)
   (stack :initform (make-array 2 :initial-element 0) :initarg :stack :type vector :accessor orgap-stack :db-kind :base :db-type "VARCHAR(255)" :db-reader string-to-vector :db-writer vector-to-string)
   (sp :initform 0 :initarg :sp :type integer :accessor orgap-sp)
   (genesx :initarg :genesx :type list :accessor orgap-genesx :documentation "rest of the genes to be read" :db-kind :base :db-type "VARCHAR(65536)" :db-reader string-to-list :db-writer list-to-string)
   ;; organism container start: contains management and statistics slots.
   (id :initform (incf *id*) :initarg :id :type integer :accessor orgcont-id :db-kind :key)
   (db-tick :type integer :accessor org-db-tick :db-kind :key)
   (lasttick :initform 0 :initarg :lasttick :type integer :accessor orgcont-lasttick)
   (nexttick :initform 0 :initarg :nexttick :type integer :accessor orgcont-nexttick)
   ;;statistics
   (age :initform 0 :initarg :age :type integer :accessor orgcont-age)
   (totage :initform 0 :initarg :totage :type integer :accessor orgcont-totage)
   (offspring-list :initform nil :initarg :offspring-list :type list :accessor orgcont-offspring-list :db-kind :virtual)
   (db-offspring-list :type string :accessor org-db-offspring-list :db-kind :base)
   (offspring-count :initform 0 :initarg :offspring-count :type integer :accessor orgcont-offspring-count)
   (offspring-energy-sum :initform 0 :initarg :offspring-energy-sum :type integer :accessor orgcont-offspring-energy-sum)
   (walk-sum :initform 0.0 :initarg :walk-sum :type float :accessor orgcont-walk-sum)
   (walk-count :initform 0 :initarg :walk-count :type integer :accessor orgcont-walk-count)
   (energy-in-sum :initform (make-energy 0) :initarg :energy-in-sum :type integer :accessor orgcont-energy-in-sum :documentation "The total energy taken in, excluding initial energy of organisms spawned in the world.")
   (energy-out-sum :initform (make-energy 0) :initarg :energy-out-sum :type integer :accessor orgcont-energy-out-sum :documentation "The total energy spent voluntarily or involuntarily.")))

(defun org-create-database-table ()
  "Create the table where the organisms are stored."
  (clsql:create-view-from-class 'org))

(defun store-orgs (tick orgs)
  "Store the organisms ORGS."
  (format t "Number of organisms is ~S: " (length orgs))
  (loop for org in orgs for i from 0 do
       (when (= (mod i 10) 0)
	 (format t "~A " i))
       (let ((org (copy-org org))) ;we have to copy the instance, otherwise CLSQL doesn't save to database
	 (with-slots (target db-target targeter db-targeter db-tick offspring-list db-offspring-list) org
	   (setf db-target (if (null target) nil (orgcont-id target))
		 db-targeter (if (null targeter) nil (orgcont-id targeter))
		 db-tick tick
		 db-offspring-list (list-to-string (mapcar #'orgcont-id (remove-if (lambda (org) (<= (orgap-energy org) 0)) offspring-list))))) ;only store alive offspring. Note that this does not affect fitness calculations, as all of them are defined only on alive offspring (see #'COMPUTE-FITNESS).
	 (clsql:update-records-from-instance org)))
  (format t "Done.~%"))

(defun restore-orgs (tick)
  "Restore the organisms stored as alive at tick TICK and return them as a list."
  (let* ((orgs-list (clsql:select 'org :where (clsql:sql-operation '== (clsql:sql-expression :attribute 'db-tick) tick) :flatp t :refresh t :caching nil))
	 (orgs (make-hash-table)))
    (loop for org in orgs-list do
	 (multiple-value-bind (genes code markers functions genesx) (make-orgap (orgap-genes org))
	   (declare (ignore genesx)) ;GENESX has to be the loaded database value
	   (setf (orgap-genes org) genes
		 (orgap-code org) code
		 (orgap-markers org) markers
		 (orgap-functions org) functions))
	 (setf (gethash (orgcont-id org) orgs) org))
    (loop for org in orgs-list do
	 (with-slots (target db-target targeter db-targeter offspring-list db-offspring-list) org
	   (setf target (gethash db-target orgs)
		 targeter (gethash db-targeter orgs)
		 offspring-list (if (null db-offspring-list)
				    nil
				    (loop for id in (string-to-list db-offspring-list) collect
					 (gethash id orgs))))))
    orgs-list))

(defun xorshift-create-database-table ()
  (clsql:create-table (clsql:sql-expression :table 'xorshift1024star)
		      `((,(clsql:sql-expression :attribute 'tick) integer :not-null)
			(,(clsql:sql-expression :attribute 'i) integer :not-null)
			(,(clsql:sql-expression :attribute 'value) integer :not-null))
		      :constraints '("PRIMARY KEY (tick,i)")))

(defun store-xorshift-random-number-generator (tick)
  "Store the xorshift default random number generator state."
  (let ((s (xorshift:xorshift1024*-random-state-s xorshift:*xorshift1024*-random-state*))
	(p (xorshift:xorshift1024*-random-state-q xorshift:*xorshift1024*-random-state*))
	(i -1))
    (flet ((store-64bit (v)
	     (clsql:insert-records :into (clsql:sql-expression :table 'xorshift1024star)
				   :attributes '(tick i value)
				   :values (list tick (incf i) (mod v (expt 2 32))))
	     (clsql:insert-records :into (clsql:sql-expression :table 'xorshift1024star)
				   :attributes '(tick i value)
				   :values (list tick (incf i) (ash v -32)))))
      (loop for i below 16 do
	   (store-64bit (aref s i)))
      (clsql:insert-records :into (clsql:sql-expression :table 'xorshift1024star)
			    :attributes '(tick i value)
			    :values (list tick (incf i) p)))))

(defun restore-xorshift-random-number-generator (tick)
  "Restore the xorshift random number generator state stored at tick TICK."
  (let ((s (loop repeat 16 collect 0))
	(i -1))
    (flet ((restore-64bit ()
	     (let ((a (car (clsql:select (clsql:sql-expression :attribute 'value) :from (clsql:sql-expression :table 'xorshift1024star) :where (clsql:sql-operation 'and (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick) (clsql:sql-operation '== (clsql:sql-expression :attribute 'i) (incf i))) :flatp t)))
		   (b (car (clsql:select (clsql:sql-expression :attribute 'value) :from (clsql:sql-expression :table 'xorshift1024star) :where (clsql:sql-operation 'and (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick) (clsql:sql-operation '== (clsql:sql-expression :attribute 'i) (incf i))) :flatp t))))
	       (+ a (ash b 32)))))
      (loop for i below 16 do
	   (setf (elt s i) (restore-64bit)))
      (let ((p (car (clsql:select (clsql:sql-expression :attribute 'value) :from (clsql:sql-expression :table 'xorshift1024star) :where (clsql:sql-operation 'and (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick) (clsql:sql-operation '== (clsql:sql-expression :attribute 'i) (incf i))) :flatp t))))
	(setf xorshift:*xorshift1024*-random-state* (xorshift:make-xorshift1024*-random-state s p))))))

(defun restore-last-world (world-max-energy &key (database-name *default-database-name*))
  (when clsql:*default-database* ;useful for developing
    (clsql:disconnect))
  (clsql:connect (list database-name) :database-type :sqlite3 :encoding :UTF-8)
  (let ((tick (car (clsql:select (clsql:sql-operation 'max (clsql:sql-expression :attribute 'tick)) :from (clsql:sql-expression :table 'sun) :flatp t))))
    (format t "Restoring world from tick ~A~%" tick)
    (restore-xorshift-random-number-generator tick)
    (let ((orgs (restore-orgs tick)))
      (setf *id* (reduce #'max orgs :key #'orgcont-id :initial-value (orgcont-id (car orgs))))
      (setf *world-tick* tick)
      (setf *world-max-energy* world-max-energy)
      (setf *world* (restore-world tick))
      (let ((sun-ids (clsql:select (clsql:sql-expression :attribute 'id)
				   :from (clsql:sql-expression :table 'sun)
				   :where (clsql:sql-operation '== (clsql:sql-expression :attribute 'tick) tick))))
	(setf *world-sun* (loop for id in sun-ids collect
			       (restore-sun tick id))))
      (setf *world-clouds* nil) ;TODO: When I add clouds, store/restore them as well
      (let ((store (make-instance 'database-store :database-name database-name :nexttick (+ tick *database-store-world-wait*))))
	(setf *orgs* (make-hash-table))
	(orgs-add-orgs orgs)
	(setf *event-heap*
	      (let ((heap (make-instance 'cl-heap:fibonacci-heap :key #'eventsource-nexttick :sort-fun #'<)))
		(cl-heap:add-all-to-heap heap orgs)
		(cl-heap:add-all-to-heap heap *world-sun*)
		(cl-heap:add-all-to-heap heap *world-clouds*)
		(cl-heap:add-to-heap heap store)
		heap)))
      (setf *cursor* nil)))
  (clsql:disconnect)
  nil)

;; load edit-distance functions
(load "edit-distance.lisp")

;; load organism implementation
(load "~/lisp/gatest-orgap-lisp.lisp")
;;(load "~/lisp/gatest-orgap-lightning.lisp")
;;(defvar *num-energies* (+ 1 *num-instructions*) "The total number of different energies at each world coordinate")
(defvar *num-energies* 1 "The total number of different energies at each world coordinate")

(defun copy-org (org)
  (with-slots (genes code markers functions ip wait angle target targeter x y total-energy energy skin off-genes off-length as bs cs an bn memory stack sp genesx id lasttick nexttick age totage offspring-list offspring-count offspring-energy-sum walk-sum walk-count energy-in-sum energy-out-sum) org
    (make-instance 'org :genes genes :code code :markers markers :functions functions :ip ip :wait wait :angle angle :target target :targeter targeter :x x :y y :total-energy total-energy :energy energy :skin skin :off-genes off-genes :off-length off-length :as as :bs bs :cs cs :an an :bn bn :memory memory :stack stack :sp sp :genesx genesx :id id :lasttick lasttick :nexttick nexttick :age age :totage totage :offspring-list offspring-list :offspring-count offspring-count :offspring-energy-sum offspring-energy-sum :walk-sum walk-sum :walk-count walk-count :energy-in-sum #|(alexandria:copy-array energy-in-sum)|#energy-in-sum :energy-out-sum #|(alexandria:copy-array energy-out-sum)|#energy-out-sum)))

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
       (let* ((maxx (float (array-dimension *world* 0)))
	      (maxy (float (array-dimension *world* 1)))
	      (x (random maxx))
	      (y (random maxy)))
	 (loop until (>= (aref *world* (floor x) (floor y) 0) 0) do
	      (setf x (random maxx)
		    y (random maxy)))
	 (multiple-value-bind (genes code markers functions genesx) (make-orgap genes)
	   (let ((org (make-instance 'org :genes genes :code code :markers markers :functions functions :x x :y y :angle (random (* 2 pi 128)) :total-energy 0 :energy energy :genesx genesx)))
	     (setf (orgcont-nexttick org) *orgap-min-wait*)
	     (when (= 0 (orgap-code-length org))
	       (error "Organism ~A has code length 0" org))
	     org)))))

(defmethod eventsource-nexttick ((eventsource org))
  (orgcont-nexttick eventsource))

(defmethod eventsource-nexttick ((eventsource nature-object))
  (nature-nexttick eventsource))

(defmethod eventsource-nexttick ((eventsource database-store))
  (database-store-nexttick eventsource))

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

(defun create-database-store (database-name nexttick)
  (let* ((store (make-instance 'database-store :database-name database-name :nexttick nexttick))
	 (database-name (database-store-database-name store)))
    (when clsql:*default-database* ;useful for developing
      (clsql:disconnect))
    ;; this was useful for developing, but now we really want to keep the database file save.
    ;;(when (probe-file database-name)
    ;;  (delete-file database-name))
    (unless (probe-file database-name)
      (clsql:connect (list database-name) :database-type :sqlite3 :encoding :UTF-8)
      (xorshift-create-database-table)
      (org-create-database-table)
      (sun-create-database-table)
      (world-create-database-table)
      (clsql:disconnect))
    store))

(defun set-default-world (&key (w 400) (h 200) (world-energy 0) (world-instructions 16) (orgs 250) (org-energy 4000) (reset-random-state t) (world-max-energy 4000) (energy-per-coordinate-per-tick .00001) (fraction-covered .01) (position-function #'sun-position-circle) (num-barriers-horizontal 5) (barrier-width-horizontal 40) (num-barriers-vertical 5) (barrier-width-vertical 30) (clouds-edge 1) (clouds-speed-per-tick 1) (clouds-position-function #'cloud-position-line) (clouds-rain-per-coordinate-per-tick .00001) (database-name *default-database-name*))
  (when reset-random-state
    (reset-random-state))
  (setf *id* 0)
  (setf *world-tick* 0)
  (setf *world-max-energy* world-max-energy)
  (setf *world* (make-world w h world-energy *num-instructions* world-instructions))
  (world-set-barriers! *world* num-barriers-horizontal barrier-width-horizontal num-barriers-vertical barrier-width-vertical)
  (setf *world-sun* (list (make-sun 0 (array-dimension *world* 0) (array-dimension *world* 1) energy-per-coordinate-per-tick fraction-covered position-function)))
  ;;(setf *world-clouds* (loop for instruction-index below *num-instructions* collect (make-cloud (1+ instruction-index) (array-dimension *world* 0) (array-dimension *world* 1) (+ 1 instruction-index) clouds-edge clouds-speed-per-tick clouds-position-function clouds-rain-per-coordinate-per-tick)))
  ;;(setf *world-clouds* (list (make-cloud 1 (array-dimension *world* 0) (array-dimension *world* 1) -1 clouds-edge clouds-speed-per-tick clouds-position-function clouds-rain-per-coordinate-per-tick)))
  (setf *world-clouds* nil)
  (let ((store (create-database-store database-name *world-tick*))
	(orgs (make-default-orgs orgs org-energy)))
    (setf *orgs* (make-hash-table))
    (orgs-add-orgs orgs)
    (setf *event-heap*
	  (let ((heap (make-instance 'cl-heap:fibonacci-heap :key #'eventsource-nexttick :sort-fun #'<)))
	    (cl-heap:add-all-to-heap heap orgs)
	    (cl-heap:add-all-to-heap heap *world-sun*)
	    (cl-heap:add-all-to-heap heap *world-clouds*)
	    (cl-heap:add-to-heap heap store)
	    heap)))
  (setf *cursor* nil)
  nil)

(when (null *orgs*)
  (if (probe-file *default-database-name*)
      (restore-last-world *world-max-energy*)
      (set-default-world)))

(defun print-orgap (org)
  (with-slots (ip genes off-genes as bs cs an bn stack) org
    (format t "orgap ip:~3A/~3A as:~20A bs:~20A cs:~3A an:~A bn:~A~%"
	    ip (length genes) as bs cs an bn)
    (format t "orgap stack:~S~%"
	    stack)
    (format t "~A length:~S hash:~S~%" genes (length genes) (mru-cache:lsxhash genes))))

(defun compute-fitness (org &optional fitness-function)
  (with-slots (offspring-list) org
    ;; permanently remove dead offspring from offspring list.
    (setf offspring-list (delete-if (lambda (org) (<= (orgap-energy org) 0)) offspring-list))
    (if (> (get-energy (orgap-energy org) 0) 0)
	(+ (funcall fitness-function org) (apply #'+ (mapcar (lambda (org) (compute-fitness org fitness-function)) offspring-list)))
	0)))

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
	    (compute-fitness org #'orgcont-energy-out-sum))
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
	   (compute-fitness org #'orgcont-offspring-count))
	 (compute-fitness-energy (org)
	   (compute-fitness org #'orgcont-energy-out-sum)))
    (format t "fitness min:~A avg:~A max:~A fitness-1 min:~A avg:~A max:~A fitness-offspring min:~A avg:~A max:~A~%" (min-hash-table *orgs* #'compute-fitness-energy) (float (avg-hash-table *orgs* #'compute-fitness-energy)) (max-hash-table *orgs* #'compute-fitness-energy) (min-hash-table *orgs* #'compute-fitness-1) (float (avg-hash-table *orgs* #'compute-fitness-1)) (max-hash-table *orgs* #'compute-fitness-1) (min-hash-table *orgs* #'compute-fitness-offspring) (float (avg-hash-table *orgs* #'compute-fitness-offspring)) (max-hash-table *orgs* #'compute-fitness-offspring))))

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
    (setf energy-index (1+ (xorshift:random *num-instructions*))))
  (let ((world-w (array-dimension *world* 0))
	(world-h (array-dimension *world* 1)))
    (multiple-value-bind (x y) (funcall position-function nature-object (nature-nexttick nature-object))
      (let* ((rx (mod (+ (floor x) (floor (* edge (xorshift:random-gaussian)))) world-w))
	     (ry (mod (+ (floor y) (floor (* edge (xorshift:random-gaussian)))) world-h))
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

(defmethod idleloop-event ((store database-store))
  (with-slots (database-name nexttick) store
    (when clsql:*default-database* ;useful for developing
      (clsql:disconnect))
    (clsql:connect (list database-name) :database-type :sqlite3 :encoding :UTF-8)
    (clsql:with-transaction ()
      (format t "Storing the world to database ~A at tick ~A.~%" database-name nexttick)
      (store-xorshift-random-number-generator nexttick)
      (let ((orgs (loop for val being the hash-value of *orgs* collect val)))
	(store-orgs nexttick orgs))
      (loop for sun in *world-sun* do
	   (store-sun nexttick sun))
      (store-world nexttick *world*)
      (format t "Done storing the world.~%"))
    (clsql:disconnect)
    (incf (database-store-nexttick store) *database-store-world-wait*)
    (cl-heap:add-to-heap *event-heap* store))
  nil)

(let ((lastloop nil))
  (defun idleloop (lasttick)
    (declare (optimize (debug 3)))
    (let ((loop-start-real-time (get-internal-real-time))
	  (total-ins-count 0))
      ;; sun, clouds and organisms conceptually are event sources that have an #'EVENTSOURCE-NEXTTICK.
      (loop until (let* ((eventsource (cl-heap:peep-at-heap *event-heap*)))
		    (or (null eventsource) (> (eventsource-nexttick eventsource) lasttick)))
	 do
	 ;;(prind *world-tick* *orgs* *event-heap*)
	   (let* ((eventsource (cl-heap:pop-heap *event-heap*)))
	     (let ((ins-count (idleloop-event eventsource)))
	       (when (typep eventsource 'org)
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
	     (print-orgcont (argmax-hash-table *orgs* (lambda (org) (compute-fitness org #'orgcont-energy-out-sum)))))))))

;;(software-render-texture)
