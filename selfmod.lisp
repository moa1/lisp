(setq myself '(+ 5 5))

(setq poss '(5 (+ 0 0) (- 0 0) 3))

(defun choose (poss)
  (nth (random (length poss)) poss))

(defun mutate (code)
  (if (< (random 1.0) 0.1)
      (if (< (random 2) 1)
	  (choose poss))
      (if (consp code)
	  (cons (mutate (car code)) (mutate (cdr code)))
	  code)))

