(load "~/quicklisp/setup.lisp")
(ql:quickload :spatial-trees)
(ql:quickload :utils)
(ql:quickload :spatial-trees.nns)

(defstruct r
  x1 y1 x2 y2)

(defun random-r (x y w h)
  "Make a random rectangle within the rectangle with left upper corner X,Y and width and height W,H."
  (let* ((x1 (+ x (random w)))
	 (y1 (+ y (random h)))
	 (x2 (+ x1 (random (- (+ x w) x1))))
	 (y2 (+ y1 (random (- (+ y h) y1)))))
    (make-r :x1 x1 :y1 y1 :x2 x2 :y2 y2)))

(defun r-rect (r)
  "Make a bounding box function."
  (rectangles:make-rectangle :lows (list (r-x1 r) (r-y1 r))
			     :highs (list (r-x2 r) (r-y2 r))))

(defun make-tree ()
  (spatial-trees:make-spatial-tree :r :rectfun #'r-rect))

(defparameter *tree* (make-tree))

(defun insert-random-rs (tree x y w h n)
  (loop for i below n do
       (spatial-trees:insert (random-r x y w h) tree)))

(defun list-colliders (tree x1 y1 x2 y2)
  (let* ((r (make-r :x1 x1 :y1 y1 :x2 x2 :y2 y2))
	 (l (spatial-trees:search r tree)))
    ;;(format t "colliders with ~S:~%~S~%" r l)
    l))

(defun remove-colliders (tree x1 y1 x2 y2)
  (let ((l (list-colliders tree x1 y1 x2 y2)))
    (loop for e in l do
	 (spatial-trees:delete e tree))))

(defun test-insert (n1 n2)
  "Compare the run-times of adding an additional rectangle in two trees with N1 and N2 randomly generated rectangles."
  (let ((t1 (make-tree))
	(t2 (make-tree)))
    (insert-random-rs t1 0 0 10 10 n1)
    (insert-random-rs t2 0 0 10 10 n2)
    (utils:timediff
     (spatial-trees:insert (random-r 0 0 100 100) t1)
     (spatial-trees:insert (random-r 0 0 100 100) t2)
     :showtimes t)))
     
(defun test-search (n1 n2)
  "Compare the run-times of searching the colliders with a rectangle that does not have any colliders in two trees with N1 and N2 randomly generated rectangles."
  (let ((t1 (make-tree))
	(t2 (make-tree)))
    (insert-random-rs t1 0 0 10 10 n1)
    (insert-random-rs t2 0 0 10 10 n2)
    (utils:timediff
     (spatial-trees:search (random-r -1000 -1000 100 100) t1)
     (spatial-trees:search (random-r -1000 -1000 100 100) t2)
     :showtimes t)))

(defun test-delete (n1 n2)
  "Compare the run-times of deleteing a rectangle in two trees with N1 and N2 randomly generated rectangles."
  (let ((t1 (make-tree))
	(t2 (make-tree)))
    (insert-random-rs t1 0 0 10 10 n1)
    (insert-random-rs t2 0 0 10 10 n2)
    (utils:timediff
     (let ((r (random-r 0 0 10 10)))
       (spatial-trees:insert r t1)
       (spatial-trees:delete r t1))
     (let ((r (random-r 0 0 10 10)))
       (spatial-trees:insert r t2)
       (spatial-trees:delete r t2))
     :showtimes t)))

;;(defun test-nearest-neighbor
