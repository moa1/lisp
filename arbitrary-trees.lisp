;; for development of the algorithms, see file arbitrary-trees.dia

(defun enumerate-objects-to-bins (j i)
  "Enumerate all the possibilities that exist for assigning J objects to I bins (bins may have 0 or more objects).
The possible assignments are returned as lists L where bin X has (elt L X) objects."
  (assert (> i 0)) ; need at least 1 bin to distribute objects to
  (if (= i 1) ; trivial case: 1 bin
      (list (list j)) ; 1 bin and j objects
      (let ((l nil)) ; possiblities (lists l)
	(loop for k upto j do
	     (loop for l1 in (enumerate-objects-to-bins (- j k) (1- i)) do
;;		  (format t "k:~A l1:~A~%" k l1)
		  (setf l (cons (cons k l1) l))))
	l)))

(defun enumerate-set-combinations (sets f)
  "SETS is a list of lists with objects in them.
This function computes all possible combinations of objects in the sets, i.e. one object for each set for each possibility.
For each combination, the function F is called with the combination as argument."
  (labels ((rec (s comb)
	     (if (null s)
		 (funcall f (reverse comb)) ; can't use nreverse here b/c the list is also used in other 
		 (let ((bin (car s))
		       (rest (cdr s)))
		   (loop for obj in bin do
			(rec rest (cons obj comb)))))))
    (rec sets nil)))

(defun enumerate-tree-structures (n)
  "Enumerates all arbitrary trees with N nodes, where NIL counts as a tree with one node.
Returns a list of possibilities, where a possibility is a tree."
  (assert (> n 0))
  (if (= n 1)
      (list nil)
      (enumerate-tree-structures-1based (1- n))))

(defun enumerate-tree-structures-1based (n)
  "Enumerates all arbitrary trees with N nodes, where '(A) counts as a tree with one node.
Returns a list of possibilities, where a possibility is a tree."
  ;;(format t "enumerate-tree-structures n=~A~%" n)
  (assert (> n 0))
  (if (= n 1) ;tree with 1 node
      (list '(a) '(())) ;1 possibility
      (let ((trees nil)) ;contains all possible trees
	(loop for i from 1 upto n do ;i is the number of subtrees in this layer == i is the number of parallel nodes in this layer (i.e. for a tree with max. 3 nodes, there can be 1, 2, or 3 nodes below the top node.)
	     ;;(format t "subtrees:~A~%" i)
	     (loop for k in (enumerate-objects-to-bins (- n i) i) do ;k is one possibility of distributing n-i objects to i bins
		  ;;(format t "k:~A~%" k)
		  (let ((tr (loop for l below (length k) for m in k collect ;l is the bin (or subtree) number that holds m nodes. this loop builds up the tree
				 (progn
				   ;;(format t "subtree ~A has ~A nodes~%" l m)
				   (if (= m 0)
				       (list 'a '()) ;leaf
				       (enumerate-tree-structures-1based m)))))
			(comb nil))
		    ;;(format t "new trees:~A~%" tr)
		    (enumerate-set-combinations tr (lambda (x) (setf comb (cons x comb))))
		    ;;(format t "combinations:~A~%" comb)
		    (setf trees (nconc comb trees)))))
	trees)))

(defun traverse-tree-symbols (f tree)
  "Calls function F for every symbol in tree TREE with the symbol as argument."
  (labels ((rec (tree)
	     (when (not (null tree))
		 (if (listp tree)
		     (progn
		       (rec (car tree))
		       (rec (cdr tree)))
		     (funcall f tree)))))
    (rec tree)
    nil))

(defun count-symbols-in-tree (tree)
  "Returns the number of symbols in tree TREE."
  (let ((c 0))
    (flet ((f (s)
	     (declare (ignore s))
	     (incf c)))
      (traverse-tree-symbols #'f tree)
      c)))

(assert (= (count-symbols-in-tree '((A A) A)) 3))
(assert (= (count-symbols-in-tree '(A (((A))))) 2))
(assert (= (count-symbols-in-tree '()) 0))

(defun replace-symbols-in-tree (tree repl)
  "Traverses tree TREE and every time a non-list (e.g. a symbol) is encountered replace it (non-destructively) with the next element (e.g. a symbol) in list REPL.
The length of REPL must be equal to the number of symbols in TREE.
Returns the new tree."
  (labels ((rec (tree new)
	     (if (null tree)
		 new
		 (if (listp tree)
		     (cons (rec (car tree) nil) (rec (cdr tree) nil))
		     (let ((s (car repl)))
		       (setf repl (cdr repl))
		       s)))))
    (rec tree nil)))

(assert (equal (replace-symbols-in-tree '((A A) A) '(1 2 3)) '((1 2) 3)))
(assert (equal (replace-symbols-in-tree '(((() A)) A) '(1 2)) '(((() 1)) 2)))

(defun enumerate-labelled-trees (n symbols new-struct-f f)
  "Enumerates all tree structures and fill each with every possible symbol combination.
N is the tree node count, SYMBOLS is a list of possible symbols, NEW-STRUCT is called with each tree structure as parameter (but with all symbols replaced by A).
Repeatedly calls F with each possible tree as parameter."
  (let ((structs (enumerate-tree-structures n)))
    (loop for struct in structs do
	 (funcall new-struct-f struct)
	 (let* ((c (count-symbols-in-tree struct))
		(symset (loop for i below c collect symbols)))
	   (flet ((replace-and-call (symcomb)
		    (funcall f (replace-symbols-in-tree struct symcomb))))
	     (enumerate-set-combinations symset #'replace-and-call)))))
  nil)

(defun count-labelled-trees (n num-symbols)
  "Return the number of possible trees with N nodes and NUM-SYMBOLS possible symbols in each leaf."
  (let ((trees (enumerate-tree-structures n)))
    (loop for tree in trees sum
	 (expt num-symbols (count-symbols-in-tree tree)))))

(defun count-tree-nodes (tree)
  "Returns the number of nodes in tree.
The root node counts as one node, i.e. (count-tree-nodes 'A) == 1 and (count-tree-nodes '(A)) == 2.
I.e. each open bracket and each symbol counts as 1 node."
  (let ((ht (make-hash-table :test #'eq)))
    (labels ((rec-cached (tree c)
	       (multiple-value-bind (val present-p) (gethash tree ht)
		 (if present-p
		     (+ val c)
		     (let ((res (rec tree 0)))
		       (setf (gethash tree ht) res)
		       (+ res c)))))
	     (rec (tree c)
	       ;;(format t "rec tree:~A c:~A~%" tree c)
	       (if (null tree)
		   c
		   (if (listp tree)
		       (if (listp (car tree))
			   (rec-cached (cdr tree) (rec-cached (car tree) (1+ c)))
			   (rec-cached (cdr tree) (rec-cached (car tree) c)))
		       (1+ c)))))
      (if (listp tree)
	  (rec-cached tree 1)
	  (rec-cached tree 0)))))

(assert (= 1 (count-tree-nodes '())))
(assert (= 3 (count-tree-nodes '((A)))))
(assert (= 3 (count-tree-nodes '(A ()))))
(assert (= 4 (count-tree-nodes '(A (A)))))

;; (count-labelled-trees (count-tree-nodes '((0 <) (0 swap -) () ifte)) 5) == 234784662 (5 different symbols)
;; (expt 12 7) == 35831808 (there are 12 symbols or inside brackets in '((0 <) (0 swap -) () ifte), and 7==5+2 (2 stands for opening/closing bracket symbols))
