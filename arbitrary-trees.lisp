;; for development of the algorithms, see file arbitrary-trees.dia

(defun enumerate-objects-to-bins (j i)
  "Enumerate all the possibilities that exist for assigning j objects to i bins where j >= i (bins may have 0 or more objects).
The possible assignments are returned as lists l where bin x has (elt l x) objects."
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
  "sets is a list of lists with objects in them.
This function computes all possible combinations of objects in the sets, i.e. one object for each set for each possibility.
For each combination, the function f is called with the combination as argument."
  (labels ((rec (s comb)
	     (if (null s)
		 (funcall f (reverse comb)) ; can't use nreverse here b/c the list is also used in other 
		 (let ((bin (car s))
		       (rest (cdr s)))
		   (loop for obj in bin do
			(rec rest (cons obj comb)))))))
    (rec sets nil)))

(defun enumerate-tree-structures (n)
  "Enumerates all arbitrary trees with n nodes.
Returns a list of possibilities, where a possibility is a tree."
  ;;  (format t "enumerate-tree-structures n=~A~%" n)
  (assert (> n 0))
  (if (= n 1) ; tree with 1 node
      (list (list 'a)) ; 1 possibility
      (let ((trees nil)) ; contains all possible trees
	(loop for i from 1 upto n do ; i is the number of subtrees in this layer
	   ;;	     (format t "subtrees:~A~%" i)
	     (loop for k in (enumerate-objects-to-bins (- n i) i) do ; k is one possibility of distributing n-i objects to i bins
		;;		  (format t "k:~A~%" k)
		  (let ((tr (loop for l below (length k) for m in k collect ; l is the bin (or subtree) number that holds m nodes. this loop builds up the tree
				 (progn
				   ;;				   (format t "subtree ~A has ~A nodes~%" l m)
				   (if (= m 0)
				       (list 'a) ; leaf
				       (enumerate-tree-structures m)))))
			(comb nil))
		    ;;		    (format t "new trees:~A~%" tr)
		    (enumerate-set-combinations tr (lambda (x) (setf comb (cons x comb))))
		    ;;			(format t "combinations:~A~%" comb)
		    (setf trees (nconc comb trees)))))
	trees)))

(defun traverse-tree-symbols (f tree)
  "Calls function f for every symbol in tree with the symbol as argument."
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
  "Returns the number of symbols in tree."
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
  "Traverses tree and every time a symbol is encountered replace it with the next symbol in repl.
The length of repl must be equal to the number of symbols in tree.
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

(defun enumerate-trees (n symbols f)
  "Enumerates all tree structures and fill each with every possible symbol combination.
Repeatedly calls f with each possible tree as parameter."
  (let ((structs (enumerate-tree-structures n)))
    (loop for struct in structs do
	 (let* ((c (count-symbols-in-tree struct))
		(symset (loop for i below c collect symbols)))
	   (flet ((replace-and-call (symcomb)
		    (funcall f (replace-symbols-in-tree struct symcomb))))
	     (enumerate-set-combinations symset #'replace-and-call)))))
  nil)

(defun count-tree-nodes (tree)
  "Returns the number of nodes in tree.
The root node counts as one node, i.e. (count-tree-nodes 'A) == 1 and (count-tree-nodes '(A)) == 2."
  (labels ((rec (tree c)
	     ;;(format t "rec tree:~A c:~A~%" tree c)
	     (if (null tree)
		 c
		 (if (listp tree)
		     (if (listp (car tree))
			 (rec (cdr tree) (rec (car tree) (1+ c)))
			 (rec (cdr tree) (rec (car tree) c)))
		     (1+ c)))))
    (if (listp tree)
	(rec tree 1)
	(rec tree 0))))
