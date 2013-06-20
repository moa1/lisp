;; for development of the algorithms, see file arbitrary-trees.dia

(defun distribute (j i)
  "enumerate all the possibilities that exist for assigning j objects to i bins where j >= i (bins may have 0 or more objects).
each bin will have atleast objects.
return as list of possible assignments. the possible assignments are returned as lists l where bin x has (elt l x) objects."
  (assert (> i 0)) ; need at least 1 bin to distribute objects to
  (if (= i 1) ; trivial case: 1 bin
      (list (list j)) ; 1 bin and j objects
      (let ((l nil)) ; possiblities (lists l)
	(loop for k upto j do
	     (loop for l1 in (distribute (- j k) (1- i)) do
;;		  (format t "k:~A l1:~A~%" k l1)
		  (setf l (cons (cons k l1) l))))
	l)))

(defun enumerate-all-trees (n)
  "build all arbitrary trees with n nodes.
return as list of possibilities, where a possibility is a tree in list notation."
  (and (= n 3) (format t "enumerate-all-trees n=~A~%" n))
  (assert (> n 0))
  (if (= n 1) ; tree with 1 node
      (list (list 'a)) ; 1 possibility
      (let ((trees nil)) ; contains all possible trees
	(loop for i from 1 upto n do ; i is the number of subtrees in this layer
	     (and (= n 3) (format t "subtrees:~A~%" i))
	     (loop for k in (distribute (- n i) i) do ; k is one possibility of distributing n-i objects to i bins
		  (and (= n 3) (format t "k:~A~%" k))
		  (let ((tr (loop for l below (length k) for m in k collect ; l is the bin (or subtree) number that holds m nodes. this loop builds up the tree
				 (progn
				   (and (= n 3) (format t "subtree ~A has ~A nodes~%" l m))
				   (if (= m 0)
				       'a ; leaf
				       (enumerate-all-trees m))))))
		    (format t "new trees:~A~%" tr)
		    (setf trees (cons tr trees)))))
	trees)))

				
