(load "~/quicklisp/setup.lisp")
(ql:quickload :optima)

;; TODO: rename this file to fingertree.lisp.

;; foldl :: (a -> b -> a) -> a -> [b] -> a
;; foldl, applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:
;;  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
;; The list must be finite.

;; foldr :: (a -> b -> b) -> b -> [a] -> bSource
;; foldr, applied to a binary operator, a starting value (typically the right-identity of the operator), and a list, reduces the list using the binary operator, from right to left:
;;  foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)


;; data Node a = Node2 a a | Node3 a a a
;; data FingerTree a = Empty
;;                   | Single a
;;                   | Deep (Digit a) (FingerTree (Node a)) (Digit a)
;; type Digit a = [a]  ; where the list [a] must have between 1 and 4 elements

;; or alternatively, and more efficently:
;; data Digit a = One   a
;;              | Two   aa
;;              | Three aaa
;;              | Four  aaaa


;; In both `reducer` and `reducel`, the

;; infixr 5 /
;; (/)                        :: a -> FingerTree a -> FingerTree a
;; a / Empty                  = Single a
;; a / Single b               = Deep [a] Empty [b]
;; a / Deep [b, c, d, e] m sf = Deep [a, b] ((Node3 c d e) / m) sf
;; a / Deep pr m sf           = Deep ([a] ++ pr) m sf

;; infixl 5 \
;; (\)                        :: FingerTree a -> a -> FingerTree a
;; Empty                  \ a = Single a
;; Single b               \ a = Deep [b] Empty [a]
;; Deep pr m [e, d, c, b] \ a = Deep pr (m \ (Node3 e d c)) [b, a]
;; Deep pr m sf           \ a = Deep pr m (sf ++ [a ])


;; toTree :: (Reduce f ) => f a -> FingerTree a
;; toTree s = s /' Empty

;; where /' is the lifting of /.


;; data ViewL s a = NilL | ConsL a (s a)

;; viewL                :: FingerTree a -> ViewL FingerTree a
;; viewL Empty          = NilL
;; viewL (Single x)     = ConsL x Empty
;; viewL (Deep pr m sf) = ConsL (head pr) (deepL (tail pr) m sf)

;; where (head) and (tail) are the accessors of lists, e.g. [1,2,3].

;; deepL          :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
;; deepL [] m sf = case viewL m of
;;                       NilL -> toTree sf
;;                       ConsL a m' -> Deep (toList a) m' sf
;; deepL pr m sf = Deep pr m sf


(defstruct node2
  node0 node1)

(defstruct node3
  node0 node1 node2)

(defun make-node3* (node0 node1 node2)
  (make-node3 :node0 node0 :node1 node1 :node2 node2))

(defstruct deep
  digit-l
  ft
  digit-r)

(defstruct fingertree
  (type :empty) ;one of (:empty :single :deep)
  object ;depending on the value of TYPE: (:empty no meaning) (:single pointer to a) (:deep pointer to instance of structure DEEP).
  )

(defconstant +empty+ (make-fingertree :type :empty)
  "An empty fingertree.")

(defun push-l (ft a)
  (optima:match ft
    ((fingertree (type :empty))
     (make-fingertree :type :single :object a))
    ((fingertree (type :single) (object b))
     (make-fingertree :type :deep :object (make-deep :digit-l (list a)
						     :ft +empty+
						     :digit-r (list b))))
    ((fingertree (type :deep) (object (deep (digit-l (list b c d e)) (ft m) (digit-r sf))))
     (make-fingertree :type :deep :object (make-deep :digit-l (list a b)
						     :ft (push-l m (make-node3* c d e))
						     :digit-r sf)))
    ((fingertree (type :deep) (object (deep (digit-l pr) (ft m) (digit-r sf))))
     (make-fingertree :type :deep :object (make-deep :digit-l (cons a pr)
						     :ft m
						     :digit-r sf)))))

