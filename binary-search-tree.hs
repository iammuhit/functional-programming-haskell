module BinarySearchTree(BSTree(Void,BSNode), subTree, Tree(Node), count, labels, height) where

{- Binary Search Trees

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
              are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree  -- do not modify this line

{-  subTree a b t

    Takes two integers and the Binary Search Tree

    RETURNS: The sub-tree where the nodes are greater than or equal to a but smaller than b
    EXAMPLES: subTree 5 8 (BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                            3
                            (BSNode Void 5 Void))
                    6
                    (BSNode Void
                            7
                            (BSNode Void 8 (BSNode Void 9 Void)))) == BSNode (BSNode Void 5 Void) 6 (BSNode Void 7 Void)
-}
subTree :: Integer -> Integer -> BSTree -> BSTree

subTree a b (Void) = Void
subTree a b (BSNode Void x Void) 
    | x >= a && x < b   = BSNode Void x Void
    | otherwise         = Void

subTree a b (BSNode Void x r) 
    | x >= a && x < b   = subTree a b (BSNode Void x (subTree a b r))
    | otherwise         = subTree a b r

subTree a b (BSNode l x Void) 
    | x >= a && x < b   = subTree a b (BSNode (subTree a b l) x Void)
    | otherwise         = subTree a b l

subTree a b (BSNode l x r) 
    | x >= a && x < b   = BSNode (subTree a b l) x (subTree a b r)
    | otherwise         = subTree a b (BSNode (subTree a b l) x (subTree a b r))


data Tree a = Node a [Tree a]

tree1 = Node "hej" []
tree2 = Node 1 [Node 2 []]
tree3 = Node "hej" [Node "hello" [Node "ni hao" [Node "ahoj" []]],
                    Node "bonjour" [Node "privet" [Node "guten tag" []]],
                    Node "namaste" [Node "ciao" [Node "as-salam alaykom" [Node "saluton" [Node "hei" [Node "halo" []]],
                                                                          Node "kon-nichiwa" [Node "an-nyong ha-se-yo " [Node "ola" []]],
                                                                          Node "sa-wat-dee" [Node "selam" [Node "jambo" []]]]]]]

{-  count t

    Takes a tree of type Tree a

    RETURNS: The number of nodes of that tree
    EXAMPLES: count (Node "hej" []) == 1
              count (Node 1 [Node 2 []]) == 2
-}
count :: Tree a -> Integer
count (Node _ []) = 1
count (Node r (n:xn)) = count (n) + count (Node r xn)

{-  labels t

    Takes a tree of type Tree a

    RETURNS: The list of all node labels of that tree
    EXAMPLES: labels (Node "hej" []) == ["hej"]
              labels (Node 1 [Node 2 []]) == [1, 2]
-}
labels :: Tree a -> [a]
labels (Node n []) = [n]
labels (Node n xn) = n : concatMap labels xn

{-  height t

    Takes a tree of type Tree a

    RETURNS: The height of tree
    EXAMPLES: height (Node "hej" []) == 1
              height (Node 1 [Node 2 []]) == 2
-}
height :: Tree a -> Integer
height (Node _ []) = 1
height (Node _ xn) = 1 + maximum (map height xn)