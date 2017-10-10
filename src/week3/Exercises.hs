module Exercise where

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x : xs) = if p x then x : filter p xs else filter p xs

{-}
data Tree = Leaf | Node Int Tree Tree deriving Show

t1 = Node 41 Leaf Leaf
t2 = Node 42 t1 Leaf
t3 = Node 43 t2 Leaf
t4 = Node 43 t2 (Node 43 Leaf Leaf)

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ l r) = 1 + (max (treeDepth l) (treeDepth r))

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node v l r) = v + (treeSum l) + (treeSum r)

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node v l r) l_lim h_lim =
  l_lim <= v && v < h_lim && isSortedTree l l_lim v && isSortedTree r v h_lim

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node v l Leaf) = Node v l (Node (v + 1) Leaf Leaf)
addNewMax (Node v l r)    = Node v l (addNewMax r)

t11 = addNewMax Leaf
t12 = addNewMax t11
t13 = addNewMax t12
t14 = addNewMax t13
t15 = addNewMax t14
-}

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

treezip :: (Tree a) -> (Tree b) -> (Tree (a,b))
treezip Leaf _ = Leaf
treezip _ Leaf = Leaf
treezip (Node v1 l1 r1) (Node v2 l2 r2) =
  Node (v1, v2) (treezip l1 l2) (treezip r1 r2)

treeunzip :: (Tree (a,b)) -> (Tree a, Tree b)
treeunzip Leaf = (Leaf, Leaf)
treeunzip (Node (v1, v2) l r) =
  let (l1, l2) = treeunzip l
      (r1, r2) = treeunzip r
  in ((Node v1 l1 r1), (Node v2 l2 r2))
