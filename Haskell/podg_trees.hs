module BinaryTree (BinaryTree (Empty, Node), root, left, right, isEmpty) where

data BinaryTree t = Empty | Node { root :: t,
                                   left :: BinaryTree t,
                                   right :: BinaryTree t}
                              deriving (Eq,Ord,Show,Read)

isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty _ = False

maxSumPath :: (Num t, Ord t) => BinaryTree t -> t
maxSumPath Empty = 0
maxSumPath (Node x l r) = x + max (maxSumPath l) (maxSumPath r)

leaf :: BinaryTree t -> Bool
leaf (Node _ Empty Empty) = True
leaf _ = False

tree = Node 1 (Node 6 Empty Empty) (Node 7 Empty Empty)

bloom :: BinaryTree t -> BinaryTree t
bloom Empty = Empty
bloom a@(Node x l r)
     | leaf a == True = (Node x (Node x Empty Empty) (Node x Empty Empty))
     | otherwise = (Node x (bloom l) (bloom r) ) 

prune ::  BinaryTree t -> BinaryTree t
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node x l r) = (Node x (prune l) (prune r))

type BST = BinaryTree

bstInsert :: Ord t => BST t -> t -> BST t
bstInsert Empty x = (Node x Empty Empty)
bstInsert t@(Node x l r) a
         | a < x = Node x (bstInsert l a) r
         | a > x = Node x l (bstInsert r a)
         | otherwise = t 

bstSearch :: Ord t => BST t -> t -> Bool
bstSearch Empty x = False
bstSearch (Node x l r) a
         | x == a = True
         | a < x = bstSearch l a
         |otherwise = bstSearch r a

tree1 = bstInsert (bstInsert (bstInsert Empty 1) 7) 3

bstSize :: BST t -> Int
bstSize Empty = 0
bstSize (Node x l r) = 1 + bstSize l + bstSize r

bstFromList :: Ord t => [t] -> BST t
bstFromList xs = foldl bstInsert Empty xs

bstToList :: BST t -> [t]
bstToList Empty = []
bstToList (Node x l r) = bstToList l ++ [x] ++ bstToList r

bstSort :: Ord t => [t] -> [t]
bstSort = bstToList . bstFromList

data Direction = L | R deriving Show

bstPath :: Ord t => BST t -> t -> Maybe [Direction]
bstPath Empty _ = Nothing
bstPath (Node x l r) a
     | a == x = Just []
     | a < x = fmap (L :) (bstPath l a)
     | otherwise = fmap (R :) (bstPath r a)


type Map k v = BinaryTree (k, v)

mapInsert :: Ord k => Map k v -> k -> v -> Map k v
mapInsert Empty k v = Node (k, v) Empty Empty
mapInsert (Node (k, v) l r) kk vv
     | kk < k = Node (k, v) (mapInsert l kk vv) r
     | kk > k = Node (k, v) l (mapInsert r kk vv) 
     | otherwise = Node (k,vv) l r

mapSearch :: Ord k => Map k v -> k -> Maybe v
mapSearch Empty _ = Nothing
mapSearch (Node (k,v) l r) kk
   | kk == k = Just v
   | kk < k = mapSearch l kk
   | otherwise = mapSearch r kk


intervalTree :: (Num t, Ord t) => BinaryTree t -> BinaryTree (t,t)
intervalTree Empty = Empty
intervalTree (Node x l r) = Node (interval x ll rr) ll rr
               where ll = intervalTree l
                     rr = intervalTree r
                     interval x Empty Empty = (x,x)
                     interval x (Node (y1,y2) _ _) Empty = (min x y1, max x y2)
                     interval x Empty (Node (y1,y2) _ _) = (min x y1, max x y2)
                     interval x (Node (y1,y2) _ _) (Node (z1,z2) _ _) = 
                     	(minimum [x,y1,z1], maximum [x,y2,z2])