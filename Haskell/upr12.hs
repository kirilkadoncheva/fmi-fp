
data BinaryTree t = Empty | Node {root :: t , 
                                   left :: (BinaryTree t),
                                   right :: (BinaryTree t)}
                            deriving (Eq, Ord, Show, Read)
                                     


type BST = BinaryTree

data Direction = L | R deriving Show

bstInsert :: Ord t => BST t -> t -> BST t
bstInsert Empty x = Node x Empty Empty
bstInsert t@(Node x l r) x1
   | x1 < x = Node x (bstInsert l x1) r
   | x1 > x = Node x l (bstInsert r x1)
   | otherwise = t

bstSearch :: Ord t => BST t -> t -> Bool
bstSearch Empty _ = False
bstSearch (Node x l r) x1
   | x1 == x = True
   | x1 < x = bstSearch l x1
   | otherwise =  bstSearch r x1 

bstSize :: BST t -> Int
bstSize Empty = 0
bstSize (Node x l r) = 1 + bstSize l + bstSize r

bstFromList :: Ord t => [t] -> BST t
bstFromList = foldl bstInsert Empty 

bstValues :: BST t -> [t]
bstValues Empty = []
bstValues (Node x l r) = bstValues l ++ [x] ++ bstValues r

bstSort :: Ord t => [t] -> [t]
bstSort = bstValues . bstFromList

bstPath ::Ord t=> BST t -> t -> Maybe [Direction]
bstPath Empty _ = Nothing
bstPath (Node x l r) x1
          | x == x1 = Just []
          | x1 < x = navigate (bstPath l x1) L
          --fmap (L:) (bstPath l x1) - syshtoto e
          | otherwise = navigate ( bstPath r x1) R
             where navigate Nothing _ = Nothing
                   navigate (Just ds) d = Just (d : ds)


type Map k v = BinaryTree (k, v)

mapInsert :: Ord k => Map k v -> k -> v -> Map k v
mapInsert Empty k v = Node (k, v) Empty Empty
mapInsert (Node (k,v) l r) k1 v1
   | k1 == k = (Node (k, v1) l r)
   | k1 < k = (Node (k,v ) (mapInsert l k1 v1)  r)
   |otherwise =(Node (k,v ) l  (mapInsert r k1 v1) )

mapSearch :: Ord k => Map k v -> k -> Maybe v
mapSearch Empty _ = Nothing
mapSearch (Node (k,v) l r) k1
   | k == k1 = Just v
   | k1 < k = mapSearch l k1
   | otherwise = mapSearch r k1

intervalTree ::Num t => BinaryTree t -> BinaryTree (t,t)
intervalTree Empty = Empty
intervalTree (Node x Empty Empty) = Node (x,x) Empty Empty
intervalTree (Node )




intervalTree :: (Num t, Ord t) => BinaryTree t -> BinaryTree (t, t)
intervalTree Empty = Empty
intervalTree (Node x l r) = Node (interval x l' r') l' r'
  where l' = intervalTree l
        r' = intervalTree r
        interval x Empty Empty = (x, x)
        interval x (Node (y', y'') _ _) Empty = (min x y', max x y'')
        interval x Empty (Node (y', y'') _ _) = (min x y', max x y'')
        interval x (Node (y', y'') _ _) (Node (z', z'') _ _) =
          (minimum [x, y', z'], maximum [x, y'', z''])