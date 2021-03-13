 -- how to define dataTypes
 --data Weekday = Monday | Tuesday| Wednesday | Thursday | Friday | Saturday | Sunday
 --isWeekend :: Weekday -> Bool
 --isWeekend Saturday = True
 --isWeekend Sunday = True
 --isWeekend _ = False

 --data Student = Student { name :: String, fn :: Int}
 --name :: Student -> String
 --name (Student n _) = n

 --fn :: Student -> Int
 --fn (Student _ f) = f 

data BinaryTree t = Empty | Node {root :: t , 
                                   left :: (BinaryTree t),
                                   right :: (BinaryTree t)}
                            deriving ( Ord, Show, Read)
                                     
instance Eq t => Eq (BinaryTree t) where
 Empty == Empty = True
 (Node x2 l2 r2) == (Node x1 l1 r1) = (x1 == x2) && (l1==l2) && (r1 == r2)
 _ == _ = False

isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty _ = False

maxSumPath :: (Num t, Ord t)  => BinaryTree t -> t
maxSumPath Empty = 0
maxSumPath (Node x l r) = x + max(maxSumPath l) (maxSumPath r)

bloom ::  BinaryTree t -> BinaryTree t
bloom Empty = Empty
bloom t@(Node x Empty Empty) = Node x t t
bloom (Node x l r) = Node x (bloom l) (bloom r)

prune:: BinaryTree t -> BinaryTree t
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node x l r) = Node x (prune l) (prune r)