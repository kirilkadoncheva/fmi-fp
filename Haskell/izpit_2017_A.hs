
okInterval :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
okInterval f1 f2 a b 
                | a >= b = False
                | length [x | x <- [a..b], f1 x == f2 x] == (b - a + 1) = True
                | otherwise = False


largestInterval :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> (Int, Int)
largestInterval f1 f2 a b = head intervals
                         where intervals = [(p,q) | p <- [a..b], q <- [b, (b - 1)..a],
                                                         p < q,
                                                         (okInterval f1 f2 p q) == True]

data BinaryTree t = Empty | Node { root :: t,
                                   left :: BinaryTree t,
                                   right :: BinaryTree t} deriving (Eq,Ord,Show,Read)

isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty x = False


intervalTree :: (Num t, Ord t) => BinaryTree t -> BinaryTree (t,t)
intervalTree Empty = Empty
intervalTree (Node x Empty Empty) = (Node (x,x) Empty Empty)
intervalTree (Node x l r) = Node (interval x ll rr) ll rr
                             where ll = intervalTree l
                                   rr = intervalTree r
                                   interval x Empty Empty = (x,x)
                                   interval x (Node (y1,y2) _ _) Empty = ((min x y1), (max x y2))
                                   interval x Empty (Node (y1,y2) _ _)  = ((min x y1), (max x y2))
                                   interval x (Node (z1,z2) _ _) (Node (y1,y2) _ _)  = ((minimum [x, y1, z1]), (maximum [x, y2, z2]))

tree = Node 5 (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)) (Node 6 Empty Empty)

makeUnique :: [Integer] -> [Integer]
makeUnique [] = []
makeUnique (x:xs) = x : makeUnique(dropWhile (==x) xs)


sumsOfSquares = makeUnique [x | x <- [1..], a <- [1..x], b <- [1..x], x == a^2 + b^2]

type Video = (String, Double)

averageLength :: [Video] -> Double
averageLength [] = 0
averageLength xs = (foldr (+) 0 (map snd xs))/ (fromIntegral (length xs))

sort :: [Video] -> [Video]
sort [] = []
sort (x:xs) = (sort less) ++ [x] ++ (sort ge)
            where less = filter (<x) xs
                  ge = filter (>=x) xs


averageVideo :: [Video] -> String
averageVideo [] = ""
averageVideo xs = fst (head mappedPos)
  where al = averageLength xs
        mapped = sort (map (\(n,l) -> (n, al-l )) xs) 
        mappedPos = dropWhile (\(n,l) -> l < 0) mapped
                  