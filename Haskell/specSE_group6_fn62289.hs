
-- Задача 0
dropWhilep :: (a -> a -> Bool) -> [a] -> [a]
dropWhilep _ [] = []
dropWhilep _ [x] = []
dropWhilep p (x:xs)
   |  p x (head xs) == True = dropWhilep p xs
   | otherwise = xs

takeWhilep :: (a -> a -> Bool) -> [a] -> [a]
takeWhilep _ [] = []
takeWhilep _ [x] = [x]
takeWhilep p (x:xs)
   |  p x (head xs) == True = [x] ++ takeWhilep p xs
   |otherwise = [x]


group :: Eq a => [a] -> [[a]]
group [] = []
group xs = takeWhilep (==) xs : group (dropWhilep (==) xs)

-- Задача 1

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [x] = [x]
sortBy p (x:xs) = sortBy p (filter (\a -> p a x == LT ) xs) ++ [x] ++ sortBy p (filter (\a -> p a x == GT|| p a x == EQ) xs)

-- Задача 2

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c 
on f p a b = (p a) `f` (p b)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]] 
groupBy p [] = []
groupBy p xs = (takeWhilep p xs) : groupBy p (dropWhilep p xs)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f1 f2 a = ((f1 a), (f2 a))

sortDoubles :: Ord b => [(b, a)] -> [(b, a)]
sortDoubles [] = []
sortDoubles [(b, a)] = [(b, a)]
sortDoubles ((b, a) : rest) = sortDoubles (filter (\x -> fst x < b) rest) ++
                            [(b, a)] ++
                            sortDoubles (filter (\x -> fst x > b || fst x == b) rest)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn _ [x] = [x]
sortOn f l = map snd (sortDoubles mappedList)
                   where mappedList = map (\x -> ((f x), x)) l


data Down a = Down a deriving Eq
instance Ord a => Ord (Down a) where compare (Down x) (Down y) = compare y x

groupDoubles :: Eq b => [(a,b)] -> [[(a,b)]]
groupDoubles [] = []
groupDoubles l = takeWhilep (\x y -> snd x == snd y) l : 
                   groupDoubles (dropWhilep (\x y -> snd x == snd y) l)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn f [x] = [[x]]
groupOn f l = map (\x -> map fst x) (groupBy (\ (_,fx) (_,fy) -> (fx == fy)) mappedList)
              where mappedList = map (\x -> (x, (f x))) l

takeAllEqual :: Ord b => b -> [(a,b)] -> [(a,b)]
takeAllEqual _ [] = []
takeAllEqual el (x:xs) 
      |snd x == el = x : (takeAllEqual el xs)
      |otherwise = (takeAllEqual el xs)

dropAllEqual :: Ord b => b -> [(a,b)] -> [(a,b)]
dropAllEqual _ [] = []
dropAllEqual el (x:xs) 
      |snd x == el =  (dropAllEqual el xs)
      |otherwise = x : (dropAllEqual el xs)

groupInSets :: Ord b => [(a,b)] -> [[(a,b)]]
groupInSets [] = []
groupInSets (x:xs) = (takeAllEqual (snd x) (x:xs)) : (groupInSets (dropAllEqual (snd x) xs))

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn _ [] = []
classifyOn f l = map (\l -> map fst l) groupedList
                  where groupedList = groupInSets mappedList
                         where mappedList = map (\x -> (x, f x)) l 

                         


data NonEmpty a = a :| [a] --deriving Show

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty xs = [head l :| tail l | l <- ll]
       where ll = group xs

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty p xs = [head l :| tail l | l <- ll]
              where ll = groupBy p xs

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f xs = [head l :| tail l | l <- ll]
               where ll = groupOn f xs

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f xs = [head l :| tail l | l <- ll]
               where ll = classifyOn f xs