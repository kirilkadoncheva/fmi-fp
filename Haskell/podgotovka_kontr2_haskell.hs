-- 2018 - Вариант А -------------------------------------
-- Задача 1 -
repeated :: (Num t,Eq t) => t -> [t] -> Integer
repeated _ [] = 0
repeated el (x : xs)
 | x == el = 1 + repeated el xs
 |otherwise = repeated el xs


getMax :: (Num t,Eq t) => [t] -> Integer
getMax xs = foldr max 0 (map (\x -> repeated x xs) xs)

getMaxRepeated ::(Num t,Eq t)=> [t] -> [t]
getMaxRepeated xs = removeDuplicates (map (\element -> fst element) (filter (\el -> (snd el == maxFound)) (zip xs (map (\x -> repeated x xs) xs))))
 where maxFound = getMax xs

removeEl :: (Num t,Eq t) => t -> [t]-> [t]
removeEl _ [] = []
removeEl el (x : xs)
 | x == el = removeEl el xs
 |otherwise = x : removeEl el xs

removeDuplicates :: (Num t,Eq t) => [t] -> [t]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (removeEl x xs)

transformToMostFrequent :: (Num t,Eq t) => [[t]] -> [[t]]
transformToMostFrequent xs = map (\el -> getMaxRepeated el) xs

isMember :: (Num t,Eq t) => t -> [t] -> Bool
isMember _ [] = False
isMember el (x:xs) 
 | el == x = True
 | otherwise = isMember el xs 

existsInAllLists :: (Num t,Eq t) => t -> [[t]] ->Bool
existsInAllLists el xs = all (\x -> (isMember el x)) xs


 

getMostFrequent :: (Num t,Eq t) => [[t]] -> [t]
getMostFrequent (x : xs) = filter (\el -> (existsInAllLists el xs)) x 

mostFrequent :: (Num t,Eq t) => [[t]] -> t
mostFrequent xs 
 | length (getMostFrequent (transformToMostFrequent xs)) == 0 = 0
 | otherwise =  (getMostFrequent (transformToMostFrequent xs))!!0
 

-- Вариант 2016 - А --------------------------------------------------------
-- Задача 3

type Item = (String, Integer)



sort :: [Item] -> [Item]
sort [] = []
sort ((x1,x2):xs) = less ++ [(x1,x2)] ++ more
 where less = sort (filter (\(a,b) -> b<=x2) xs)
       more = sort (filter (\(a,b) -> b>x2) xs)

q = [("jhdhfdj",1),("jhdhfdj",3),("jhdhfdj",-1)]

len :: [t] -> Integer
len [] = 0
len (x:xs) = 1 + len xs 

expiringItems :: [Item] -> (String, Integer, String)
expiringItems xs = (nextToExpire, countExpired, longestExpired)
  where sorted= sort xs
        longestExpired = fst (head sorted)
        countExpired = len (filter (\(a,b) -> b < 0) sorted)
        nextToExpire = fst( head (filter (\(a,b) -> b >= 0) sorted))

-- Вариант 2018 - Б ---------------------------------------
-- Задача 3 --
isSmallerInterval :: (Integer,Integer) -> (Integer,Integer) -> Bool
isSmallerInterval (a,b) (c,d) = a < c

isProblem :: (Integer,Integer) -> (Integer,Integer) -> Bool
isProblem (a,b) (c,d) = ((a < c) && (b < c)) || ((a > d) && (b > d))

snd1 :: ([Char],(Integer,Integer),Integer) -> (Integer,Integer)
snd1 (a,b,c) = b

isSorted ::  [([Char],(Integer,Integer),Integer)]-> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = (isSmallerInterval (snd1 x) (snd1 (head xs))) && (isSorted xs)

noProblems ::  [([Char],(Integer,Integer),Integer)] -> Bool
noProblems [] = True
noProblems [x] = True
noProblems (x:xs) = (isProblem (snd1 x) (snd1 (head xs))) && (noProblems xs)

isProgram :: [([Char],(Integer,Integer),Integer)] -> Bool
isProgram list =(isSorted list) && (noProblems list) 
          
showss = [("A",(10,30),90),("B",(11,0),120),("C",(12,0),15)]

subsequences :: [a] -> [[a]]
subsequences l = [] : [suffix | prefix <- inits l,
                                suffix <- tails prefix,
                                not $ null suffix]