type DataBase = (String, Integer) 

type Server = (String, Integer, [DataBase])

getCoef :: Server -> Double
getCoef (n, size,[]) = 0
getCoef (n,size, xs) = (fromIntegral summ)/ (fromIntegral size)
                       where summ = sum (map snd xs)
                             

a = ("jdhf",12,[("a",1), ("b",3), ("c",6)])
l = [("A",12,[("a",1), ("b",3), ("c",6)]),("B",13,[("a",1), ("b",4), ("c",7)]),("C",20,[("a",1), ("b",3), ("c",6)])]

sort :: (Eq t,Ord t) =>[(String,t)] -> [(String,t)] 
sort [] = []
sort ((n,c) : rest) = (sort gE) ++ [(n,c)] ++ (sort less)
                      where gE = filter (\(a,b) -> b>=c) rest
                            less = filter (\(a,b) -> b<c) rest

sortDatabases :: (Eq t,Ord t) => [(Server,(String,t))] -> [(Server,(String,t))]
sortDatabases [] = []
sortDatabases ((s,(n,c)) : rest) =  (sortDatabases gE) ++ [(s,(n,c))] ++ (sortDatabases less)
                      where gE = filter (\(x,(a,b)) -> b>=c) rest
                            less = filter (\(x,(a,b)) -> b<c) rest

mostUtilized :: [Server] -> String
mostUtilized [] = ""
mostUtilized xs = fst (head sorted)
                 where mapCoef = map (\(n,size,dbs) -> (n, (getCoef (n,size,dbs))) ) xs
                       sorted = sort mapCoef


biggestDatabase :: Server -> DataBase
biggestDatabase (n,size,[]) = ("",0)
biggestDatabase (n,size,db) = head (sort db)

biggestDatabaseInList :: [Server] -> (Server,DataBase)
biggestDatabaseInList [] = (("",0,[]),("",0))
biggestDatabaseInList l = head sorted
                        where mappedBiggest = map (\(n,size,db) -> ((n,size,db), (biggestDatabase (n,size,db)))) l 
                              sorted = sortDatabases mappedBiggest

removeDatabase :: Server -> DataBase -> Server
removeDatabase (n,size,db) (s,a) = (n, size, (filter (\(x,y) -> x/=s && y/=a) db))

getSizes :: Server -> Integer
getSizes (n,size, db) = sum (map snd db)

addDatabase :: Server -> DataBase -> (Server,Bool)
addDatabase (n,size,db) d@(s, a)
                          | (getSizes (n,size,db)) + a > size = ((n,size,db), False)
                          | otherwise  = ((n, size, (d : db)), True)

changeServer :: [Server] -> Server -> Server -> [Server]
changeServer [] _ _ = []
changeServer ((n,s,db):xs) (n1,s1,d1) b 
                     | n == n1 && s == s1 && db == d1 = b : xs
                     | otherwise = (n,s,db) : (changeServer xs (n1,s1,d1) b)


addNew :: [Server] -> Server -> [Server]
addNew [] c = [c]
addNew l@((n,size,db): rest) c 
                       | (snd added) == False = (c : l)
                       | otherwise = addNew modified (fst added)
                        where 
                            biggestSdb = biggestDatabaseInList l
                            biggestdb = snd biggestSdb
                            added = addDatabase c biggestdb
                            removed = removeDatabase (fst biggestSdb) biggestdb
                            modified  = changeServer l (fst biggestSdb) removed



merge :: (Num t,Eq t,Enum t,Ord t) => [t] -> [t] -> [t]
merge [] a = a
merge a [] = a
merge (x:xs) (y : ys)
                 | x <= y = [x] ++ (merge xs (y:ys))
                 | otherwise = [y] ++ (merge (x:xs) ys)

sortt :: (Num t,Eq t,Enum t,Ord t) =>[t] -> [t] 
sortt [] = []
sortt (x : rest) = (sortt gE) ++ [x] ++ (sortt less)
                      where gE = filter (\b -> b>=x) rest
                            less = filter (\b -> b<x) rest

sortt1 :: (Num t,Eq t,Enum t,Ord t) =>[t] -> [t] 
sortt1 [] = []
sortt1 (x : rest) = (sortt1 less) ++ [x] ++ (sortt1 gE)
                      where gE = filter (\b -> b>=x) rest
                            less = filter (\b -> b<x) rest

repeatf :: (Num t,Eq t,Enum t,Ord t) =>  (t -> t -> t) -> t -> t -> [t]
repeatf f a b =  [(f a b)] ++ [(f b a)] ++ (merge (repeatf f (f a b) b) (repeatf f (f b a) b))

member :: (Num t,Eq t,Enum t,Ord t) =>   [t] -> t -> Bool
member [] _ = False
member (x:xs) a
                | a == x = True
                | x > a = False
                | otherwise = member xs a

g = (\ x y -> x^2 + 3*y)

bins :: (Num t,Eq t,Enum t,Ord t) =>  (t -> t -> t) -> t -> [t]
bins f z = z : (repeatf f z z)

  --[ x | x <- [z..], (member rr x) ]
       --      where rr = repeatf f z z 
 



data Tree t = Empty | Node { root :: t,
                             children :: [Tree t]} deriving (Eq, Show) 


search :: (Eq t) => Tree t -> t -> Bool
search Empty _ = False
search (Node x tr) a 
                      | a == x = True
                      | otherwise = any (==True) (map (\tt -> search tt a) tr)

count :: (Eq t) => Tree t -> t -> Integer
count Empty _ = 0
count (Node x tr) a 
                      | a == x = 1 + (sum (map (\tt -> count tt a) tr))
                      | otherwise = (sum (map (\tt -> count tt a) tr))


append :: [[t]] -> [t]
append a = foldl (++) [] a

nodes :: Tree t -> [t]
nodes Empty = []
nodes (Node x tr) = [x] ++ (append (map (\tt -> nodes tt) tr)) 

allTreesWithRoot ::(Eq t) => Tree t -> t -> [Tree t]
allTreesWithRoot Empty x = []
allTreesWithRoot  (Node x tr) a
                              | x == a = (Node x tr)  : (append (map (\tt -> allTreesWithRoot tt a) tr))
                              | otherwise = (append (map (\tt -> allTreesWithRoot tt a) tr))

tree = Node 1 [(Node 2 []), (Node 3 [(Node 4 []), (Node 5 [(Node 1 [])])])]

maxSuccessors :: (Eq t,Num t,Ord t,Enum t) => Tree t -> t -> t
maxSuccessors Empty _ = 0
maxSuccessors tt@(Node x tr) a 
                               | length allSuch == 0 = 0
                               | otherwise = head (sortt1 allSuch)
                             where subtrees = allTreesWithRoot tt a
                                   allSuch = filter (\y -> (any (\f -> (search f y)) subtrees) &&
                                                        (any (\sf -> (count tt y) == (count sf y)) (filter (\f -> (search f y)) subtrees))) (nodes tt)


--maxSuccessors tt@(Node x tr) a = [ y | y <- (nodes tt), (any (\f -> (search f y)) subtrees) &&
  --                                                      (any (\sf -> (count tt y) == (count sf y)) (filter (\f -> (search f y)) subtrees))]
  --                             where subtrees = allTreesWithRoot tt a
                                       

