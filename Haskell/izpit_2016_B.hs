transpose ::  [[t]] -> [[t]]
transpose [] = []
transpose rs 
         | any (\r -> length r == 0) rs = []
         | otherwise = (map (\r -> head r) rs) : (transpose (map (\r -> tail r) rs))

mem :: (Eq t) => [t] -> t -> Bool
mem [] _ = False
mem (x:xs) a
          | a == x = True
          |otherwise = mem xs a

hasColumn :: (Num t,Eq t) => [[t]] -> Bool
hasColumn [] = False
hasColumn m = any works columns
              where columns = transpose m
                    works [] = True
                    works xs = all (\r -> (all (\x -> mem r x) xs)) m

mapUns :: (Num t,Enum t,Eq t) => [t] ->  [(t -> t)] -> [[t]]
mapUns [] _ = []
mapUns x [] = []
mapUns xs uns = (map (head uns) xs) : mapUns xs (tail uns)

combine :: (Num t,Enum t,Eq t) => (t->t->t) -> (t->t->t) -> (t->t) -> (t -> t)
combine f g h  = (\x -> f x (g x (h x)) )


check ::(Num t,Enum t,Eq t) =>  t -> t -> [(t -> t)] -> [(t -> t -> t)] -> Bool
check a b [] _ = False
check a b _ [] = False
check a b uns bins 
             | length list == 0 = False
             | otherwise = True
             where list = [(f,g)| f <- bins, g <- bins ,  (all (\x -> (any (\h -> (h x) == (combine f g h) x) uns)) [a..b])]

	


addEndHour :: [ (String,Integer, Integer)] -> [(String,Integer,Integer,Integer)]
addEndHour [] = []
addEndHour ((n,s,m):xs) = (n,s,m,(s+(quot m 60))) : addEndHour xs   

intervalsCount :: [(String,Integer,Integer,Integer)] -> [(Integer,[(String,Integer,Integer,Integer)])]
intervalsCount [] = []
intervalsCount shows = removeEmpty (map (\h -> (h, (filter (\(n,s,m,e) -> h>=s&&h<(e+1)) shows) )) [0..23])

removeEmpty :: [(Integer,[(String,Integer,Integer,Integer)])] -> [(Integer,[(String,Integer,Integer,Integer)])]
removeEmpty [] = []
removeEmpty ((s,sh):rest) = if (length sh == 0) then (removeEmpty rest) else ((s,sh) :(removeEmpty rest)) 

findOverlapping :: [(String,Integer,Integer,Integer)] -> Integer
findOverlapping [] = 0
findOverlapping shows = minimum getMins 
                      where getMins = map (\(n,s,m,o) -> o) shows


addOverlapping :: [(Integer,[(String,Integer,Integer,Integer)])] -> [(Integer,[(String,Integer,Integer,Integer)], Integer)]
addOverlapping [] = []
addOverlapping is = added
	                where fixedShows = map (\(i,shows) -> (i, (map (\(n,s,m,e) -> (n,s,m, (m-(i-s)*60 ) ) ) shows))) is
	                      added = map (\(i,shows) -> (i, shows, (findOverlapping shows))) fixedShows
--getInterval :: [(String,Integer,Integer,Integer)] -> (Integer, Integer)
--getInterval [] = (0,0)     

sortIntervals ::  [(Integer,[(String,Integer,Integer,Integer)], Integer)] -> [(Integer,[(String,Integer,Integer,Integer)], Integer)]         
sortIntervals [] = []
sortIntervals ((ii,sshows,mm):xs) = (sortIntervals ge) ++ [(ii,sshows,mm)] ++ (sortIntervals less)
            where less = filter (\(i,shows,m) -> (length shows) < (length sshows)) xs
                  ge = filter  (\(i,shows,m) -> (length shows) >= (length sshows)) xs

showtime ::[ (String,Integer, Integer)] -> ((Integer, Integer), [String])
showtime [] = ((0,0),[])
showtime shows = ((getI answer, getTime answer), getShows answer)
	             where addedEnd = addEndHour shows
	                   intervals = intervalsCount addedEnd
	                   extendIntervals = addOverlapping intervals
	                   answerList =filter (\(i,shows,m) -> m >=60) (sortIntervals extendIntervals)
	                   answer = head answerList
	                   getShows (x,shows,m) = map (\(n,s,e,p) -> n) shows
	                   getTime (x,shows,m) = m
	                   getI (x,shows,m) = x

