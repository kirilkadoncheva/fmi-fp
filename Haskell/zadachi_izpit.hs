
generateExponents :: Int -> Int -> [Int]
generateExponents k l
           | k > l = generateExponents l k
           |
               
              

find :: (Num t,Eq t) => [t] -> t -> Bool
find [] _ = False
find (x:xs) a
     | x == a = True
     | otherwise = find xs a

findValue :: (Num t,Eq t) => [[t]] -> (Bool,t)
findValue [] = (False,0)
findValue (l :ls)
    | length l == 0 = (False,0) 
    | (all (\list -> (find list (head l))) ls) == True = (True, (head l))
    | otherwise = findValue ((tail l) : ls )

getListFromValue :: (Num t,Eq t) => [[(t,t)]] -> t -> [t]
getListFromValue [] _ = []
getListFromValue ls x = map (\list -> fst (head (filter (\(a,b) -> b == x) list))) ls

makeList :: (Num t,Eq t) => [[t]] -> [(t -> t)] -> [[(t,t)]]
makeList [] _ = []
makeList _ [] = []
makeList (x:xs) (f:fs) = (map (\a -> (a, (f a))) x) : (makeList xs fs)

allEqual :: (Num t,Eq t) => [[t]] -> [(t -> t)] -> [t]
allEqual [] _ = []
allEqual _ [] = []
allEqual list fs 
         | found == False = []
         | otherwise = getListFromValue mappedList value
         where mappedList = makeList list fs
               fmappedList = map (\l -> map snd l) mappedList
               found = fst (findValue fmappedList)
               value = snd (findValue fmappedList) 

type Ingredient = (String, Int) 

type Drug = (String, [Ingredient])

equalIngredient :: Ingredient -> Ingredient -> Bool
equalIngredient (a, _) (b, _) = (a == b)

sameIngredients :: Drug -> Drug -> Bool
sameIngredients (_, []) _ = False
sameIngredients _ ( _ , []) = True
sameIngredients (n, l) (m, (x:xs))
            | length (filter (\a -> (equalIngredient x a)) l) == 0 = False
            | otherwise = sameIngredients (n, l) (m, xs)

sameProportions :: Drug -> Drug -> Bool

 

             
--sameIngredients :: Drug -> Drug -> Bool
--sameIngredients (Drug n (x:xs)) (Drug m (y:ys))
        -- | 


 
