generateExponents :: Int -> Int -> [Int]
generateExponents k l = makeUnique ge
                  where ge = [ f | f <- [1..], x <- [1..f], y <- [1..f],
                                     f == x^k * y^l ]


makeUnique :: [Int] -> [Int]
makeUnique [] = []
makeUnique (x : xs) = x : makeUnique (dropWhile (==x) xs)

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

type Ingredient = (String, Integer) 

type Drug = (String, [Ingredient])

equalIngredient :: Ingredient -> Ingredient -> Bool
equalIngredient (a, _) (b, _) = (a == b)

sortIngredients :: [Ingredient] -> [Ingredient]
sortIngredients [] = []
sortIngredients (x:xs) = (sortIngredients less) ++
                         [x] ++
                         (sortIngredients greaterEqual)
                         where less = filter (\i -> (fst i)< (fst x)) xs
                               greaterEqual = filter (\i -> (fst i) >= (fst x)) xs


sameIngredients :: Drug -> Drug -> Bool
sameIngredients (_, []) _ = False
sameIngredients _ ( _ , []) = True
sameIngredients (n, l) (m, ll)
            | length l /= length ll = False
            | sorted1 == sorted2 = True
            | otherwise = False
             where sorted1 = map fst (sortIngredients l)
                   sorted2 = map fst (sortIngredients ll)

mergeLists ::(Num t,Fractional t) => [Integer] -> [Integer] -> [t]
mergeLists _ [] = []
mergeLists [] _ = []
mergeLists l1 l2 = ((fromIntegral (head l1))/(fromIntegral(head l2))) : mergeLists (tail l1) (tail l2)

sameProportions :: [Ingredient] -> [Ingredient] -> Bool
sameProportions [] [] = True
sameProportions [] l = False
sameProportions l [] = False
sameProportions l1 l2 
            | (length (filter (\el -> 
            	                      el == (head mapped) ) mapped)) == (length l1) = True
            | otherwise = False
              where s1 = sortIngredients l1
                    s2 = sortIngredients l2
              	    mapped = mergeLists (map snd s1) (map snd s2)




isSubstitute :: Drug -> Drug -> Bool
isSubstitute a b 
              | not (sameIngredients a b) = False
              | otherwise = (sameProportions (snd a) (snd b))

containsIngredient :: Drug -> Ingredient -> Bool
containsIngredient (a,l) x 
                 | length (filter (\i -> fst i == fst x) l) == 0 = False
                 | otherwise = True


sameOrLessIngredients :: Drug -> Drug -> Bool
sameOrLessIngredients (a,l) (b,ll)
                      | (length (filter (\i -> (containsIngredient (a,l) i)) ll)) == length ll = True 
                      | otherwise  = False


                     
quantityIndex :: Drug -> Drug -> Integer
quantityIndex d1@(a,l) d2@(b,ll) 
               |  any (<0) mappedList = -1
               |  otherwise = foldr (+) 0 mappedList
                  where s1 = sortIngredients l 
                        s2 = sortIngredients ll
                        mappedList = zipWith (-) (map snd s1) (map snd s2)



l = [("A",[("p",6),("q",9)]),("B",[("p",2),("q",3)]),("C",[("p",3)])]

             
--sameIngredients :: Drug -> Drug -> Bool
--sameIngredients (Drug n (x:xs)) (Drug m (y:ys))
        -- | 


 
