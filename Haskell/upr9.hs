fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci1 :: Int -> Integer
fibonacci1 n = iter 0 1 n
 where iter x y 0 = x
       iter x y z = iter y (x + y) (z - 1)
       
fibonacci2 :: Int -> Integer
fibonacci2 n = let iter x y 0 = x
                   iter x y z = iter y (x + y) (z - 1) 
               in iter 0 1 n    

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci3 :: Int -> Integer
fibonacci3 = (fibs!!)

add :: Int -> (Int -> Int)
add a b = a + b

add5 :: Int -> Int
add5 = add 5

addVectors :: Num t => (t, t) -> (t, t) -> (t, t) 
addVectors v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)

-- addVectors (a,b) (c,d) = (a+b, c+d)

compress ::Eq t => [t] -> [t]
compress [] = []
compress [x] = [x]
compress (x:y:xs) 
 | x == y = compress (y : xs)
 | otherwise = x : compress(y : xs)

compress1 :: Eq t => [t] -> [t]
compress1 [] = []
compress1 (x:xs) = x : compress1 (dropWhile (\y -> x == y) xs)


duplicate :: [t]->[t]
duplicate [] = []
duplicate [x] = x:[x]
duplicate (x:xs) = x : x : duplicate xs

cycle1 :: [t] -> [t]
cycle1 [] = []
cycle1 xs = iter xs
 where iter [] = iter xs
       iter (x:xs) = x : iter xs

cycle2 :: [t] -> [t]
cycle2 [] = []
cycle2 (x:xs) = x : cycle2 (xs ++ [x])

quickSort ::Ord t => [t] -> [t]
quickSort [] = []
quickSort (pivot : rest) = quickSort lessThanOrEqual ++ [pivot] ++ quickSort greaterThan
   where lessThanOrEqual = filter (<= pivot) rest
         greaterThan = filter (> pivot) rest

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
 where (firstHalf, secondHalf) = splitAt (quot (length xs) 2) xs
       merge l [] = l
       merge [] l = l
       merge (x : xs) (y : ys) 
         | x <= y = x : merge xs (y : ys)
         | otherwise = y : merge (x : xs) ys

zip1 :: [t1] -> [t2] -> [(t1,t2)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x : xs) (y : ys) = (x , y) : zip1 xs ys

unzip1 :: [(t1,t2)] -> ([t1], [t2])
unzip1 xs = (map (fst) xs, map (snd) xs)