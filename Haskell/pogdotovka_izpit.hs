

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci_iter :: Int -> Integer
fibonacci_iter n = iter 0 1 n
  where iter a b 0 = a
        iter a b n = iter b (a + b) (pred n)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

addVectors :: Num t => (t, t) -> (t, t) -> (t, t)
addVectors (a,b) (c,d) = (a + c, b + d)

compress :: Eq t => [t] -> [t]
compress [] = []
compress [x] = [x]
compress (x : xs) 
   | x == head xs = compress xs
   |otherwise = x : compress xs

duplicate :: [t] -> [t]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

cycle_ :: [t] -> [t]
cycle_ [] = []
cycle_ (x:xs) = x : cycle_ (xs ++ [x])

quickSort :: Ord t => [t] -> [t]
quickSort [] = []
quickSort (pivot : rest) = quickSort smallerEqual ++ [pivot] ++ quickSort larger
          where smallerEqual = filter (<=pivot) rest
                larger = filter (>pivot) rest

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort firstHalf `merge` mergeSort secondHalf
      where mid = length xs `quot` 2
            (firstHalf, secondHalf) = splitAt mid xs
            merge [] xs = xs
            merge xs [] = xs
            merge (x:xs) (y:ys)
               | x <= y = x : merge xs (y:ys)
               |otherwise = y : merge (x:xs) ys

zip_ :: [a] -> [b] -> [(a,b)]
zip_ [] _ = []
zip_ _ [] = []
zip_ (x:xs) (y:ys) = (x, y) : zip_ xs ys

unzip_ :: [(a,b)] -> ([a],[b])
unzip_ [] = ([],[])
unzip_ xs = (listA, listB)
                   where listA = map fst xs
                         listB = map snd xs

reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x:xs) = reverse_ xs ++ [x]

--reverse_iter : [t] -> [t]
--reverse_iter = foldl (flip (:)) []

iterate_ :: Num t => (t -> t) -> t -> [t]
iterate_ f x = [x] ++ iterate_ f (f x)

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a,b,c) | c <- [1..998],
                                b <- [1.. (999-c)],
                                let a = 1000 - b - c,
                                a^2 + b^2 == c^2]

pythagoreanTriplesCount :: Int
pythagoreanTriplesCount = length pythagoreanTriples

divides :: Integral t => t -> t -> Bool
divides _ 0 = error "0" 
divides n m = (m `mod` n) == 0

divisibleByEvery :: Integral t => t -> [t] -> Bool
x `divisibleByEvery` divs = all (`divides`x) divs

smallestMultiple :: Integral t => t -> t
smallestMultiple n = head [ x | x <- [n..], x `divisibleByEvery` [1..n]]