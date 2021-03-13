pythagoreanTriples :: [(Int,Int,Int)]
pythagoreanTriples = [(x,y,z) | z <- [1..998],
                                y <- [1..(999 - z)],
                                let x = 1000 - y - z,
                                x*x + y*y == z*z]        

divides :: Integral t => t -> t -> Bool
x `divides` y = y `mod` x == 0

divisibleBy :: Integral t => t -> [t] -> Bool
x `divisibleBy` xs = all (`divides`x) xs
                                            
smallestMultiple :: Integral t => t -> t
smallestMultiple n = head [x | x <- [n..], x `divisibleBy` [1..n]]

isPrime :: Integral t => t -> Bool
isPrime n = all (not . (`divides` n)) [2.. (floor  (sqrt  (fromIntegral  n)))]

primes :: Integral t => [t]
primes = [x | x <- [2..], isPrime x]

primesLowerThan :: Integral t => t -> [t]
primesLowerThan n = [x | x <- [2 .. n], isPrime x]

sumPrimesLowerThan :: Integral t => t -> t
sumPrimesLowerThan = sum . primesLowerThan 
                      -- where sum [] = 0
                        --     sum (x:xs) = x + sum xs

primesE :: Integral t => [t]
primesE = sieve [2..]
          where sieve (x:xs) = x : sieve [y | y <- xs, (not (x `divides` y))]


 --data Student = Student