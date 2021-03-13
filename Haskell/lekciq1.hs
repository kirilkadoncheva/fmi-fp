fact 0 = 1
fact n = n * fact (n-1)

quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
 where less = filter (<=x) xs
       more = filter (>x) xs

x :: Int
x = 4
y :: Double
y=7.8
z :: String
z="Hello"

--square :: Int -> Int


hypothenuse :: Double -> Double -> Double
hypothenuse a b = sqrt (a**2 + b**2)

div50 :: Int -> Int
div50 = div 50

twice f x = f (f x)

diag f x = f x x
plus1 = (+) 1

square = diag (*)

positive = (>0)
lastDigit = (`mod` 10)

abss x = if x<0 then -x else x

fact1 n
	| n == 0 = 1
	| n > 0  = n * fact (n-1)
	| n < 0  = error "negative number"

fact2 n = let fact n = if n == 0 then 1
	                   else n * fact (n-1)
	      in (fact n)^2

sumLastDigits n = lastDigit n + lastDigit (stripDigit n)
 where lastDigit = (`mod` 10)
       stripDigit = (`div` 10)

quadratic a b c
	| a == 0 = "lin"
	| d > 0  = "2 resh"
	| d == 0 = "1 resh"
	| otherwise = "no"
	where d = b^2 - 4*a*c

area x1 y1 x2 y2 x3 y3 = 
	let a = dist x1 y1 x2 y2
	    b = dist x2 y2 x3 y3
	    c = dist x3 y3 x1 y1
	    p = (a + b + c) / 2
	in sqrt (p * (p - a) * (p-b) * (p - c))
	where dist u1 v1 u2 v2 = sqrt (du^2 + dv^2)	
		where du = u2 - u1
		      dv = v2 - v1

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

gcd1 0 0 = error "no gcd"
gcd1 x 0 = x
gcd1 0 y = y
gcd1 x y
	|x > y = gcd1 (x-y) y
	| otherwise = gcd1 x (y-x)

False && _ = False
_     && b = b


