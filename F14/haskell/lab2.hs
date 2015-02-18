-- 4.3

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
	| (x == y) && (x == z) = 3
	| (x == y) || (y == z) = 2
	| otherwise = 0

-- 4.9

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z = (maxInt, times)
	where 
		maxInt = max (max x y) z
		times
			| (x == y) && (x == z) = 3
			| (x == y) || (y == z) = 2
			| otherwise = 1

-- 4.10

-- *Main> maxThreeOccurs 4 5 5
-- (5,2)

-- *Main> maxThreeOccurs 4 5 4
-- (5,1)

-- Tim Sheard's Assignment 1

-- 1) 3 = Num, even = Integral, even 3 = (Integral Num) -> Bool
-- You determine even 3 by combining the types of even and 3.

-- head = a, [1,2,3] = [Num], head [1,2,3] = (Num t) -> t

-- 2)

-- a)
testFloat :: Float -> Float
testFloat x = x + 1.0

floatFun :: (Float -> Float) -> Float
floatFun f = f 0.0 
    
-- b)
aFloat :: Float -> (Float -> Float)
aFloat x y = x+y 

-- c) 
cFloat :: (Float -> Float) -> (Float -> Float)
cFloat function x = (function x) + 8.5

-- 3
strlen :: String -> Int
strlen [ ] = 0
strlen (x:xs) = 1 + strlen xs

-- 4
fact :: Integer -> Integer
fact x 
	| x == 0 = 1
	| otherwise =  x * fact (x-1)

-- 5
ncopies :: Integer -> a -> [a] 
ncopies x y 
	| x == 0 = [ ]
	| otherwise = y : ncopies (x-1) y

-- 6
power :: Integer -> Integer -> Integer
power x y 
	| y == 0 = 1
	| otherwise = x * power x (y-1)

--7

  


