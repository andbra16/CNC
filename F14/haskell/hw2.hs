{- I completed most of the problems. 4.32 gave me trouble. I have done all the readings up to Ch. 4. -}

import Data.Char

-- 4.17

rangeProduct :: (Ord a, Num a, Enum a) => a -> a -> a
rangeProduct m n 
	| n < m = 0
	| otherwise = product[m..n]

-- 4.18

fac n  
	| n == 0 = 1
	| otherwise = rangeProduct 1 n

-- 4.19

mult n 0 = 0
mult n 1 = n
mult n m = n + (mult n (m-1))

-- 4.20

intSqRt :: Int -> Int
intSqRt n = test n
	where
		test x
			| x*x > n = test (x-1)
			| otherwise = x

-- 4.21

f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

fTest :: (Integer -> Integer) -> Integer -> Integer
fTest f n = maximum (map f [0..n])

-- 4.31
hComFac x 0 = x
hComFac x y = hComFac y (x `mod` y)

-- 4.32
{-power 0 = 1
power n
		| n % 2 == 0 = 2 ** (2*m) * power (n-1)
		| otherwise = 2

-}
--raise n 
--	| n`mod`2 == 0 = 

-- 5.5, 5.7, 5.9

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float
			deriving (Eq,Ord,Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

area :: Shape -> Float
area (Circle r) = pi*2*r
area (Rectangle h w) = h*w
area (Triangle s1 s2 s3) = 1/2 * s1 *s2

perimeter :: Shape -> Float
perimeter (Circle r) = pi*2*r
perimeter (Rectangle h w) = (2*h) + (2*w)
perimeter (Triangle s1 s2 s3) = s1 + s2 + s3

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


