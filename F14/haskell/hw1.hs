-- Brandon Anderson
{- I've done a thorough reading of Chapters 1-3 in the CRFP book. Some of the problems I completed introduced me to guards, boolean operators, and calling functions within other functions. This assignment also familiarized me with reading Haskell error messages and defining/reading types for functions. -}

import Data.Set
import Char
import Test.QuickCheck

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m==n) && (n==p)

-- 3.9

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m/=n) && (m/=p) && (p/=n)

-- this function checks that m doesn't equal n or p and that p doesn't equal
-- n and gives a boolean answer for each of those check. Then the function
-- ands those booleans together to give a true answer if m,n, and p were
-- different numbers.

-- 3.11a

-- threeEqual (2+3) 5 (11 'div' 2)
-- threeEqual 5 5 (11 'div' 2)
-- threeEqual 5 5 5
-- True

-- 3.12

prop_threeDifferent :: Integer -> Integer -> Integer -> Bool
prop_threeDifferent m n p =
		((m/=n)&&(m/=p)&&(n/=m)&&(n/=p)&&(p/=m)&&(p/=n)) == threeDifferent m n p

-- 3.17

charToNum :: Char -> Int
charToNum x 
	| isDigit x == True = fromEnum x
	| otherwise = 0

-- 3.20

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral (x+y+z)/3

averageCheck :: Integer -> Integer -> Integer -> Integer -> Integer
averageCheck x' x y z
	| fromIntegral x' > averageThree x y z = 1
	| otherwise = 0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z = averageCheck x x y z + averageCheck y x y z + averageCheck z x y z

-- 3.21

-- prop_howManyAboveAverage :: Integer -> Integer -> Integer -> Bool
-- prop_howManyAboveAverage = 
	

-- 3.22

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c 
	| b**2 > 4.0*a*c = 2
	| b**2 < 4.0*a*c = 0
	| otherwise = 1




