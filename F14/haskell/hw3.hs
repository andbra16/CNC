-- I completed all the problems.


import Data.List
import Data.Char

-- 5.7
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

-- 5.15
-- [0, 0.1 .. 1] is the list of floats in the tenths postition up to 1. It uses floats, so some of the numbers are not exact tenths.

-- 5.16
-- The list [2,3] contains two items. The list [[2,3]] conatins one item. 
-- [[2,3]] :: (Num t) => [[t]] 

-- 5.17
-- [2 .. 2] = 2
-- [2,7 .. 4] = 2
-- [2,2 .. 2] = the infinite list of 2

-- 5.18
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = (2*x) : doubleAll xs 

-- 5.19
capitalize :: String -> String
capitalize xs = [checkCapitals ch | ch <- xs]

checkCapitals :: Char -> Char
checkCapitals ch
	| (65 <= ord ch - 32 && ord ch - 32 <= 90) = toEnum (ord ch - 32) :: Char
	| otherwise = ch

capitalizeLetters :: String -> String
capitalizeLetters [] = []
capitalizeLetters (x:xs) = filter (/=' ') (checkLetter x : capitalizeLetters xs)
	where
		checkLetter x 
			| isLetter x == False = ' '
			| isLower x == True = toEnum (ord x - 32) :: Char
			| otherwise = x

-- 5.20
divisors :: Integer -> [Integer]
divisors x
	| x <= 0 = []
	| otherwise = div x
		where
			div n
				| n == 0 = []
				| x `mod` n == 0 = n : div (n-1) 
				| otherwise = div (n-1)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x
	| x <= 0 = False
	| (length (divisors x)) > 2 = False
	| otherwise = True

-- 5.21
matches :: Integer -> [Integer] -> [Integer]
matches _ [] = []
matches num list 
	| head list == num = num : matches num (tail list)
	| otherwise = matches num (tail list)

elem' :: Integer -> [Integer] -> Bool
elem' num list
	| matches num list == [] = False
	| otherwise = True

-- 5.22
onSeperateLines :: [String] -> String
onSeperateLines [] = ""
onSeperateLines list = head list ++ "\n" ++ onSeperateLines (tail list)

-- 5.28
type Person = String
type Book = String

type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice", "Tintin"), ("Anna", "Little Women"), 
				("Alice", "Asterix"), ("Rory", "Tintin")]

books :: Database -> Person -> [Book]
books dBase findPerson =
	[book | (person,book) <- dBase , person==findPerson]

borrowers :: Database -> Book -> [Person]
borrowers dBase book 
	= [person | (person,theBook) <- dBase , theBook==book]

borrowed :: Database -> Book -> Bool
borrowed dBase book 
	| length (borrowers dBase book) > 0 = True
	| otherwise = False

numBorrowed :: Database -> Person -> Int
numBorrowed dBase person = length (books dBase person)

-- 4
groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN num list = take num list : (groupByN num (drop num list))

-- 5
--listTrim :: [a] -> [a] -> [a]
listTrim list1 [] = list1
listTrim [] list2 = []
listTrim xs (y:ys) = listTrim (delete y xs) ys

-- 6
listDiff list1 [] = list1
listDiff [] list2 = []
listDiff xs ys = [x | x <- xs, notElem x ys]
