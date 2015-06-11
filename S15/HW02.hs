{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches (x:xs) (y:ys) 
	| x == y = 1 + exactMatches xs ys
	| otherwise = 0 + exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map countColor colors
  where countColor c = length $ filter (== c) xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches [] _ = 0
matches (x:xs) ys 
	| x `elem` ys = 1 + matches xs ys
	| otherwise = 0 + matches xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess (exMatch) (inEx) where
	exMatch = exactMatches secret guess
	inEx = (matches secret guess) - exMatch

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move1@(Move xs _ _) ys 
	| move1 == move2 = True
	| otherwise = False
	where
		move2 = getMove ys xs 	

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter $ isConsistent m  

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = concatMap (\c -> map (c:) $ allCodes (n-1)) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = loop [initialMove] where 
	codeLen = length secret
	initialMove = getMove secret $ replicate codeLen Red
	isConsistentWithAll ms c = all (`isConsistent` c) ms
	nextMove ms = getMove secret $ head $ filter (isConsistentWithAll ms) $ allCodes codeLen
	loop ms@(Move _ e _ : _)
		| e == codeLen = ms
		| otherwise = loop (nextMove ms: ms)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
