import Data.List

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
	| x = and' xs
	| otherwise = False

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
	| x == True = True
	| otherwise = or' xs

encode xs = [(length x, head x) | x <- group xs]

-- 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)
 
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

-- 12
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

-- 13
--encodeDirect = 

