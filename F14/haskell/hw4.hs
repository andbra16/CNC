-- I finished most of the problems. I had trouble with composeList, curryList, 
-- goup, and rewriting groupByN

import Data.List

-- 7.8
elemNum :: Integer -> [Integer] -> Integer
elemNum y [] = 0
elemNum y (x:xs) 
	| y == x = 1 + elemNum y xs
	| otherwise = elemNum y xs

elemNum' x = foldr (myFoldFun) 0
	where 
		myFoldFun y count 
					| x == y = count+1
					| otherwise = count

-- 7.9
unique :: [Integer] -> [Integer]
unique [] = []
unique xs = [x | x <- xs, elemNum x xs == 1]

unique' xs = foldr (test) []
	where
		test x acc
			| elemNum x xs == 1 = x:acc
			| otherwise = acc

-- 7.10 
-- ?

-- 10.9
iter 0 f x = x
iter n f x 
	| n > 0 = f (iter (n-1) f x)

testFun x = x+1

iter' f = unfoldr (\x -> Just (x, f x))

-- 11.17
curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 g x y z = g (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 g (x,y,z) = g x y z

multiply3 x y z = x*y*z
multiply3UC (x,y,z) = x*y*z

-- 11.18
--curryList :: ([a] -> d) -> (a -> [a] -> d)

--uncurryList :: (a -> [a] -> d) -> ([a] -> d)

-- 6
composeList [] = []
--composeList [f] = f
composeList (x:xs) = x (.) (composeList xs)

-- 7
--composeList' = foldr (.) []

-- 8
--groupByN n list = take (length list) (iterate(

-- 9
--group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs)
	| x == head xs = ([x,head xs] ++ [y]) : group' (tail xs)
	| otherwise = [x] : group' xs
		where 
			y 
				| head (tail xs) == head xs = head xs
				| otherwise = []

group'' [] = []
group'' [x] = [[x]]
group'' (x:xs) = (x:ys) : group'' zs
	where 
		(ys,zs) = span ((==) x) xs 

-- 10
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x:xs) = (x:ys) : groupBy eq zs
                     	where (ys,zs) = span (eq x) xs

-- 6.44
type Name = String
type Price = Int
type BarCode = Int

type Database = [(BarCode,Name,Price)]

codeIndex :: Database
codeIndex = [(4719, "Fish Fingers", 121), (5643, "Nappies", 1010), 
			 (3841, "Orange Jelly", 56), (1111, "Hula Hoops", 21),
			 (1112, "Hula Hoops (Giant)", 133), 
			 (1234, "Dry Sherry, 1lt", 540)]

type TillType = [BarCode]
type BillType = [(Name,Price)]

bill :: BillType
bill = [("Dry Sherry, 1lt",540),("Fish Fingers",121),("Orange Jelly",56),
		("Hula Hoops (Giant)",133),("Unkown Item",0),("Dry Sherry, 1lt",540)]

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence x = show y ++ "." ++ show z
	where 
		y = (x `div` 100)
 		z = (x `mod` 100)

formatLine :: (Name,Price) -> String
formatLine (x,y) = x ++ (replicate l '.') ++ (formatPence y) ++ "\n"
	where
		l = lineLength - ((length x) + (length (formatPence y)))

formatLines :: [(Name,Price)] -> String
formatLines [x] = formatLine x
formatLines (x:xs) = formatLine x ++ formatLines xs

getPrice :: (Name,Price) -> Price
getPrice (x,y) = y

makeTotal :: BillType -> Price
makeTotal [(x,y)] = y
makeTotal (x:xs) = getPrice x + makeTotal xs

formatTotal :: Price -> String
formatTotal x = "\n" ++ "Total" ++ (replicate l '.') ++ (formatPence x)
	where
		l = lineLength - (5 + (length (formatPence x)))

formatBill :: BillType -> String
formatBill xs = formatLines xs ++ formatTotal y
	where 
		y = makeTotal xs

