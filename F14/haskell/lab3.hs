main = do
	putStrLn "The base?"
	base <- getLine
	putStrLn "The Height?"
	height <- getLine
	putStrLn ("The area of that triangle is " ++ (show ((read base :: Float) 
		* (read height :: Float) * 0.5)) ++ ".")

-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "Can't do last of an empty list."

-- 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt list i = list !! (i-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list 
	| list == (reverse list) = True
	| otherwise = False

-- 7

