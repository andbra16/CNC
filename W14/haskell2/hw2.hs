import Data.List
import Control.Monad
import Data.Char
import Data.String

-- 8.10
palaIO = do putStrLn "Enter a string."
    	    pala <- getLine
    	    if (palaTest pala) 
	    	then putStrLn "That's a palindrome." 
	    	else putStrLn "Not a palindrome."

palaTest str = str == (reverse str)

-- 8.11
sumIO = do putStrLn "Enter an integer."
           int1 <- getLine
           putStrLn "Enter another integer."
           int2 <- getLine
           putStrLn ("The sum of the two integers is: " ++ show(sumStr int1 int2))

sumStr x y = (read x) + (read y)

-- 8.12
putNIO = do 
			putStrLn "Enter an integer."
			num <- getLine
			putStrLn "Enter a string."
			str <- getLine
			putNTimes (read num) str

--putNTimes :: Integer -> String -> IO ()
putNTimes int str
	| int > 1 = do
					putStrLn str
					putNTimes (int-1) str
	| otherwise = do return ()

--8.13
multiSumIO = do
				putStrLn "How many integers would you like to sum."
				n <- getLine
				ints <- forM [1 .. (read n)] (\a -> do
					putStrLn "Enter an integer to sum."
					int <- getLine
					return (read int))
				putStrLn ("The sum of the integers you entered are: " ++ show(sum ints))

-- less elegant version of above
multiSum2 = do
				putStrLn "How many integers would you like to sum."
				n <- getLine
				if (read n>0) then (sumAll n 0) else putStrLn "The sum is 0."

sumAll n num = do
					if (read n>0)
					   then do
								putStrLn "Enter an integer to sum."
						 		num2 <- getLine
								sumAll (show(read n-1)) (num+(read num2))
					   else putStrLn ("The sum of the integers you entered are: " ++ show(num)) 
	
-- 8.14
wc = do
		putStrLn "This program will run until you enter an empty line."
		putStrLn "Then it will output the number of lines, words, and characters you inputed."
		counter 0 0 0

counter lineC wordC charC = do
	putStrLn "Enter a line now."
	line <- getLine
	if (line == "") 
	   then do
				putStrLn ("Number of lines: " ++ show(lineC))
				putStrLn ("Number of words: " ++ show(wordC))
				putStrLn ("Number of characters: " ++ show(charC))
		else do
				let newWords = length (words line)
				let newChars = length line
				counter (lineC+1) (wordC+newWords) (charC+newChars)

-- 8.15
palinCheck = do
				putStrLn "Enter a sentence to see if it is a palindrome."
				pala <- getLine
				--remove whitespace and capitals
				let newPala = filter (/=' ') (map toLower pala)
				--remove punctuation
				let fixPala = filter isLetter newPala
				if (fixPala == (reverse fixPala))
					then putStrLn "It is a palindrome."
					else putStrLn "Not a palindrome."

-- 8.16
palinCheck2 = do
				putStrLn "This program will test lines for palindromes until an empty line is inputed."
				palaTester

palaTester = do
				putStrLn "Enter a line now."
				pala <- getLine
				if (pala == "")
					then (return ())
					else do
							let newPala = filter (/=' ') (map toLower pala)
							let fixPala = filter isLetter newPala
							if (fixPala == (reverse fixPala))
								then do
										putStrLn "It is a palindrome."
										palaTester
								else do
										putStrLn "Not a palindrome."
										palaTester

-- 8.17
sumTester = do
				putStrLn "This program sums integers until a zero is inputed."
				putStrLn "Enter an integer now."
				num <- getLine
				let sum = (read num :: Integer)
				summer num sum

summer num sum = do
					if (read num == 0)
						then putStrLn ("The sum is: " ++ show(sum))
						else do
								putStrLn "Enter another integer."
								num2 <- getLine
								summer num2 ((sum)+(read num2))

-- 8.18
sortInt = do
			putStrLn "This program takes integers until a 0 is inputed, and then sorts them"
			putStrLn "Input an integer now."
			num <- getLine
			let x = [] :: [Integer]
			sorter num x

sorter num list1 = do
					if (read num == 0)
						then do
								putStrLn ("Sorted integers are: " ++ (show(sort list1))) 
						else do
								putStrLn "Input another integer."
								num2 <- getLine
								sorter num2 ((read num) : list1)

-- 8.19
copy = do
		line <- getLine
		let whileCopy = do
							if (line == "")
								then (return ())
								else do
										putStrLn line
										line <- getLine
										whileCopy
		whileCopy

-- The copy program takes a line that you first input. Then outputs that 
-- line. Then it repeatedly grabs that line and prints that line, forever.
