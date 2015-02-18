import Data.List

unfold p h t x | p x = []
			   | otherwise = h x : unfold p h t (t x)


rmd [] = []
rmd (a:as) = let rest = rmd as
			 in if a `elem` as then rest else a:rest

sul ys = all (`elem` ys)

app xs ys = zipWith ($) xs ys

testApp = take 4 $ app (map (*) [1,2..]) [1,3..]

twasf p = foldr clip [] where
	clip x xs | p x = x : xs
			  | otherwise = []

enc [] = []
enc (x:xs) = (length $ x : takeWhile (==x) xs, x)
				: enc (dropWhile (==x) xs)

whatFun2 p as = [a | a <- as, p a]

whatFun4 p = foldr (\x xs -> if p x then x:xs else []) [] 
