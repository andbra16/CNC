insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

insert x [] = [x]
insert x (y:ys)
	|  x <= y = x : y : ys 
	| otherwise = y : insert x ys

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = splitAt (length xs `quot` 2) xs
               in merge (mergesort as) (mergesort bs)

data Tree a = Nil | Node a (Tree a) (Tree a)
