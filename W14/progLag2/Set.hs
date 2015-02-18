data Set a = Empty | Set [a]
	deriving (Show,Eq)

member e Empty = False
member e (Set xs) = elem e xs

insert e (Set xs)
	| member e (Set xs) == True = Set xs
	| otherwise = Set (e:xs)

remove e Empty = Empty
remove e (Set xs) = Set (filter (==e) xs) 

