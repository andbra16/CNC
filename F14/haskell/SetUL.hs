module SetUL (Set, null, member, empty, fromList, toList, insert, delete)
	where

import Prelude hiding (null)

newtype Set a = Set [a]

instance (Eq a) => Eq (Set a) where
	Set a == Set b = (setEqual a b)

setEqual [] [] = True
setEqual (xs) (ys) = ((all (`elem` ys) xs) && (all (`elem` xs) ys))
	
instance (Ord a, Eq a, Show a) => Show (Set a) where
	show (Set a) = "fromList " ++ show (rmd a)


-- Set functions
null :: Set a -> Bool
null (Set []) = True
null _ = False 

member :: Ord a => a -> Set a -> Bool
member a (Set xs) = elem a xs

empty :: Set a
empty = Set []	

fromList :: Ord a => [a] -> Set a
fromList x = Set x

toList :: Set a -> [a]
toList (Set x) = x

insert :: Ord a => a -> Set a -> Set a
insert x (Set ys)
	| (x `elem` ys) = Set ys 
	| otherwise =  Set (x:ys)

delete :: Ord a => a -> Set a -> Set a
delete x (Set ys) = Set [a | a <- ys, not (x==a)] 	

-- Subsidiary functions

rmd [] = []
rmd (x:xs)
    | x `elem` xs = rmd xs
    | otherwise = x : rmd xs
