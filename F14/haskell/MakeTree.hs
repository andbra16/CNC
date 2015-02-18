module MakeTree (makeTree) where

import Types (Tree(Leaf,Node), Bit(L,R), HCode, Table)

-- make tree

makeTree :: [(Char,Int)] -> Tree
makeTree = makeCodes . toTreeList

-- auxillary functions

toTreeList :: [(Char,Int)] -> [Tree]
toTreeList = map (uncurry Leaf)

makeCodes :: [Tree] -> Tree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

insTree :: Tree -> [Tree] -> [Tree]
insTree t [] = [t]
insTree t (t1:ts)
	| (value t <= value t1) = t:t1:ts
	| otherwise = t1 : insTree t ts

pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (v1+v2) t1 t2
	where
		v1 = value t1
		v2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _) = n
