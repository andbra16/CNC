module CodeTable (codeTable) where

import Types (Tree(Leaf,Node), Bit(L,R), HCode, Table)

-- code table

codeTable :: Tree -> Table
codeTable = convert []

-- auxillary functions

convert :: HCode -> Tree -> Table
convert cd (Leaf c n) = [(c,cd)]
convert cd (Node n t1 t2) = (convert (cd++[L]) t1) ++ (convert (cd++[R]) t2)


