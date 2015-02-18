module MakeCode (codes, codeTable) where

import Types
import Frequency
import MakeTree
import CodeTable 

-- tree from frequency

codes :: [Char] -> Tree
codes = makeTree . frequency
