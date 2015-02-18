module Main (main, codeMessage, decodeMessage, codes, codeTable) where

import Types (Tree(Leaf,Node), Bit(L,R), HCode, Table)
import Coding (codeMessage, decodeMessage)
import MakeCode (codes, codeTable)

-- Main expression: print the coded and decoded

main = print decoded

-- The example message to be coded.

message :: String
message = "there are green hills here"

-- The Huffman tree generate from the example.
treeEx :: Tree
treeEx = codes "there is a green hill"

-- The coding table generated from the example.
tableEx :: Table
tableEx = codeTable (codes "there is a green hill")

-- The example in code.
coded :: HCode
coded = codeMessage tableEx message

-- The example coded and then decoded
decoded :: String
decoded = decodeMessage treeEx coded
