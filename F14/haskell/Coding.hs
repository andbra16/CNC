-- Coding.hs
--
-- Provides the methods for coding and decoding a Huffman Tree

module Coding (codeMessage, decodeMessage) where

import Types (Tree(Leaf,Node), Bit(L,R), HCode, Table)

-- functions for coding and decoding

codeMessage :: Table -> [Char] -> HCode
codeMessage tbl = concat.map(lookupTable tbl)

decodeMessage :: Tree -> HCode -> [Char]
decodeMessage tr = decodeByt tr
	where
		decodeByt (Node n t1 t2) (L:rest) = decodeByt t1 rest
		decodeByt (Node n t1 t2) (R:rest) = decodeByt t2 rest
		decodeByt (Leaf c n) rest = c : decodeByt tr rest
		decodeByt t [] = []

-- auxillary functions

lookupTable :: Table -> Char -> HCode
lookupTable ((ch,n):tb) c 
	| ch==c = n
	| otherwise = lookupTable tb c
