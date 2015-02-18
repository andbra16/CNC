-- Types.hs
--
-- The types used in the Huffman coding example.

module Types (Tree(Leaf,Node), Bit(L,R), HCode, Table) where

-- Trees to represent the relative frequencies of characters
-- and therefore the Huffman codes.

data Tree = Leaf Char Int | Node Int Tree Tree
	deriving (Eq, Show)

-- The types of bits, Huffman codes and tables of Huffman codes.

data Bit = L | R 
	deriving (Eq, Show)

type HCode = [Bit]

type Table = [(Char, HCode)]
