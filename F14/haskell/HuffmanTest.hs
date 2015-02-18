import Types
import Coding
import Frequency
import MakeTree
import CodeTable
import MakeCode

-- test coding and decoding
tree1 = Node 0 (Leaf 'a' 0) (Node 0 (Leaf 'b' 0) (Leaf 't' 0))
mess1 = [R,L,L,R,R,R,R,L,R,R]
table1 = [('a',[L]),('b',[R,L]),('t',[R,R])]

dTest1 = decodeMessage tree1 mess1 == "battat"
--True

cTest1 = codeMessage table1 "battat" == [R,L,L,R,R,R,R,L,R,R]
--True

-- test frequency sort
testF = frequency "battat" == [('b',1),('a',2),('t',3)]
--True

-- test makeTree
testMT = makeTree [('b',1),('a',2),('t',3)] == 
			Node 6 (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) (Leaf 't' 3)
--True

-- test codeTable
testCT = codeTable (Node 6 (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) (Leaf 't' 3)) 
			== [('b', [L,L]),('a',[L,R]),('t',[R])]
--True

-- test makecode
testMC = codes "battat" == 
	Node 6 (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) (Leaf 't' 3)
--True

-- all tests
testList = [dTest1, cTest1, testF, testMT, testCT, testMC]
--[True,True,True,True,True,True]

test = and testList
--True

-- Bigger Test 

-- The example message to be coded.
message :: String
message = "there are green hills here"

--The Huffman tree generate from the example.
treeEx :: Tree
treeEx = codes "there is a green hill"

--The coding table generated from the example.
tableEx :: Table
tableEx = codeTable (codes "there is a green hill")

--The example in code.
coded :: HCode
coded = codeMessage tableEx message

--The example coded and then decoded
decoded :: String
decoded = decodeMessage treeEx coded

-- print the coded and decoded message
quickTest = print decoded
