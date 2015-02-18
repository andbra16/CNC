Q1) e(4), e(5), e(7) = 3
	deq = 3 (reversed list of 3 elements)
	deq = 0
	enq(8) = 1
	deq = 0
	deq = 1 (reversed list of 1 element)
	enq(9) = 1
	enq(10) = 1
	deq = 2 (reversed list of 2 elements)
	enq(11) = 1

Total cons operations: 13

Q2) max length path = 9

Q3) max length path = 5

Q4) Qualified types are typeclasses that may take on multiple types. 
We need them for certain operators and data structures that work on 
multiple types. The Set type can work on anything that is of the Ord typeclass.

empty :: Set a
member :: Ord a => a -> Set a -> Bool
insert :: Ord a => a -> Set a -> Set a

You wouldn't be able to insert a string in a set because strings are not of
the Ord type.

Q5)

Q6) reverse''' [1,2,3,4,5]
	reverse''' [2,3,4,5] ++ [1]
	reverse''' [3,4,5] ++ [2,1]
	reverse''' [4,5] ++ [3,2,1]
	reverse''' [5] ++ [4,3,2,1]
	reverse''' [] ++ [5,4,3,2,1]
	[] ++ [5,4,3,2,1]
	[5,4,3,2,1]

	reverse' [1,2,3,4,5]
	rev [] [1,2,3,4,5]
	rev [1] [2,3,4,5]
	rev [2,1] [3,4,5]
	rev [3,2,1] [4,5]
	rev [4,3,2,1] [5]
	rev [5,4,3,2,1] []
	[5,4,3,2,1]

	reverse'' [1,2,3,4,5]
 	foldl revOp [] [1,2,3,4,5]
	foldl revOp [1] [2,3,4,5]
	foldl revOp [2,1] [3,4,5]
	foldl revOp [3,2,1] [4,5]
	foldl revOp [4,3,2,1] [5]
	foldl revOp [5,4,3,2,1] []
	[5,4,3,2,1]

Q7)	
