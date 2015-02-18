import Stack
import Queue

makeStack s = makeStack' s (Stack [])
	where
		makeStack' [] stack = stack
		makeStack' (x:xs) stack = makeStack' xs (push x stack)

makeQueue s = Queue s


isPala s = testPala (makeStack s) (makeQueue s)
	where
	testPala (Stack []) (Queue []) = True 
	testPala stack queue
		| fst(pop stack) == fst(dequeue queue) = testPala (snd(pop stack)) (snd(dequeue queue))
		| otherwise = False 
