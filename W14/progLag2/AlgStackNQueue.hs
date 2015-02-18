-- Stack

data Stack a = EmptyS | Stack a (Stack a)
	deriving (Show)

isEmpty :: Stack t -> Bool
isEmpty EmptyS = True
isEmpty _ = False

push :: t -> Stack t -> Stack t
push x xs = Stack x xs

pop :: Stack t -> (t, Stack t)
pop EmptyS = error "Stack is empty."
pop (Stack x EmptyS) = (x, EmptyS) 
pop (Stack x xs) = (x, xs)

-- Queue

data Queue a = EmptyQ | Queue a (Queue a) 
	deriving (Show)

isEmptyQueue :: Queue a -> Bool
isEmptyQueue EmptyQ = True
isEmptyQueue _ = False

enqueue :: a -> Queue a -> Queue a
enqueue x xs = Queue x xs

dequeue :: Queue a -> (a, Queue a)
dequeue EmptyQ = error "Queue is empty."
dequeue xs = (top xs, deq xs)
	where 
		top (Queue y EmptyQ) = y
		top (Queue y ys) = top ys
		deq (Queue y EmptyQ) = EmptyQ
		deq (Queue y (Queue ys EmptyQ)) = Queue y EmptyQ
		deq (Queue y ys) = Queue y (deq ys)
		

-- Palindrome test

makeStack s = makeStack' s (EmptyS)
	where
		makeStack' [] stack = stack
		makeStack' (x:xs) stack = makeStack' xs (push x stack)

makeQueue s = makeQueue' s (EmptyQ)
	where 
		makeQueue' [] queue = queue
		makeQueue' (x:xs) queue = makeQueue' xs (enqueue x queue)

isPala s = testPala (makeStack s) (makeQueue s)
    where
    testPala (EmptyS) (EmptyQ) = True
    testPala stack queue
        | fst(pop stack) == fst(dequeue queue) = testPala (snd(pop stack))      (snd(dequeue queue))
        | otherwise = False


