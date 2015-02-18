module Queue (Queue(..), isEmptyQueue, enqueue, dequeue) where

newtype Queue a = Queue [a]
	deriving (Show)

isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue x) = null x

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs++[x])

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue []) = error "Queue is empty."
dequeue (Queue (x:xs)) = (x, Queue xs)
