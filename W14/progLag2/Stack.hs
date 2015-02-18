module Stack (Stack(..), isEmpty, push, pop, emptyS) where

newtype Stack a = Stack [a]
	deriving (Show)

isEmpty :: Stack a -> Bool
isEmpty (Stack x) = null x

push :: a -> Stack a -> Stack a
push x (Stack a) = Stack (x:a)

pop :: Stack a -> (a, Stack a)
pop (Stack []) = error "Stack is empty."
pop (Stack (x:xs)) = (x, Stack xs)

emptyS = Stack []
