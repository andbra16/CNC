{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = fibgen 0 1 where
	fibgen a b = a : fibgen b (a+b)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs 

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons (f x) (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) (Cons y ys)  = Cons x (Cons y (sInterleave xs ys))

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x xs) = x : (sTake (n-1) xs)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 (fmap (+1) nats)

ruler :: Stream Integer
ruler = fmap f (sIterate (+1) 1)
	where f x 
		| odd x = 0
		| otherwise = 1 + f (x `div` 2)

-- Exercise 7 -----------------------------------------
rgen :: Int -> Int
rgen n = (n * 1103515245 + 12345) `rem` 2^31

-- | Implementation of C rand
rand :: Int -> Stream Int
rand x = sIterate rgen x

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
