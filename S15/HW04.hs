{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P[0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P(y:ys)) == (P(z:zs)) = (y:ys) == (z:zs)
	--(P[]) == (P(z:zs)) = False
	--(P(z:zs)) == (P[]) = False
 
-- Exercise 3 -----------------------------------------

getLast :: Num a => [a] -> a
getLast ys = ys !! (length ys-1)

getString :: Num a => Poly a -> String
getString (P[]) = ""
getString (P(ys))
	| length (ys) == 1 && (head ys) == 0 = "" 
	| length (ys) == 1 = show (head ys) 
	| length (ys) == 2 = checkX (getLast ys) ++ getString (P(init ys))
	| otherwise = checkX (getLast ys) ++ getString (P(init ys))
		where
			checkX y
				| y == 0 = ""
				| y == 1 && (length ys) == 2 = "x + " 
				| y == -1 && (length ys) == 2 = "-x + "
				| y == 1 && (length ys) > 2 = "x^" ++ 
								show (length ys-1) ++ " + "
				| y == -1 && (length ys) > 2 = "-x^" ++ 
								show (length ys-1) ++ " + "
				| otherwise = show y ++ "x^" ++ show (length ys-1) ++ " + "


instance (Num a, Eq a, Show a) => Show (Poly a) where
	show p = getString p

-- Exercise 4 -----------------------------------------

getLastList :: Num a => [a] -> [a]
getLastList ys = [ys !! (length ys-1)]

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P[]) p2 = p2
plus p1 (P[]) = p1
plus (P(y:ys)) (P(z:zs))
	| length (y:ys) == length (z:zs) = P(zipWith (+) (y:ys) (z:zs))
	| length (y:ys) > length (z:zs) =  P(zipWith (+) (y:ys) (z:zs) ++ 
									[getLast (y:ys)])
	| otherwise =  P(zipWith (+) (y:ys) (z:zs) ++ [getLast (z:zs)])

-- Exercise 5 -----------------------------------------

mStep :: Num a => a -> Poly a -> Poly a
mStep y (P(ys)) = P $ map (*y) ys

mSpecial (P(ys)) = P(0:ys)

times :: Num a => Poly a -> Poly a -> Poly a
times (P[]) _ = P[]
times (P (y:ys)) p2 = plus (mStep y p2) (times (P(ys)) (mSpecial p2))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P(ys)) = P $ map (\q -> -q) ys
    fromInteger y = P[fromInteger y]
    -- No meaningful definitions exist
    abs = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P[]) _ = 0
applyP (P(y:ys)) z = y + z*(applyP (P(ys)) z)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

