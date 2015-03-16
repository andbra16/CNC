import Prelude hiding (Either(..))

-- 1
class Functor f => Pointed f where
	pure :: a -> f a

infixl 4 <*>
class Pointed f => Applicative f where
	(<*>) :: f (a -> b) -> f a -> f b

-- 2
data Either a b = Left a | Right b
	deriving (Show)

instance Functor (Either a) where
	fmap f (Left a) = Left a
	fmap f (Right b) = Right (f b)

instance Pointed (Either a) where
	pure = Right

instance Applicative (Either a) where
	(Right f) <*> something = fmap f something
	(Left x) <*> something = Left x

-- 3
instance Functor ((->) r) where
	fmap f g = (\x -> f (g x))

instance Pointed [] where
	pure x = [x]

instance Applicative [] where
	gs <*> xs = [g x | g <- gs, x <- xs]

-- 4

instance Pointed ((->) r) where
	pure x = (\y -> x)

instance Applicative ((->) r) where
	f <*> g = \x -> f x (g x)

-- Proofs

{-

1)
instance Functor [] where
	fmap _ [] = []
	fmap g (x:xs) = g x : g x : fmap g xs 

Show: fmap id (x:xs) = id (x:xs)

i) Left side of equation

fmap id (x:xs) 
	= id x : id x : fmap id xs
	= (x:x:xs)

ii) Right side of equation

id (x:xs)
	= (x:xs)

Contradiction, the left side does not equal the right side. Therefore, this
instance of functor does not obey the id functor LAW!!!

2)

instance Functor Maybe where
	fmap f Nothing = Nothing
	fmap f (Just a) = Just (f a)

Show: fmap id Nothing = id Nothing

i) Left side of the equation

fmap id Nothing
	= id Nothing
	= Nothing

ii) Right side of the equation

id Nothing
	= Nothing

Right side = Left side

Show: fmap id (Just a) = id (Just a)

i) Left side of the equation

fmap id (Just a)
	= Just (id a)
	= Just a

ii) Right side of the equation

id (Just a)
	= Just a

Left side = Right side

Show: fmap (g.h) Nothing = fmap g Nothing . fmap h Nothing

i) Left side of the equation

fmap (g.h) Nothing
	= fmap g Nothing . fmap h Nothing
	= g Nothing . h Nothing
	= Nothing . Nothing
	= Nothing

ii) Right side of equation

fmap g Nothing . fmap h Nothing
	= g Nothing . h Nothing
	= Nothing . Nothing
	= Nothing

Right side = Left side

Show: fmap (g.h) (Just a) = fmap g (Just a) . fmap h (Just a)

i) Left side

fmap (g.h) (Just a)
	= fmap g (Just a) . fmap h (Just a)
	= Just (g a) . Just (h a)

ii) Right side

fmap g (Just a) . fmap h (Just a)
	= Just (g a) . Just (h a)

Right side = Left side
	
-- 3

instance Pointed Maybe where
	pure x = Just x

Show: fmap g . pure Nothing = pure Nothing . g

i) Left side

fmap g . pure Nothing
	= Nothing . g

i) Right side

pure Nothing . g
	= Nothing . g

Show: fmap g . pure x = pure x . g

i) Left side

fmap g . pure x
	= Just x . g

ii) Right side

pure x . g
	= Just x . g

-- 4

Show: fmap g Nothing = pure g <*> Nothing

i) Left side

fmap g Nothing
	= Nothing

ii) Right side

 pure g <*> Nothing
	= Just g <*> Nothing
	= Nothing

Show: fmap g (Just x) = pure g <*> x  

i) Left side

fmap g (Just x)
	= Just (g x)

ii) Right side

pure g <*> x
	= Just g <*> x
	= Just (g x)

-- 5

Show: fmap id ((,)a) = id ((,)a)

i) Left side

fmap id ((,)a)
	= ((,) id a)
	= ((,) a)

ii) Right side

id ((,)a)
	= ((,)a)

Show: fmap (g.h) ((,)a) = fmap g ((,)a) . fmap h ((,)a)

i) Left side

fmap (g.h) ((,)a)
	= fmap g ((,)a) . fmap h ((,)a)
	= ((,)g a) . ((,)h a)

ii) Right side

fmap g ((,)a) . fmap h ((,)a)
	= ((,)g a) . ((,)h a)

-}
