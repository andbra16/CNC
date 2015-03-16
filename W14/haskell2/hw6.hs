import Prelude hiding (Maybe(..), Either(..))

-- Applicative definitions 

infixl 4 <*>
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- reader app

instance Functor ((->) env) where
  fmap = (.)

instance Applicative ((->) env) where
  pure g = \env -> g
  eg <*> ex = \env -> (eg env) (ex env)

-- maybe app
data Maybe a = Nothing | Just a
	deriving (Show)

instance Functor Maybe where
  fmap g Nothing  = Nothing
  fmap g (Just x) = Just (g x)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _  = Nothing
  (Just g) <*> x = fmap g x

-- either app
data Either a b = Left a | Right b
	deriving (Show)

instance Functor (Either a) where
	fmap f (Left a) = Left a
	fmap f (Right b) = Right (f b)

instance Applicative (Either a) where
	pure = Right
	(Right f) <*> something = fmap f something
	(Left x) <*> something = Left x

-- 1) Define and test your own Maybe Monad instance. You'll need to hide Maybe(..) imported with the Prelude.

instance Monad Maybe where
	return = Just
	(Just x) >>= f = f x
	Nothing >>= _ = Nothing
	fail _ = Nothing

m1 = return "WHAT" :: Maybe String
m2 = Just 9 >>= \x -> return (x*10)
m3 = Nothing >>= \x -> return (x*10)

-- 2) Define and test your own (Either e) Monad instance. Again, you'll need to hide Either(..) imported with the Prelude.

instance Monad (Either a) where
	return = Right
	(Right x) >>= f = f x
	(Left x) >>= _ = Left x

e1 = Right 9 >>= \x -> return (x+1)
e2 = Left 2 >>= \x -> return (x+1)

-- 3) Define your own ((->)r) Reader Monad instance. You may need to wrap this in a newtype like you did for the ((->)r) Applicative Reader.

{-
newtype Reader r a = Reader {
	runReader :: r -> a
}

instance Monad (Reader r) where
	return a = Reader $ \_ -> a
	m >>= k = Reader $ \r -> runReader (k (runReader m r)) r
-}

instance Monad ((->) r) where
	return x = \_ -> x
	h >>= f = \w -> f (h w) w

-- 4) Give your own implementations for the following Monad Utility functions (listed in TCOP p34). Remember, you have fmap, bind (>>=), and return to use. You can also use the "do" notation to do these problems. It's a good idea learning idea to try coding both ways: using do and using bind (>>=).

myap :: (Monad m) => m (a -> b) -> m a -> m b
myap mf m = do {f <- mf; x <- m; return (f x)}

mysequence :: Monad m => [m a] -> m [a]
mysequence [] = return []
mysequence (x:xs) = do {v <- x; vs <- mysequence xs; return (v:vs)}

mymapM :: Monad m => (a -> m b) -> [a] -> m [b]
mymapM f as = mysequence (map f as)

fish :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
fish f g = \x -> f x >>= g

myjoin :: (Monad m) => m (m a) -> m a
myjoin x = x >>= id

-- 5) Give your own definition for a JoinMonad class based on the "join" operator rather than the "bind" operator. (See TCOP p32-33).

class Applicative m => JoinMonad m where
 	join :: m (m a) -> m a

-- 6) Define and test your own Maybe JoinMonad instance.

instance JoinMonad Maybe where
	join (Just x) =  x
	join Nothing = Nothing

jm1 = join (Just (Just 1))
jm2 = join (Just Nothing)
jm3 = join (Nothing)

-- 7) Define and test your own (Either e) JoinMonad instance.

instance JoinMonad (Either e) where
	join (Right x) = x
	join (Left x) = Left x 

je1 = join (Right (Right 1))
je2 = join (Left (Right 2))
je3 = join (Right (Left 3))

-- 8) Define and test your own ((->)r) Reader JoinMonad instance.

instance JoinMonad ((->) r) where
	join xss = (\k -> xss k k)

-- 9) Extend your implementation of your JoinMonad class with the following utility functions Remember, you have fmap and pure to use as well as join.

ap :: JoinMonad m => m (a -> b) -> m a -> m b
ap mf m = mf `bind` \f ->
		  m `bind` \x ->
	      pure (f x)

seq1 :: JoinMonad m => [m a] -> m [a]
seq1 [] = pure []
seq1 (x:xs) = x `bind` \v-> seq1 xs `bind` \vs -> pure (v:vs)

mapM :: JoinMonad m => (a -> m b) -> [a] -> m [b]
mapM f as = seq1 (map f as)

fish2 :: JoinMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
fish2 f g = \x -> f x `bind` g

bind :: JoinMonad m =>  m a -> (a -> m b) -> m b
bind m f = join (fmap f m)

-- 10) Give your own definition for a FishMonad class based on the "fish" operator rather than the "bind" operator. (See TCOP p32-33).

class Applicative m => FishMonad m where
    fishy :: (a -> m b) -> (b -> m c) -> a -> m c

-- 11) Define and test your own Maybe FishMonad instance.

instance FishMonad Maybe where
	fishy f g = \x -> case (f x) of
						Nothing -> Nothing
						(Just y) -> g y 

fm1 = fishy (\x -> Just (x*2)) (\x -> Just (x+2)) 3
fm2 = fishy (\x -> Just (x+1)) (\x -> Nothing) 3
fm3 = fishy (\x -> Nothing) (\x -> Just (x+1)) 3

-- 12) Define and test your own (Either e) FishMonad instance.

instance FishMonad (Either e) where
	fishy f g = \x -> case (f x) of
						Left y -> Left y
						Right y -> g y

--instance FishMonad (Either e) where
--    fishy f g = \x -> case (f x) of
--                        Left y -> Left y
--                        Right y -> case (g y) of
--                                        Left z -> Right y
--                                        Right z -> Right z


fe1 = fishy (\x -> Right (x*2)) (\x -> Right (x+2)) 3
fe2 = fishy (\x -> Right (x*2)) (\x -> Left (x+2)) 3
fe3 = fishy (\x -> Left (x*2)) (\x -> Right (x+2)) 3

-- 13) Define and test your own ((->)r) Reader FishMonad instance.

--instance FishMonad ((->) r) where

-- 14) Extend your implementation of your FishMonad with the following utility functions.

bind' :: FishMonad m => m a -> (a -> m b) -> m b
bind' m f = fishy (\x -> m) f id

join' :: FishMonad m => (m (m a) -> m a)
join' x = x `bind'` id
