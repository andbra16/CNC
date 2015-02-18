import Prelude hiding (Maybe(..))

-- Functor Programming

-- 1
mapTuple f (a,b) = (a, f b)

-- 2
instance Functor ((,)b) where
	fmap = mapTuple

-- 3
mapLam f g = (\x -> f (g x))

-- 4
instance Functor ((->) r) where
	fmap = mapLam

-- 5
newtype MyIO a = MyIO (IO a)
	
-- 6
mapIO f (MyIO action) = MyIO resultAction where
	resultAction = do
					result <- action
					return (f result)

x = MyIO (return (3)) :: MyIO (Int)

-- 7
instance Functor MyIO where
	fmap = mapIO 

testAction = fmap (+1) (MyIO (return 2))

showlt (MyIO action) = do
						x <- action
						print x

test = showlt testAction

-- Pointed Programming

-- 1
class Functor f => Pointed f where
  pure  :: a -> f a
  

-- 2
instance Pointed ((->) r) where
	pure x = (\y -> x)

-- 3

data Maybe a = Nothing | Just a
	deriving (Show)

instance Functor Maybe where
	fmap f Nothing = Nothing
	fmap f (Just a) = Just (f a)

instance Pointed Maybe where
	pure x = Just x

-- 4

-- the purpose of the Pointed class is to be able to package something
-- into another datatype

-- 5

-- ((,)a) can't be made into the Pointed class because the type a is
-- arbitrary and you can't generate a value of type a out of nothing.

{-Here's a proof for binary trees that gives a detailed form for the
proof. The proof for GTrees differs from this proof in the need to handle
the list map of a function over the list of sub-GTrees of a node.
-}

data Tree a = NilT | Node a (Tree a) (Tree a) deriving Show
	
instance Functor Tree where
	fmap f NilT         = NilT
	fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- A simple test 


{-Prove the Functor Laws for the Tree functor (tf.1 and tf.2)
fmap id    = id                   (tf.1)
fmap (g.h) = fmap g . fmap h      (tf.2)

The proof is by induction because the Tree data type is not finite.

------------------------------
(a) Claim: (tf.1) fmap id = id
------------------------------
The proof is by structural induction over the height of the tree.
We need to show: fmap id t = id t for all binary trees t

BASE case: the tree of form NilT
Show: fmap id NilT = id NilT

(i) Left side of equation
fmap id NilT = NilT

(ii) Right side of equation
id NilT = NilT

So both sides of the equation are equal for the NilT tree.


INDUCTION STEP case: the tree of form (Node x l r)
Show: fmap id (Node x l r) = id (Node x l r)        for all subtrees l, r

(i) Left side of equation
fmap id (Node x l r) 
  = Node (id x) (fmap id l)  (fmap id r)    -- definition of fmap
  = Node (id x) l r                         -- structural induction hypothesis
  = Node x l r                              -- definition of id

(ii) Left side of equation
id (Node x l r)
  = Node x l r                              -- defintion of id

So again, both sides of the equation are equal for the tree of form (Node x l r)

By the principle of structural induction over binary trees
The claim: (tf.1) fmap id = id
  is True
QED proof of (tf.1)

----------------------------------------------
(b) Claim: (tf.2) fmap (g.h) = fmap g . fmap h
----------------------------------------------
Again, the proof is by structural induction over the height of the tree.
We need to show: fmap (g.h) t = (fmap g t . fmap h t) for all binary trees t

BASE case: the tree of form NilT
Show: fmap (g.h) NilT = fmap g NilT . fmap h NilT

(i) Left side of equation
fmap (g.h) NilT = fmap g NilT . fmap h NilT   (definition of composition)

(ii) Right side of equation
fmap g NilT . fmap h NilT = 

So both sides of the equation are equal for the NilT tree.


INDUCTION STEP case: the tree of form (Node x l r)
Show: fmap (g.h) (Node x l r) = fmap g (Node x l r) . fmap g (Node x l r)
  for all binar trees l,r

(i) Left side of equation
fmap (g.h) (Node x l r)
  = fmap g (Node x l r) . fmap h (Node x l r)
  = Node (g x) (fmap g l) (fmap g r) . Node (h x) (fmap h l) (fmap h r)

(ii) Right side of equation
fmap g (Node x l r) . fmap h (Node x l r)
  = Node (g x) (fmap g l) (fmap g r) . Node (h x) (fmap h l) (fmap h r)
  =
  =


So again, both sides of the equation are equal for the tree of form (Node x l r)

By the principle of structural induction over binary trees
The claim: (tf.2) fmap (g.h) = fmap g . fmap h
  is True
QED proof of (tf.2)
-}
