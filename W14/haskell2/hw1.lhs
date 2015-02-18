Haskell Winter 2015
Homework 1
------------------------------------------------------------------------------

Fill in the requested code in this file, test your mapping functions, and
cnc_submit hw1.lhs for your homework.



> import Prelude hiding (Maybe(..), Either(..))

1.  Use the following Error data type for this problem.

> data Maybe a = Nothing | Just a
> 	deriving (Show)

  (a) Give the type and Haskell code for an appropriate map function over
      the (Maybe a) type.

> mapMaybe :: (a -> b) -> Maybe a -> Maybe b 

> mapMaybe f Nothing = Nothing
> mapMaybe f (Just a) = Just (f a)

  (b) Declare the Maybe type constructor as an instance of the Functor class.
  
> instance Functor Maybe where
>	fmap = mapMaybe

2.  Use the following Error data type for this problem.

> data Error a = Ok a | Error String
>	deriving (Show)

  (a) Give the type and Haskell code for an appropriate map function over
      the (Error a) type.

> mapError :: (a -> b) -> Error a -> Error b

> mapError f (Ok a) = Ok (f a) 
> mapError f (Error a) = Error a

  (b) Declare the Error type constructor as an instance of the Functor class.

> instance Functor Error where
>	fmap = mapError

3. Consider the following binary tree data type.

> data Tree a = Nil | Node a (Tree a) (Tree a)
> 	    deriving (Eq, Read,Show)

  (a) Give the type and function definition for mapT, the map function over the
      binary tree data type above

> mapT :: (a -> b) -> Tree a -> Tree b 

> mapT f Nil = Nil 
> mapT f (Node a t1 t2) = Node (f a) (mapT f t1) (mapT f t2) 

  (b) Declare the Tree type constructor as an instance of the Functor class.

> instance Functor Tree where
>	fmap = mapT

4. Consider the following General Tree data type.

> data GTree a = GNil | GNode a [GTree a]
> 	deriving (Show)


  (a) Give a type and a code definition for a mapGT function over GTree. Give
      the most general type.


> mapGT :: (a -> b) -> GTree a -> GTree b 

> mapGT f GNil = GNil 
> mapGT f (GNode a gts) = GNode (f a) [mapGT f gt | gt <- gts] 

  (b) Declare the Gtree type constructor as an instance of the Functor class.

> instance Functor GTree where
>	fmap = mapGT

5. Consider the (Either a b) data type.

> data Either a b = Left a | Right b
>	deriving (Show)

  (a) Give a type and a code definition for a mapEither function over the
      Either type. This is a bit tricky because the Either type constructor
      takes two parameters and the mapEither will only work on one of them.
      Recall that (Either a b) means ((Either a) b).

> --mapEitherL :: 

> mapEitherL f (Left a) = Left (f a)
> mapEitherL f (Right b) = Right b

> mapEitherR f (Left a) = Left a
> mapEitherR f (Right b) = Right (f b)


  (b) Declare the (Either a) type constructor as an instance of the Functor
      class.


> instance Functor (Either a) where
>	fmap = mapEitherR


-----------------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------------

> t1 = mapMaybe (+2) (Just 2)
> t2 = mapMaybe (+2) (Nothing)

> t3 = mapError (+2) (Ok 2)
> t4 = mapError (+2) (Error "blah blah blah")

> t5 = mapT (+2) (Node 2 (Node 3 Nil Nil) (Node 2 Nil Nil))

> t6 = mapGT (+2) (GNode 5 [(GNode 2 [GNil]), GNil])

> t7 = mapEitherL (+2) (Left 2)
> t8 = mapEitherL (+2) (Right True)
