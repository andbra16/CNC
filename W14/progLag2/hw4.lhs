
Only used in the measurements:

>	class Monoid a where
>		id :: a
>		binop :: a -> a -> a

Reduce is a class that had a reducer and reducel similar to
a foldr and foldl:

>	class Reduce f where
>		reducer :: (a -> b -> b) -> (f a -> b -> b)
>		reducel :: (b -> a -> b) -> (b -> f a -> b)

Lists are instances of Reduce

>	instance Reduce [] where
>		reducer (f') x z = foldr f' z x
>		reducel (f'') x z = foldl f'' x z


*Main> reducel (\x -> \y -> y:x) [] [1,2,3,4]
[4,3,2,1]
*Main>


Once we have an instance of Reduce, we can use it to produce
a list:

>	toList :: (Reduce f) => f a -> [a]
>	toList s = s `cons'` [] where (cons') = reducer (:)

Here is a toList of a fingerTree. Since fingerTrees are instances
of Reduce, they have the functions reducer and reducel which
traverse the tree and apply the function. 

*Main> toList t1
[1,2,3,4,5,6,7,8]
*Main> toList [1,2,3,4]
[1,2,3,4]
*Main> toList (Node2 4 5)
[4,5]
*Main>


So now are some trees, with a 2 and 3 Node.


>	data Tree a = Zero a | Succ (Tree (Node a))
>		deriving Show
>	data Node a = Node2 a a | Node3 a a a
>		deriving Show

Now we have a FingerTree which keeps Digits at its leaves, 

>	data FingerTree a = Empty
>						| Single a
>						| Deep (Digit a) (FingerTree (Node a)) (Digit a)
>							deriving Show

Represent Digits as a list, for simplicity:

>	type Digit a = [a]

Nodes are instances of reduce, as an example I can reduce 

*Main> (reducer (&&) (Node2 False False)) False
False
*Main> (reducer (&&) (Node2 True True)) True
True
*Main> (reducer (||) (Node2 True True)) True
True
*Main> (reducer (||) (Node2 True True)) False
True
*Main> (reducer (||) (Node2 False False)) False
False
*Main>


>	instance Reduce Node where
>		reducer lt (Node2 a b) z = a `lt` (b `lt` z)
>		reducer lt (Node3 a b c) z = a `lt` (b `lt` (c `lt` z))
>		reducel gt z (Node2 b a) = (z `gt` b) `gt` a
>		reducel gt z (Node3 c b a) = ((z`gt`c) `gt` b) `gt` a

Similarly I can reduce a finger tree:
*Main> reducer (:) t1 []
[1,2,3,4,5,6,7,8]
*Main> t1
Deep [1,2,3,4] (Single (Node3 5 6 7)) [8]
*Main>

>	instance Reduce FingerTree where
>		reducer lt Empty z = z
>		reducer lt (Single x) z = x `lt` z
>		reducer lt (Deep pr m sf) z = (reducer lt pr  ((reducer (reducer lt)) m  (reducer lt sf  z)))
>		reducel gt z Empty = z
>		reducel gt z (Single x) = z `gt` x
>		reducel gt z (Deep pr m sf) = (reducel gt (reducel (reducel gt) (reducel gt z pr) m) sf)



Deque operations (O(1))

First adding a new element on to the left of a sequence unless the
initial buffer already has 4 elements ... in that case we need to split.

>	insr :: a -> FingerTree a -> FingerTree a
>	insr a  Empty	= Single a
>	insr a (Single b) = Deep [a] Empty [b]
>	insr a (Deep [b,c,d,e] m sf) = Deep [a,b] (insr (Node3 c d e) m) sf
>	insr a (Deep pr m sf) = Deep ([a] ++ pr) m sf

>	insl :: FingerTree a -> a  -> FingerTree a
>	insl Empty a	= Single a
>	insl (Single b) a = Deep [b] Empty [a]
>	insl (Deep pr m [e,d,c,b] ) a = Deep pr (insl m (Node3 e d c))  [b,a]
>	insl (Deep pr m sf) a = Deep pr m (sf ++ [a])

I can lift insr and insl to make finger trees from Reduce instances:


*Main> liftinsr [1,2,3,4] Empty
Deep [1,2,3] Empty [4]
*Main> liftinsr [1,2,3,4,5,6,7] Empty
Deep [1,2,3] (Single (Node3 4 5 6)) [7]
*Main>

This takes a list to a fingerTree

>	liftinsr :: (Reduce f) => f a -> FingerTree a -> FingerTree a
>	liftinsr = reducer insr
>	liftinsl :: (Reduce f) => FingerTree a -> f a -> FingerTree a
>	liftinsl = reducel insl

>	toTree :: (Reduce f) => f a -> FingerTree a
>	toTree s = liftinsr s Empty

ViewL and ViewR are the extracts from left and right.

>	data ViewL s a = NilL | ConsL a (s a)
>		deriving Show

>	viewL :: FingerTree a -> ViewL FingerTree a
>	viewL Empty = NilL
>	viewL (Single x) = ConsL x Empty
>	viewL (Deep pr m sf) = ConsL (head pr) (deepL (tail pr) m sf)

>	deepL :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
>	deepL [] m sf = case viewL m of 
>					NilL -> toTree sf
>					ConsL a m' -> Deep (toList a) m' sf
>	deepL pr m sf = Deep pr m sf

>	isEmptyL :: FingerTree a -> Bool
>	isEmptyL x = case viewL x of
>						NilL -> True
>						ConsL _ _ -> False

>	headL :: FingerTree a -> a
>	headL x = case viewL x of ConsL a _ -> a

>	tailL :: FingerTree a -> FingerTree a
>	tailL x = case viewL x of ConsL _ x' -> x'


1. Complete the mirror image of view.

>	data ViewR s a = NilR | ConsR (s a) a
>		deriving Show

>	viewR :: FingerTree a -> ViewR FingerTree a 
>	viewR Empty = NilR
>	viewR (Single x) = ConsR Empty x
>	viewR (Deep pr m sf) = ConsR (deepR pr m (tail sf)) (head sf)

>	deepR :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
>	deepR pr m [] = case viewR m of
>						NilR -> toTree pr
>						ConsR m' a -> Deep pr m' (toList a)
>	deepR pr m sf = Deep pr m sf

>	isEmptyR :: FingerTree a -> Bool
>	isEmptyR x = case viewR x of
>					NilR -> True
>					ConsR _ _ -> False

>	headR :: FingerTree a -> a
>	headR x = case viewR x of ConsR _ a -> a

>	tailR :: FingerTree a -> FingerTree a
>	tailR x = case viewR x of ConsR x' _ -> x' 



2. Create finger trees from lists:
         [1], [1,2,3,4]

Single 1

Deep [1,2,3] Empty [4]

3. Treating your finger tree as a deque, use insL and insR to add
to the front and the rear of the deque.

*Main> insl (Single 1) 6
Deep [1] Empty [6]

*Main> insl (Deep [1,2,3] Empty [4]) 6
Deep [1,2,3] Empty [4,6]

*Main> insr 6 (Single 1)
Deep [6] Empty [1]

*Main> insr 6 (Deep [1,2,3] Empty [4])
Deep [6,1,2,3] Empty [4]


4. Use the viewL and viewR to "dequeue" elements from the left and right
respectively.

*Main> viewL (Single 1)
ConsL 1 Empty

*Main> viewL (Deep [1,2,3] Empty [4])
ConsL 1 (Deep [2,3] Empty [4])

*Main> viewR (Single 1)
ConsR Empty 1

*Main> viewR (Deep [1,2,3] Empty [4])
ConsR (Deep [1,2] Empty [3]) 4


5. Make a fingerTree of nodes using toTree and the Node constructors.

*Main> toTree [1]
Single 1

*Main> toTree [1,2,3,4]
Deep [1,2,3] Empty [4]


6. In the paper, digits are represented as lists. Instead, use the
data type for Exercise 1:

 

data Digit a = One a
		  		| Two a a
				| Three a a a
				| Four a a a a

Rework the defintions.


headR :: FingerTree a -> a
headR (One a) = a
headR (Two _ b) = b
headR (Three _ _ c) = c
headR (Four _ _ _ d) = d


tailR :: FingerTree a -> FingerTree a
tailR (Two a _) = One a
tailR (Three a b _) = Two a b
tailR (Four a b c _) = Three a b c


viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m (One x)) = case viewR m of
		NilR -> toTree pr
		ConsR m' a -> ConsR (Deep pr m' (nodeToDigit a) x)
viewR1 (Deep pr m sf) = ConsR (deepR pr m (tail sf)) (head sf)


nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 a b) = Two a b
nodeToDigit (Node3 a b c) = Three a b c


deepR1 :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
deepR1 pr m [] = case viewR m of
						NilR -> toTree pr
						ConsR m' a -> Deep pr m' (nodeToDigit a)
deepR1 pr m sf = Deep pr m sf


