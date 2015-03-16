>	data Q a = P [a] [a]
>		deriving Show

>	len (P l r) = length l + length r

>	insert e (P l r) = makeq (P l (e:r))

>	remove (P (l:ls) r) = (l, (makeq (P ls r)))

>	makeq (P l r)
>		| length r <= length l		= P l r
>		| length r == length l + 1	= P (rot l r []) []


>	rot [] (r:rs) a =  r:a
>	rot (l:ls) (r:rs) a = l:rot ls rs (r:a)

Part 1

1) Implement append

>	app (P [] []) (P l2 r2) = P l2 r2
>	app (P l1 r1) (P [] []) = P l1 r1
> 	app (P l1 r1) (P (x:xs) r2) = app (makeq (P l1 (x:r1))) (makeq (P xs r2))

> 	testapp = app (P [1,2] [3,4]) (P [5,6] [7,8])

2) Implement fold

>	foldQ f z (P [] []) = z
> 	foldQ f z (P (x:xs) ys) = (f x) (foldQ f z (makeq (P xs ys)))

> 	testfold = foldQ (+) 0 (P [1,2,3] [4,5]) 

3) Implement map

>	mapQ f (P [] []) = P [] []
>	mapQ f (P (xs) (ys)) = P (map f xs) (map f ys)

>	testmap = mapQ (*2) (P [1,2] [3,4])

4) Implement reverse

>	reverseQ (P (xs) (ys)) = P (reverse ys) (reverse xs)

>	testreverse = reverseQ (P [1,2] [3,4])

Part 2

Do the rewriting on the append to show how the evaluated/unevaluated portions relate

app (P [1] [2]) (P [3] [4])
app (makeq (P [1] [3,2])) (makeq (P [] [4]))
app (P (rot [1] [3,2] []) []) (P (rot [] [4] []) [])
app (P [1,2,3] []) (P [4] [])
app (makeq (P [1,2,3] [4])) (makeq (P [] []))
app (P [1,2,3] [4]) (P [] [])
P [1,2,3] [4]

Part 3

First make sure you understand why the paired version of the queue is O(log n) for removes. Then analyze the big O as was done in the lecture notes.

In the worst case, a remove requires the Queue to be rotated. When being 
rotated every element in both lists of the Queue must be concatenated but
rot uses an accumulator so it effectively concats two at a time making it
O(log n).

Part 4

Implement the deque described in the paper.



Part 4



