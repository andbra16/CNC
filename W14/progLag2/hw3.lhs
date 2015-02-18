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

1)


>	app (P l1 r1) (P l2 r2)
>		| (l1  == [] && r1 == []) = P l2 r2
>		| 
