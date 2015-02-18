ode reading and type analysis in Haskell
-- Examples

------------------------------------------------------------------------------
-- Part 1

-- For each of the following higher order functions give the most general type,
-- including the type class context.
------------------------------------------------------------------------------

-- foldr

> foldrX f z []     =  z
> foldrX f z (x:xs) =  f x (foldrX f z xs)


Solve the typing problem by solving type contraints.
1. FoldrX takes three arguments, the third is a list, [a] for some type a.
foldrX :: a -> b -> [c] -> d

2. The second argument f of FoldrX is itself a function with two arguments and
the return type of f is the same as the return type of foldrX
f :: e -> h -> d 

3.  The second argument of f is the same as the return type of foldrX.
f      :: e -> d -> d   (h = d)
foldrX :: a -> b -> [c] -> d

4. The second argument of foldrX is the same as return type of foldrX.
f      :: e -> d -> d
foldrX :: a -> d -> [c] -> d  (b = d)

5. The first argument of f is the same as the type of the list elements.
f      :: c -> d -> d  (e = c)
foldrX :: a -> d -> [c] -> d

6. The first argument of foldrX is the type of the function f
f      :: c -> d -> d  (e = c)
foldrx :: (c -> d -> d) -> d -> [c] -> d  (a = c -> d -> d)



------------------------------------------------------------------------------
-- Part 2

-- For each of the following functions
-- (a) Determine the most general type, including the type class context.
-- (b) Explain what the function does (not how it works, but what it does).
-- (c) Give a non-trivial example of how the function works.
------------------------------------------------------------------------------

> whatfun p = and . map p

------------------------------------------------------------------------------
(a) What is the type of whatfun?
------------------------------------------------------------------------------

Solve the typing problem by solving type contraints. Be sure to
use fresh type variables for every type initially. Note: there's
another, somewhat easier approach just below this typing.

1. Known typings
and :: [Bool] -> Bool
map :: (d -> e) -> [d] -> [e]
(.) :: (b -> c) -> (a -> b) -> a -> c  (composition operator)

2. Whatfun is a function of one argument as written
whatfun :: g -> h
p :: g

3. The first argument to whatfun (p) is the same as the first argument to map.
whatfun :: (d -> e) -> h  (g = (d -> e)
p :: d -> e
whatfun p :: h

4. map is applied to one argument as written
map p :: [d] -> [e]

5. The first argument to (.) is the same as the type of the and function
(b -> c) = ([Bool] -> Bool)
-- so b = [Bool], c = Bool

6. The second argument to (.) is the same as the type of (map p) 
(a -> b) = [d] -> [e]
-- so a = [d], b = [e] = [Bool], e = Bool 

7. The constraints in the previous step gives the refined typings
(.) :: ([Bool] -> Bool) -> ([d] -> [Bool]) -> [d] -> Bool
map p :: [d] -> [Bool]
whatfun :: (d -> e) -> h  (from 3 above)
whatfun :: (d -> Bool) -> h
p :: d -> Bool
whatfun p :: h

8. The (.) operator applied to its two arguments
and . map p :: [d] -> Bool

9. The (whatfun p) has the same type as (and . map p)
giving the following constraints (see 7 and 8)
h = [d] -> Bool

10. From 7, 8, 9 we get the following typings
and . map p :: [d] -> Bool
whatfun p :: [d] -> Bool
p :: d -> Bool
whatfun :: d -> Bool -> ([d] -> Bool)


------------------------------------------------------------------------------
(a) What is the type of whatfun' (easier)
------------------------------------------------------------------------------
You might find it simpler to type the whatfun function by changing the
definition from composition to application

> whatfun' p xs = and $ map p xs -- same as and (map p xs)

Solve the typing problem by solving type contraints. Be sure to
use fresh type variables for every type initially

1. Known typings
and :: [Bool] -> Bool
map :: (a -> b) -> [a] -> [b]

2. The whatfun' function has two arguments
whatfun' :: c -> d -> e

3. The first argument to whatfun' is the same as the first argument to map
c = a -> b
whatfun' :: (a -> b) -> d -> e

4. The second argument to whatfun' is the same as the second argument to map
d = [a]
whatfun' :: (a -> b) -> [a] -> e

5. The result of map is the same as the first argument to the and function.
[b] = [Bool], b = Bool
whatfun' :: (a -> Bool) -> [a] -> e

6. The result type of the and function is the same as the result type
of whatfun'
e = Bool
whatfun' :: (a -> Bool) -> [a] -> Bool


------------------------------------------------------------------------------
(b) What does the whatfun function do?
------------------------------------------------------------------------------
From the typing we see that whatfun takes a predicate. From the definition
of whatfun we see that the predicate is mapped down a list. The result of
the map is a list of Boolean values. The and function returns True if all
of the Boolean values on the list are true. That means the resulting 
whatfun' function returns True if the the argument predicate holds True
for all of the original list arguments. The whatfun function is the all
function in the Prelude.

------------------------------------------------------------------------------
(c) Two  non-trivial test examples
------------------------------------------------------------------------------

> test1_whatfun = whatfun (>0) [3, 4, 6, 0, 12]
> test2_whatfun = whatfun (>0) [3, 4, 6, 5, 12]


------------------------------------------------------------------------------
-- Part 3 Practice Exercise
------------------------------------------------------------------------------

  (a) What is the type of the following enc function? Give the most general
      type and include any required class constraints. The function length
      returns type Int.

> enc [] = []
> enc (x:xs) = (length $ x : takeWhile (==x) xs, x)
>                : enc (dropWhile (==x) xs)





  (b) What does the enc function in part (a) do?
      Describe what it does, not how it works.







  (c) Give a non-trivial test case for the enc function in part (a) and show
      the expected result.

