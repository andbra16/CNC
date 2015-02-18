-- I completed most of the problems, I had trouble with the Either and Maybe types, and didn't get to unfold.

import Data.List

-- 14.4, 14.5
data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr | Mult Expr Expr |
			 Div Expr Expr



show' :: Expr -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mult e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
show' (Div e1 e2) = "(" ++ show' e1 ++ "/" ++ show' e2 ++ ")"

a = (Add (Sub (Lit 3) (Lit 1)) (Lit 3)) 

size :: Expr -> Integer
size (Lit n) = 0
size (Add e1 e2) = 1 + (size e1 + size e2)
size (Sub e1 e2) = 1 + (size e1 + size e2) 
size (Mult e1 e2) = 1 + (size e1 + size e2)
size (Div e1 e2) = 1 + (size e1 + size e2)

eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)
eval (Div e1 (Lit 0)) = error "Can't divide by 0"
eval (Div e1 e2) = fromIntegral ((eval e1) `div` (eval e2))

-- 14.6, 14.15

data Expr' = Lit' Integer | Op Ops Expr' Expr' | If BExp Expr' Expr'

data Ops = Add' | Sub' | Mul' | Div' | Mod'

data BExp = BoolLit Bool | And BExp BExp | Not BExp | Equal Expr' Expr' |
			Greater Expr' Expr'

b = (Op Add' (Op Sub' (Lit' 3) (Lit' 1)) (Lit' 3))

--show'' :: a -> String
show'' (Lit' n) = show n
show'' (Op Add' e1 e2) = "(" ++ show'' e1 ++ "+" ++ show'' e2 ++ ")"
show'' (Op Sub' e1 e2) = "(" ++ show'' e1 ++ "-" ++ show'' e2 ++ ")"
show'' (Op Mul' e1 e2) = "(" ++ show'' e1 ++ "*" ++ show'' e2 ++ ")"
show'' (Op Div' e1 e2) = "(" ++ show'' e1 ++ "/" ++ show'' e2 ++ ")"
show'' (Op Mod' e1 e2) = "(" ++ show'' e1 ++ "mod" ++ show'' e2  ++ ")"

size' :: Expr' -> Integer
size' (Lit' n) = 0
size' (Op Add' e1 e2) = 1 + (size' e1 + size' e2)
size' (Op Sub' e1 e2) = 1 + (size' e1 + size' e2)
size' (Op Mul' e1 e2) = 1 + (size' e1 + size' e2)
size' (Op Div' e1 e2) = 1 + (size' e1 + size' e2)
size' (Op Mod' e1 e2) = 1 + (size' e1 + size' e2)

eval' :: Expr' -> Integer
eval' (Lit' n) = n
eval' (Op Add' e1 e2) = (eval' e1) + (eval' e2)
eval' (Op Sub' e1 e2) = (eval' e1) - (eval' e2)
eval' (Op Mul' e1 e2) = (eval' e1) * (eval' e2)
eval' (Op Div' e1 (Lit' 0)) = error "Can't divide by 0"
eval' (Op Div' e1 e2) = fromIntegral ((eval' e1) `div` (eval' e2))
eval' (Op Mod' e1 e2) = (eval' e1) `mod` (eval' e2)
eval' (If b e1 e2) 
	| bEval b == True = eval' e1
	| otherwise = eval' e2

bEval :: BExp -> Bool
bEval (BoolLit n) = n
bEval (Not (BoolLit n)) = not n
bEval (Not e1) = not (bEval e1)
bEval (And e1 e2) = (bEval e1) && (bEval e2)
bEval (Equal e1 e2) = (eval' e1) == (eval' e2)
bEval (Greater e1 e2) = (eval' e1) > (eval' e2)

-- 14.10

data NTree = NilT | Node Integer NTree NTree

c = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)

elemT x NilT = False
elemT x (Node n t1 t2) = x == n || elemT x t1 || elemT x t2

-- 14.13
collapse :: NTree -> [Integer]
collapse NilT = []
collapse (Node n t1 t2) = collapse t1 ++ [n] ++ collapse t2

sort' :: NTree -> [Integer]
sort' NilT = []
sort' t = sort (collapse t)

-- 14.19
data Either a b = Left a | Right b
	deriving (Eq,Ord,Read,Show)


-- 14.20
--join :: (a -> c) -> (b -> d) -> Either a b -> Either c d
--join f g (Left x) = 

-- 14.23
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x

process :: [Int] -> Int -> Int -> Int
process xs n m  
	| (n>((length xs)-1) || n<0) = 0
	| (m>((length xs)-1) || m<0) = 0
	| otherwise = (xs !! n) + (xs !! m)

-- 14.25
--squashMaybe :: Maybe (Maybe a) -> Maybe a
--squashMaybe x
--	| x == (Just (Just Nothing)) = Nothing
--	| otherwise = Just x

-- 14.26
--composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)

-- 5
data Bit = B1 | B0
    deriving (Eq, Show)

bitList1 = [B1,B0,B1,B1]

bin2int :: [Bit] -> Int
bin2int [] = 0
bin2int [x]
    | x == B1 = 1
    | otherwise = 0
bin2int xs
    | head xs == B1 = 2^(length xs - 1) + bin2int (tail xs)
    | otherwise = bin2int (tail xs)

-- 6
int2bin :: Int -> [Bit]
int2bin 0 = [B0]
int2bin 1 = [B1]
int2bin x
    | x `mod` 2 == 0 = B0 : reverse (int2bin (x `div` 2))
    | otherwise = B1 : reverse (int2bin (x `div` 2))

-- 7
make8 :: [Bit] -> [Bit]
make8 [] = []
make8 xs 
	| length xs == 8 = xs
	| length xs < 8 = make8(B0 : xs)
	| otherwise = make8 (tail xs)

-- 8
--encode :: String -> [Bit]

-- 9 
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = map make8 (take 8 xs : chop8 (drop 8 xs))

-- 10

-- 11
bin2int' = foldr func 0
    where
        func x acc
            | x == B1 = 2^(acc) + acc
            | otherwise = acc

