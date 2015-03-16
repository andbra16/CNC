import Control.Monad.Instances

-- file: evalWithMaybe-skel.hs
-- Neal Nelson
-- Skeleton code - this code will not run as-is

-- Contents
-- 1. A simple evaluator using the Maybe type to propogate failure.

-- 2. The evaluator is coded by lifting binary operations to the Maybe Functor

-- 3. The evaluator is coded with both the Maybe functor and the
--    Reader ((->) r) Functor to propogate failure and also abstract away
--    from carrying an explicit environment for variables.

-- 1) McBride and Paterson standard evaluator

data Exp = Val1 Int
		 | Var1 String
		 | BinOp1 (Int -> Int -> Int) Exp Exp

eval :: Exp -> Env -> Int
eval (Val1 x) env 		   = x
eval (Var1 x) env 		   = fetch x env
eval (BinOp1 op e1 e2) env = op (eval e1 env) (eval e2 env)

t1 = Val1 3
t2 = Var1 "a"
t3 = eval (BinOp1 (+) t1 t2) [("a", 2)]

-- b) evaluator with applicative class

evalA :: Exp -> Env -> Int
evalA (Val1 x) = pure x
evalA (Var1 x) = fetch x
evalA (BinOp1 op e1 e2) = pure op <*> (evalA e1) <*> (evalA e2)



------------------------------------------------------------------------------
-- 1. A simple evaluator with failure propogation rather than rudely bailing
--    with Prelude exception
------------------------------------------------------------------------------

-- The evaluator assumes abstract expressions have read-only variables and only
-- binary Int operators

-- Abstract Syntax
data Expr = Val Int
          | Var String
          | BinOp (Int -> Int -> Int) Expr Expr

-- Environment mapping variables to values
type Env = [(String, Int)]

-- An environment lookup with abrupt termination on lookup failure
fetch x env = head [b | (a,b) <- env, x == a ]


-- The evaluator with explicit plumbing of Nothing failure propogation
evalMRef :: Expr -> Env -> Maybe Int
evalMRef (Val x) env          = Just x
evalMRef (Var v) env          = lookup v env  -- lookup returns Maybe type
evalMRef (BinOp op e1 e2) env = 
	case (evalMRef e1 env) of 
		Nothing -> Nothing
		Just v1 -> case (evalMRef e2 env) of
						Nothing -> Nothing
						Just v2 -> Just (op v1 v2)
	

------------------------------------------------------------------------------
-- The simple evaluator coded by lifting to the Maybe Functor
------------------------------------------------------------------------------

-- Lift a binary operator to the Maybe Functor
lift2 :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
lift2 f (Just x) (Just y) = Just (f x y)
-- fill in code. 


evalMRef2 :: Expr -> Env -> Maybe Int
evalMRef2 (Val x) env 	= Just x
evalMRef2 (Var v) env 	= lookup v env
evalMRef2 (BinOp op e1 e2) env = solv op res1 res2 where
	solv op res1 res2
		| res1 == Nothing = Nothing
		| res2 == Nothing = Nothing
		| otherwise = lift2 op res1 res2
	res1 = evalMRef2 e1 env
	res2 = evalMRef2 e2 env

b1 = evalMRef2 (Val 1) []
b2 = evalMRef2 (Var "a") [("a",5)]
b3 = evalMRef2 (BinOp (+) (Val 1) (Var "a")) [("a",5)]
b4 = evalMRef2 (BinOp (+) (Val 1) (Var "a")) []

------------------------------------------------------------------------------
-- The simple evaluator coded by using both the Reader and Maybe applicative
-- Functors. The Reader Functor ((->) env) abstracts away the need to
-- explicitly carry around the Env environment map.
------------------------------------------------------------------------------


-- The Applicative Class
infixl 4 <*>
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- The Reader ((->) r) instance of the Applicative Class for evaluation in an
-- environment env.
--instance Functor ((->) env) where
--  fmap = (.)

instance Applicative ((->) env) where
  pure g = \env -> g
  eg <*> ex = \env -> (eg env) (ex env)


-- The Maybe instance of the Applicative Class for failure propogation
-- This Functor instance for Maybe is already in the Prelude

--instance Functor Maybe where
--  fmap g Nothing  = Nothing
--  fmap g (Just x) = Just (g x)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _  = Nothing
  (Just g) <*> x = fmap g x      -- note the shortcut using fmap

-- Code lift2 using Applicative just for fun
liftA2 :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
liftA2 g x y = pure g <*> x <*> y
--           = g <$> x <*> y
--           = fmap g x <*> y


-- The Maybe evaluator expressed using Applicative Functors 
evalM :: Expr -> Env -> Maybe Int
evalM (Val x)          = pure (pure x)
evalM (Var v)          = lookup v
--evalM (BinOp op e1 e2) = pure ((pure op) <*> (evalM e1) <*> (evalM e2))
evalM (BinOp op e1 e2) = pure (liftA2 op) <*> (evalM e1) <*> (evalM e2)

------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

e1 = Val 3
e2 = BinOp (+) (Val 3) (Val 4)
e3 = BinOp (*) (BinOp (-) (Val 9) (Val 4)) (BinOp (-) (Val 7) (Val 2)) 
e4 = BinOp (+) (BinOp (*) (Val 9) (Val 4)) (BinOp (div) (Val 7) (Val 2)) 
e5 = BinOp (-) (Val 3) (BinOp (-) (Val 4) (Val 6)) 
e6 = Var "a" 
e7 = BinOp (+) (Var "a") (Var "b")
e8 = BinOp (*) (BinOp (-) (Var "a") (Val 4)) (BinOp (-) (Val 7) (Var "b")) 
e9 = BinOp (+) (BinOp (*) (Val 9) (Var "a")) (BinOp (div) (Var "b") (Var "c")) 


test_evalM = [ evalM e1 [] == Just 3
              ,evalM e2 [] == Just 7
              ,evalM e3 [] == Just 25
              ,evalM e4 [] == Just 39
              ,evalM e5 [] == Just 5
              ,evalM e6 [("a",5)] == Just 5
              ,evalM e7 [("a",5), ("b",8)] == Just 13
              ,evalM e8 [("a",7), ("b",2)] == Just 15
              ,evalM e9 [("a",7), ("b",8), ("c", 2)] == Just 67
              ]
test_evalM_fail =
  evalM e9 [("a",7), ("b",8)]

test_evalMRef = [ evalMRef e1 [] == Just 3
                 ,evalMRef e2 [] == Just 7
                 ,evalMRef e3 [] == Just 25
                 ,evalMRef e4 [] == Just 39
                 ,evalMRef e5 [] == Just 5
                 ,evalMRef e6 [("a",5)] == Just 5
                 ,evalMRef e7 [("a",5), ("b",8)] == Just 13
                 ,evalMRef e8 [("a",7), ("b",2)] == Just 15
                 ,evalMRef e9 [("a",7), ("b",8), ("c", 2)] == Just 67
                 ]
test_evalMRef_fail =
  evalMRef e9 [("a",7), ("b",8)]

test_evalMRef2 = [ evalMRef2 e1 [] == Just 3
                 ,evalMRef2 e2 [] == Just 7
                 ,evalMRef2 e3 [] == Just 25
                 ,evalMRef2 e4 [] == Just 39
                 ,evalMRef2 e5 [] == Just 5
                 ,evalMRef2 e6 [("a",5)] == Just 5
                 ,evalMRef2 e7 [("a",5), ("b",8)] == Just 13
                 ,evalMRef2 e8 [("a",7), ("b",2)] == Just 15
                 ,evalMRef2 e9 [("a",7), ("b",8), ("c", 2)] == Just 67
                 ]
test_evalMRef2_fail =
  evalMRef e9 [("a",7), ("b",8)]


-- proofs

{- 

1) Prove the Functor laws for the ((->) r) type constructor.

instance Functor ((->) r) where
	fmap f g = (\x -> f (g x))

Show: fmap id ((->) r) = id ((->) r)

i) Left Side

fmap id ((->) r)
	= (\id -> 

ii) Right Side

id ((->) r)

Show: fmap (g.h) ((->) r) = fmap g ((->) r) . fmap h ((->) r)

i) Left Side

fmap (g.h) ((->) r)


ii) Right Side

fmap g ((-> r) . fmap h ((->) r)

2) Prove the Pointed law for the ((->) r) type constructor.







-}


