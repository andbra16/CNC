import Control.Monad

-- Reader Monad
--instance Monad ((->) r) where
--    return x = \_ -> x
--    h >>= f = \w -> f (h w) w

-- Environment mapping variables to values
type Env = [(String, Int)]

-- An environment lookup with abrupt termination on lookup failure
fetch x env = head [b | (a,b) <- env, x == a ]

data Expr = Val1 Int
         | Var1 String
         | BinOp1 (Int -> Int -> Int) Expr Expr

-- 1) Recode one of your evaluators from Assignment 5 part 1 using Monad instead of Applicative. This is the evaluator that just bails with error instead of using the Maybe type and propogating Nothing. You can use the do notation and you may actually find this easier than the original Applicative version.

eval :: Expr -> Env -> Int
eval (Val1 x)          = return x
eval (Var1 x)	       = fetch x
eval (BinOp1 op e1 e2) = do
							a1 <- eval e1  
							a2 <- eval e2
							return (op a1 a2)

t1 = Val1 3
t2 = Var1 "a"
t3 = eval (BinOp1 (+) t1 t2) [("a", 2)]

-- 2) Recode your evaluator of Assignment 5 part 1b using the Reader Monad in the Control.Monad library rather than your own ((->) env) monad. The easiest way to do this is to import Control.Monad.Instances and you should get the ((->) env) Monad instance imported for you. You'll have to comment out your own instance definition. You could use the Control.Monad.Reader library, but that is beyond the scope of our work at this point because it requires an understanding of Monad Transformers.

evalR :: Expr -> Env -> Int
evalR (Val1 x)          = return x
evalR (Var1 x)          = fetch x
evalR (BinOp1 op e1 e2) = do
                            a1 <- evalR e1
                            a2 <- evalR e2
                            return (op a1 a2)

tr1 = Val1 3
tr2 = Var1 "a"
tr3 = evalR (BinOp1 (+) t1 t2) [("a", 2)]

-- 3) Now take your evaluator from the previous step and combine the ((->) r) Reader Monad with the Maybe functor for values and code the evaluator so that it propogates Nothing on errors. This is the Monadic version of the evaluator from Assignment 5 part 2d. I'll call this a Reader+Maybe evaluator.

evalM :: Expr -> Env -> Maybe Int
evalM (Val1 x) = return (return x)
evalM (Var1 v) = lookup v
evalM (BinOp1 op e1 e2) = do
							a1 <- evalM e1
							a2 <- evalM e2
						 	return ((liftM2 op) a1 a2)
						 	
--evalM (BinOp1 op e1 e2) = let
--							a1 = evalM e1
--							a2 = evalM e2
--						  in (return (liftM2 op)) `ap` a1 `ap` a2



tm1 = Val1 3
tm2 = Var1 "a"
tm3 = evalM (BinOp1 (+) tm1 tm2) [("a",2)]


-- 4) Recode your Reader evaluator using a State Applicative and then do a version using a State Monad. In order for this effort to have any meaning you'll have to add an assignment statement to the expression language so that state can be updated. Here's a State Evaluator Skeleton to get you started. This state evaluator step of the assignment is by far the best part to complete.

data ExprAsn = Val Int
             | Var String
             | BinOp (Int -> Int -> Int) ExprAsn ExprAsn
             | Asn String ExprAsn

-- A simple environment update
store :: String -> Int -> Env -> Env
store var val env = (var, val) : env

-- We need the newtype so the functor can be single-parameter as required
-- As a composition:  State a = ((->) Env ) . ((,) Env)
newtype State a = State (Env -> (Env, a))

instance Functor State where
  fmap f (State s) = State (\env -> let (newEnv, a) = s env
									in (newEnv, f a))


-- The State functor lookup that we actually need for the Applicative functor
fetchS :: String -> State Int
fetchS var = State (\env -> (env, fetch var env) )

-- The State functor update that we actually need for the Applicative functor
storeS :: String -> State Int -> State Int
storeS var (State s) = State ( \env -> let (env', val) = s env
                                       in (store var val env', val) )

-- The general apply function for the State functor (more commonly runST)
applyS :: State Int -> Env -> (Env, Int)
applyS (State s) env = s env

evalSRef :: ExprAsn -> Env -> (Env, Int)
evalSRef (Val x) env     = (env, x)
evalSRef (Var var) env   = (env, fetch var env)
evalSRef (BinOp op e1 e2) env = let
                                    (env1, res1) = evalSRef e1 env
                                    (env2, res2) = evalSRef e2 env1
                                in (env2, op res1 res2)

evalSRef (Asn var e) env = let (env1, res) = evalSRef e env
                           in (store var res env1, res)
------------------------------------------------------------------------------
-- 3. Type Class Definitions
------------------------------------------------------------------------------

-- The Applicative Class
infixl 4 <*>
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- The MonadBind class
infixl 1 >>>=
class Applicative f => MonadBind f where
  (>>>=) :: f a -> (a -> f b) -> f b          -- (>>=) is already taken

instance Applicative State where
  pure g = State (\env -> (env, g))
  State eg <*> State ex = State (\env -> let
                                            (env1, f) = eg env
                                            (env2, x) = ex env1
                                         in (env2, f x))

-- The applicative State evaluator
evalS :: ExprAsn -> State Int
evalS (Val x)          = pure x
evalS (Var var)        = fetchS var
evalS (BinOp op e1 e2) = (pure op) <*> evalS e1 <*> evalS e2
evalS (Asn var e)      = storeS var (evalS e)

-- We need to be able to run the code constructed by EvalS in the state functor
evalSRun :: ExprAsn -> Env -> (Env, Int)
evalSRun expr env = applyS (evalS expr) env

-- State as a home-grown MonadBind functor
instance MonadBind State where
  State s >>>= f    = State (\env -> let
                                         (newEnv, a) = s env
                                         (State g) = f a
                                     in g newEnv)

-- The MonadBind State evaluator.
evalSMB :: ExprAsn -> State Int
evalSMB (Val x)          = pure x
evalSMB (Var var)        = fetchS var
evalSMB (BinOp op e1 e2) = evalSMB e1 >>>= \r1 ->
                           evalSMB e2 >>>= \r2 ->
                           pure (op r1 r2)

evalSMB (Asn var e)      = storeS var (evalSMB e)

-- We need to be able to run the constructed evaluator
evalSMBRun :: ExprAsn -> Env -> (Env, Int)
evalSMBRun expr env = applyS (evalSMB expr) env

-- State as the build-in MonadBind functor
instance Monad State where
  State s >>= f    = State (\env -> let
                                        (newEnv, a) = s env
                                        State g = f a
                                    in g newEnv)


  return g = State (\env -> (env, g))

-- The Monad State evaluator
evalSM :: ExprAsn -> State Int
evalSM (Val x)          = return x
evalSM (Var var)        = fetchS var
evalSM (BinOp op e1 e2) = do
                            r1 <- evalSM e1
                            r2 <- evalSM e2
                            return (op r1 r2)

valSM (Asn var e)      = storeS var (evalSM e)

-- We need to be able to run the constructed evaluator
evalSMRun :: ExprAsn -> Env -> (Env, Int)
evalSMRun expr env = applyS (evalSM expr) env

------------------------------------------------------------------------------
-- 5. Testing
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

-- For assignment tests
e10 = BinOp (+) (BinOp (*) (Asn "a" (Val 6)) (Var "b"))
                (BinOp (+) (Var "a") (BinOp (*) (Asn "b" (Val 3)) (Var "b")))

test_evalSMB = [ evalSMBRun e1 [] == evalSRef e1 [] -- should do em all this way
               ,evalSMBRun e2 [] == evalSRef e2 []
               ,evalSMBRun e3 [] == evalSRef e3 []
               ,evalSMBRun e4 [] == evalSRef e4 []
               ,evalSMBRun e5 [] == evalSRef e5 []
               ,evalSMBRun e6 [("a",5)] == ([("a",5)], 5)
               ,evalSMBRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
               ,evalSMBRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
               ,evalSMBRun e9 [("a",7), ("b",8), ("c", 2)]
                         == ([("a",7), ("b",8), ("c", 2)], 67)
               ,evalSMBRun e10 [("a",7), ("b",2)]
                   == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
               ]

test_evalSM = [ evalSMRun e1 [] == evalSRef e1 [] -- should do em all this way
               ,evalSMRun e2 [] == evalSRef e2 []
               ,evalSMRun e3 [] == evalSRef e3 []
               ,evalSMRun e4 [] == evalSRef e4 []
               ,evalSMRun e5 [] == evalSRef e5 []
               ,evalSMRun e6 [("a",5)] == ([("a",5)], 5)
               ,evalSMRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
               ,evalSMRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
               ,evalSMRun e9 [("a",7), ("b",8), ("c", 2)]
                         == ([("a",7), ("b",8), ("c", 2)], 67)
               ,evalSMRun e10 [("a",7), ("b",2)]
                   == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
               ]

test_evalS = [ evalSRun e1 [] == ([], 3)
              ,evalSRun e2 [] == ([], 7)
              ,evalSRun e3 [] == ([], 25)
              ,evalSRun e4 [] == ([], 39)
              ,evalSRun e5 [] == ([], 5)
              ,evalSRun e6 [("a",5)] == ([("a",5)], 5)
              ,evalSRun e7 [("a",5), ("b",8)] == ([("a",5), ("b",8)], 13)
              ,evalSRun e8 [("a",7), ("b",2)] == ([("a",7), ("b",2)], 15)
              ,evalSRun e9 [("a",7), ("b",8), ("c", 2)]
                          == ([("a",7), ("b",8), ("c", 2)], 67)
              ,evalSRun e10 [("a",7), ("b",2)]
                          == ([("b", 3), ("a",6), ("a", 7), ("b",2)], 27)
              ]

