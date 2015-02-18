type Declaration = (Variable, Type)
type Variable = String

data Program = Program [Declaration] [Statement]
	deriving (Eq)

data Type = IntType | BoolType | FloatType | CharType 
	deriving (Eq, Show)

data Statement = Skip | Block [Statement] | Cond Expr Statement Statement 
					| Loop Expr Statement | Asn Variable Expr
					deriving (Eq, Show)

data Expr = VarRef Variable | ValExpr Value | Binary BinOp Expr Expr
				| Unary UnaryOp Expr deriving (Eq, Show)

data BinOp = And | Or | Eq | Neq | Lt | Le | Gt| Ge | Add | Sub | Mul
				| Div deriving (Eq, Show)

data UnaryOp = Not | Neg deriving (Eq, Show)

data Value = IntVal Int | FloatVal Float | BoolVal Bool | CharVal Char
				deriving (Eq, Show)

class Show a => ShowTree a where
	showTree :: Int -> a -> String
	showTree k v = indentShow k v

instance Show Program where
	show v = "Program:" ++ showTree 4 v ++ "\n"

instance ShowTree Program where
	showTree k (Program ds ss)
		= indent k "Declarations:"
	      ++ indentShow (k+4) ds
    	  ++ indent k "Block:"
     	  ++ showTree (k+4) ss

instance ShowTree Statement where
  showTree k Skip             = indent k "Skip"
  showTree k (Block ss)      = indent k "Block"
                                ++ showTree (k+4) ss
  showTree k (Cond e1 s1 s2) = indent k "Cond"
  showTree k (Loop e1 s1)    = indent k "Loop"
  showTree k (Asn v e)       = indent k "Asn"
                                ++ indentShow (k+4) v
                                ++ showTree (k+4) e

instance ShowTree a => ShowTree [a] where
  showTree k ss = concat $ map (showTree k) ss

instance ShowTree Expr where
  showTree k (Binary op e1 e2) = showTree k op
                                  ++ showTree (k+4) e1
                                  ++ showTree (k+4) e2
  showTree k (VarRef var)  = indentShow k var
  showTree k (ValExpr val) = indentShow k val        
  showTree k (Unary op e1) = showTree k op ++ showTree (k+4) e1

instance ShowTree BinOp
instance ShowTree UnaryOp
instance ShowTree Value


indent k str= '\n':replicate k ' ' ++ str
indentShow k v = indent k (show v)


------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

testhello = putStr $ show hello

-- The hello.cpp Clite program
hello = Program [ ("c", CharType)
                  ,("i", IntType)
                 ]
                 [ Asn "c" (ValExpr (CharVal 'h'))
                  ,Asn "i" (Binary Add
                                     (VarRef "c")
                                     (ValExpr (IntVal 3))
                            )
                 ]
