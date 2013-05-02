module FirstClassFunctions where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

--BEGIN:A42
data Value = IntV  Int
           | BoolV Bool
           | ClosureV String Exp Env  -- new
  deriving (Eq, Show)
--END:A42

--BEGIN:Summ14
data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
         | Function  String Exp      -- new
         | Call      Exp Exp         -- changed
  deriving (Eq, Show)
  
type Env = [(String, Value)]

evaluate :: Exp -> Env -> Value
evaluate exp env = eval exp where
    eval (Literal v)      = v
    eval (Unary op a)     = unary op (eval a)
    eval (Binary op a b)  = binary op (eval a) (eval b)
    eval (If a b c)       = if fromBoolV (eval a)
                            then eval b
                            else eval c
    eval (Variable x)     = fromJust (lookup x env)
    eval (Let x exp body) = evaluate body newEnv
      where newEnv = (x, eval exp) : env
    eval (Function x body) = ClosureV x body env     -- new
    eval (Call fun arg)   = evaluate body newEnv    -- changed
      where ClosureV x body closeEnv = eval fun
            newEnv = (x, eval arg) : closeEnv

fromBoolV (BoolV b) = b
--END:Summ14

-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary Add (IntV a)  (IntV b)  = IntV (a + b)
binary Sub (IntV a)  (IntV b)  = IntV (a - b)
binary Mul (IntV a)  (IntV b)  = IntV (a * b)
binary Div (IntV a)  (IntV b)  = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV a)  (IntV b)  = BoolV (a < b)
binary LE  (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE  (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT  (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ  a         b         = BoolV (a == b)



