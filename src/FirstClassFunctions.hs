module FirstClassFunctions where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

--BEGIN:A42
data Value = Int  Int
           | Bool Bool
           | Closure String Exp Env  -- new
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
    eval (If a b c)       = if fromBool (eval a)
                            then eval b
                            else eval c
    eval (Variable x)     = fromJust (lookup x env)
    eval (Let x exp body) = evaluate body newEnv
      where newEnv = (x, eval exp) : env
    eval (Function x body) = Closure x body env     -- new
    eval (Call fun arg)   = evaluate body newEnv    -- changed
      where Closure x body closeEnv = eval fun
            newEnv = (x, eval arg) : closeEnv

fromBool ((Bool b)) = b
--END:Summ14

-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

unary Not (Bool b) = Bool (not b)
unary Neg (Int i)  = Int (-i)

binary Add (Int a)  (Int b)  = Int (a + b)
binary Sub (Int a)  (Int b)  = Int (a - b)
binary Mul (Int a)  (Int b)  = Int (a * b)
binary Div (Int a)  (Int b)  = Int (a `div` b)
binary And (Bool a) (Bool b) = Bool (a && b)
binary Or  (Bool a) (Bool b) = Bool (a || b)
binary LT  (Int a)  (Int b)  = Bool (a < b)
binary LE  (Int a)  (Int b)  = Bool (a <= b)
binary GE  (Int a)  (Int b)  = Bool (a >= b)
binary GT  (Int a)  (Int b)  = Bool (a > b)
binary EQ  a        b        = Bool (a == b)



