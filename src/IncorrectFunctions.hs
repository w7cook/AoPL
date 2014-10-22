module IncorrectFunctions where

import Prelude hiding (LT, GT, EQ)
import Data.Maybe
import Base
import Operators
import FunctionalEnvironment

--BEGIN:A32
data Value = IntV  Int
           | BoolV Bool
           | Function String Exp  -- new
  deriving (Eq, Show)
--END:A32

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   String Exp Exp
         | Call      Exp Exp         -- changed
  deriving (Eq, Show)

type Env = String -> Maybe Value

-- most of this is the same as IntBool
--BEGIN:A34
evaluate :: Exp -> Env -> Value
evaluate (Literal v) env      = v
--END:A34
evaluate (Unary op a) env     = unary op (evaluate a env)
evaluate (Binary op a b) env  = binary op (evaluate a env) (evaluate b env)
evaluate (If a b c) env = 
  let BoolV test = evaluate a env in
    if test then evaluate b env
            else evaluate c env
evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = bindF x (evaluate exp env) env
evaluate (Variable x) env     =
  case env x of
    Nothing -> error ("Variable " ++ x ++ " undefined")
    Just v  -> v
--BEGIN:A35
evaluate (Call fun arg) env = evaluate body newEnv
  where Function x body = evaluate fun env
        newEnv = bindF x (evaluate arg env) env
--END:A35

emptyEnv = \x->Nothing

-- Same as IntBool
execute exp = evaluate exp emptyEnv

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