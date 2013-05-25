module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

--BEGIN:More3
data Value = IntV  Int
           | BoolV Bool
 deriving (Eq, Show)
--END:More3

--BEGIN:More99
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Show, Eq)

data UnaryOp = Neg | Not
  deriving (Show, Eq)

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
  deriving (Show, Eq)
--END:More99

--BEGIN:More12
type Env = [(String, Value)]

-- Evaluate an expression in an environment
--BEGIN:Stat2
evaluate :: Exp -> Env -> Value
--END:Stat2
evaluate (Literal v) env      = v
evaluate (Unary op a) env     = unary op (evaluate a env)
--BEGIN:Hand13
evaluate (Binary op a b) env  = binary op (evaluate a env) (evaluate b env)
--END:Hand13
evaluate (Variable x) env     = fromJust (lookup x env)
evaluate (Let x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp env) : env
--END:More12 BEGIN:More14
evaluate (If a b c) env =
  let BoolV test = evaluate a env in
    if test then evaluate b env
            else evaluate c env

execute exp = evaluate exp []
--END:More14

--BEGIN:More16
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
--END:More16

