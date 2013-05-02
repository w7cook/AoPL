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
evaluate :: Exp -> Env -> Value
evaluate exp env = eval exp where
    eval (Literal v)      = v
    eval (Unary op a)     = unary op (eval a)
    eval (Binary op a b)  = binary op (eval a) (eval b)
    eval (Variable x)     = fromJust (lookup x env)
    eval (Let x exp body) = evaluate body newEnv
      where newEnv = (x, eval exp) : env
--END:More12 BEGIN:More14
    eval (If a b c)      = if fromBoolV (eval a)
                           then eval b
                           else eval c
fromBoolV (BoolV b) = b
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

