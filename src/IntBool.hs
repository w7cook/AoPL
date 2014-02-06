module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe

--BEGIN:More3
data Value = IntV  Int
           | BoolV Bool
 deriving (Eq)
--END:More3

instance Show Value where
	show (IntV n) = show n
	show (BoolV True) = "true"
	show (BoolV False) = "false"

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
         | Declare   String Exp Exp
--END:More99

instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp level (Literal i)      = show i
showExp level (Variable x)    = x
showExp level (Declare x a b) = 
	if level > 0 then
  	paren ("var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b)
  else
    "var " ++ x ++ " = " ++ showExp 0 a ++ ";\n" ++ showExp 0 b
showExp level (If a b c)    = 
	if level > 0 then
	  paren ("if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c)
	else
	  "if (" ++ showExp 0 a ++ ")\n" ++ showExp 0 b ++ ";\nelse " ++ showExp 0 c
showExp level (Unary Neg a)    = "-" ++ showExp 99 a
showExp level (Unary Not a)    = "!" ++ showExp 99 a
showExp level (Binary op a b)  = showBinary level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where levels = [(Or, 1), (And, 2), (GT, 3), (LT, 3), (LE, 3), (GE, 3), (EQ, 3), 
  							  (Add, 4), (Sub, 4), (Mul, 5), (Div, 5)] 
        names = [(Or, "||"), (And, "&&"), (GT, ">"), (LT, "<"), (LE, "<="), (GE, ">="), (EQ, "=="), 
  							  (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/")] 

showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b
      

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
evaluate (Declare x exp body) env = evaluate body newEnv
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

