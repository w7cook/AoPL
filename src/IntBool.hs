module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Value
import Data.Maybe

--BEGIN:More99
data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   String Exp Exp
--END:More99

--BEGIN:More12
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


-- Code to display expressions

instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp level (Literal i)      = show i
showExp level (Variable x)    = x
showExp level (Declare x a b) = 
	if level > 0 then paren result else result
  	where result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c)    = 
	if level > 0 then paren result else result
	  where result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
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
      
