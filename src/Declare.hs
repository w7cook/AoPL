module Declare where
import Base
import Data.Maybe

-- Define the data type for the abstract syntax of expressions, including variable declarations
--BEGIN:Summ3
data Exp = Number     Int
         | Add        Exp Exp
         | Subtract   Exp Exp
         | Multiply   Exp Exp
         | Divide     Exp Exp
         | Variable   String
         | Declare    String Exp Exp
--END:Summ3

-- An environment is a list of strings and values
type Env = [(String, Int)]

--BEGIN:Eval38
-- Evaluate an expression in an environment
evaluate :: Exp -> Env -> Int
evaluate (Number i) env     = i
evaluate (Add a b) env       = evaluate a env + evaluate b env
evaluate (Subtract a b) env  = evaluate a env - evaluate b env
evaluate (Multiply a b) env  = evaluate a env * evaluate b env
evaluate (Divide a b) env    = evaluate a env `div` evaluate b env
evaluate (Variable x) env    = fromJust (lookup x env)
--BEGIN:Impl3
evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp env) : env
--END:Eval38 END:Impl3

-- execute evaluates an expression in an empty environment
--BEGIN:DeclExec
execute e = evaluate e []
--END:DeclExec

-- Helper code to "show" expressions. Showing an expression is the process of converting 
-- the expression into a string. It is complicated by the need to handle precedence
instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp level (Number i)      = show i
showExp level (Variable x)    = x
showExp level (Declare x a b) = 
	if level > 0 then paren result else result
  	where result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (Add a b)       = showBinary level 1 a " + " b
showExp level (Subtract a b)  = showBinary level 1 a " - " b
showExp level (Multiply a b)  = showBinary level 2 a "*" b
showExp level (Divide a b)    = showBinary level 2 a "/" b

showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ op ++ showExp inner b
