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
   deriving Show

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
