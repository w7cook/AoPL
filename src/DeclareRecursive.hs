module DeclareRecursive where
import Base
import Data.Maybe
import Declare hiding (evaluate)

-- Evaluate an expression in an environment
evaluate :: Exp -> Env -> Int
evaluate (Number i) env     = i
evaluate (Add a b) env      = evaluate a env + evaluate b env
evaluate (Subtract a b) env = evaluate a env - evaluate b env
evaluate (Multiply a b) env = evaluate a env * evaluate b env
evaluate (Divide a b) env   = evaluate a env `div` evaluate b env
evaluate (Variable x) env   = fromJust (lookup x env)
--BEGIN:Impl5
evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp newEnv) : env
--END:Impl5
