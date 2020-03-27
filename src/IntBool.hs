module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Value
import Operators
import Data.Maybe

--BEGIN:More99
data Exp  = Literal   Value
          | Unary     UnaryOp Exp
          | Binary    BinaryOp Exp Exp
          | If        Exp Exp Exp
          | Variable  String
          | Declare   String Exp Exp
--END:More99
  deriving Show

--BEGIN:More12
-- Evaluate an expression in an environment
--BEGIN:Stat2
evaluate :: Exp -> Env -> Value
--END:Stat2
evaluate (Literal v) env   = v
evaluate (Unary op a) env  = unary op (evaluate a env)
--BEGIN:Hand13
evaluate (Binary op a b) env = 
  binary op (evaluate a env) (evaluate b env)
--END:Hand13
evaluate (Variable x) env         = fromJust (lookup x env)
evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp env) : env
--END:More12 BEGIN:More14
evaluate (If a b c) env =
  let BoolV test = evaluate a env in
    if test then evaluate b env
            else evaluate c env
--END:More14

