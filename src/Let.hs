module Let where
import Base
import Data.Maybe

--BEGIN:Summ3
data Exp = Number     Int
         | Add        Exp Exp
         | Subtract   Exp Exp
         | Multiply   Exp Exp
         | Divide     Exp Exp
         | Variable   String
         | Let        String Exp Exp
--END:Summ3

type Env = [(String, Int)]

--BEGIN:Eval38
-- Evaluate an expression in an environment
evaluate :: Exp -> Env -> Int
evaluate exp env = eval exp where
  eval (Number i)      = i
  eval (Add a b)       = eval a + eval b
  eval (Subtract a b)  = eval a - eval b
  eval (Multiply a b)  = eval a * eval b
  eval (Divide a b)    = eval a `div` eval b
--END:Eval38 BEGIN:Eval9
  eval (Variable x)    = fromJust (lookup x env)
  eval (Let x exp body)     = evaluate body newEnv
    where newEnv = (x, eval exp) : env
--END:Eval9

data Value = Int  Int
           | Bool Bool
 deriving Eq

instance Show Value where
  show (Int i) = if i < 0 then paren (show i) else show i
  show (Bool b) = show b