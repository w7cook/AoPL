module FunctionalEnvironment where
import Data.Maybe
import IntBool hiding (main)


--BEGIN:Repr31
emptyEnvF :: EnvF
emptyEnvF = \var -> Nothing
--END:Repr31

--BEGIN:Repr19
bindF var val env = \testVar -> if testVar == var
                                then Just val
                                else env testVar
--END:Repr19

--BEGIN:Repl801
type EnvF = String -> Maybe Value
--END:Repl801

--BEGIN:Repr34
-- Evaluate an expression in a (functional) environment
evaluateF :: Exp -> EnvF -> Value
evaluateF exp env = eval exp where
    eval (Literal v)      = v
    eval (Unary op a)     = unary op (eval a)
    eval (Binary op a b)  = binary op (eval a) (eval b)
    eval (Variable x)     = fromJust (env x)            -- changed
    eval (Let x exp body) = evaluateF body newEnv
      where newEnv = bindF x (eval exp) env             -- changed
--END:Repr34

main = 
  do "need tests here"
  