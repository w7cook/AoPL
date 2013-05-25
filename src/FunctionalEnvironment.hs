module FunctionalEnvironment where
import Data.Maybe
import IntBool

--BEGIN:Repr31
emptyEnvF :: EnvF
emptyEnvF = \var -> Nothing
--END:Repr31

bindF :: String -> a -> (String -> Maybe a) -> (String -> Maybe a)
--BEGIN:Repr15
-- bindF :: String -> Value -> EnvF -> EnvF
--END:Repr15

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
evaluateF (Literal v) env      = v
evaluateF (Unary op a) env     = unary op (evaluateF a env)
evaluateF (Binary op a b) env  = binary op (evaluateF a env) (evaluateF b env)
evaluateF (Variable x) env     = fromJust (env x)        -- changed
evaluateF (Let x exp body) env = evaluateF body newEnv
  where newEnv = bindF x (evaluateF exp env) env             -- changed
--END:Repr34
