module CheckedMonad where

import Prelude hiding (LT, GT, EQ, id)
import FirstClassFunctions hiding (evaluate)
import ErrorChecking hiding (evaluate)

--BEGIN:Monad5
instance Monad Checked where
  return v = Good v
  a >>= f =
    case a of
      Error msg -> Error msg
      Good v    -> f v
--END:Monad5

--BEGIN:Mona13
evaluate :: Exp -> Env -> Checked Value
evaluate (Literal v) env     = return v
evaluate (Unary op a) env = do
  av <- evaluate a env
  checked_unary op av
--BEGIN:Hask4
evaluate (Binary op a b) env = do
  av <- evaluate a env
  bv <- evaluate b env
  checked_binary op av bv
--END:Hask4
evaluate (If a b c) env = do
  av <- evaluate a env
  case av of
    BoolV cond -> evaluate (if cond then b else c) env
    _ -> Error ("Expected boolean but found " ++ show av)
-- variables and declarations
evaluate (Variable x) env    =
  case lookup x env of
    Nothing -> Error ("Variable " ++ x ++ " undefined")
    Just v  -> return v
evaluate (Declare x e body) env = do    -- non-recursive case
  ev <- evaluate e env
  let newEnv = (x, ev) : env
  evaluate body newEnv
-- function definitions and function calls
evaluate (Function x body) env = return (ClosureV x body env)
evaluate (Call fun arg) env = do
  funv <- evaluate fun env
  case funv of
    ClosureV x body closeEnv -> do
      argv <- evaluate arg env
      let newEnv = (x, argv) : closeEnv
      evaluate body newEnv
    _ -> Error ("Expected function but found " ++ show funv)
--END:Mona13
