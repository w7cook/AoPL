module CheckedMonad where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe
import FirstClassFunctions hiding (evaluate)
import ErrorChecking hiding (evaluate)

--BEGIN:Monad5
instance Monad Checked where
  a >>= f =
    case a of
      Error msg -> Error msg
      Good v -> f v
  return v = Good v
--END:Monad5

--BEGIN:Mona13
evaluate :: Exp -> Env -> Checked Value
evaluate exp env = eval exp where
    eval (Literal v)      = return v
    eval (Variable x)     =
      case lookup x env of
        Nothing -> Error ("Variable " ++ x ++ " undefined")
        Just v  -> return v
    eval (Unary op a) = do
      av <- eval a
      checked_unary op av
    eval (Binary op a b) = do
      av <- eval a
      bv <- eval b
      checked_binary op av bv
    eval (If a b c) = do
      av <- eval a
      case av of
        (Bool cond) -> eval (if cond then b else c)
        _ -> Error ("Expected boolean but found " ++ show av)
    eval (Let x e body) = do    -- non-recursive case
      ev <- eval e
      let newEnv = (x, ev) : env
      evaluate body newEnv
    eval (Function x body) = return (Closure x body env)
    eval (Call fun arg) = do
      funv <- eval fun
      case funv of
        Closure x body closeEnv -> do
          argv <- eval arg
          let newEnv = (x, argv) : closeEnv
          evaluate body newEnv
        _ -> Error ("Expected function but found " ++ show funv)
--END:Mona13
