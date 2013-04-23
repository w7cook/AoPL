module StatefulMonad where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe
import MutableState hiding (Stateful, evaluate)

--BEGIN:StatefulMonad1
data Stateful t = Stateful (Memory -> (t, Memory))
--END:StatefulMonad1

--BEGIN:StatefulMonad2
instance Monad Stateful where
  return val = Stateful (\m -> (val, m))
  (Stateful c) >>= f = 
    Stateful (\m -> 
      let (val, m') = c m
          Stateful f' = f val
      in f' m')
--END:StatefulMonad2
        
--BEGIN:StatefulMonad3
evaluate :: Exp -> Env -> Stateful Value
evaluate exp env = eval exp where
    eval (Literal v) = return v
    eval (Unary op a) = do
      av <- eval a
      return (unary op av)
    eval (Binary op a b) = do
      av <- eval a
      bv <- eval b
      return (binary op av bv)
    eval (If a b c) = do
      Bool cond <- eval a
      eval (if cond then b else c)
    eval (Let x e body) = do    -- non-recursive case
      ev <- eval e
      let newEnv = (x, ev) : env
      evaluate body newEnv
    eval (Variable x) = return (fromJust (lookup x env))
    eval (Function x body) = return (Closure x body env)
    eval (Call fun arg) = do
      Closure x body closeEnv <- eval fun
      argv <- eval arg
      let newEnv = (x, argv) : closeEnv
      evaluate body newEnv
    eval (Mutable e) = do
      ev <- eval e
      newMemory ev        
    eval (Access a) = do
      Address i <- eval a
      readMemory i
    eval (Assign a e) = do
      Address i <- eval a
      ev <- eval e
      updateMemory ev i
      return ev
--END:StatefulMonad3

--BEGIN:StatefulHelper1
newMemory val = Stateful (\mem-> (Address (length mem), mem ++ [val]))
--END:StatefulHelper1

--BEGIN:StatefulHelper2
readMemory i = Stateful (\mem-> (access i mem, mem))
--END:StatefulHelper2

--BEGIN:StatefulHelper3
updateMemory val i = Stateful (\mem-> ((), update i val mem))
--END:StatefulHelper3

runStateful (Stateful c) = 
   let (val, mem) = c [] in val

t0 = Let "x" (Literal (Int 99)) (Variable "x")
t1 = Let "x" (Mutable (Literal (Int 3)))
         (Access (Variable "x"))
         
main = do
  print (runStateful (evaluate (Literal (Int 6)) []))
  print (runStateful (evaluate t0 []))
  print (runStateful (evaluate t1 []))
  