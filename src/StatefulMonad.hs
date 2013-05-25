module StatefulMonad where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe
import Stateful hiding (Stateful, evaluate)

--BEGIN:StatefulMonad1
data Stateful t = ST (Memory -> (t, Memory))
--END:StatefulMonad1

--BEGIN:StatefulMonad2
instance Monad Stateful where
  return val = ST (\m -> (val, m))
  (ST c) >>= f = 
    ST (\m -> 
      let (val, m') = c m
          ST f' = f val
      in f' m')
--END:StatefulMonad2
        
--BEGIN:StatefulMonad3
evaluate :: Exp -> Env -> Stateful Value
evaluate (Literal v) env = return v
evaluate (Unary op a) env = do
  av <- evaluate a env
  return (unary op av)
evaluate (Binary op a b) env = do
  av <- evaluate a env
  bv <- evaluate b env
  return (binary op av bv)
evaluate (If a b c) env = do
  BoolV cond <- evaluate a env
  evaluate (if cond then b else c) env
evaluate (Let x e body) env = do    -- non-recursive case
  ev <- evaluate e env
  let newEnv = (x, ev) : env
  evaluate body newEnv
evaluate (Variable x) env = 
  return (fromJust (lookup x env))
evaluate (Function x body) env = 
  return (ClosureV  x body env)
evaluate (Call fun arg) env = do
  ClosureV  x body closeEnv <- evaluate fun env
  argv <- evaluate arg env
  let newEnv = (x, argv) : closeEnv
  evaluate body newEnv
evaluate (Mutable e) env = do
  ev <- evaluate e env
  newMemory ev        
evaluate (Access a) env = do
  AddressV i <- evaluate a env
  readMemory i
evaluate (Assign a e) env = do
  AddressV i <- evaluate a env
  ev <- evaluate e env
  updateMemory ev i
  return ev
--END:StatefulMonad3

--BEGIN:StatefulHelper1
newMemory val = ST (\mem-> (AddressV (length mem), mem ++ [val]))
--END:StatefulHelper1

--BEGIN:StatefulHelper2
readMemory i = ST (\mem-> (access i mem, mem))
--END:StatefulHelper2

--BEGIN:StatefulHelper3
updateMemory val i = ST (\mem-> ((), update i val mem))
--END:StatefulHelper3

runStateful (ST c) = 
   let (val, mem) = c [] in val


  