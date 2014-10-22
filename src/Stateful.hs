module Stateful where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe
import Operators

--BEGIN:Addr11
data Value = IntV  Int
           | BoolV Bool
           | ClosureV String Exp Env
           | AddressV Int        -- new
  deriving (Eq, Show)
--END:Addr11

--BEGIN:Memo4
type Memory = [Value]
--END:Memo4

--BEGIN:Acce3
access i mem = mem !! i
--END:Acce3

--BEGIN:Upda4
update :: Int -> Value -> Memory -> Memory
update addr val mem =
  let (before, _ : after) = splitAt addr mem in
    before ++ [val] ++ after
--END:Upda4

--BEGIN:Stat8
type Stateful t = Memory -> (t, Memory)
--END:Stat8

--BEGIN:Summ7
data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   String Exp Exp
         | Function  String Exp
         | Call      Exp Exp
         | Seq       Exp Exp
         | Mutable   Exp         -- new
         | Access    Exp         -- new
         | Assign    Exp Exp   -- new
  deriving (Eq, Show)
  
type Env = [(String, Value)]
--END:Summ7

--BEGIN:Summ9
--BEGIN:Stat11
evaluate :: Exp -> Env -> Stateful Value
--END:Stat11
evaluate (Literal v) env mem  = (v, mem)

evaluate (Unary op a) env mem =
  let (av, mem') = evaluate a env mem in
    (unary op av, mem')

--BEGIN:Sema27
evaluate (Binary op a b) env mem =
  let (av, mem') = evaluate a env mem in
    let (bv, mem'') = evaluate b env mem' in
      (binary op av bv, mem'')
--END:Sema27

evaluate (If a b c) env mem =
  let (BoolV test, mem') = evaluate a env mem in
    evaluate (if test then b else c) env mem'

evaluate (Variable x) env mem = (fromJust (lookup x env), mem)

evaluate (Declare x e body) env mem =
  let (ev, mem') = evaluate e env mem
      newEnv = (x, ev) : env
  in
    evaluate body newEnv mem'

evaluate (Function x body) env mem = (ClosureV x body env, mem)

evaluate (Call f a) env mem  =
  let (ClosureV x body closeEnv, mem') = evaluate f env mem
      (av, mem'') = evaluate a env mem'
      newEnv = (x, av) : closeEnv
  in
    evaluate body newEnv mem''

evaluate (Seq a b) env mem  =
  let (_, mem') = evaluate a env mem in
    evaluate b env mem'

--END:Summ9 BEGIN:Summ11 BEGIN:Sema20
evaluate (Mutable e) env mem =
  let (ev, mem') = evaluate e env mem in
    (AddressV (length mem'), mem' ++ [ev])
--END:Sema20

--BEGIN:Sema23
evaluate (Access a) env mem =
  let (AddressV i, mem') = evaluate a env mem in
    (access i mem', mem')
--END:Sema23

--BEGIN:Sema25
evaluate (Assign a e) env mem =
  let (AddressV i, mem') = evaluate a env mem in
    let (ev, mem'') = evaluate e env mem' in
      (ev, update i ev mem'')
--END:Sema25
--END:Summ11

execute exp = v
  where (v, _) = evaluate exp [] []
  
-- same as in IntBool.hs

unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary Add (IntV a)  (IntV b)  = IntV (a + b)
binary Sub (IntV a)  (IntV b)  = IntV (a - b)
binary Mul (IntV a)  (IntV b)  = IntV (a * b)
binary Div (IntV a)  (IntV b)  = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV a)  (IntV b)  = BoolV (a < b)
binary LE  (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE  (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT  (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ  a         b         = BoolV (a == b)
binary op  a         b         = error ("Invalid binary " ++ show op ++ " operation: " ++ show a ++ ", " ++ show b) 
