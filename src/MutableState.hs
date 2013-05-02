module MutableState where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe

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
  let (before, _:after) = splitAt addr mem in
    before ++ [val] ++ after
--END:Upda4

--BEGIN:Upda7
mul10 addr mem =
  let n = fromIntV (access addr mem) in
    update addr (toValue (10 * n)) mem
fromIntV (IntV n) = n
toValue n = (IntV n)
--END:Upda7

--BEGIN:Upda9
testMul10 = mul10 1 [toValue 3, toValue 4, toValue 5, toValue 6]
--END:Upda9

--BEGIN:Upda13
mul10 :: Int -> Memory -> Memory
--END:Upda13

--BEGIN:Stat8
type Stateful t = Memory -> (Value, Memory)
--END:Stat8

--BEGIN:Summ7
data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
         | Function  String Exp
         | Call      Exp Exp
         | Mutable   Exp         -- new
         | Access    Exp         -- new
         | Assign    Exp Exp   -- new
  deriving (Eq, Show)
  
type Env = [(String, Value)]
--END:Summ7

--BEGIN:Summ9
evaluate :: Exp -> Env -> Stateful Value
evaluate exp env mem = eval exp mem where
    eval (Literal v) mem    = (v, mem)
    eval (Unary op a) mem   =
      let (av, mem') = eval a mem in
        (unary op av, mem')
    eval (Binary op a b) mem =
      let (av, mem') = eval a mem in
        let (bv, mem'') = eval b mem' in
          (binary op av bv, mem'')
    eval (If a b c) mem =
      let (av, mem') = eval a mem in
        eval (if fromBoolV av then b else c) mem'
    eval (Variable x) mem = (fromJust (lookup x env), mem)
    eval (Let x e body) mem =
      let (ev, mem') = eval e mem
          newEnv = (x, ev) : env
      in
        evaluate body newEnv mem'
    eval (Function x body) mem = (ClosureV x body env, mem)
    eval (Call f a) mem  =
      let (ClosureV x body closeEnv, mem') = eval f mem
          (av, mem'') = eval a mem'
          newEnv = (x, av) : closeEnv
      in
          evaluate body newEnv mem''
--END:Summ9 BEGIN:Summ11
    eval (Mutable e) mem =
      let (ev, mem') = eval e mem in
        (AddressV (length mem'), mem' ++ [ev])
    eval (Access a) mem =
      let (AddressV i, mem') = eval a mem in
          (access i mem', mem')
    eval (Assign a e) mem =
      let (AddressV i, mem') = eval a mem in
        let (ev, mem'') = eval e mem' in
          (ev, update i ev mem'')
fromBoolV (BoolV b) = b
--END:Summ11

-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

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
