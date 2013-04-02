module MutableState where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe

--BEGIN:Addr11
data Value = Int  Int
           | Bool Bool
           | Closure String Exp Env
           | Address Int        -- new
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
  let n = fromInt (access addr mem) in
    update addr (toValue (10 * n)) mem
fromInt (Int n) = n
toValue n = (Int n)
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
        eval (if fromBool av then b else c) mem'
    eval (Variable x) mem = (fromJust (lookup x env), mem)
    eval (Let x e body) mem =
      let (ev, mem') = eval e mem
          newEnv = (x, ev) : env
      in
        evaluate body newEnv mem'
    eval (Function x body) mem = (Closure x body env, mem)
    eval (Call f a) mem  =
      let (Closure x body closeEnv, mem') = eval f mem
          (av, mem'') = eval a mem'
          newEnv = (x, av) : closeEnv
      in
          evaluate body newEnv mem''
--END:Summ9 BEGIN:Summ11
    eval (Mutable e) mem =
      let (ev, mem') = eval e mem in
        (Address (length mem'), mem' ++ [ev])
    eval (Access a) mem =
      let (Address i, mem') = eval a mem in
          (access i mem', mem')
    eval (Assign a e) mem =
      let (Address i, mem') = eval a mem in
        let (ev, mem'') = eval e mem' in
          (ev, update i ev mem'')
fromBool (Bool b) = b
--END:Summ11

-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

unary Not (Bool b) = Bool (not b)
unary Neg (Int i)  = Int (-i)

binary Add (Int a)  (Int b)  = Int (a + b)
binary Sub (Int a)  (Int b)  = Int (a - b)
binary Mul (Int a)  (Int b)  = Int (a * b)
binary Div (Int a)  (Int b)  = Int (a `div` b)
binary And (Bool a) (Bool b) = Bool (a && b)
binary Or  (Bool a) (Bool b) = Bool (a || b)
binary LT  (Int a)  (Int b)  = Bool (a < b)
binary LE  (Int a)  (Int b)  = Bool (a <= b)
binary GE  (Int a)  (Int b)  = Bool (a >= b)
binary GT  (Int a)  (Int b)  = Bool (a > b)
binary EQ  a        b        = Bool (a == b)
