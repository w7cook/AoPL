module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

import Base

--BEGIN:More3
data Value = IntV  Int
           | BoolV Bool
 deriving (Eq, Show)
--END:More3

--BEGIN:More99
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Show, Eq)

data UnaryOp = Neg | Not
  deriving (Show, Eq)

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
  deriving (Show, Eq)
--END:More99

--BEGIN:More12
type Env = [(String, Value)]

-- Evaluate an expression in an environment
evaluate :: Exp -> Env -> Value
evaluate exp env = eval exp where
    eval (Literal v)      = v
    eval (Unary op a)     = unary op (eval a)
    eval (Binary op a b)  = binary op (eval a) (eval b)
    eval (Variable x)     = fromJust (lookup x env)
    eval (Let x exp body) = evaluate body newEnv
      where newEnv = (x, eval exp) : env
--END:More12 BEGIN:More14
    eval (If a b c)      = if fromBoolV (eval a)
                           then eval b
                           else eval c
fromBoolV (BoolV b) = b
--END:More14

--BEGIN:More16
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
--END:More16

--BEGIN:More19
-- 4
t1 = Literal (IntV 4)
-- -4 - 6
t2 = Binary Sub (Literal (IntV (-4))) (Literal (IntV 6))
-- 3 - (-2) - (-7)
t3 = Binary Sub (Literal (IntV 3))
                (Binary Sub (Literal (IntV (-2))) (Literal (IntV (-7))))
-- 3*(8 + 5)
t4 = Binary Mul (Literal (IntV 3))
                (Binary Add (Literal (IntV 8)) (Literal (IntV 5)))
-- 3 + 8 * 2
t5 = Binary Add (Literal (IntV 3))
                (Binary Mul (Literal (IntV 8)) (Literal (IntV 2)))
--END:More19

--BEGIN:More98
-- if 3 > 3*(8 + 5) then 1 else 0
t6 = If (Binary GT (Literal (IntV 3)) t4)
        (Literal (IntV 1))
        (Literal (IntV 0))
-- 2 + (if 3 <= 0 then 9 else -5)
t7 = Binary Add (Literal (IntV 2))
                (If (Binary LE (Literal (IntV 3))
                                        (Literal (IntV 0)))
                    (Literal (IntV 9))
                    (Literal (IntV (-5))))
--END:More98

main = do
  test (\e-> evaluate e []) t1
  test (\e-> evaluate e []) t2
  test (\e-> evaluate e []) t3
  test (\e-> evaluate e []) t4
  test (\e-> evaluate e []) t5
  test (\e-> evaluate e []) t6
  test (\e-> evaluate e []) t7

--BEGIN:Type6
-- if 3 then 5 else 8
err1 = If (Literal (IntV 3)) (Literal (IntV 5)) (Literal (IntV 8))
-- 3 + True
err2 = Binary Add (Literal (IntV 3)) (Literal (BoolV True))
-- 3 || True
err3 = Binary Or (Literal (IntV 3)) (Literal (BoolV True))
-- -True
err4 = Unary Neg (Literal (BoolV True))
--END:Type6

