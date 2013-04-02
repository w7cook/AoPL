module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

import Base

--BEGIN:More3
data Value = Int  Int
           | Bool Bool
 deriving (Eq, Show)
--END:More3

--BEGIN:More99
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving Eq

data UnaryOp = Neg | Not
  deriving Eq

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
  deriving Eq
--END:More99

instance Show BinaryOp where
  show op = fromJust (lookup op [(Add, " + "), (Sub, " - "), (Mul, "*"), (Div, "/"), (And, "&"), (Or, " | "),
                                  (LT, " < "), (LE, " <= "), (GE, " >= "), (GT, " > "), (EQ, " = ")])
instance Show UnaryOp where
  show op = fromJust (lookup op [(Neg, "-"), (Not, "not ")])
instance Show Exp where
  show e = showExp 0 e
showExp level (Literal v)     = show v
showExp level (Variable a)    = a
showExp level (Unary op a)    = show op ++ showExp 99 a
showExp level (Binary op a b) = showBinary level (precedence op) a op b
showExp level (Let x exp body) =
  if 0 < level then paren result else result
     where result = "let " ++ x ++ " = " ++ showExp 0 exp ++ " in " ++ showExp 0 body
showExp level (If c a b) =
  if 0 < level then paren result else result
     where result = "if " ++ showExp 0 c ++ " then " ++ showExp 0 a ++ " else " ++ showExp 0 b
showBinary outer inner a op b =
  if inner < outer then paren result else result
     where result = showExp inner a ++ show op ++ showExp inner b
precedence op = fromJust (lookup op [(Add, 4), (Sub, 4), (Mul, 5), (Div, 5), (And, 2), (Or, 1),
                                     (LT, 3), (LE, 3), (GE, 3), (GT, 3), (EQ, 3)])

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
    eval (If a b c)      = if fromBool (eval a)
                           then eval b
                           else eval c
fromBool (Bool b) = b
--END:More14

--BEGIN:More16
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
--END:More16

--BEGIN:More19
-- 4
t1 = Literal (Int 4)
-- -4 - 6
t2 = Binary Sub (Literal (Int (-4))) (Literal (Int 6))
-- 3 - (-2) - (-7)
t3 = Binary Sub (Literal (Int 3))
                (Binary Sub (Literal (Int (-2))) (Literal (Int (-7))))
-- 3*(8 + 5)
t4 = Binary Mul (Literal (Int 3))
                (Binary Add (Literal (Int 8)) (Literal (Int 5)))
-- 3 + 8 * 2
t5 = Binary Add (Literal (Int 3))
                (Binary Mul (Literal (Int 8)) (Literal (Int 2)))
--END:More19

--BEGIN:More98
-- if 3 > 3*(8 + 5) then 1 else 0
t6 = If (Binary GT (Literal (Int 3)) t4)
        (Literal (Int 1))
        (Literal (Int 0))
-- 2 + (if 3 <= 0 then 9 else -5)
t7 = Binary Add (Literal (Int 2))
                (If (Binary LE (Literal (Int 3))
                                        (Literal (Int 0)))
                    (Literal (Int 9))
                    (Literal (Int (-5))))
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
err1 = If (Literal (Int 3)) (Literal (Int 5)) (Literal (Int 8))
-- 3 + True
err2 = Binary Add (Literal (Int 3)) (Literal (Bool True))
-- 3 || True
err3 = Binary Or (Literal (Int 3)) (Literal (Bool True))
-- -True
err4 = Unary Neg (Literal (Bool True))
--END:Type6

