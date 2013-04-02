module ErrorChecking where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe
import FirstClassFunctions hiding (evaluate)

--BEGIN:Hand5
data Checked a = Good a | Error String
 deriving Show
--END:Hand5

--BEGIN:Hand8
evaluate :: Exp -> Env -> Checked Value
evaluate exp env = eval exp where
    eval (Literal v)      = Good v
--END:Hand8 BEGIN:Hand11
    eval (Variable x)     =
       case lookup x env of
         Nothing -> Error ("Variable " ++ x ++ " undefined")
         Just v  -> Good v
--END:Hand11 BEGIN:Hand15
    eval (Binary op a b)  =
        case eval a of
          Error msg -> Error msg
          Good av ->
            case eval b of
              Error msg -> Error msg
              Good bv ->
                checked_binary op av bv
--END:Hand15

--BEGIN:Hand17
checked_unary :: UnaryOp -> Value -> Checked Value
checked_unary Not (Bool b) = Good (Bool (not b))
checked_unary Neg (Int i)  = Good (Int (-i))

checked_binary :: BinaryOp -> Value -> Value -> Checked Value
checked_binary Add (Int a)  (Int b)  = Good (Int (a + b))
checked_binary Sub (Int a)  (Int b)  = Good (Int (a - b))
checked_binary Mul (Int a)  (Int b)  = Good (Int (a * b))
checked_binary Div (Int a)  (Int 0)  = Error "Divide by zero"
checked_binary Div (Int a)  (Int b)  = Good (Int (a `div` b))
checked_binary And (Bool a) (Bool b) = Good (Bool (a && b))
checked_binary Or  (Bool a) (Bool b) = Good (Bool (a || b))
checked_binary LT  (Int a)  (Int b)  = Good (Bool (a < b))
checked_binary LE  (Int a)  (Int b)  = Good (Bool (a <= b))
checked_binary GE  (Int a)  (Int b)  = Good (Bool (a >= b))
checked_binary GT  (Int a)  (Int b)  = Good (Bool (a > b))
checked_binary EQ  a        b        = Good (Bool (a == b))
checked_binary _   _        _        = Error "Type error"
--END:Hand17


--BEGIN:Hand28
testUBV = evaluate (Variable "x") []
--END:Hand28

--BEGIN:Hand32
testDBZ2 = evaluate (Binary Div (Literal (Int 3)) (Literal (Int 0)) ) []
--END:Hand32

main = do
  print testUBV
  print testDBZ2