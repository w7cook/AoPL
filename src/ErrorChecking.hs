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
checked_unary Not (BoolV b) = Good (BoolV (not b))
checked_unary Neg (IntV i)  = Good (IntV (-i))

checked_binary :: BinaryOp -> Value -> Value -> Checked Value
checked_binary Add (IntV a)  (IntV b)  = Good (IntV (a + b))
checked_binary Sub (IntV a)  (IntV b)  = Good (IntV (a - b))
checked_binary Mul (IntV a)  (IntV b)  = Good (IntV (a * b))
checked_binary Div (IntV a)  (IntV 0)  = Error "Divide by zero"
checked_binary Div (IntV a)  (IntV b)  = Good (IntV (a `div` b))
checked_binary And (BoolV a) (BoolV b) = Good (BoolV (a && b))
checked_binary Or  (BoolV a) (BoolV b) = Good (BoolV (a || b))
checked_binary LT  (IntV a)  (IntV b)  = Good (BoolV (a < b))
checked_binary LE  (IntV a)  (IntV b)  = Good (BoolV (a <= b))
checked_binary GE  (IntV a)  (IntV b)  = Good (BoolV (a >= b))
checked_binary GT  (IntV a)  (IntV b)  = Good (BoolV (a > b))
checked_binary EQ  a         b         = Good (BoolV (a == b))
checked_binary _   _         _         = Error "Type error"
--END:Hand17


--BEGIN:Hand28
testUBV = evaluate (Variable "x") []
--END:Hand28

--BEGIN:Hand32
testDBZ2 = evaluate (Binary Div (Literal (IntV 3)) (Literal (IntV 0)) ) []
--END:Hand32

main = do
  print testUBV
  print testDBZ2