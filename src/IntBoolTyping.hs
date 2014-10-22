module IntBoolTyping where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Value
import Operators
import IntBool
import Data.Maybe

data Type = IntT | BoolT 
   deriving (Eq, Show)

type TypeEnv = [(String, Type)]
   
--BEGIN:Type12
-- Type-check an expression in a typing environment
typeCheck :: Exp -> TypeEnv -> Type
typeCheck (Literal (IntV _)) env  = IntT
typeCheck (Literal (BoolV _)) env = BoolT
typeCheck (Unary op a) env     = checkUnary op (typeCheck a env)
typeCheck (Binary op a b) env  = checkBinary op (typeCheck a env) (typeCheck b env)
typeCheck (Variable x) env     = fromJust (lookup x env)
typeCheck (Declare x exp body) env = typeCheck body newEnv
  where newEnv = (x, typeCheck exp env) : env
typeCheck (If a b c) env =
  if BoolT /= typeCheck a env then
    error ("Conditional must return a boolean: " ++ show a)
  else if typeCheck b env /= typeCheck c env then
    error ("Result types are not the same in " ++ show b ++ ", " ++ show c)
  else
    typeCheck b env
--END:Type12

--BEGIN:Type16
checkUnary Not BoolT = BoolT
checkUnary Neg IntT  = IntT
checkUnary op  a     = error ("Mismatched argument for " ++ 
                              show op ++ " " ++ show a)

checkBinary Add IntT  IntT  = IntT
checkBinary Sub IntT  IntT  = IntT
checkBinary Mul IntT  IntT  = IntT
checkBinary Div IntT  IntT  = IntT
checkBinary And BoolT BoolT = BoolT
checkBinary Or  BoolT BoolT = BoolT
checkBinary LT  IntT  IntT  = BoolT
checkBinary LE  IntT  IntT  = BoolT
checkBinary GE  IntT  IntT  = BoolT
checkBinary GT  IntT  IntT  = BoolT
checkBinary EQ  a     b     | a == b = BoolT
checkBinary op  a     b      = 
  error ("Mismatched binary types for " ++ 
         show a ++ " " ++ show op ++ " " ++ show b)
--END:Type16