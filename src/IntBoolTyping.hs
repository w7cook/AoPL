module IntBoolTyping where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Value
import IntBool
import Data.Maybe

data Type = TInt | TBool 
   deriving (Eq, Show)

type TypeEnv = [(String, Type)]
   
--BEGIN:Type12
-- Type-check an expression in a typing environment
typeCheck :: Exp -> TypeEnv -> Type
typeCheck (Literal (IntV _)) env  = TInt
typeCheck (Literal (BoolV _)) env = TBool
typeCheck (Unary op a) env     = checkUnary op (typeCheck a env)
typeCheck (Binary op a b) env  = checkBinary op (typeCheck a env) (typeCheck b env)
typeCheck (Variable x) env     = fromJust (lookup x env)
typeCheck (Declare x exp body) env = typeCheck body newEnv
  where newEnv = (x, typeCheck exp env) : env
typeCheck (If a b c) env =
  if TBool == typeCheck a env then
    if typeCheck b env == typeCheck c env then
      typeCheck b env
    else
      error ("Result types are not the same in " ++ show b ++ ", " ++ show c)
  else
    error ("Conditional must return a boolean: " ++ show a)
--END:Type12

--BEGIN:Type16
checkUnary Not TBool = TBool
checkUnary Neg TInt  = TInt
checkUnary op  a     = error ("Mismatched argument for " ++ " " ++ show op ++ " " ++ show a)

checkBinary Add TInt  TInt  = TInt
checkBinary Sub TInt  TInt  = TInt
checkBinary Mul TInt  TInt  = TInt
checkBinary Div TInt  TInt  = TInt
checkBinary And TBool TBool = TBool
checkBinary Or  TBool TBool = TBool
checkBinary LT  TInt  TInt  = TBool
checkBinary LE  TInt  TInt  = TBool
checkBinary GE  TInt  TInt  = TBool
checkBinary GT  TInt  TInt  = TBool
checkBinary EQ  a     b     | a == b = TBool
checkBinary op  a     b      = error ("Mismatched binary types for " ++ show a ++ " " ++ show op ++ " " ++ show b)

--END:Type16