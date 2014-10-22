module FirstClassFunctionsTyping where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe
import Data.List
import Operators

--BEGIN:TypeA42
data Type = IntT
           | BoolT
           | FunT Type Type  -- new
  deriving (Eq, Show)
--END:TypeA42

--BEGIN:TypeA42
data Value = IntV  Int
           | BoolV Bool
  deriving (Eq, Show)
--END:TypeA42

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   String Exp Exp
         | Function  String Type Exp      -- changed
         | Call      Exp Exp         
  deriving (Eq, Show)


type TypeEnv = [(String, Type)]

--BEGIN:TypeSumm14

--BEGIN:TypeSummDecl
typeCheck :: Exp -> TypeEnv -> Type
--END:TypeSummDecl
typeCheck (Literal (IntV _)) env = IntT
typeCheck (Literal (BoolV _)) env = BoolT

typeCheck (Unary op a) env = 
  checkUnary op (typeCheck a env)

typeCheck (Binary op a b) env = 
  checkBinary op (typeCheck a env) (typeCheck b env)

typeCheck (If a b c) env = 
  if BoolT /= typeCheck a env then
    error ("Conditional must return a boolean: " ++ show a)
  else if typeCheck b env /= typeCheck c env then
    error ("Result types are not the same in " ++ show b ++ ", " ++ show c)
  else
    typeCheck b env

typeCheck (Variable x) env = fromJust (lookup x env)

typeCheck (Declare x exp body) env = typeCheck body newEnv
  where newEnv = (x, typeCheck exp env) : env

typeCheck (Function x t body) env = FunT t (typeCheck body newEnv)     -- new
  where newEnv = (x, t) : env

--BEGIN:TypeA47
typeCheck (Call fun arg) env = 
  case typeCheck fun env of
    FunT a b -> if a /= typeCheck arg env then
                  error "Invalid argument type"
                else
                  b
    _ -> error "Expected function"
--END:TypeSumm14 END:TypeA47

execute exp = typeCheck exp []

-- same as IntBoolTyping.hs

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
