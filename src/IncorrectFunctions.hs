import Data.Maybe
import Base
import FunctionalEnvironment hiding (main)

--BEGIN:A32
data Value = IntV  Int
           | BoolV Bool
           | Function String Exp  -- new
  deriving (Eq, Show)
--END:A32

--BEGIN:A15
testP2 =
 Let "f" (Literal (Function "x"
                      (Binary Mul (Variable "x")
                                  (Variable "x"))))
   (Call (Variable "f") (Literal (IntV 10)))
--END:A15

data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
         | Call      Exp Exp         -- changed
  deriving (Eq, Show)

type Env = String -> Maybe Value

evaluate :: Exp -> Env -> Value
evaluate exp env = eval exp
  where  -- TODO: not needed to show this code here?
    eval (Literal v)      = v
    eval (Unary op a)     = unary op (eval a)
    eval (Binary op a b)  = binary op (eval a) (eval b)
    eval (If a b c)       = if fromBoolV (eval a)
                            then eval b
                            else eval c
    eval (Let x exp body) = evaluate body newEnv
      where newEnv = bindF x (eval exp) env
    eval (Variable x)     = fromJust (env x)
    --BEGIN:A35
    eval (Call fun arg)   = evaluate body newEnv
      where Function x body = eval fun
            newEnv = bindF x (eval arg) env
    --END:A35

fromBoolV (BoolV b) = b
unary op (a) = (unary op a)
binary op (a) (b) = (binary op a b)

--BEGIN:Prob3
teste1 = let add = \a -> (\b -> b + a) in add 3 2
--END:Prob3

--BEGIN:Prob5
testE2 =
 Let "add" (Literal (Function "a"
             (Literal (Function "b"
                (Binary Add (Variable "b")
                            (Variable "a"))))))
             (Call (Call (Variable "add")
                             (Literal (IntV 3)))
                   (Literal (IntV 2)))
--END:Prob5

main = do
  print (evaluate testE2 (\x->Nothing))

