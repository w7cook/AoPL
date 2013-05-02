import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

data Value = IntV  Int
           | BoolV Bool
 deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving Eq

data UnaryOp = Neg | Not
  deriving Eq

--BEGIN:Top15
type FunEnv = [(String, Function)]
data Function = Function [String] Exp
--END:Top15

--BEGIN:Top18
data Program = Program FunEnv Exp
--END:Top18

--BEGIN:Top22
f1 = Function ["n", "m"]
      (If (Binary EQ (Variable "m") (Literal (IntV 0)))
          (Literal (IntV 1))
          (Binary Mul
            (Variable "n")
            (Call "power" [Variable  "n",
                           Binary  Sub (Variable  "m")
                                         (Literal (IntV 1))])))

p1 = Program [("power", f1)]
             (Call "power" [Literal (IntV 3),
                            Literal (IntV 4)])
--END:Top22

--BEGIN:Eval41
execute :: Program -> Value
execute (Program funEnv main) = evaluate main [] funEnv
--END:Eval41

--BEGIN:Summ12
data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Let       String Exp Exp
         | Call      String [Exp]
      
type Env = [(String, Value)]
   
evaluate :: Exp -> Env -> FunEnv -> Value
evaluate exp env funEnv = eval exp where
    eval (Literal v)      = v
    eval (Unary op a)     = unary op (eval a)
    eval (Binary op a b)  = binary op (eval a) (eval b)
    eval (If a b c)       = if fromBoolV (eval a)
                            then eval b
                            else eval c
    eval (Variable x)     = fromJust (lookup x env)
    eval (Let x exp body) = evaluate body newEnv funEnv
      where newEnv = (x, eval exp) : env
    eval (Call fun args)   = evaluate body newEnv funEnv
      where Function xs body = fromJust (lookup fun funEnv)
            newEnv = zip xs [eval a | a <- args]
--END:Summ12

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

fromBoolV (BoolV b) = b

--BEGIN:A13
testP1 = Program
  [("f", Function ["x"]
           (Binary Mul (Variable "x")
                       (Variable "x")))]
  (Call "f" [Literal (IntV 10)])
--END:A13

main = do
  print (execute testP1)
   