import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

data Value = Int  Int
           | Bool Bool
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
      (If (Binary EQ (Variable "m") (Literal (Int 0)))
          (Literal (Int 1))
          (Binary Mul
            (Variable "n")
            (Call "power" [Variable  "n",
                           Binary  Sub (Variable  "m")
                                         (Literal (Int 1))])))

p1 = Program [("power", f1)]
             (Call "power" [Literal (Int 3),
                            Literal (Int 4)])
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
    eval (If a b c)       = if fromBool (eval a)
                            then eval b
                            else eval c
    eval (Variable x)     = fromJust (lookup x env)
    eval (Let x exp body) = evaluate body newEnv funEnv
      where newEnv = (x, eval exp) : env
    eval (Call fun args)   = evaluate body newEnv funEnv
      where Function xs body = fromJust (lookup fun funEnv)
            newEnv = zip xs [eval a | a <- args]
--END:Summ12

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

fromBool (Bool b) = b

--BEGIN:A13
testP1 = Program
  [("f", Function ["x"]
           (Binary Mul (Variable "x")
                       (Variable "x")))]
  (Call "f" [Literal (Int 10)])
--END:A13

main = do
  print (execute testP1)
   