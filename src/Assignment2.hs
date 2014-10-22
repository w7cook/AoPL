module Assignment2 where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

data Value = IntV  Int
           | BoolV Bool
           | ClosureV Pattern Exp Env   -- functions have patterns
           | TupleV [Value]             -- tuple value
  deriving (Eq, Show)

data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   Pattern Exp Exp
         | Function  Pattern Exp         -- functions have patterns
         | Tuple     [Exp]                  -- tuple expression
         | Call      Exp Exp         -- changed
  deriving (Eq, Show)

data Pattern = VarP String              -- variable patterns
             | TupleP [Pattern]         -- tuple patterns
	deriving (Eq, Show)
	
type Env = [(String, Value)]

evaluate :: Exp -> Env -> Value
evaluate (Literal v) env = v

evaluate (Unary op a) env = 
  unary op (evaluate a env)

evaluate (Binary op a b) env = 
  binary op (evaluate a env) (evaluate b env)

evaluate (If a b c) env = 
  let BoolV test = evaluate a env in
    if test then evaluate b env
            else evaluate c env

evaluate (Variable x) env = fromJust (lookup x env)

evaluate (Declare p exp body) env = evaluate body newEnv
  where newEnv = match p (evaluate exp newEnv) ++ env

evaluate (Function x body) env = ClosureV x body env     -- new

evaluate (Call fun arg) env = evaluate body newEnv    -- changed
  where ClosureV p body closeEnv = evaluate fun env
        newEnv = match p (evaluate arg env) ++ closeEnv

evaluate (Tuple es) env = TupleV [ evaluate e env | e <- es ]

match (VarP x) v = [(x, v)]
match (TupleP ps) (TupleV vs) = concat [ match p v | (p, v) <- zip ps vs ]
match _ _ = []
      
execute exp = evaluate exp []

-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

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



