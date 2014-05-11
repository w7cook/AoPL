module TopLevelFunctions where

import Prelude hiding (LT, GT, EQ, showList)
import Data.Maybe
import Value

--BEGIN:Top15
type FunEnv = [(String, Function)]
data Function = Function [String] Exp
--END:Top15

--BEGIN:Top18
data Program = Program FunEnv Exp
--END:Top18

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
         | Declare   String Exp Exp
         | Call      String [Exp]

            
--BEGIN:Eval59
evaluate :: Exp -> Env -> FunEnv -> Value
--END:Eval59
evaluate (Literal v) env funEnv      = v

evaluate (Unary op a) env funEnv     = 
  unary op (evaluate a env funEnv)

evaluate (Binary op a b) env funEnv  = 
  binary op (evaluate a env funEnv) (evaluate b env funEnv)

evaluate (If a b c) env funEnv       = 
  let BoolV test = evaluate a env funEnv in
    if test then evaluate b env funEnv
            else evaluate c env funEnv

evaluate (Variable x) env funEnv     = fromJust (lookup x env)

evaluate (Declare x exp body) env funEnv = evaluate body newEnv funEnv
  where newEnv = (x, evaluate exp env funEnv) : env

--BEGIN:Eval31
evaluate (Call fun args) env funEnv   = evaluate body newEnv funEnv
  where Function xs body = fromJust (lookup fun funEnv)
        newEnv = zip xs [evaluate a env funEnv | a <- args]
--END:Summ12 END:Eval31


paren x = "(" ++ x ++ ")"

instance Show Program where
  show (Program fenv exp) = (showList "\n" (map showFun fenv)) ++ "\n" ++ showExp 0 exp

showFun (name, Function args body) = "function " ++ name ++ "(" ++ showList ", " args ++ ") {\n  " ++ showExp 0 body ++ "\n}"

instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp level (Literal i)      = show i
showExp level (Variable x)    = x
showExp level (Declare x a b) = 
  if level > 0 then paren result else result
    where result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c)    = 
  if level > 0 then paren result else result
    where result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ " else " ++ showExp 0 c
showExp level (Unary Neg a)    = "-" ++ showExp 99 a
showExp level (Unary Not a)    = "!" ++ showExp 99 a
showExp level (Binary op a b)  = showBinary level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where levels = [(Or, 1), (And, 2), (GT, 3), (LT, 3), (LE, 3), (GE, 3), (EQ, 3), 
                  (Add, 4), (Sub, 4), (Mul, 5), (Div, 5)] 
        names = [(Or, "||"), (And, "&&"), (GT, ">"), (LT, "<"), (LE, "<="), (GE, ">="), (EQ, "=="), 
                  (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/")] 
showExp level (Call f args)    = f ++ "(" ++ showList ", " (map (showExp 0) args) ++ ")"

showList sep [] = ""
showList sep [e] = e
showList sep (e:es) = e ++ sep ++ showList sep es

showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b
