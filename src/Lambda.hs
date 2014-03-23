module Lambda where
import Base

data Exp = Var String
         | Abs String Exp
         | App Exp Exp


instance Show Exp where
    show exp = showExp False exp

showExp parens (Var x) = x
showExp parens (Abs x t) = 
   if parens then "(" ++ result ++ ")" else result
      where result = "\\" ++ x ++ "." ++ showExp False t
showExp parens (App (Abs s t) t2) = 
   if parens then "(" ++ result ++ ")" else result
      where result = showExp True (Abs s t) ++ " " ++ showExp True t2
showExp parens (App t1 t2) = 
   if parens then "(" ++ result ++ ")" else result
      where result = showExp False t1 ++ " " ++ showExp True t2


         