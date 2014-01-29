module Substitute where
import Base

--BEGIN:Vari99
data Exp = Number   Int
         | Add      Exp Exp
         | Subtract Exp Exp
         | Multiply Exp Exp
         | Divide   Exp Exp
         | Variable String        -- added
   deriving (Eq)
--END:Vari99

--BEGIN:Subst10
instance Show Exp where
  show e = showExp 0 e

showExp level (Number i)      = show i
showExp level (Add a b)       = showBinary level 1 a " + " b
showExp level (Subtract a b)  = showBinary level 1 a " - " b
showExp level (Multiply a b)  = showBinary level 2 a "*" b
showExp level (Divide a b)    = showBinary level 2 a "/" b
showExp level (Variable x)    = x

showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ op ++ showExp inner b
--END:Subst10

--BEGIN:Subs9
substitute1:: (String, Int) -> Exp -> Exp
substitute1 (var, val) exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) = if var == name
                          then Number val
                          else Variable name
--END:Subs9

--BEGIN:Mult6
type Env = [(String, Int)]
--END:Mult6

--BEGIN:Mult8
substitute :: Env -> Exp -> Exp
substitute env exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) =
    case lookup name env of
      Just val -> Number val
      Nothing  -> Variable name
--END:Mult8

--BEGIN:Mult14
substitute1R env exp = foldr substitute1 exp env
--END:Mult14


