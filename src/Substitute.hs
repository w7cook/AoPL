import Base

--BEGIN:Vari99
data Exp = Number   Int
         | Add      Exp Exp
         | Subtract Exp Exp
         | Multiply Exp Exp
         | Divide   Exp Exp
         | Variable String        -- added
--END:Vari99
   deriving Eq

instance Show Exp where
  show e = showExp 0 e
showExp level (Number i) = if i < 0 then paren (show i) else show i
showExp level (Add a b)       = showBinary level 1 a " + " b
showExp level (Subtract a b)  = showBinary level 1 a " - " b
showExp level (Multiply a b)  = showBinary level 2 a "*" b
showExp level (Divide a b)    = showBinary level 2 a "/" b
showExp level (Variable a)    = a
showBinary outer inner a op b =
  if inner < outer then paren result else result
     where result = showExp inner a ++ op ++ showExp inner b

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

--BEGIN:Mult4
e1 = [ ("x", 3), ("y", -1) ]
--END:Mult4

exp1 = Add x (Add (Multiply (Number 2) y) z)
check1 = check "subst-fold" (substitute e1 exp1) (substitute e1 exp1)

--BEGIN:Subs11
x = Variable "x"
y = Variable "y"
main'1 = do
  test (substitute1 ("x", 5)) (Add x (Number 2))
  test (substitute1 ("x", 5)) (Number 2)
  test (substitute1 ("x", 5)) x
  test (substitute1 ("x", 5)) (Add (Multiply x x) x)
  test (substitute1 ("x", 5)) (Add x y)
--END:Subs11

--BEGIN:Mult10
z = Variable "z"
main'2 = do
  test (substitute e1) (Add x y)
  test (substitute e1) (Number 2)
  test (substitute e1) x
  test (substitute e1) (Add (Multiply x x) x)
  test (substitute e1) (Add x (Add (Multiply (Number 2) y) z))
--END:Mult10

main = do
  main'1
  main'2
