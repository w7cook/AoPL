module Simple where
import Prelude hiding (exp)
import Base
import Text.Parsec

-- BEGIN:Abst3
data Exp = Number     Int
         | Add        Exp Exp
         | Subtract   Exp Exp
         | Multiply   Exp Exp
         | Divide     Exp Exp
-- END:Abst3

--BEGIN:Eval3
evaluate :: Exp -> Int
evaluate (Number i)      = i
evaluate (Add a b)       = evaluate a + evaluate b
evaluate (Subtract a b)  = evaluate a - evaluate b
evaluate (Multiply a b)  = evaluate a * evaluate b
evaluate (Divide a b)    = evaluate a `div` evaluate b
--END:Eval3

--BEGIN:Form17
instance Show Exp where
  show e = showExp 0 e
--END:Form17

--BEGIN:Form18
showExp level (Number i) = if i < 0 then paren (show i) else show i
showExp level (Add a b)       = showBinary level 1 a " + " b
showExp level (Subtract a b)  = showBinary level 1 a " - " b
showExp level (Multiply a b)  = showBinary level 2 a "*" b
showExp level (Divide a b)    = showBinary level 2 a "/" b

showBinary outer inner a op b =
  if inner < outer then paren result else result
     where result = showExp inner a ++ op ++ showExp inner b
--END:Form18
