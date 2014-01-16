module Simple where

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

--BEGIN:Form10
instance Show Exp where
  show (Number i)      = show i
  show (Add a b)       = showBinary a "+" b
  show (Subtract a b)  = showBinary a "-" b
  show (Multiply a b)  = showBinary a "*" b
  show (Divide a b)  = showBinary a "/" b
showBinary a op b = show a ++ op ++ show b
--END:Form10
