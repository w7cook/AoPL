import Let

--BEGIN:Summ19
substitute1 (var, val) exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) = if var == name
                          then Number val
                          else Variable name
  subst (Let x exp body)  = Let x (subst exp) body'
    where body' = if x == var
                  then body
                  else subst body
--END:Summ19

--BEGIN:Summ4
evaluate :: Exp -> Int
evaluate (Number i)       = i
evaluate (Add a b)        = evaluate a + evaluate b
evaluate (Subtract a b)   = evaluate a - evaluate b
evaluate (Multiply a b)   = evaluate a * evaluate b
evaluate (Divide a b)     = evaluate a `div` evaluate b
evaluate (Let x exp body) = evaluate (substitute1 (x, evaluate exp) body)
--END:Summ4
