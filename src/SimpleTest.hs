import Base
import Simple
import SimpleParse

--BEGIN:Abst1
-- 3 - -2 - -7
t1 = Subtract (Subtract (Number 3) (Number (-2))) (Number (-7))
--END:Abst1

--BEGIN:Erro3
testDBZ = evaluate (parseExp "8 / 0")
--END:Erro3

--BEGIN:Eval5
main'2 = do
  putStrLn "Evaluating the following expression:"
  putStr "  "
  print t1
  putStrLn "Produces the following result:"
  putStr "  "
  print (evaluate t1)
--END:Eval5

execute script = show (evaluate (parseExp script))

main = do
  tagged "Eval7" main'2

  