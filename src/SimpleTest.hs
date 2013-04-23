import Base
import Simple


--BEGIN:Abst1
-- 4
t1 = Number 4
--END:Abst1 BEGIN:Abst6
-- -5 + 6
t2 = Add (Number (-5)) (Number 6)
--END:Abst6 BEGIN:Abst15
-- 3 - (-2) - (-7)
t3 = Subtract (Subtract (Number 3) (Number (-2))) (Number (-7))
--END:Abst15 BEGIN:Abst16
-- 3 * (8 + 5)
t4 = Multiply (Number 3) (Add (Number 8) (Number 5))
--END:Abst16 BEGIN:Abst17
-- 3 + 8 * 2
t5 = Add (Number 3) (Multiply (Number 8) (Number 2))
--END:Abst17

--BEGIN:Erro3
testDBZ = evaluate (Divide (Number 8) (Number 0))
--END:Erro3

--BEGIN:Eval5
main'2 = do
  putStrLn "Evaluating the following expression:"
  putStr "  "
  print t3
  putStrLn "Produces the following result:"
  putStr "  "
  print (evaluate t3)
--END:Eval5

--BEGIN:Form8
main'3 = do
  test evaluate t1
  test evaluate t2
  test evaluate t3
  test evaluate t4
  test evaluate t5
--END:Form8

main = do
  main'2
  main'3

  