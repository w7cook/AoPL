import Stateful hiding (Stateful, evaluate)
import StatefulMonad 

t0 = Let "x" (Literal (IntV 99)) (Variable "x")
t1 = Let "x" (Mutable (Literal (IntV 3)))
         (Access (Variable "x"))
         
main = do
  print (runStateful (evaluate (Literal (IntV 6)) []))
  print (runStateful (evaluate t0 []))
  print (runStateful (evaluate t1 []))