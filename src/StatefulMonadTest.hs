import Stateful hiding (Stateful, evaluate)
import StatefulMonad 

t0 = Declare "x" (Literal (IntV 99)) (Variable "x")
t1 = Declare "x" (Mutable (Literal (IntV 3)))
         (Access (Variable "x"))
         
main = do
  print (runStateful (evaluate (Literal (IntV 6)) []))
  print (runStateful (evaluate t0 []))
  print (runStateful (evaluate t1 []))