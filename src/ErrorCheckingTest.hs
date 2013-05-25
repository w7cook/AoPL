
import FirstClassFunctions hiding (evaluate)
import ErrorChecking

--BEGIN:Hand28
testUBV = evaluate (Variable "x") []
--END:Hand28

--BEGIN:Hand32
testDBZ2 = evaluate (Binary Div (Literal (IntV 3)) (Literal (IntV 0)) ) []
--END:Hand32

main = do
  print testUBV
  print testDBZ2