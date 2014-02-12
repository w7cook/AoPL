
import Base
import FirstClassFunctions hiding (evaluate, execute)
import FirstClassFunctionsParse
import ErrorChecking

--BEGIN:Hand28
testUBV = execute (parseExp "x")
--END:Hand28

--BEGIN:Hand32
testDBZ2 = execute (parseExp "3 / 0")
--END:Hand32

main = do
  tagged "testUBV" (print testUBV)
  tagged "testDBZ2" (print testDBZ2)