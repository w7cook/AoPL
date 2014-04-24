
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

--BEGIN:Hand34
test3 = execute (parseExp "-true")
test4 = execute (parseExp "!4")
test5 = execute (parseExp "4 + true")
test6 = execute (parseExp "4 && false")
--END:Hand34

main = do
  tagged "testUBV" (print testUBV)
  tagged "testDBZ2" (print testDBZ2)
  print test3
  print test4
  print test5
  print test6
  