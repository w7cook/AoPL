import Prelude hiding (LT, GT, EQ, id)
import Base
import Control.Exception
import IntBool
import IntBoolParse
import IntBoolTyping

--BEGIN:More19
t1 = "4"
t2 = "-4 - 6"
t3 = "if (3==6) -2 else -7"
t4 = "3*(8 + 5)"
t5 = "3 + 8 * 2"
--END:More19

--BEGIN:More98
t6 = "if (3 > 3*(8 + 5)) 1 else 0"
t7 = "2 + (if (3 <= 0) 9 else -5)"
--END:More98

test1 fun = do
  print $ fun (parseExp t1)
  print $ fun (parseExp t2)
  print $ fun (parseExp t3)
  print $ fun (parseExp t4)
  print $ fun (parseExp t5)
  print $ fun (parseExp t6)
  print $ fun (parseExp t7)

--BEGIN:Type6
err1 = "if (true) 5 else 8"
err2 = "3 + true"
err3 = "3 || true"
err4 = "-true"
--END:Type6

test2 fun = do
  print $ fun (parseExp err1)
  print $ fun (parseExp err2)
  print $ fun (parseExp err3)
  print $ fun (parseExp err4)

main = do
  tagged "More23" (test1 execute `catch` showError)
  tagged "Type6run" (test2 execute `catch` showError)
  tagged "TypeCheck1" (test1 (\e-> typeCheck e []) `catch` showError)
  tagged "TypeCheck2" (test2 (\e-> typeCheck e []) `catch` showError)
  