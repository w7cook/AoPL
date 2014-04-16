import Prelude hiding (LT, GT, EQ, id)
import Base
import IntBool
import IntBoolParse
import IntBoolTyping

--BEGIN:More19
t1 = "4"
t2 = "-4 - 6"
t3 = "if (3==6) -2; else -7"
t4 = "3*(8 + 5)"
t5 = "3 + 8 * 2"
--END:More19

--BEGIN:More98
t6 = "if (3 > 3*(8 + 5)) 1; else 0"
t7 = "2 + (if (3 <= 0) 9; else -5)"
--END:More98

test1 msg fun = do
  test msg fun (parseExp t1)
  test msg fun (parseExp t2)
  test msg fun (parseExp t3)
  test msg fun (parseExp t4)
  test msg fun (parseExp t5)
  test msg fun (parseExp t6)
  test msg fun (parseExp t7)

--BEGIN:Type6
err1 = "if (3) 5; else 8"
err2 = "3 + true"
err3 = "3 || true"
err4 = "-true"
--END:Type6

test2 msg fun = do
  test msg fun (parseExp err1)
  test msg fun (parseExp err2)
  test msg fun (parseExp err3)
  test msg fun (parseExp err4)

main = do
  tagged "More23" (test1 "execute" execute)
  tagged "Type6run" (test2 "execute" execute)
  tagged "TypeCheck1" (test1 "typeCheck" (\e-> typeCheck e []))
  tagged "TypeCheck2" (test2 "typeCheck" (\e-> typeCheck e []))
  