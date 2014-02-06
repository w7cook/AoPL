import Prelude hiding (LT, GT, EQ, id)
import Declare
import DeclareParse
import Base

--BEGIN:More19
t1 = "4"
t2 = "-4 - 6"
t3 = "var x = 3; x"
t4 = "var x = 3; var y = x*x; x"
t5 = "var x = 3; var x = x*x; x"
--END:More19

--BEGIN:More98
t6 = "var x = 3; var y = x*x; y"
t7 = "2 + (var x =2; x)"
--END:More98

test1 = do
  test "execute" execute (parseExp t1)
  test "execute" execute (parseExp t2)
  test "execute" execute (parseExp t3)
  test "execute" execute (parseExp t4)
  test "execute" execute (parseExp t5)
  test "execute" execute (parseExp t6)
  test "execute" execute (parseExp t7)

main = do
  tagged "DeclTest1" test1
  
  