import Prelude hiding (LT, GT, EQ, id)
import IntBool
import Base

--BEGIN:More19
-- 4
t1 = Literal (IntV 4)
-- -4 - 6
t2 = Binary Sub (Literal (IntV (-4))) (Literal (IntV 6))
-- 3 - -2 - -7
t3 = Binary Sub (Literal (IntV 3))
                (Binary Sub (Literal (IntV (-2))) (Literal (IntV (-7))))
-- 3*(8 + 5)
t4 = Binary Mul (Literal (IntV 3))
                (Binary Add (Literal (IntV 8)) (Literal (IntV 5)))
-- 3 + 8 * 2
t5 = Binary Add (Literal (IntV 3))
                (Binary Mul (Literal (IntV 8)) (Literal (IntV 2)))
--END:More19

--BEGIN:More98
-- if 3 > 3*(8 + 5) then 1 else 0
t6 = If (Binary GT (Literal (IntV 3)) t4)
        (Literal (IntV 1))
        (Literal (IntV 0))
-- 2 + (if 3 <= 0 then 9 else -5)
t7 = Binary Add (Literal (IntV 2))
                (If (Binary LE (Literal (IntV 3))
                                        (Literal (IntV 0)))
                    (Literal (IntV 9))
                    (Literal (IntV (-5))))
--END:More98

test1 = do
  test "execute" execute t1
  test "execute" execute t2
  test "execute" execute t3
  test "execute" execute t4
  test "execute" execute t5
  test "execute" execute t6
  test "execute" execute t7

--BEGIN:Type6
-- if 3 then 5 else 8
err1 = If (Literal (IntV 3)) (Literal (IntV 5)) (Literal (IntV 8))
-- 3 + True
err2 = Binary Add (Literal (IntV 3)) (Literal (BoolV True))
-- 3 || True
err3 = Binary Or (Literal (IntV 3)) (Literal (BoolV True))
-- -True
err4 = Unary Neg (Literal (BoolV True))
--END:Type6

test2 = do
  test "execute" execute err1
  test "execute" execute err2
  test "execute" execute err3
  test "execute" execute err4

main = do
  tagged "More23" test1
  tagged "Type6run" test2
  
  