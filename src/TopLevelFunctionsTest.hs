import Base
import Prelude hiding (LT, GT, EQ)
import IntBool hiding (Exp, Literal, Unary, Binary, If, Variable, Let, evaluate, execute)
import TopLevelFunctions

--BEGIN:Top22
f1 = Function ["n", "m"]
      (If (Binary EQ (Variable "m") (Literal (IntV 0)))
          (Literal (IntV 1))
          (Binary Mul
            (Variable "n")
            (Call "power" [Variable  "n",
                           Binary  Sub (Variable  "m")
                                         (Literal (IntV 1))])))

p1 = Program [("power", f1)]
             (Call "power" [Literal (IntV 3),
                            Literal (IntV 4)])
--END:Top22

--BEGIN:A13
testP1 = Program
  [("f", Function ["x"]
           (Binary Mul (Variable "x")
                       (Variable "x")))]
  (Call "f" [Literal (IntV 10)])
--END:A13

main = do
  tagged "TLF1" (test "execute" execute testP1)
  tagged "TLF2" (test "execute" execute p1)
  