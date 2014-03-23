import Base
import Stateful
import StatefulParse

--BEGIN:Upda7
mul10 addr mem =
  let IntV n = access addr mem in
    update addr (IntV (10 * n)) mem
--END:Upda7

--BEGIN:Upda9
testMul10 = mul10 1 [IntV 3, IntV 4, IntV 5, IntV 6]
--END:Upda9

--BEGIN:Upda13
mul10 :: Int -> Memory -> Memory
--END:Upda13

t1 = parseExp ("var x = mutable 3;"++
     "var y = mutable true;"++
     "if (@y) (x = @x + 1); else x;"++
     "@x")

main'2 = do
  print testMul10

main = do
  tagged "Upda11" main'2
  test "evaluate" execute t1
  
  
