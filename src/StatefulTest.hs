import Base
import Stateful

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

main'2 = do
  print testMul10

main = do
  tagged "Upda11" main'2
  
