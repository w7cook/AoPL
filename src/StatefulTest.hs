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

 -- execute evaluates an expression in an empty environment
--BEGIN:DeclExec
executeX exp = show v 
  where (v, _) = evaluate exp [] []
--END:DeclExec

main = do
  tagged "Upda11" (print testMul10)
  testMain parseExp executeX
     