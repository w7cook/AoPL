
import IncorrectFunctions

--BEGIN:Prob3
teste1 = let add = \a -> (\b -> b + a) in add 3 2
--END:Prob3

--BEGIN:Prob5
testE2 =
 Declare"add" (Literal (Function "a"
               (Literal (Function "b"
                  (Binary Add (Variable "b")
                              (Variable "a"))))))
               (Call (Call (Variable "add")
                               (Literal (IntV 3)))
                     (Literal (IntV 2)))
--END:Prob5

--BEGIN:A15
testP2 =
 Declare "f" (Literal (Function "x"
                        (Binary Mul (Variable "x")
                                    (Variable "x"))))
   (Call (Variable "f") (Literal (IntV 10)))
--END:A15

emptyEnv = \x->Nothing

main = do
  print (evaluate testE2 emptyEnv)
  print (evaluate testP2 emptyEnv)
  