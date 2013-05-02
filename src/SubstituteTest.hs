import Base
import Substitute

--BEGIN:Mult4
e1 = [ ("x", 3), ("y", -1) ]
--END:Mult4

exp1 = Add x (Add (Multiply (Number 2) y) z)
check1 = check "subst-fold" (substitute e1 exp1) (substitute e1 exp1)

--BEGIN:Subs11
x = Variable "x"
y = Variable "y"
main'1 = do
  test (substitute1 ("x", 5)) (Add x (Number 2))
  test (substitute1 ("x", 5)) (Number 2)
  test (substitute1 ("x", 5)) x
  test (substitute1 ("x", 5)) (Add (Multiply x x) x)
  test (substitute1 ("x", 5)) (Add x y)
--END:Subs11

--BEGIN:Mult10
z = Variable "z"
main'2 = do
  test (substitute e1) (Add x y)
  test (substitute e1) (Number 2)
  test (substitute e1) x
  test (substitute e1) (Add (Multiply x x) x)
  test (substitute e1) (Add x (Add (Multiply (Number 2) y) z))
--END:Mult10

main = do
  tagged "Subs13" main'1
  tagged "Mult11" main'2
  