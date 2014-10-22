import Base
import Prelude hiding (LT, GT, EQ)
import Assignment2
import Assignment2Parse

p1 = parseExp ("var T = function (a)  { function (b)  { a } };"++
               "var F = function (a)  { function (b)  { b } };"++
               "var not = function (b)  { b(F)(T) };"++
               "not(F)(100)(-5)")
     
p2 = parseExp ("var f = function (a, (b)) { a + 2 * b};"++
							"f(3, (4)) - f(5, (2))")
							
p3 = parseExp ("var f = function (a) { if (a == 0) 1 else a*f(a-1) };"++
							"(f(5), f(10))")
							
main = do
  test "execute" execute p1
  test "execute" execute p2
  test "execute" execute p3

