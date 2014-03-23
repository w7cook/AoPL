import Base
import Prelude hiding (LT, GT, EQ)
import FirstClassFunctions
import FirstClassFunctionsParse

p1 = parseExp ("var T = function(a) { function(b) { a } };"++
               "var F = function(a) { function(b) { b } };"++
               "var not = function(b) { b(F)(T) };"++
               "not(F)")
               
p2 = parseExp (
      "var x = 5;"++
      "var f = function(y) { x - y };"++
      "var x = f(9);"++
      "f(x)")

p3 = parseExp (
      "var x = 5;"++
      "var f = function(y) { var y = x * y; function(x) { x + y } };"++
      "var g = f(2);"++
      "g(5)")

p4 = parseExp (
      "var comp = function(f) { function(g) { function(x) { f(g(x)) }}};"++
      "var inc = function(x) { x + 1 };"++
      "var square = function(x) { x * x };"++
      "var f = comp(inc)(square);"++
      "f(5)")

p5 = parseExp (
      "var map = function(f) { function(x) { function(y) { f(x) + f(y) }}};"++
      "var g = function(x) { x + 1 };"++
      "map(g)(3)(4)")
     
main = do
  tagged "FirstClassT1" (do
  	test "execute" execute p1
  	test "execute" execute p2
  	test "execute" execute p3
  	test "execute" execute p4
  	test "execute" execute p5
   )
  	