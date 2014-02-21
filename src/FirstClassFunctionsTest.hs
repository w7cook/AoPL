import Base
import Prelude hiding (LT, GT, EQ)
import FirstClassFunctions
import FirstClassFunctionsParse

p1 = parseExp ("var T = function(a) { function(b) { a } };"++
               "var F = function(a) { function(b) { b } };"++
               "var not = function(b) { b(F)(T) };"++
               "not(F)")
               
main = do
  tagged "FirstClassT1" (test "execute" execute p1)
