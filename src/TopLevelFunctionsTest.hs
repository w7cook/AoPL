import Base
import Prelude hiding (LT, GT, EQ)
import TopLevelFunctions
import TopLevelFunctionsParse

p1 = parseExp "function power(n, m) { if (m == 0) 1 else n*power(n, m-1) } power(3, 4)"

main = do
  tagged "Top22" (test "execute" execute p1)
  