import Base
import Prelude hiding (LT, GT, EQ)
import TopLevelFunctions
import TopLevelFunctionsParse

--BEGIN:Eval41
execute :: Program -> String
execute (Program funEnv main) = show (evaluate main [] funEnv)
--END:Eval41

main = tagged "Top22" (testMain parseExp execute)
  