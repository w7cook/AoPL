import Base
import IncorrectFunctions
import IncorrectFunctionsParse

execute exp = show (evaluate exp emptyEnv)

--BEGIN:Prob3
teste1 = let add = \a -> (\b -> b + a) in add 3 2
--END:Prob3

main = testMain parseExp execute