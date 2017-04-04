import Base
import Prelude hiding (LT, GT, EQ)
import CheckedMonad
import FirstClassFunctionsParse

execute exp = show (evaluate exp [])

main = testMain parseExp execute
    