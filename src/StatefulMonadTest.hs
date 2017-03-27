import Prelude hiding (LT, GT, EQ, id)
import StatefulMonad
import Base
import StatefulParse

execute exp = show (runStateful (evaluate exp []))

main = testMain parseExp execute
      
  