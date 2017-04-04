import Prelude hiding (LT, GT, EQ, id)
import Base
import Control.Exception hiding (evaluate)
import IntBool
import IntBoolParse
import IntBoolTyping

execute exp = show (evaluate exp [])

main = testMain parseExp execute
   
  