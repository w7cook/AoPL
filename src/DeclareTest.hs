import Prelude hiding (LT, GT, EQ, id)
import Declare
import DeclareParse
import Base

-- execute evaluates an expression in an empty environment
--BEGIN:DeclExec
execute exp = show (evaluate exp [])
--END:DeclExec

main = testMain parseExp execute
      
  