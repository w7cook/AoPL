{
module SubstituteParse where
import Data.Char
import Substitute
import Lexer
}

%name parser
%tokentype { Token }

-- BEGIN:SubstLexer
%token 
    id     { TokenIdent $$ }
    digits { Digits $$ }
    '+'    { Symbol "+" }
    '-'    { Symbol "-" }
    '*'    { Symbol "*" }
    '/'    { Symbol "/" }
    '('    { Symbol "(" }
    ')'    { Symbol ")" }
-- END:SubstLexer

%%

-- BEGIN:SubstGrammar
Term : Term '+' Factor    { Add $1 $3 }
     | Term '-' Factor    { Subtract $1 $3 }
     | Factor             { $1 }

Factor : Factor '*' Primary    { Multiply $1 $3 }
       | Factor '/' Primary    { Divide $1 $3 }
       | Primary               { $1 }

Primary : digits         { Number $1 }
        | '-' digits     { Number (- $2) }
        | id             { Variable $1 }
        | '(' Term ')'   { $2 }
-- END:SubstGrammar

{

symbols = ["+", "-", "*", "/", "(", ")"]
keywords = []
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}
