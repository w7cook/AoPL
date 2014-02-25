{
module LambdaParse where
import Data.Char
import Lambda
import Lexer
}

%name parser
%tokentype { Token }

%token 
    id     { TokenIdent $$ }
    '\\'   { Symbol "\\" }
    '.'    { Symbol "." }
    '('    { Symbol "(" }
    ')'    { Symbol ")" }

%%

Exp  : Exp Prim       { App $1 $2 }
     | Prim            { $1 }

Prim : id               { Var $1 }
     | '\\' id '.' Exp { Abs $2 $4 }
     | '(' Exp ')'     { $2 }
     
{

symbols = [".", "\\", "(", ")"]
keywords = []
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}
