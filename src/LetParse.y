{
module Main where
import Data.Char
import Simple
import Lexer
}

%name parser
%tokentype { Token }

%token 
    var   { TokenVar }
    ';'   { TokenSemi }
    int   { TokenInt $$ }
    id    { TokenIdent $$ }
    '='   { TokenEq }
    '+'   { TokenPlus }
    '-'   { TokenMinus }
    '*'   { TokenTimes }
    '/'   { TokenDiv }
    '('   { TokenOB }
    ')'   { TokenCB }

%%

Exp : var id '=' Exp ';' Exp  { Declare $2 $4 $6 }
    | Term      { $1 }

Term : Term '+' Factor    { Add $1 $3 }
     | Term '-' Factor    { Subtract $1 $3 }
     | Factor             { $1 }

Factor : Factor '*' Primary    { Multiply $1 $3 }
       | Factor '/' Primary    { Divide $1 $3 }
       | Primary               { $1 }

Primary : int            { Number $1 }
        | '-' int        { Number (- $2) }
        | '(' Exp ')'   { $2 }

{

parseExp = parser . lexer [("var", TokenVar)]

parseInput = do
  input <- getContents
  print (parseExp input))

}
