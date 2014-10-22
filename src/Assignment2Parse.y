{
module Assignment2Parse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Assignment2
import Lexer
}

%name parser
%tokentype { Token }

%token 
    function { TokenKeyword "function" }
    if    { TokenKeyword "if" }
    else  { TokenKeyword "else" }
    true  { TokenKeyword "true" }
    false { TokenKeyword "false" }
    var   { TokenKeyword "var" }
    ';'   { Symbol ";" }
    ','   { Symbol "," }
    id    { TokenIdent $$ }
    digits { Digits $$ }
    '='    { Symbol "=" }
    '+'    { Symbol "+" }
    '-'    { Symbol "-" }
    '*'    { Symbol "*" }
    '/'    { Symbol "/" }
    '<'    { Symbol "<" }
    '>'    { Symbol ">" }
    '<='   { Symbol "<=" }
    '>='   { Symbol ">=" }
    '=='   { Symbol "==" }
    '&&'   { Symbol "&&" }
    '!'    { Symbol "!" }
    '||'   { Symbol "||" }
    '('    { Symbol "(" }
    ')'    { Symbol ")" }
    '{'    { Symbol "{" }
    '}'    { Symbol "}" }

%%
       
Exp : function '(' Patterns ')' '{' Exp '}'  { Function (TupleP $3) $6 }
    | var Pattern '=' Exp ';' Exp           { Declare $2 $4 $6 }
    | if '(' Exp ')' Exp else Exp  { If $3 $5 $7 }
    | Or                               { $1 }

Pattern :  id							{ VarP $1 }
			  |  '(' Pattern ')' { $2 }
			  |  '(' Patterns ')' { TupleP $2 }
			  
Patterns : Pattern				{ [$1] }
         | Patterns ',' Pattern  { $1 ++ [$3] }
  
Or   : Or '||' And        { Binary Or $1 $3 }
     | And                { $1 }

And   : And '&&' Comp      { Binary And $1 $3 }
     | Comp                { $1 }

Comp : Comp '==' Term     { Binary EQ $1 $3 }
     | Comp '<' Term      { Binary LT $1 $3 }
     | Comp '>' Term      { Binary GT $1 $3 }
     | Comp '<=' Term     { Binary LE $1 $3 }
     | Comp '>=' Term     { Binary GE $1 $3 }
     | Term               { $1 }

Term : Term '+' Factor    { Binary Add $1 $3 }
     | Term '-' Factor    { Binary Sub $1 $3 }
     | Factor             { $1 }

Factor : Factor '*' Primary    { Binary Mul $1 $3 }
       | Factor '/' Primary    { Binary Div $1 $3 }
       | Primary               { $1 }

Primary : Primary '(' Exps ')' { Call $1 (Tuple $3) }
        | digits         { Literal (IntV $1) }
        | true           { Literal (BoolV True) }
        | false          { Literal (BoolV False) }
        | '-' Primary    { Unary Neg $2 }
        | '!' Primary    { Unary Not $2 }
        | id             { Variable $1 }
        | '(' Exp ')'    { $2 }
        | '(' Exps ')'    { Tuple $2 }
 
Exps : Exp  { [$1] }
     | Exps ',' Exp { $1 ++ [$3] }

{

symbols = ["+", "-", "*", "/", "(", ")", "{", "}", ";", ",", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!"]
keywords = ["function", "var", "if", "else", "true", "false"]
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}
