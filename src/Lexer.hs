module Lexer where

import Data.Char
import Data.List

--BEGIN:BasicToken BEGIN:Token
data Token = Digits Int
           | Symbol String
--END:BasicToken
           | TokenKeyword String
           | TokenIdent String
--END:Token
           | TokenVar String
   deriving Show

lexer :: [String] -> [String] -> String -> [Token]
lexer symbols keywords str = lex str where
  lex [] = []
  lex (c:cs) 
    | isSpace c = lex cs
    | isAlpha c = lexAlpha keywords (c:cs)
    | isDigit c = lexDigits (c:cs)
    | True      = lexSym symbols (c:cs)

  lexSym :: [String] -> String -> [Token]
  lexSym (s:ss) cs = 
    case stripPrefix s cs of
      Nothing -> lexSym ss cs
      Just rest  -> Symbol s : lex rest  
  lexSym [] (c:cs) = error ("Unrecognized symbol '" ++ [c] ++ "'")
  
  lexDigits cs = Digits (read num) : lex rest
    where (num, rest) = span isDigit cs
  
  lexAlpha keywords str = token : lex rest where 
    (first, rest) = span isAlphaNum str
    token = if elem first keywords 
            then TokenKeyword first
            else TokenIdent first

happyError t = error ("Parse error at " ++ show t ++ "\n")
