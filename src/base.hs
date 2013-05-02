{-# OPTIONS -XFlexibleContexts #-}

module Base where
import Prelude hiding (catch)
import Text.Parsec
import Data.Functor.Identity
import Control.Exception
import Text.Groom

ws :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
ws op = do
  x <- op
  spaces
  return x

infixl 5 +@
e +@ op = chainl1 e (ws op)

as :: Monad m => m t -> (t -> b) -> m b
as p f = do 
  x <- p
  return (f x)
  
betweenC :: Stream s m Char =>
     Char -> ParsecT s u m a -> Char -> ParsecT s u m a  
betweenC l p r = between (ws $ char l) (ws $ char r) p

-- lexeme parsers
sym :: Stream s m Char => String -> b -> ParsecT s u m b
sym name value = do
  ws $ string name
  return value

number :: ParsecT [Char] u Data.Functor.Identity.Identity Int
number = do
  sign <- option 1 (sym "-" (-1))
  xs <- ws $ many1 digit
  return (sign * (read xs :: Int))

check msg a b = putStrLn (if a == b then "OK" else "*** CHECK " ++ msg ++ " Failed ***")

tagged tag code = do 
  putStrLn ("BEGIN" ++ ":" ++ tag)
  code
  putStrLn ("END" ++ ":" ++ tag)
  putStrLn ""

--BEGIN:Form6
test fun input = do 
    putStrLn (groom input)
    putStr " ==> "
    putStrLn (groom (fun input)) `catch` showError
    putStrLn ""
--END:Form6
    
showError :: SomeException -> IO ()
showError ex = putStr ("Exception: " ++ trimError (show ex))

trimError e = trim e where
  trim (':' : ' ' : msg) = msg
  trim (c:cs) = trim cs
  trim [] = e


-- some parser helpers

