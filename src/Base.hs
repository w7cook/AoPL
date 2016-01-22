{-# OPTIONS -XFlexibleContexts #-}

module Base where
import Prelude hiding (catch)
import Control.Exception
import System.Environment

check msg a b = putStrLn (if a == b then "OK" else "*** CHECK " ++ msg ++ " Failed ***")

tagged tag code = do 
  putStrLn ("BEGIN" ++ ":" ++ tag)
  code
  putStrLn ("END" ++ ":" ++ tag)
  putStrLn ""

test msg fun input = do 
    putStrLn (msg ++ " " ++ show input ++ "")
    putStr " ==> "
    putStrLn (show (fun input)) `catch` showError
    putStrLn ""
    
showError :: SomeException -> IO ()
showError ex = putStr ("Exception: " ++ trimError (show ex))  where
  trimError e = trim e where
    trim (':' : ' ' : msg) = msg
    trim (c:cs) = trim cs
    trim [] = e
      
paren x = "(" ++ x ++ ")"

genMain = do
  args <- getArgs
  case args of
    [] -> getContents
    [file] -> readFile file