{-# OPTIONS -XFlexibleContexts #-}

module Base where
import Prelude hiding (catch)
import Control.Exception
import System.Environment
import Data.Text hiding (zip, tail, dropWhile, concat, length, head)

check msg a b = putStrLn (if a == b then "OK" else "*** CHECK " ++ msg ++ " Failed ***")

tagged tag code = do 
  putStrLn ("BEGIN" ++ ":" ++ tag)
  code
  putStrLn ("END" ++ ":" ++ tag)
  putStrLn ""

showErrorText :: SomeException -> String
showErrorText ex = trimError (show ex)

trimError e = trim e where
  trim (':' : ' ' : msg) = msg
  trim (c:cs) = trim cs
  trim [] = e

showError :: SomeException -> IO ()
showError ex = putStr ("Exception: " ++ trimError (show ex))
      
paren x = "(" ++ x ++ ")"

testMain parse execute = do
  args <- getArgs
  script <- case args of
    [] -> getContents
    [file] -> readFile file

  let comments = splitOn (pack "\n--") (pack ('\n' : script))
  let nocomments = if length comments > 1 then 
        concat [ dropWhile (/= '\n') (unpack c) | c <- comments ]
      else
        unpack (head comments)
  let items = split (== '#') (pack ("\n" ++ nocomments))
  let badpairs = [ breakOn (pack "\n") item | item <- items ]
  let pairs = [ (q, a) | ((_, q), (a, _)) <- zip badpairs (tail badpairs) ]
  mapM (process parse execute) pairs
  return ()

process parse execute (qt, at) = do
     let q = unpack (strip qt)
     putStrLn q
     catch (do
       let script = parse q
       catch (do
         let result = execute script 
         let a = unpack (strip at)
         putStrLn $ "=>> " ++ result
         if result == a
         then return ()
         else do
           putStrLn $ "ERROR!! Correct: " ++ a
           putStrLn (show script))
         (\e -> do
             putStrLn ("Error: " ++ show (e :: SomeException))
             putStrLn (show script)))
       (\e -> putStrLn ("Error: " ++ show (e :: SomeException)))
     putStrLn ""