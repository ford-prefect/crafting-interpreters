module Main where

import Control.Exception (catch, throw)
import System.Environment (getArgs)
import System.Exit
import System.IO (Handle, IOMode (ReadMode), hGetLine, openFile, stdin, hGetContents)
import System.IO.Error (isEOFError, isUserError)

import Scanner

report :: Int -> String -> String -> IO ()
report line whr message =
  putStrLn $ "[line " ++ show line ++ "] Error" ++ whr ++ ": " ++ message

err :: Int -> String -> IO ()
err line msg = do
  report line "" msg
  ioError . userError $ "Execution error"

run :: String -> IO ()
run code =
  let (mErr, tokens) = scan code
   in do
     print mErr
     print tokens

runFile :: String -> IO ()
runFile fileName =
  catch (openFile fileName ReadMode >>= hGetContents >>= run) handler
  where
    handler :: IOError -> IO ()
    handler e = do
      putStrLn $ "Error: " ++ show e
      exitFailure

readLines :: Handle -> [IO String]
readLines handle = do
  hGetLine handle : readLines handle

runHandle :: Handle -> IO ()
runHandle handle = do
  catch
    (mapM_ (>>= run) (readLines handle))
    ( \e -> do
        if isEOFError e
          then exitSuccess
          else throw e
    )

runPrompt :: IO ()
runPrompt = catch runIt handler
  where
    runIt = runHandle stdin
    handler :: IOError -> IO ()
    handler e =
      if isUserError e
        then runIt -- the error was printed already, go again
        else do
          putStrLn ("Exception: " ++ show e)
          runIt

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: hlox (<script>)"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  if length args > 1
    then printUsage
    else
      if length args == 1
        then runFile $ head args
        else runPrompt
