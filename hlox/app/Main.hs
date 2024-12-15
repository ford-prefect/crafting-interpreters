module Main where

import Control.Exception (catch, throw)
import System.Environment (getArgs)
import System.Exit
import System.IO (Handle, IOMode(ReadMode), hGetLine, openFile, stdin)
import System.IO.Error (isEOFError)

report :: Int -> String -> String -> IO ()
report line whr message =
  putStrLn $ "[line " ++ show line ++ "] Error" ++ whr ++ ": " ++ message

err :: Int -> String -> IO ()
err line msg = do
  report line "" msg
  ioError . userError $ "Execution error"

run :: String -> IO ()
run code = do
  putStrLn ("Running line: " ++ code)

readLines :: Handle -> [IO String]
readLines handle = do
  hGetLine handle : readLines handle

runHandle :: Handle -> IO ()
runHandle handle = do
  catch (mapM_ (>>= run) (readLines handle))
    (\e -> do
      if isEOFError e
         then exitSuccess
         else throw e)

runFile :: String -> IO ()
runFile fileName =
  catch (openFile fileName ReadMode >>= runHandle) handler
  where
    handler :: IOError -> IO ()
    handler = const exitFailure

runPrompt :: IO ()
runPrompt = catch (runHandle stdin) handler
  where
    handler :: IOError -> IO ()
    handler = const $ runHandle stdin

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: hlox (<script>)"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  if length args > 1 then printUsage
  else if length args == 1 then runFile $ head args
  else runPrompt
