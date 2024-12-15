module Main where

import Control.Exception (catch)
import System.Environment (getArgs)
import System.Exit
import System.IO (Handle, IOMode(ReadMode), hClose, hGetLine, openFile, stdin)
import System.IO.Error (isEOFError)

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
      hClose handle
      if isEOFError e
         then exitSuccess
         else exitFailure)

runFile :: String -> IO ()
runFile fileName =
  openFile fileName ReadMode >>= runHandle

runPrompt :: IO ()
runPrompt = runHandle stdin

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
