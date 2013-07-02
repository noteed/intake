-- Invoked takes a file composed of two parts: the first part is a command to
-- execute, the second part is an expected result that the command should
-- produce. That's all Folks!
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> invoke filename
    _ -> putStrLn $ "Usage: invoked FILENAME"

invoke :: String -> IO ()
invoke filename = do
  content <- readFile filename
  let content' = lines content
      command : arguments = words' $ head content'
      expected = unlines $ tail content'
      expected' = if last content /= '\n' then init expected else expected
  (code, out, err) <- readProcessWithExitCode command arguments ""
  when (code /= ExitSuccess) $ do
    hPutStrLn stderr "The command exit code is not 0."
    exitFailure
  when (not $ null err) $ do
    hPutStrLn stderr "The command stderr is not empty."
    hPutStrLn stderr "The command stderr is:"
    hPutStrLn stderr err
    exitFailure
  when (out /= expected') $ do
    hPutStrLn stderr "The command stdout is not the expected output."
    hPutStrLn stderr "The command stdout is:"
    hPutStrLn stderr out
    hPutStrLn stderr "The expected output is:"
    hPutStrLn stderr expected'
    exitFailure
  exitSuccess

-- | Same as the standard `words` function but try to respect quoted strings.
-- Will explode on unbalanced quotes.
words' :: String -> [String]
words' s_ = case dropWhile (== ' ') s_ of
  "" -> []
  '"' : s -> case span (/= '"') s of
    (a, '"' : b) -> a : words' b
    _ -> error "Unbalanced quotes."
  s -> case span (/= ' ') s of
    (a, b) -> a : words' b
