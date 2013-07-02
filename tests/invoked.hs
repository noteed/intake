{-# LANGUAGE RecordWildCards #-}
-- Invoked takes a file composed of two parts: the first part is a command to
-- execute, the second part is an expected result that the command should
-- produce. That's all Folks!
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> invoke filename
    _ -> putStrLn $ "Usage: invoked FILENAME"

invoke :: String -> IO ()
invoke filename = readInvoke filename >>= runInvoke >>= exitWith

-- | Represent a single job with its expected behavior.
data Invoke = Invoke
  { inCommand :: String -- ^ The command to execute.
  , inArguments :: [String] -- ^ Arguments for the command.
  , inStdin :: String -- ^ Standard input to provide to the command.
  , inStderr :: String -- ^ Expected standard error.
  , inStdout :: String -- ^ Expected standard output.
  , inExitCode :: ExitCode -- ^ Expected exit code.
  }

readInvoke :: String -> IO Invoke
readInvoke filename = do
  content <- readFile filename
  let content' = lines content
      command : arguments = words' $ head content'
      expected = unlines $ tail content'
      expected' = if last content /= '\n' then init expected else expected
  return $ Invoke command arguments "" "" expected' ExitSuccess

runInvoke :: Invoke -> IO ExitCode
runInvoke Invoke{..} = do
  (code, out, err) <- readProcessWithExitCode inCommand inArguments inStdin
  if code /= inExitCode
    then unexpectedExitCode
    else if err /= inStderr
         then unexpectedStderr err
         else if out /= inStdout
              then unexpectedStdout out
              else return ExitSuccess
  where
  unexpectedExitCode = do
    hPutStrLn stderr $
      "The command exit code is not " ++ show inExitCode ++ "."
    return $ ExitFailure 1
  unexpectedStderr err' = do
    hPutStrLn stderr "The command stderr is not the expected output."
    hPutStrLn stderr "The command stderr is:"
    hPutStrLn stderr err'
    hPutStrLn stderr "The expected stderr is:"
    hPutStrLn stderr inStderr
    return $ ExitFailure 1
  unexpectedStdout out' = do
    hPutStrLn stderr "The command stdout is not the expected output."
    hPutStrLn stderr "The command stdout is:"
    hPutStrLn stderr out'
    hPutStrLn stderr "The expected output is:"
    hPutStrLn stderr inStdout
    return $ ExitFailure 1

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
