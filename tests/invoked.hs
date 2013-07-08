{-# LANGUAGE RecordWildCards #-}
-- Invoked takes a file composed of two parts: the first part is a command to
-- execute, the second part is an expected result that the command should
-- produce. That's all Folks!
module Main (main) where

import Data.Maybe (fromJust)
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
  , inStderr :: Maybe String -- ^ Expected standard error.
  , inStdout :: Maybe String -- ^ Expected standard output.
  , inExitCode :: Maybe ExitCode -- ^ Expected exit code.
  }

defaultInvoke :: Invoke
defaultInvoke = Invoke "true" [] "" (Just "") (Just "") (Just ExitSuccess)

readInvoke :: String -> IO [Invoke]
readInvoke filename = do
  content <- readFile filename
  let content' = lines content
      command : arguments = words' $ head content'
      expected = unlines $ tail content'
      expected' = if last content /= '\n' then init expected else expected
  return [Invoke command arguments "" (Just "") (Just expected') (Just ExitSuccess)]

runInvoke :: [Invoke] -> IO ExitCode
runInvoke [] = return ExitSuccess
runInvoke (i : is) = do
  r <- runInvoke' i
  case r of
    ExitSuccess -> runInvoke is
    _ -> return r

runInvoke' :: Invoke -> IO ExitCode
runInvoke' Invoke{..} = do
  (code, out, err) <- readProcessWithExitCode inCommand inArguments inStdin
  if maybe False (code /=) inExitCode
    then unexpectedExitCode
    else if maybe False (err /=) inStderr
         then unexpectedStderr err
         else if maybe False (out /=) inStdout
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
    hPutStrLn stderr $ fromJust inStderr
    return $ ExitFailure 1
  unexpectedStdout out' = do
    hPutStrLn stderr "The command stdout is not the expected output."
    hPutStrLn stderr "The command stdout is:"
    hPutStrLn stderr out'
    hPutStrLn stderr "The expected output is:"
    hPutStrLn stderr $ fromJust inStdout
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
