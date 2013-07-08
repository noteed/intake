{-# LANGUAGE RecordWildCards #-}
-- Intake job representation and processing.
module Intake.Job where

import Data.Maybe (fromJust)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

defaultMain :: Job -> IO ()
defaultMain i = runJob' i >>= exitWith

run :: String -> IO ()
run filename = readJob filename >>= runJob >>= exitWith

-- | Represent a single job with its expected behavior.
data Job = Job
  { jobCommand :: String -- ^ The command to execute.
  , jobArguments :: [String] -- ^ Arguments for the command.
  , jobStdin :: String -- ^ Standard input to provide to the command.
  , jobStderr :: Maybe String -- ^ Expected standard error.
  , jobStdout :: Maybe String -- ^ Expected standard output.
  , jobExitCode :: Maybe ExitCode -- ^ Expected exit code.
  }

defaultJob :: Job
defaultJob = Job "true" [] "" (Just "") (Just "") (Just ExitSuccess)

readJob :: String -> IO [Job]
readJob filename = do
  content <- readFile filename
  let content' = lines content
      command : arguments = words' $ head content'
      expected = unlines $ tail content'
      expected' = if last content /= '\n' then init expected else expected
  return [Job command arguments "" (Just "") (Just expected') (Just ExitSuccess)]

runJob :: [Job] -> IO ExitCode
runJob [] = return ExitSuccess
runJob (i : is) = do
  r <- runJob' i
  case r of
    ExitSuccess -> runJob is
    _ -> return r

runJob' :: Job -> IO ExitCode
runJob' Job{..} = do
  (code, out, err) <- readProcessWithExitCode jobCommand jobArguments jobStdin
  if maybe False (code /=) jobExitCode
    then unexpectedExitCode
    else if maybe False (err /=) jobStderr
         then unexpectedStderr err
         else if maybe False (out /=) jobStdout
              then unexpectedStdout out
              else return ExitSuccess
  where
  unexpectedExitCode = do
    hPutStrLn stderr $
      "The command exit code is not " ++ show jobExitCode ++ "."
    return $ ExitFailure 1
  unexpectedStderr err' = do
    hPutStrLn stderr "The command stderr is not the expected output."
    hPutStrLn stderr "The command stderr is:"
    hPutStrLn stderr err'
    hPutStrLn stderr "The expected stderr is:"
    hPutStrLn stderr $ fromJust jobStderr
    return $ ExitFailure 1
  unexpectedStdout out' = do
    hPutStrLn stderr "The command stdout is not the expected output."
    hPutStrLn stderr "The command stdout is:"
    hPutStrLn stderr out'
    hPutStrLn stderr "The expected output is:"
    hPutStrLn stderr $ fromJust jobStdout
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
