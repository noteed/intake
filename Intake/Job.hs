{-# LANGUAGE RecordWildCards #-}
-- | 'Job' handling.
module Intake.Job where

import Data.Maybe (fromJust)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import Intake.Core (Job(..), JobResult(..))

defaultMain :: Job -> IO ()
defaultMain i = runJob i >>= resultToExitCode i >>= exitWith

run :: String -> IO ()
run filename = readJobs filename >>= runJobs >>= exitWith

readJobs :: String -> IO [Job]
readJobs filename = do
  content <- readFile filename
  let content' = lines content
      command : arguments = words' $ head content'
      expected = unlines $ tail content'
      expected' = if last content /= '\n' then init expected else expected
  return [Job command arguments "" (Just "") (Just expected') (Just ExitSuccess)]

runJobs :: [Job] -> IO ExitCode
runJobs [] = return ExitSuccess
runJobs (i : is) = do
  r <- runJob i >>= resultToExitCode i
  case r of
    ExitSuccess -> runJobs is
    _ -> return r

runJob :: Job -> IO JobResult
runJob Job{..} = do
  (code, out, err) <- readProcessWithExitCode jobCommand jobArguments jobStdin
  return $ JobResult code err out

resultToExitCode :: Job -> JobResult -> IO ExitCode
resultToExitCode Job{..} JobResult{..} = do
  let code = jobExitCode'
      err = jobStderr'
      out = jobStdout'
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
