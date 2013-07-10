{-# LANGUAGE RecordWildCards #-}
-- | 'Job' handling.
module Intake.Job where

import Data.Maybe (fromJust)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.Regex.Posix ((=~))

import Intake.Core (Job(..), JobResult(..))

defaultMain :: Job -> IO ()
defaultMain i = runJobs [] [i] >>= exitWith

defaultMain' :: [Job] -> IO ()
defaultMain' i = runJobs [] i >>= exitWith

run :: String -> IO ()
run filename = readJobs filename >>= runJobs [] >>= exitWith

readJobs :: String -> IO [Job]
readJobs filename = do
  content <- readFile filename
  let content' = lines content
      command : arguments = words' $ head content'
      expected = unlines $ tail content'
      expected' = if last content /= '\n' then init expected else expected
  return [Job command arguments "" (Just "") (Just expected') (Just ExitSuccess)]

runJobs :: [(String, String)] -> [Job] -> IO ExitCode
runJobs _ [] = return ExitSuccess
runJobs env (i : is) = do
  r <- runJob env i
  c <- resultToExitCode env i r
  case c of
    ExitSuccess -> do
      let env' = maybe (error "") (env ++) $ jobMatches r
      runJobs env' is
    _ -> return c

runJob :: [(String, String)] -> Job -> IO JobResult
runJob env Job{..} = do
  (code, out, err) <- readProcessWithExitCode jobCommand (map (replace env) jobArguments) jobStdin
  return $ JobResult code err out (maybe (Just []) (match env out . replace env) jobStdout)

resultToExitCode :: [(String, String)] -> Job -> JobResult -> IO ExitCode
resultToExitCode env Job{..} JobResult{..} = do
  let code = jobExitCode'
      err = jobStderr'
      out = jobStdout'
  if maybe False (code /=) jobExitCode
    then unexpectedExitCode
    else if maybe False ((err /=) . replace env) jobStderr
         then unexpectedStderr err
         else if jobMatches == Nothing
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
    hPutStrLn stderr $ replace env $ fromJust jobStdout
    return $ ExitFailure 1

-- | Same as the standard `words` function but try to respect quoted strings.
-- Will explode on unbalanced quotes. TODO no exploding, pretty plz.
words' :: String -> [String]
words' s_ = case dropWhile (== ' ') s_ of
  "" -> []
  '"' : s -> case span (/= '"') s of
    (a, '"' : b) -> a : words' b
    _ -> error "Unbalanced quotes."
  s -> case span (/= ' ') s of
    (a, b) -> a : words' b

----------------------------------------------------------------------
-- Regex stuff to check process outputs.
----------------------------------------------------------------------

-- | If match, return an association list of names to matched strings.
-- In the pattern, `{{$variables}} are replaced from the environment.
match :: [(String, String)] -> String -> String -> Maybe [(String, String)]
match env s pattern = case s =~ regex of
  ("", s' , "", groups) | s' == s && length groups == length names ->
    Just $ zip names groups
  _ -> Nothing
  where (regex, names) = patternToRegex pattern'
        pattern' = replace env pattern

patternToRegex :: String -> (String, [String])
patternToRegex cs = ('^' : regex ++ "$", names)
  where (regex, names) = patternToRegex' cs

-- TODO no exploshun on unbalanced {{}}.
patternToRegex' :: String -> (String, [String])
patternToRegex' ('{' : '{' : cs) = ("([^}]*)" ++ rest', name : names)
  where (name, '}' : '}' : rest) = span (/= '}') cs
        (rest', names) = patternToRegex' rest
patternToRegex' (c : cs) = (escape c ++ regex, names)
  where (regex, names) = patternToRegex' cs
patternToRegex' [] = ([], [])

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|[]"

replace :: [(String, String)] -> String -> String
replace env ('{' : '{' : '$' : cs) = fromJust (lookup name env)  ++ rest' -- TODO explodin'
  where (name, '}' : '}' : rest) = span (/= '}') cs
        rest' = replace env rest
replace env (c : cs) = c : replace env cs
replace _ [] = []
