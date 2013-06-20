module Main (main) where

import System.Environment (getArgs)
import Test.HUnit (assertEqual)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)

import Intake

main :: IO ()
main = do
  as <- getArgs
  case as of
    ["run", name] -> do
      WorkflowId i' <- run (WorkflowName name) []
      putStrLn i'
    ["status", i'] -> do
      e <- loadEnvironment (WorkflowIdPrefix i')
      putStrLn $ show $ status e
    -- "show" to improve coverage
    ["show", i'] -> do
      e <- loadEnvironment (WorkflowIdPrefix i')
      print e -- To cover the (read arguments) in loadEnvironment'
    -- "show" to improve coverage
    ["show"] -> do
      print echoA
      print $ Run 0
      let jstatus :: [JStatus]
          jstatus = read "[Waiting, Ready, Started, Completed]"
      print $ filter (/= Ready) jstatus
    [] -> do
      defaultMain tests

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: [Test]
tests =
  [ testCase "echo a" $ do
      assertEqual "instanciated" WInstanciated (status echoA)
      assertEqual "started" (WStarted [0]) (status . fst $ step echoA)
      assertEqual "run 0" [Run 0] (snd $ step echoA)
      assertEqual "nothing to change" (fst $ step echoA) (fst. step . fst $ step echoA)
      assertEqual "nothing to run" [] (snd . step . fst $ step echoA)
      assertEqual "completed" (WCompleted) (status . setStatus 0 Completed . fst $ step echoA)
      assertEqual "nothing to run" [] (snd . step . setStatus 0 Completed . fst $ step echoA)
  , testCase "echo a ; echo b" $ do
      assertEqual "instanciated" WInstanciated (status echoAB)
      -- TODO assertEqual "started" (WStarted [0]) (status . fst $ step echoAB)
  ]

echoA :: WorkflowEnv
echoA = wrap $ echo "a"

echoAB :: WorkflowEnv
echoAB = wrap $ echo "a" `Sequence` echo "b"

echo :: String -> Workflow
echo s = Job "echo" $ words s

wrap :: Workflow -> WorkflowEnv
wrap = WorkflowEnv (WorkflowName "test") (WorkflowId "_") [] . initializeWorkflow

-- property: if `step` issues some Run l, the state of the Job l is Started.

----------------------------------------------------------------------
-- Command-line
----------------------------------------------------------------------

-- > intake define workflow workflow.conf
-- > intake run workflow arg0 arg1
-- c40bb15c4280
-- > intake status c40bb15c4280
-- Running (waiting on 2 jobs).
-- > intake status c40bb15c4280
-- Success (4 jobs, 3 successes, 1 failure) (can receive `benchmark`).
-- > intake send benchmark c40bb15c4280
-- > intake status c40bb15c4280
-- Success (5 jobs, 4 successes, 1 failure).
