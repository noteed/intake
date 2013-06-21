module Main (main) where

import System.Environment (getArgs)
import Test.HUnit (assertBool, assertEqual)
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
      print echoAB
      print echoAB'
      print echoRA
      print $ Run 0
      print $ WorkflowIdPrefix "a"
      let jstatus :: [JStatus]
          jstatus = read "[Waiting, Ready, Started, Completed]"
      print $ filter (/= Ready) jstatus
      print $ filter (/= WInstanciated) [WStarted [], WCompleted]
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
      assertBool "started'" $ echoA /= fst (step echoA)
      assertEqual "run 0" [Run 0] (snd $ step echoA)
      assertEqual "nothing to change" (fst $ step echoA) (fst. step . fst $ step echoA)
      assertEqual "nothing to run" [] (snd . step . fst $ step echoA)
      assertEqual "completed" (WCompleted) (status . complete 0 . fst $ step echoA)
      assertEqual "nothing to run" [] (snd . step . complete 0 . fst $ step echoA)

  , testCase "echo a >> echo b" $ do
      assertEqual "instanciated" (wrap' $ SJob 0 "echo" ["a"] Ready `SSequence` SJob 1 "echo" ["b"] Waiting) (echoAB)
      assertEqual "instanciated'" WInstanciated (status echoAB)
      assertEqual "run 0" [Run 0] (snd $ step echoAB)
      assertEqual "started" (WStarted [0]) (status . fst $ step echoAB)
      assertEqual "still started" (WStarted []) (status . complete 0 . fst $ step echoAB)
      assertEqual "still started"
        (wrap' $ SJob 0 "echo" ["a"] Completed `SSequence` SJob 1 "echo" ["b"] Ready)
        (complete 0 . fst $ step echoAB)
      assertEqual "still started"
        (wrap' $ SJob 0 "echo" ["a"] Completed `SSequence` SJob 1 "echo" ["b"] Started)
        (fst . step . complete 0 . fst $ step echoAB)
      assertEqual "run 1" [Run 1] (snd . step . complete 0 . fst $ step echoAB)
      assertEqual "still started" (WStarted [1]) (status . fst . step . complete 0 . fst $ step echoAB)
      assertEqual "completed" (WCompleted) (status . complete 1 . fst . step . complete 0 . fst $ step echoAB)
      assertEqual "nothing to run" [] (snd . step . complete 1 . fst . step . complete 0 . fst $ step echoAB)
  ]

echoA :: WorkflowEnv
echoA = wrap $ echo "a"

echoAB :: WorkflowEnv
echoAB = wrap $ echo "a" `Sequence` echo "b"

echoAB' :: WorkflowEnv
echoAB' = wrap $ echo "a" `Parallel` echo "b"

echoRA :: WorkflowEnv
echoRA = wrap $ Retry 2 $ echo "a"

echo :: String -> Workflow
echo s = Job "echo" $ words s

wrap :: Workflow -> WorkflowEnv
wrap = wrap' . initializeWorkflow

wrap' :: WorkflowState -> WorkflowEnv
wrap' = WorkflowEnv (WorkflowName "test") (WorkflowId "_") []

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