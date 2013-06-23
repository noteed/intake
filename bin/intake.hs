{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Version (showVersion)
import Paths_intake (version)
import System.Console.CmdArgs.Implicit
import Test.HUnit (assertBool, assertEqual)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)

import Intake.Core
import Intake.Process (backend)

main :: IO ()
main = (runCmd =<<) . cmdArgs $ modes
  [ cmdRun
  , cmdStatus
  , cmdShow
  , cmdTests &= auto
  ]
  &= summary versionString
  &= program "intake"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "Intake " ++ showVersion version ++ " - Copyright (c) 2013 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdRun
    { cmdWorkflowName :: String
    , cmdArguments :: [String]
    , cmdCommand :: Bool
    }
  | CmdStatus
    { cmdWorkflowIdPrefix :: String
    }
  | CmdShow
    { cmdMaybeWorkflowIdPrefix :: Maybe String
    }
  | CmdTests
  deriving (Data, Typeable)

-- | Create a 'Run' command.
cmdRun :: Cmd
cmdRun = CmdRun
  { cmdWorkflowName = def
    &= argPos 0
    &= typ "WORKFLOW"
  , cmdArguments = def
    &= args
    &= typ "ARGS"
  , cmdCommand = def
    &= explicit
    &= name "c"
    &= help "When this flag is given, accepts a command instead of a workflow\
      \ name. A workflow with the command as its sole job is then\
      \ instanciated and run."
  } &= help "Run a workflow with the provided arguments."
    &= explicit
    &= name "run"

-- | Create a 'Status' command.
cmdStatus :: Cmd
cmdStatus = CmdStatus
  { cmdWorkflowIdPrefix = def
    &= argPos 0
    &= typ "ID"
  } &= help "Report the current status of a workflow instance."
    &= explicit
    &= name "status"

-- | Create a 'Show' command, used to improve code coverage. TODO remove it.
cmdShow :: Cmd
cmdShow = CmdShow
  { cmdMaybeWorkflowIdPrefix = def
    &= args
    &= typ "ID"
  } &= help "Show some data structure to improve code coverage."
    &= explicit
    &= name "show"

-- | Create a 'Tests' command. TODO remove it.
cmdTests :: Cmd
cmdTests = CmdTests
    &= help "Run the test suite."
    &= explicit
    &= name "tests"

runCmd :: Cmd -> IO ()
runCmd CmdRun{..} =
  if cmdCommand
  then do
    WorkflowId i <- run backend (Left cmdWorkflowName) cmdArguments
    putStrLn $ take 12 i ++ "  " ++ i
  else do
    WorkflowId i <-
      run backend (Right $ WorkflowName cmdWorkflowName) cmdArguments
    putStrLn $ take 12 i ++ "  " ++ i

runCmd CmdStatus{..} = do
  e <- inspect backend (WorkflowIdPrefix cmdWorkflowIdPrefix)
  putStrLn $ show $ status e

runCmd CmdShow{..} = do
  case cmdMaybeWorkflowIdPrefix of
    Just i -> do
      e <- inspect backend (WorkflowIdPrefix i)
      print e -- To cover the (read arguments) in loadEnvironment'
    Nothing -> do
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

runCmd CmdTests{..} = defaultMain tests

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
      assertBool "completed" (isCompleted . envState . complete 1 . fst . step . complete 0 . fst $ step echoAB)

  , testCase "echo a // echo b" $ do
      assertEqual "instanciated" (wrap' $ SJob 0 "echo" ["a"] Ready `SParallel` SJob 1 "echo" ["b"] Ready) (echoAB')
      assertEqual "instanciated'" WInstanciated (status echoAB')
      assertEqual "run 0 and 1" [Run 0, Run 1] (snd $ step echoAB')
      assertEqual "started" (WStarted [0, 1]) (status . fst $ step echoAB')
      assertEqual "still started" (WStarted [1]) (status . complete 0 . fst $ step echoAB')
      assertEqual "still started" (WStarted [0]) (status . complete 1 . fst $ step echoAB')
      assertEqual "still started" (WCompleted) (status . complete 0 . complete 1 . fst $ step echoAB')
      assertEqual "still started" (WCompleted) (status . complete 1 . complete 0 . fst $ step echoAB')
      assertEqual "nothing to run" [] (snd . step . complete 0 . fst $ step echoAB')
      assertEqual "nothing to run" [] (snd . step . complete 1 . fst $ step echoAB')
      assertEqual "nothing to run" [] (snd . step . complete 1 . fst . step . complete 0 . fst $ step echoAB')
      assertBool "completed" (isCompleted . envState . complete 1 . fst . step . complete 0 . fst $ step echoAB')

  , testCase "echo a /2" $ do
      assertEqual "instanciated" (wrap' $ SRetry 2 0 $ SJob 0 "echo" ["a"] Ready) (echoRA)
      assertEqual "instanciated'" WInstanciated (status echoRA)
      assertEqual "run 0" [Run 0] (snd $ step echoRA)
      assertEqual "started" (WStarted [0]) (status . fst $ step echoRA)
      assertEqual "completed" (WCompleted) (status . complete 0 . fst $ step echoRA)
      assertBool "completed" (isCompleted . envState . complete 0 . fst $ step echoRA)
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
wrap' = WorkflowEnv (Right $ WorkflowName "test") (WorkflowId "_") []

-- property: if `step` issues some Run l, the state of the Job l is Started.

-- property: `retry n a` behaves as `a` when a does not fail.

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

-- TODO `run -c command` arguments: execute a workflow with a single job
-- made of the command without first needing to `define` it.
