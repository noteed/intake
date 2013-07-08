{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)
import qualified Data.Map as M
import Data.Version (showVersion)
import Paths_intake (version)
import System.Console.CmdArgs.Implicit
import Test.HUnit (assertBool, assertEqual)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)

import qualified Intake.Client as Client
import Intake.Core
import qualified Intake.Engine as Engine
import Intake.Http (serve)
import Intake.Job (run)
import Intake.Process (backend, work)

main :: IO ()
main = (runCmd =<<) . cmdArgs $ modes
  [ cmdRun
  , cmdStatus
  , cmdLogs
  , cmdWork
  , cmdServe
  , cmdWorkflows
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
    , cmdFile :: Bool
    }
  | CmdStatus
    { cmdWorkflowIdPrefix :: String
    }
  | CmdLogs
    { cmdWorkflowIdPrefix :: String
    }
  | CmdWork
    { cmdWorkflowId :: String
    , cmdJobId :: Int
    , cmdCommandName :: String
    , cmdArguments :: [String]
    }
  | CmdServe
  | CmdWorkflows
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
  , cmdFile = def -- TODO -f and -c must be exclusive.
    &= explicit
    &= name "f"
    &= help "When this flag is given, accepts a file name instead of a\
      \ workflow name."
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

-- | Create a 'Logs' command.
cmdLogs :: Cmd
cmdLogs = CmdLogs
  { cmdWorkflowIdPrefix = def
    &= argPos 0
    &= typ "ID"
  } &= help "Display the logs (stdout only) of a workflow instance. \
      \TODO This only displays the logs for job #0."
    &= explicit
    &= name "logs"

-- | Create a 'Work' command. Used by the `Intake.Process` backend.
cmdWork :: Cmd
cmdWork = CmdWork
  { cmdWorkflowId = def
    &= argPos 0
    &= typ "ID"
  , cmdJobId = def
    &= argPos 1
    &= typ "ID"
  , cmdCommandName = def
    &= argPos 2
    &= typ "CMD"
  , cmdArguments = def
    &= args
    &= typ "ARGS"
  } &= help "Run a job. This is used by `intake` internally."
    &= explicit
    &= name "work"

-- | Create a 'Serve' command.
cmdServe :: Cmd
cmdServe = CmdServe
    &= help "Start the Intake server."
    &= explicit
    &= name "serve"

-- | Create a 'Workflows' command.
cmdWorkflows :: Cmd
cmdWorkflows = CmdWorkflows
    &= help "List available workflow definitions."
    &= explicit
    &= name "workflows"

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
runCmd CmdRun{..} = do
  if cmdFile
    then run cmdWorkflowName
    else do
      let n = (if cmdCommand then Left else Right . WorkflowName) cmdWorkflowName
      WorkflowId i <- Client.instanciate n
      putStrLn $ take 12 i ++ "  " ++ i

runCmd CmdStatus{..} = do
  Client.status (WorkflowIdPrefix cmdWorkflowIdPrefix) >>= print

runCmd CmdLogs{..} = do
  ls <- logs backend (WorkflowIdPrefix cmdWorkflowIdPrefix)
  putStr ls

runCmd CmdWork{..} = do
  work (WorkflowId cmdWorkflowId) cmdJobId cmdCommandName cmdArguments

runCmd CmdServe{..} = do
  chan <- newChan
  _ <- forkIO $ Engine.loop chan M.empty False
  serve chan

runCmd CmdWorkflows{..} = do
  Client.workflows >>= print

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
      assertEqual "instanciated" (wrap' $ SSingle 0 (defaultJob' "echo" ["a"]) Ready `SSequence` SSingle 1 (defaultJob' "echo" ["b"]) Waiting) (echoAB)
      assertEqual "instanciated'" WInstanciated (status echoAB)
      assertEqual "run 0" [Run 0] (snd $ step echoAB)
      assertEqual "started" (WStarted [0]) (status . fst $ step echoAB)
      assertEqual "still started" (WStarted []) (status . complete 0 . fst $ step echoAB)
      assertEqual "still started"
        (wrap' $ SSingle 0 (defaultJob' "echo" ["a"]) Completed `SSequence` SSingle 1 (defaultJob' "echo" ["b"]) Ready)
        (complete 0 . fst $ step echoAB)
      assertEqual "still started"
        (wrap' $ SSingle 0 (defaultJob' "echo" ["a"]) Completed `SSequence` SSingle 1 (defaultJob' "echo" ["b"]) Started)
        (fst . step . complete 0 . fst $ step echoAB)
      assertEqual "run 1" [Run 1] (snd . step . complete 0 . fst $ step echoAB)
      assertEqual "still started" (WStarted [1]) (status . fst . step . complete 0 . fst $ step echoAB)
      assertEqual "completed" (WCompleted) (status . complete 1 . fst . step . complete 0 . fst $ step echoAB)
      assertEqual "nothing to run" [] (snd . step . complete 1 . fst . step . complete 0 . fst $ step echoAB)
      assertBool "completed" (isCompleted . envState . complete 1 . fst . step . complete 0 . fst $ step echoAB)

  , testCase "echo a // echo b" $ do
      assertEqual "instanciated" (wrap' $ SSingle 0 (defaultJob' "echo" ["a"]) Ready `SParallel` SSingle 1 (defaultJob' "echo" ["b"]) Ready) (echoAB')
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
      assertEqual "instanciated" (wrap' $ SRetry 2 0 $ SSingle 0 (defaultJob' "echo" ["a"]) Ready) (echoRA)
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
echo s = Single $ defaultJob' "echo" (words s)

wrap :: Workflow -> WorkflowEnv
wrap = wrap' . initializeWorkflow

wrap' :: WorkflowState -> WorkflowEnv
wrap' = WorkflowEnv (Right $ WorkflowName "test") (WorkflowId "_") []

-- property: if `step` issues some Run l, the state of the Single l is Started.

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
