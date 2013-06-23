{-# LANGUAGE TupleSections #-}
-- | Implement the Intake workflow with System.Process.
module Intake.Process where

import Control.Applicative ((<$>))
import Control.Concurrent (readMVar)
import Control.Monad (filterM)
import qualified Control.Monad.Random as R
import Data.List (foldl', isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents
  , getHomeDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process
import System.Process.Internals (ProcessHandle(..), ProcessHandle__(..))

import Intake.Core hiding (inspect, run)

backend :: Backend
backend = Backend run inspect

-- | Implement `Intake.Core.run`.
run :: WorkflowName -> [String] -> IO WorkflowId
run name@(WorkflowName n) arguments = do
  i@(WorkflowId i') <- newWorkflowId
  putStrLn $ take 12 i' ++ "  Loading workflow `" ++ n ++ "` with arguments "
    ++ show arguments ++ "."
  e <- loadWorkflow name i arguments
  let (e', rs) = step e
  mapM_ (start e) rs
  saveEnvironment e'
  return i

newWorkflowId :: IO WorkflowId
newWorkflowId = WorkflowId <$>
  (sequence $ replicate 64 $ R.fromList $ map (,1) $ ['a'..'f'] ++ ['0'..'9'])

start :: WorkflowEnv -> Run -> IO ()
start e r = do
  home <- getHomeDirectory
  let (cmd, args) = maybe (error "No such job ID.") id $ extract e r
      WorkflowId i' = envId e
      Run l = r
      dir = home </> ".intake" </> i' </> show l
  putStrLn $ take 12 i' ++ "  Starting `" ++ cmd
    ++ "` with arguments " ++ show args ++ "."
  createDirectoryIfMissing True dir
  writeFile (dir </> "cmdline") $ cmd ++ show args
  writeFile (dir </> "state") "Started"
  writeFile (dir </> "stderr") ""
  writeFile (dir </> "stdout") ""

loadWorkflow :: WorkflowName -> WorkflowId -> [String] -> IO WorkflowEnv
loadWorkflow name i arguments = do
  w <- readWorkflow name
  let s = initializeWorkflow w
  return $ WorkflowEnv name i arguments s

readWorkflow :: WorkflowName -> IO Workflow
readWorkflow (WorkflowName name) = return $ case name of
  "a" -> Job "echo" ["a"]
  "ab" -> Job "echo" ["a"] `Sequence` Job "echo" ["b"]
  _ -> error "No such workflow."
--  (Job "echo" ["a"] `Sequence` Job "echo" ["b"])
--    `Parallel` Job "echo" ["c"]

saveEnvironment :: WorkflowEnv -> IO ()
saveEnvironment e = do
  home <- getHomeDirectory
  let WorkflowId i' = envId e
      dir = home </> ".intake" </> i'
      WorkflowName n = envName e
  createDirectoryIfMissing True dir
  writeFile (dir </> "workflow") n
  writeFile (dir </> "arguments") (show $ envArguments e)

inspect :: WorkflowIdPrefix -> IO WorkflowEnv
inspect (WorkflowIdPrefix i') = do
  home <- getHomeDirectory
  let dir = home </> ".intake"
  content_ <- getDirectoryContents dir
  let content = filter (\d -> i' `isPrefixOf` d) content_
  dirs <- filterM (doesDirectoryExist . (dir </>)) content
  case dirs of
    [] -> error "No such build."
    [i] -> loadEnvironment (WorkflowId i)
    _ -> error "More than one build."
  
loadEnvironment :: WorkflowId -> IO WorkflowEnv
loadEnvironment i@(WorkflowId i') = do
  home <- getHomeDirectory
  let dir = home </> ".intake" </> i'
  name <- readFile $ dir </> "workflow"
  arguments <- readFile $ dir </> "arguments"
  e <- loadWorkflow (WorkflowName name) i (read arguments)
  -- TODO will the file be kept open if the envArguments is never needed ?

  content_ <- getDirectoryContents $ dir
  let content = filter (all (`elem` "0123456789")) content_
  dirs <- filterM (doesDirectoryExist . (dir </>)) content
  states <- mapM (readFile . (\d -> dir </> d </> "state")) dirs
  return $ foldl' (\x (l, s) -> setStatus (read l) (read s) x) e $ zip dirs states
