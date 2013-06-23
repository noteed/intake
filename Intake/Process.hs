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
import System.IO (withFile, IOMode(WriteMode))
import System.Process
import System.Process.Internals (ProcessHandle(..), ProcessHandle__(..))

import Intake.Core hiding (advance, inspect, instanciate, load)

backend :: Backend
backend = Backend instanciate inspect advance

-- | Implement `Intake.Core.instanciate`.
instanciate :: WorkflowName -> [String] -> IO WorkflowId
instanciate name@(WorkflowName n) arguments = do
  i@(WorkflowId i') <- newWorkflowId
  putStrLn $ take 12 i' ++ "  Instanciating workflow `" ++ n ++ "` with arguments "
    ++ show arguments ++ "."
  e <- loadWorkflow name i arguments
  saveEnvironment e
  return i

-- | Implement `Intake.Core.advance`.
advance :: WorkflowId -> IO ()
advance i = do
  e <- loadEnvironment i
  let (e', rs) = step e
  mapM_ (start e') rs

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
  writeFile (dir </> "cmdline") $ cmd ++ " " ++ show args
  writeFile (dir </> "state") "Started"

  (_, _, _, h) <-
    -- TODO close stdin.
    withFile (dir </> "stderr") WriteMode $ \e ->
    withFile (dir </> "stdout") WriteMode $ \o ->
    createProcess (proc cmd args)
      { std_out = UseHandle o, std_err = UseHandle e }
  mi <- processHandleToInt h
  case mi of
    Right i -> writeFile (dir </> "pid") $ show i
    Left ExitSuccess -> writeFile (dir </> "exitcode") $ show 0
    Left (ExitFailure c) -> writeFile (dir </> "exitcode") $ show c

loadWorkflow :: WorkflowName -> WorkflowId -> [String] -> IO WorkflowEnv
loadWorkflow name i arguments = do
  w <- readWorkflow name arguments
  let s = initializeWorkflow w
  return $ WorkflowEnv name i arguments s

-- TODO return a Maybe.
readWorkflow :: WorkflowName -> [String] -> IO Workflow
readWorkflow (WorkflowName name) arguments = return $ case name of
  "a" -> Job "echo" ["a"]
  "ab" -> Job "echo" ["a"] `Sequence` Job "echo" ["b"]
  "ping" -> Job "ping" arguments
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

-- | Convenience function to turn a ProcessHandle into an Int.
processHandleToInt :: ProcessHandle -> IO (Either ExitCode Int)
processHandleToInt (ProcessHandle mvar) = do
  h <- readMVar mvar
  case h of
    OpenHandle i -> return . Right $ fromIntegral i
    ClosedHandle c -> return $ Left c
