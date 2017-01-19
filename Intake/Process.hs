{-# LANGUAGE TupleSections #-}
-- | Implement the Intake engine backend with 'System.Process'. The
-- environment is saved to/loaded from the file system.
module Intake.Process where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (readMVar)
import Control.Monad (filterM)
import qualified Control.Monad.Random as R
import Data.List (foldl', isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getDirectoryContents, getHomeDirectory)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((</>))
import System.IO (withFile, IOMode(WriteMode))
import System.Process
import System.Process.Internals (ProcessHandle(..), ProcessHandle__(..))

import Intake.Core hiding (inspect, instanciate, load, logs, start)

backend :: Backend
backend = Backend instanciate start loadEnvironment inspect logs

-- | Implement `Intake.Core.instanciate` by loading a workflow from the file
-- system and saving the instance to the file system too.
instanciate :: (Either String WorkflowName) -> [String] -> IO WorkflowEnv
instanciate name arguments = do
  i@(WorkflowId i') <- newWorkflowId
  e <- case name of
    Left cmd -> do
      putStrLn $ take 12 i' ++ "  Instanciating command `" ++ cmd ++
       "` with arguments " ++ show arguments ++ "."
      makeWorkflow cmd i arguments
    Right (WorkflowName n) -> do
      putStrLn $ take 12 i' ++ "  Instanciating workflow `" ++ n ++
        "` with arguments " ++ show arguments ++ "."
      loadWorkflow (WorkflowName n) i arguments
  saveEnvironment e
  return e

newWorkflowId :: IO WorkflowId
newWorkflowId = WorkflowId <$>
  (sequence $ replicate 64 $ R.fromList $ map (,1) $ ['a'..'f'] ++ ['0'..'9'])

start :: WorkflowEnv -> Run -> IO ()
start e r = do
  home <- getHomeDirectory
  let (cmd, args) = maybe (error "No such job ID.") id $ extract e r -- TODO no error
      WorkflowId i' = envId e
      Run l = r
      dir = home </> ".intake" </> i' </> show l
  putStrLn $ take 12 i' ++ "  Starting `" ++ cmd
    ++ "` with arguments " ++ show args ++ "."
  createDirectoryIfMissing True dir
  writeFile (dir </> "cmdline") $ cmd ++ " " ++ show args
  writeFile (dir </> "state") "Started"

  (_, _, _, h) <- createProcess $ proc "intake" $
    -- The -- is not documented in cmdargs but but makes sure that any flag
    -- present in args will be left untouched by it.
    ["work", "--", i', show l, cmd] ++ args
  mi <- processHandleToInt h
  case mi of
    Right i -> writeFile (dir </> "pid") $ show i
    Left ExitSuccess -> writeFile (dir </> "exitcode") "0"
    Left (ExitFailure c) -> writeFile (dir </> "exitcode") $ show c

  _ <- waitForProcess h
  return ()

-- | Wrap a command so that its "state" file is changed to "Completed" upon
-- completion of the process.
work :: WorkflowId -> Int -> String -> [String] -> IO ()
work (WorkflowId i') l cmd args = do
  home <- getHomeDirectory
  let dir = home </> ".intake" </> i' </> show l
  (_, _, _, h) <-
    -- TODO close stdin.
    withFile (dir </> "stderr") WriteMode $ \err ->
    withFile (dir </> "stdout") WriteMode $ \out ->
    createProcess (proc cmd args)
      { std_out = UseHandle out, std_err = UseHandle err }
  code <- waitForProcess h
  case code of
    ExitSuccess -> writeFile (dir </> "exitcode") "0"
    ExitFailure c -> writeFile (dir </> "exitcode") $ show c
  writeFile (dir </> "state") "Completed"
  exitWith code

makeWorkflow :: String -> WorkflowId -> [String] -> IO WorkflowEnv
makeWorkflow cmd i arguments = do
  let w = Single $ defaultJob' cmd arguments
      s = initializeWorkflow w
  return $ WorkflowEnv (Left cmd) i arguments s

loadWorkflow :: WorkflowName -> WorkflowId -> [String] -> IO WorkflowEnv
loadWorkflow name i arguments = do
  w <- readWorkflow name arguments
  let s = initializeWorkflow w
  return $ WorkflowEnv (Right name) i arguments s

-- TODO return a Maybe.
readWorkflow :: WorkflowName -> [String] -> IO Workflow
readWorkflow (WorkflowName name) arguments = return $ case name of
  "a" -> Single (defaultJob' "echo" ["a"])
  "ab" -> Single (defaultJob' "echo" ["a"]) `Sequence` Single (defaultJob' "echo" ["b"])
  "sleep2" -> Single (defaultJob' "sleep" ["2"]) `Sequence` Single (defaultJob' "echo" ["a"])
    `Sequence` Single (defaultJob' "sleep" ["3"]) `Sequence` Single (defaultJob' "echo" ["b"])
  "ping" -> Single (defaultJob' "ping" arguments)
  _ -> error "No such workflow."
--  (Single "echo" ["a"] `Sequence` Single "echo" ["b"])
--    `Parallel` Single "echo" ["c"]

saveEnvironment :: WorkflowEnv -> IO ()
saveEnvironment e = do
  home <- getHomeDirectory
  let WorkflowId i' = envId e
      dir = home </> ".intake" </> i'
  createDirectoryIfMissing True dir
  case envName e of
    Right (WorkflowName n) -> writeFile (dir </> "workflow") n
    Left cmd -> writeFile (dir </> "command") cmd
  writeFile (dir </> "arguments") (show $ envArguments e)

inspect :: WorkflowIdPrefix -> IO WorkflowEnv
inspect i = do
  i' <- fullWorkflowId i
  loadEnvironment i'
  
loadEnvironment :: WorkflowId -> IO WorkflowEnv
loadEnvironment i@(WorkflowId i') = do
  home <- getHomeDirectory
  let dir = home </> ".intake" </> i'
  arguments <- readFile $ dir </> "arguments"
  exist <- doesFileExist (dir </> "command")
  e <-
    if exist
    then do
      cmd <- readFile $ dir </> "command"
      makeWorkflow cmd i (read arguments)
    else do
      name <- readFile $ dir </> "workflow"
      loadWorkflow (WorkflowName name) i (read arguments)
  -- TODO will the file be kept open if the envArguments is never needed ?

  content_ <- getDirectoryContents $ dir
  let content = filter (all (`elem` "0123456789")) content_
  dirs <- filterM (doesDirectoryExist . (dir </>)) content
  states <- mapM (readFile . (\d -> dir </> d </> "state")) dirs
  return $ foldl' (\x (l, s) -> setStatus (read l) (read s) x) e $ zip dirs states

logs :: WorkflowIdPrefix -> IO String
logs i = do
  (WorkflowId i') <- fullWorkflowId i
  home <- getHomeDirectory
  let dir = home </> ".intake" </> i' </> "0" --TODO
  readFile $ dir </> "stdout"

fullWorkflowId :: WorkflowIdPrefix -> IO WorkflowId
fullWorkflowId (WorkflowIdPrefix i') = do
  home <- getHomeDirectory
  let dir = home </> ".intake"
  content_ <- getDirectoryContents dir
  let content = filter (\d -> i' `isPrefixOf` d) content_
  dirs <- filterM (doesDirectoryExist . (dir </>)) content
  case dirs of
    [] -> error "No such instance."
    [i] -> return $ WorkflowId i
    _ -> error "More than one instance."

-- | Convenience function to turn a ProcessHandle into an Int.
processHandleToInt :: ProcessHandle -> IO (Either ExitCode Int)
processHandleToInt (ProcessHandle mvar _) = do
  h <- readMVar mvar
  case h of
    OpenHandle i -> return . Right $ fromIntegral i
    ClosedHandle c -> return $ Left c
