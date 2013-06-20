{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import qualified Control.Monad.Random as R
import Data.List (foldl', isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents
  , getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

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

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

newtype WorkflowIdPrefix = WorkflowIdPrefix String
  deriving Show

-- | Some random string to identify a workflow instance.
newtype WorkflowId = WorkflowId String
  deriving Show

-- | Identify a workflow configuration, as registered with `intake define`.
newtype WorkflowName = WorkflowName String
  deriving Show

data Workflow =
    Job String [String] -- ^ command and arguments
  | Sequence Workflow Workflow
  | Parallel Workflow Workflow
  | Retry Int Workflow
  deriving Show

-- | Same as `Workflow` but with additional data.
data WorkflowState =
    SJob Int String [String] JStatus -- ^ label, command and arguments
  | SSequence WorkflowState WorkflowState
  | SParallel WorkflowState WorkflowState
  | SRetry Int Int WorkflowState -- ^ Maximum allowed attempts , already tried.
  deriving Show

data JStatus = Waiting | Ready | Started | Completed
  deriving (Eq, Ord, Show, Read)

data WorkflowEnv = WorkflowEnv
  { envName :: WorkflowName
  , envId :: WorkflowId
  , envArguments :: [String]
  , envState :: WorkflowState
  }
  deriving Show

newtype Run = Run Int
  deriving Show

-- | Whole Workflow status.
data WStatus = WInstanciated | WStarted [Int] | WCompleted
  deriving Show

----------------------------------------------------------------------
-- IO
----------------------------------------------------------------------

newWorkflowId :: IO WorkflowId
newWorkflowId = WorkflowId <$>
  (sequence $ replicate 64 $ R.fromList $ map (,1) $ ['a'..'f'] ++ ['0'..'9'])

run :: WorkflowName -> [String] -> IO WorkflowId
run name@(WorkflowName n) arguments = do
  i@(WorkflowId i') <- newWorkflowId
  putStrLn $ take 12 i' ++ "  Loading workflow `" ++ n ++ "` with arguments "
    ++ show arguments ++ "."
  e <- loadWorkflow name i arguments
  let (e', rs) = step e
  mapM_ (start i e) rs
  saveEnvironment e'
  return i

start :: WorkflowId -> WorkflowEnv -> Run -> IO ()
start (WorkflowId i') e r = do
  home <- getHomeDirectory
  let (cmd, args) = extract e r
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
  "a" -> echo "a"
  "ab" -> echo "a" `Sequence` echo "b"
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

loadEnvironment :: WorkflowIdPrefix -> IO WorkflowEnv
loadEnvironment (WorkflowIdPrefix i') = do
  home <- getHomeDirectory
  let dir = home </> ".intake"
  content_ <- getDirectoryContents dir
  let content = filter (\d -> i' `isPrefixOf` d) content_
  dirs <- filterM (doesDirectoryExist . (dir </>)) content
  case dirs of
    [] -> error "No such build."
    [i] -> loadEnvironment' (WorkflowId i)
    _ -> error "More than one build."
  
loadEnvironment' :: WorkflowId -> IO WorkflowEnv
loadEnvironment' i@(WorkflowId i') = do
  home <- getHomeDirectory
  let dir = home </> ".intake" </> i'
  name <- readFile $ dir </> "workflow"
  arguments <- readFile $ dir </> "arguments"
  e <- loadWorkflow (WorkflowName name) i (read arguments)

  content_ <- getDirectoryContents $ dir
  let content = filter (all (`elem` "0123456789")) content_
  dirs <- filterM (doesDirectoryExist . (dir </>)) content
  states <- mapM (readFile . (\d -> dir </> d </> "state")) dirs
  return $ foldl' (\x (l, s) -> setStatus (read l) (read s) x) e $ zip dirs states

----------------------------------------------------------------------
-- Pure
----------------------------------------------------------------------

initializeWorkflow :: Workflow -> WorkflowState
initializeWorkflow = fst . makeReady False . fst . labelWorkflow 0

labelWorkflow  :: Int -> Workflow -> (WorkflowState, Int)
labelWorkflow l w = case w of
  Job c as -> (SJob l c as Waiting, succ l)
  Sequence a b -> let (a', l') = labelWorkflow l a
                      (b', l'') = labelWorkflow l' b
                  in (SSequence a' b', l'')

makeReady  :: Bool -> WorkflowState -> (WorkflowState, Bool)
makeReady ready w = case w of
  SJob l c as Waiting -> (SJob l c as (if ready then Waiting else Ready), True)
  SSequence a b -> let (a', ready') = makeReady ready a
                       (b', ready'') = makeReady ready' b
                   in (SSequence a' b', ready'')

step :: WorkflowEnv -> (WorkflowEnv, [Run])
step e =
  let (s, rs) = step' $ envState e
  in (e { envState = s }, rs)

step' :: WorkflowState -> (WorkflowState, [Run])
step' s = do
  case s of
    SJob l cmd args Ready -> (SJob l cmd args Started, [Run l])

extract :: WorkflowEnv -> Run -> (String, [String])
extract e (Run l) = case envState e of
  SJob l' cmd args _ | l' == l -> (cmd, args)

status :: WorkflowEnv -> WStatus
status e = case envState e of
  SJob _ _ _ Completed -> WCompleted
  SJob l _ _ Started -> WStarted [l]
  _ -> WInstanciated

setStatus :: Int -> JStatus -> WorkflowEnv -> WorkflowEnv
setStatus l s e = case envState e of
  SJob l' cmd args _ | l == l' -> e { envState = SJob l cmd args s }

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

test0 :: (WorkflowEnv, [Run])
test0 = step $ wrap $ echo "a"

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
