{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Intake where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import qualified Control.Monad.Random as R
import Data.List (foldl', isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents
  , getHomeDirectory)
import System.FilePath ((</>))

----------------------------------------------------------------------
-- Types
-- The trivial instances are there because the coverage reporting
-- does not count the corresponding 'deriving' clauses.
----------------------------------------------------------------------

newtype WorkflowIdPrefix = WorkflowIdPrefix String

instance Show WorkflowIdPrefix where
  show (WorkflowIdPrefix s) = s

-- | Some random string to identify a workflow instance.
newtype WorkflowId = WorkflowId String

instance Show WorkflowId where
  show (WorkflowId s) = s

instance Eq WorkflowId where
  (WorkflowId a) == (WorkflowId b) = a == b

-- | Identify a workflow configuration, as registered with `intake define`.
newtype WorkflowName = WorkflowName String

instance Show WorkflowName where
  show (WorkflowName s) = s

instance Eq WorkflowName where
  (WorkflowName a) == (WorkflowName b) = a == b

data Workflow =
    Job String [String] -- ^ command and arguments
  | Sequence Workflow Workflow
  | Parallel Workflow Workflow
  | Retry Int Workflow

-- | Same as `Workflow` but with additional data.
data WorkflowState =
    SJob Int String [String] JStatus -- ^ label, command and arguments
  | SSequence WorkflowState WorkflowState
  | SParallel WorkflowState WorkflowState
  | SRetry Int Int WorkflowState -- ^ Maximum allowed attempts , already tried.

instance Show WorkflowState where
  show (SJob l cmd args s) = "#" ++ show l ++ " " ++ cmd ++ " " ++ show args ++ "(" ++ show s ++ ")"
  show (SSequence a b) = "(" ++ show a ++ " >> " ++ show b ++ ")"
  show (SParallel a b) = "(" ++ show a ++ " // " ++ show b ++ ")"
  show (SRetry m n a) = show a ++ " : " ++ show n ++ "/" ++ show m

instance Eq WorkflowState where
  SJob i cmd args s == SJob i' cmd' args' s' = i == i' && cmd == cmd'
   && args == args' && s == s'
  SSequence a b == SSequence a' b' = a == a' && b == b'
  SParallel a b == SParallel a' b' = a == a' && b == b'
  SRetry m n a == SRetry m' n' a' = m == m' && n == n' && a == a'
  _ == _ = False

data JStatus = Waiting | Ready | Started | Completed
  deriving (Eq, Show, Read)

data WorkflowEnv = WorkflowEnv
  { envName :: WorkflowName
  , envId :: WorkflowId
  , envArguments :: [String]
  , envState :: WorkflowState
  }

instance Show WorkflowEnv where
  show WorkflowEnv{..} = unlines
    [ "workflow: " ++ show envName
    , "id: " ++ show envId
    , "arguments: " ++ show envArguments
    , "state: " ++ show envState
    ]

instance Eq WorkflowEnv where
  a == b = envName a == envName b &&
    envId a == envId b &&
    envArguments a == envArguments b &&
    envState a == envState b

newtype Run = Run Int

instance Show Run where
  show (Run i) = "run #" ++ show i

instance Eq Run where
  (Run a) == (Run b) = a == b

-- | Whole Workflow status.
data WStatus = WInstanciated | WStarted [Int] | WCompleted
  deriving (Eq, Show)

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
  let (cmd, args) = maybe (error "No such job ID.") id $ extract e r
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
  -- TODO will the file be kept open if the envArguments is never needed ?

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
  Parallel a b -> let (a', l') = labelWorkflow l a
                      (b', l'') = labelWorkflow l' b
                  in (SParallel a' b', l'')
  Retry n a -> let (a', l') = labelWorkflow l a
               in (SRetry n 0 a', l')

makeReady  :: Bool -> WorkflowState -> (WorkflowState, Bool)
makeReady ready w = case w of
  SJob l c as Waiting -> (SJob l c as (if ready then Waiting else Ready), True)
  SJob l c as Ready -> (SJob l c as Ready, False)
  SJob l c as Started -> (SJob l c as Started, False)
  SJob l c as Completed -> (SJob l c as Completed, False)
  SSequence a b -> let (a', ready') = makeReady ready a
                       (b', ready'') = makeReady ready' b
                   in (SSequence a' b', ready'')
  SParallel a b -> let (a', ready') = makeReady ready a
                       (b', ready'') = makeReady ready b
                   in (SParallel a' b', ready' || ready'')
  SRetry m n a -> let (a', ready') = makeReady ready a
                  in (SRetry m n a', ready')

-- | Find all `Ready` jobs, and start them (i.e. change their status to
-- `Started` and return their ID as Run commands).
step :: WorkflowEnv -> (WorkflowEnv, [Run])
step e =
  let (s, rs) = step' $ envState e
  in (e { envState = s }, rs)

step' :: WorkflowState -> (WorkflowState, [Run])
step' s = do
  case s of
    SJob l cmd args Ready -> (SJob l cmd args Started, [Run l])
    SSequence a b ->
      if isCompleted a
      then let (b', rs) = step' b
           in (SSequence a b', rs)
      else let (a', rs) = step' a
           in (SSequence a' b, rs)
    SParallel a b ->
      let (a', ars) = step' a
          (b', brs) = step' b
      in (SParallel a' b', ars ++ brs)
    job -> (job, [])

isCompleted :: WorkflowState -> Bool
isCompleted s = case s of
  SJob _ _ _ Completed -> True
  SSequence a b -> isCompleted a && isCompleted b
  SParallel a b -> isCompleted a && isCompleted b
  SRetry _ _ a -> isCompleted a
  _ -> False

extract :: WorkflowEnv -> Run -> Maybe (String, [String])
extract e r = extract' (envState e) r

extract' :: WorkflowState -> Run -> Maybe (String, [String])
extract' s (Run l) = case s of
  SJob l' cmd args _ | l' == l -> Just (cmd, args)
  SSequence a b -> case extract' a (Run l) of
    Nothing -> extract' b (Run l)
    found -> found

status :: WorkflowEnv -> WStatus
status e = status' $ envState e

status' :: WorkflowState -> WStatus
status' s = case s of
  SJob _ _ _ Completed -> WCompleted
  SJob l _ _ Started -> WStarted [l]
  SSequence a b -> case (status' a, status' b) of
    (WCompleted, WCompleted) -> WCompleted
    (WInstanciated, WInstanciated) -> WInstanciated
    (WStarted as, WStarted bs) -> WStarted $ as ++ bs -- I guess this can't happen.
    (WStarted as, _) -> WStarted as
    (_, WStarted bs) -> WStarted bs
    (WCompleted, WInstanciated) -> WStarted []
    (WInstanciated, WCompleted) -> WStarted []
  SParallel a b -> case (status' a, status' b) of
    (WCompleted, WCompleted) -> WCompleted
    (WInstanciated, WInstanciated) -> WInstanciated
    (WStarted as, WStarted bs) -> WStarted $ as ++ bs
    (WStarted as, _) -> WStarted as
    (_, WStarted bs) -> WStarted bs
    (WCompleted, WInstanciated) -> WStarted []
    (WInstanciated, WCompleted) -> WStarted []
  _ -> WInstanciated

setStatus :: Int -> JStatus -> WorkflowEnv -> WorkflowEnv
setStatus l s e = e { envState = setStatus' l s (envState e) }

setStatus' l st s = case s of
  SJob l' cmd args st' | l == l' -> SJob l' cmd args st
                       | otherwise -> SJob l' cmd args st'
  SSequence a b -> setStatus' l st a `SSequence` setStatus' l st b
  SParallel a b -> setStatus' l st a `SParallel` setStatus' l st b

complete l e = e { envState = fst $ makeReady False $ complete' l $ envState e }

complete' l = setStatus' l Completed
