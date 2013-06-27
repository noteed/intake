module Intake.Engine where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan, writeChan, Chan)
import Control.Concurrent.MVar (putMVar, MVar)
import Control.Monad (unless, when)
import System.Process (waitForProcess)

import Intake.Core hiding (advance, instanciate)
import Intake.Process (instanciate, loadEnvironment, saveEnvironment, start)

type Envs = Map WorkflowId WorkflowEnv

data Command =
    Quit
    -- ^ Stop the main loop.
  | Instanciate (Either String WorkflowName) [String] (MVar WorkflowId)
    -- ^ Instanciate a workflow. The MVar is used to return the WorkflowId.
  | Start WorkflowId Run
    -- ^ Start a job for a given workflow.
  | End WorkflowId Run
    -- ^ Notify the engine that a job was completed.
  | Free WorkflowId
    -- ^ Notify the engine that a workflow was completed.

loop :: Chan Command -> Envs -> Bool -> IO ()
loop chan envs once = do
  let continue envs' = loop chan envs' once
      advance e = do
        let (e', rs) = step e
        mapM_ (writeChan chan . Start (envId e')) rs
        when (isCompleted $ envState e') $ writeChan chan (Free $ envId e')
  c <- readChan chan
  case c of
    Instanciate name args mvar -> do
      e <- instanciate name args
      saveEnvironment e
      putMVar mvar $ envId e
      advance e
      continue (M.insert (envId e) e envs)
    Start i r -> do
      e <- loadEnvironment i
      h <- start e r
      _ <- forkIO $ do
        _ <- waitForProcess h
        writeChan chan $ End i r
      continue envs
    End i (Run r) -> do
      e <- loadEnvironment i
      let e' = complete r e
      advance e'
      continue envs
    Free i -> do
      let envs' = M.delete i envs
      unless (once && M.null envs') $ continue envs'
    Quit -> return ()
