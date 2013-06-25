module Intake.Engine where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Control.Monad (when)
import System.Process (waitForProcess)

import Intake.Core hiding (advance, instanciate, run)
import Intake.Process (instanciate, loadEnvironment, saveEnvironment, start)

data Command =
    Quit
  | Instanciate (Either String WorkflowName) [String]
  | Start WorkflowId Run
  | End WorkflowId Run

run :: (Either String WorkflowName) -> [String] -> IO ()
run name args = do
  chan <- newChan
  writeChan chan $ Instanciate name args
  loop chan

loop :: Chan Command -> IO ()
loop chan = do
  let continue = loop chan
      advance e = do
        let (e', rs) = step e
        mapM_ (writeChan chan . Start (envId e')) rs
        when (isCompleted $ envState e') $ writeChan chan Quit
  c <- readChan chan
  case c of
    Instanciate name args -> do
      e <- instanciate name args
      saveEnvironment e
      advance e
      continue
    Start i r -> do
      e <- loadEnvironment i
      h <- start e r
      _ <- forkIO $ do
        _ <- waitForProcess h
        writeChan chan $ End i r
      continue
    End i (Run r) -> do
      e <- loadEnvironment i
      let e' = complete r e
      advance e'
      continue
    Quit -> return ()
