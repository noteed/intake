module Intake.Engine where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan, writeChan, Chan)
import Control.Concurrent.MVar (putMVar)
import Control.Monad (forM_, when)

import Intake.Core
import Intake.Process (backend)

-- | If `once` it True, when the number of instanciated workflows reaches
-- zero, the loop will exit.
loop :: Chan Command -> Int -> Bool -> IO ()
loop chan envs once = do
  let continue envs' = loop chan envs' once
      advance e = do
        let (e', rs) = step e
        writeChan chan $ Start (envId e') rs
        when (isCompleted $ envState e') $ writeChan chan (Free $ envId e')
  c <- readChan chan
  case c of
    Instanciate name args mvar -> do
      e <- instanciate backend name args
      putMVar mvar $ envId e
      advance e
      continue $ succ envs
    Start i rs -> do
      e <- load backend i
      forM_ rs $ \r -> do
        _ <- forkIO $ do
          start backend e r
          writeChan chan $ End i r
        return ()
      continue envs
    End i (Run r) -> do
      e <- load backend i
      let e' = complete r e
      advance e'
      continue envs
    Free _ -> do
      let envs' = pred envs
      if once && envs' == 0
        then writeChan chan Quit
        else continue envs'
    Quit -> return ()
