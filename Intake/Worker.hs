{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Intake.Worker where

import Control.Concurrent.Chan (readChan, writeChan, Chan)
import Control.Monad (when)
import Data.Aeson (object, (.=), Value)

import Intake.Types (showJSON, WalkerInput(..), WorkerInput(..), Handler(..), Worker)


------------------------------------------------------------------------------
-- | This worker doesn't do anything beside logging its tasks and returning
-- always success.
successWorker :: Bool -> Worker
successWorker doLog = worker (successHandler doLog)

successHandler :: Bool -> Handler
successHandler doLog = Handler
  { handleBuild = \inputs walkerC workflow activity args ->
      successTask doLog inputs walkerC "WorkerBuild" workflow activity args
  , handleRun = \inputs walkerC workflow activity args ->
      successTask doLog inputs walkerC "WorkerRrun" workflow activity args
  , handleClone = \inputs walkerC workflow activity args ->
      successTask doLog inputs walkerC "WorkerClone" workflow activity args
  }

successTask :: Bool -> Chan WorkerInput -> Chan WalkerInput -> String -> String -> String -> Value -> IO ()
successTask doLog inputs walkerC name workflow activity args = do
  let args' = object ["tag" .= ("success" :: String)]
  when doLog $ do
    logging ("intake-worker " ++ workflow ++ " Handling task \"" ++ activity ++"\" " ++ name ++ " " ++ showJSON args ++ "...")
    logging ("intake-worker " ++ workflow ++ " Enqueuing result " ++ showJSON args')
  writeChan walkerC (WalkerTaskResult activity args')
  successWorker doLog inputs walkerC


------------------------------------------------------------------------------
worker :: Handler -> Chan WorkerInput -> Chan WalkerInput -> IO ()
worker Handler{..} inputs walkerC = do
  input <- readChan inputs
  case input of

    WorkerBuild workflow activity args ->
      handleBuild inputs walkerC workflow activity args

    WorkerRun workflow activity args ->
      handleRun inputs walkerC workflow activity args

    WorkerClone workflow activity args ->
      handleClone inputs walkerC workflow activity args

    WorkerDone ->
      -- Let the worker die.
      return ()


--------------------------------------------------------------------------------
logging :: String -> IO ()
logging = putStrLn
