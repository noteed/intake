{-# LANGUAGE OverloadedStrings #-}
module Intake.Worker where

import Control.Concurrent.Chan (readChan, writeChan, Chan)
import Data.Aeson (object, (.=))

import Intake.Types (WalkerInput(..), WorkerInput(..))


------------------------------------------------------------------------------
worker :: Chan WorkerInput -> Chan WalkerInput -> IO ()
worker inputs walkerC = do
  input <- readChan inputs
  case input of

    WorkerBuild workflow activity args ->
      handleTask inputs walkerC "WorkerBuild" workflow activity args

    WorkerRun workflow activity args ->
      handleTask inputs walkerC "WorkerRun" workflow activity args

    WorkerDone ->
      -- Let the worker die.
      return ()

handleTask inputs walkerC name workflow activity args = do
  logging ("intake-worker " ++ workflow ++ " Handling task \"" ++ activity ++"\" " ++ name ++ " " ++ show args ++ "...")
  logging ("intake-worker " ++ workflow ++ " Enqueuing task result...")
  let args' = object ["tag" .= ("success" :: String)]
  writeChan walkerC (WalkerTaskResult activity args')
  worker inputs walkerC


--------------------------------------------------------------------------------
logging = putStrLn
