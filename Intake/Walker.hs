{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Intake.Walker where

import Control.Concurrent.Chan (readChan, writeChan, Chan)
import Control.Concurrent.MVar (putMVar, takeMVar, MVar)
import Control.Monad (when)
import Data.Aeson (object, ToJSON, Value, (.=))

import Lovelace

import Intake.Types (showJSON, WalkerInput(..), WalkState(..), WorkerInput(..))
import Intake.Workflow


------------------------------------------------------------------------------
walker :: Bool -> MVar WalkState -> Chan WalkerInput -> Chan WorkerInput -> IO Value
walker doLog walkMV inputs workerC = do
  input <- readChan inputs
  walk <- takeMVar walkMV
  walk' <- case input of

    WalkerStart -> do
      when doLog $
        logging ("intake-walker " ++ workflowName (wsWorkflow walk) ++ " Starting instance with " ++ showJSON (wsArgs walk))
      let t' = RToken (wsArgs walk)
          s' = start (wsWorkflow walk) (wsState walk) t'
      handleStep doLog walk workerC s' t'

    WalkerTaskResult activity args -> do
      when doLog $
        logging ("intake-walker " ++ workflowName (wsWorkflow walk) ++ " Received result " ++ showJSON args)
      handleTaskResult doLog walk workerC activity args

  putMVar walkMV walk'
  case wsOutput walk' of
    Nothing -> walker doLog walkMV inputs workerC
    Just output -> return output

-- Step is te result of stepping the workflow instance.
-- (E.g. after start or continue.) It can be directly a token, or
-- a task, which can be feeded k, to return later a token
-- (handled in WalkerTaskResult).
handleStep :: (ToJSON s) => Bool -> WalkState -> Chan WorkerInput -> Step s RTask RToken String -> RToken -> IO WalkState
handleStep doLog walk workerC (Step workflow a s t) (RToken k) =
  when doLog (
    logging ("intake-walker " ++ workflowName workflow ++ " Handling activity \""
      ++ activityName a ++ "\" " ++ case t of
      Task' task -> show task ++ showJSON k -- k is the previous activity output
      Token (RToken token) -> showJSON token))
  >> case t of
  -- Run the task returned by the activity.
  Task' BuildImageTask -> enqueueTask doLog walk workerC workflow a k WorkerBuild
  Task' CloneTask -> enqueueTask doLog walk workerC workflow a k WorkerClone
  Task' RunImage -> enqueueTask doLog walk workerC workflow a k WorkerRun
  Task' SendMailTask -> do
    when doLog $
      logging ("intake-walker " ++ workflowName workflow ++ " Prentending to send an email...")
    let t' = RToken (object ["tag" .= ("success" :: String)])
        s' = continue workflow a s t'
    handleStep doLog walk workerC s' t'
  Task' WaitGetRequest -> do
    when doLog $
      logging ("intake-walker " ++ workflowName workflow ++ " Prentending to wait a GET request...")
    let t' = RToken (object ["tag" .= ("success" :: String)])
        s' = continue workflow a s t'
    handleStep doLog walk workerC s' t'
  Task' Barrier -> do
    when doLog $
      logging ("intake-walker " ++ workflowName workflow ++ " Prentending to wait on a barrier...")
    let t' = RToken (object ["tag" .= ("success" :: String)])
        s' = continue workflow a s t'
    handleStep doLog walk workerC s' t'
  Token (RToken t') | final workflow a -> do
    when doLog $
      logging ("intake-walker " ++ workflowName workflow ++ " Completed with \"" ++ activityName a ++ "\" " ++ showJSON t')
    return walk { wsOutput = Just t' }
  Token t'@(RToken t_) -> do
    when doLog $
      logging ("intake-walker " ++ workflowName workflow ++ " Handling token " ++ showJSON t_)
    let s' = continue workflow a s t'
    handleStep doLog walk workerC s' t'

handleTaskResult :: Bool -> WalkState -> Chan WorkerInput -> String -> Value -> IO WalkState
handleTaskResult doLog walk@WalkState{..} workerC activity args = do
  case lookup activity (activities wsWorkflow) of
    Just activity' -> do
      let t' = RToken args
          s' = continue wsWorkflow activity' wsState t'
      handleStep doLog walk workerC s' t'
    Nothing -> do
      when doLog $
        logging "ERROR: Can't find activity."
      return walk

enqueueTask doLog walk workerC workflow a k t = do
  when doLog $
    logging ("intake-walker " ++ workflowName workflow ++ " Enqueuing task...")
  writeChan workerC (t (workflowName workflow) (activityName a) k)
  return walk


--------------------------------------------------------------------------------
logging :: String -> IO ()
logging = putStrLn
