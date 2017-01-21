{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Intake.Walker where

import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar, MVar)
import Data.Aeson (decode, encode, object, ToJSON, Value, (.=))
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)

import Lovelace

import Intake.Types
import Intake.Workflow


------------------------------------------------------------------------------
walker :: MVar WalkState -> Chan WalkerInput -> Chan WorkerInput -> IO Value
walker walkMV inputs workerC = do
  input <- readChan inputs
  walk <- takeMVar walkMV
  walk' <- case input of

    WalkerStart -> do
      logging ("intake-walker " ++ workflowName (wsWorkflow walk) ++ " Starting instance with " ++ showJSON (wsArgs walk))
      let t' = RToken (wsArgs walk)
          s' = start (wsWorkflow walk) (wsArgs walk) t'
      handleStep walk workerC s' t'

    WalkerTaskResult activity args -> do
      logging ("intake-walker " ++ workflowName (wsWorkflow walk) ++ " Received result " ++ showJSON args)
      handleTaskResult walk workerC activity args

  putMVar walkMV walk'
  case wsOutput walk' of
    Nothing -> walker walkMV inputs workerC
    Just output -> return output

-- Step is te result of stepping the workflow instance.
-- (E.g. after start or continue.) It can be directly a token, or
-- a task, which can be feeded k, to return later a token
-- (handled in WalkerTaskResult).
handleStep :: (ToJSON s) => WalkState -> Chan WorkerInput -> Step s RTask RToken String -> RToken -> IO WalkState
handleStep walk workerC (Step workflow a s t) (RToken k) =
  logging ("intake-walker " ++ workflowName workflow ++ " Handling activity \""
    ++ activityName a ++ "\" " ++ case t of
    Task' task -> show task ++ showJSON k -- k is the previous activity output
    Token (RToken token) -> showJSON token)
  >> case t of
  -- Run the task returned by the activity.
  Task' BuildImageTask -> enqueueTask walk workerC workflow a k WorkerBuild
  Task' CloneTask -> enqueueTask walk workerC workflow a k WorkerClone
  Task' RunImage -> enqueueTask walk workerC workflow a k WorkerRun
  Task' SendMailTask -> do
    logging ("intake-walker " ++ workflowName workflow ++ " Prentending to send an email...")
    let t' = RToken (object ["tag" .= ("success" :: String)])
        s' = continue workflow a s t'
    handleStep walk workerC s' t'
  Task' WaitGetRequest -> do
    logging ("intake-walker " ++ workflowName workflow ++ " Prentending to wait a GET request...")
    let t' = RToken (object ["tag" .= ("success" :: String)])
        s' = continue workflow a s t'
    handleStep walk workerC s' t'
  Task' Barrier -> do
    logging ("intake-walker " ++ workflowName workflow ++ " Prentending to wait on a barrier...")
    let t' = RToken (object ["tag" .= ("success" :: String)])
        s' = continue workflow a s t'
    handleStep walk workerC s' t'
  Token (RToken t') | final workflow a -> do
    logging ("intake-walker " ++ workflowName workflow ++ " Completed with \"" ++ activityName a ++ "\" " ++ showJSON t')
    return walk { wsOutput = Just t' }
  Token t' -> do
    logging ("intake-walker " ++ workflowName workflow ++ " Handling token...")
    let s' = continue workflow a s t'
    handleStep walk workerC s' t'

handleTaskResult walk@WalkState{..} workerC activity args = do
  case lookup activity (activities wsWorkflow) of
    Just activity' -> do
      let t' = RToken args
          s' = continue wsWorkflow activity' wsState t'
      handleStep walk workerC s' t'
    Nothing -> do
      logging "ERROR: Can't find activity."
      return walk

enqueueTask walk workerC workflow a k t = do
  logging ("intake-walker " ++ workflowName workflow ++ " Enqueuing task...")
  writeChan workerC (t (workflowName workflow) (activityName a) k)
  return walk


------------------------------------------------------------------------------
showJSON = LB.unpack . encode


--------------------------------------------------------------------------------
logging = putStrLn
