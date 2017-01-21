module Intake.Types where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time () -- For the instance Show UTCTime.

import Lovelace (Workflow)

import Intake.Workflow (RTask, RToken)


------------------------------------------------------------------------------
data WalkerInput =
    WalkerStart  -- ^ Ask the walker thread to start an instance.
  | WalkerTaskResult String Value -- ^ Notify the walker thread a task has completed, specifying the activity name, and the result as JSON.
  deriving Show

data WorkerInput =
    WorkerBuild String String Value -- ^ Ask the worker to perform a build, specifying the workflow name, activity name, and the build arguments.
  | WorkerRun String String Value
  | WorkerClone String String Value
  | WorkerDone -- ^ Ask the worker to die.
  deriving Show


------------------------------------------------------------------------------
data WalkState = WalkState
  { wsCreated :: UTCTime
  , wsWorkflow :: Workflow Value RTask RToken String
  , wsType :: Text
  , wsArgs :: Value
  , wsState :: Value
  , wsOutput :: Maybe Value -- Nothing when the walk isn't complete.
  }
  deriving Show
