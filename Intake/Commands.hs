{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | cmdargs definitions for intake.
module Intake.Commands where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent.MVar (newMVar)
import Data.Aeson (decode, object, Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Paths_intake (version)
import System.Console.CmdArgs.Explicit

import Lovelace (Workflow)

import Intake.Types (WalkerInput(..), WalkState(..), Worker, WorkerInput(..))
import Intake.Walker (walker)
import Intake.Worker (successWorker)
import Intake.Workflow (toWorkflow, RTask, RToken)


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString = "reesd-intake " ++ showVersion version
  ++ " - Copyright 2017 Hypered SPRL."


------------------------------------------------------------------------------
-- | Process the command-line choice.
-- TODO Move the runCmd implementation in another module.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault intakeModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd Parse{..} = do
  logging ("Parsing file " ++ cmdFilePath ++ "...")
  mdef <- parseFile cmdFilePath
  case mdef of
    Right def -> print def
    Left err -> error err

processCmd Run{..} = do
  logging ("Running file " ++ cmdFilePath ++ "...")
  mdef <- parseFile cmdFilePath
  margs <- maybe (return (Right (object []))) parseFileArgs cmdFilePathArgs
  case mdef of
    Right def -> case margs of
      Right args -> run True (successWorker True) def args >> return ()
      Left err -> error err
    Left err -> error err


------------------------------------------------------------------------------
parseFile :: String -> IO (Either String (Workflow Value RTask RToken String))
parseFile filepath = do
  input <- LB.readFile filepath
  case decode input of
    Just definition -> return (toWorkflow definition)
    Nothing -> return (Left "Can't decode input as workflow definition.")

parseFileArgs :: String -> IO (Either String Value)
parseFileArgs filepath = do
  input <- LB.readFile filepath
  case decode input of
    Just args -> return (Right args)
    Nothing -> return (Left "Can't decode input as valid JSON.")


------------------------------------------------------------------------------
run :: Bool -> Worker -> Workflow Value RTask RToken String -> Value -> IO Value
run doLog worker def args = do
  -- The walker is not responsible of instanciating the workflow.
  w <- instanciate def args
  walkMV <- newMVar w
  walkerC <- newChan
  workerC <- newChan

  -- Worker thread.
  forkIO (worker workerC walkerC)

  -- Walker thread.
  writeChan walkerC WalkerStart
  output <- walker doLog walkMV walkerC workerC

  -- Kill the worker thread.
  writeChan workerC WorkerDone

  return output


------------------------------------------------------------------------------
instanciate :: Workflow Value RTask RToken String -> Value -> IO WalkState
instanciate def args = do
  now <- getCurrentTime
  return WalkState
    { wsCreated = now
    , wsWorkflow = def
    , wsType = "JSON"
    , wsArgs = args
    , wsState = object []
    , wsOutput = Nothing
    }


------------------------------------------------------------------------------
-- | Available commands.
data Cmd =
    Parse
    { cmdFilePath :: String
    }
  | Run
    { cmdFilePath :: String
    , cmdFilePathArgs :: Maybe String
    }
  | Help
  | Version
  | None
  deriving Show


------------------------------------------------------------------------------
intakeModes :: Mode Cmd
intakeModes = (modes "intake" None "UNIX Process workflows."
  [ intakeParseMode
  , intakeRunMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

intakeParseMode :: Mode Cmd
intakeParseMode = mode' "parse" intakeParse
  "Parse a JSON workflow file."
  [ fileFlag
  ]

intakeRunMode :: Mode Cmd
intakeRunMode = mode' "run" intakeRun
  "Run a JSON workflow file."
  [ fileFlag
  , argsFlag
  ]


fileFlag = flagReq ["file"]
  (\x r -> Right (r { cmdFilePath = x }))
  "PATH"
  "JSON workflow definition."

argsFlag = flagReq ["args"]
  (\x r -> Right (r { cmdFilePathArgs = Just x }))
  "PATH"
  "JSON workflow input."

------------------------------------------------------------------------------
intakeParse = Parse
  { cmdFilePath = ""
  }

intakeRun = Run
  { cmdFilePath = ""
  , cmdFilePathArgs = Nothing
  }


--------------------------------------------------------------------------------
logging = putStrLn


------------------------------------------------------------------------------
-- | Same as `mode` but without an `Arg a` argument.
-- TODO Possibly move this into another repository. Currently this is
-- probably fine as all Reesd projects import reesd-database.
mode' :: Name -> a -> Help -> [Flag a] -> Mode a
mode' name value help flags = (modeEmpty value)
  { modeNames = [name]
  , modeHelp = help
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup flags
  }
