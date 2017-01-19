{-# LANGUAGE RecordWildCards #-}
-- | This should be a re-implementation of the Reesd workflow system. It uses
-- Lovelace and Haskell channels instead of a real queue. It should be merged
-- with/replace intake.hs.
module Main (main) where

import Data.Version (showVersion)
import Paths_intake (version)
import System.Console.CmdArgs.Explicit


------------------------------------------------------------------------------
main :: IO ()
main = processArgs intakeModes >>= processCmd


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

processCmd Dummy{..} = do
  logging ("Dummy command...")


------------------------------------------------------------------------------
-- | Available commands.
data Cmd =
    Parse
    { cmdFilePath :: String
    }
  | Dummy
  | Help
  | Version
  | None
  deriving Show


------------------------------------------------------------------------------
intakeModes :: Mode Cmd
intakeModes = (modes "intake" None "UNIX Process workflows."
  [ intakeParseMode
  , intakeDummyMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

intakeParseMode :: Mode Cmd
intakeParseMode = mode' "parse" intakeParse
  "Parse a JSON workflow file."
  [ flagReq ["file"]
      (\x r -> Right (r { cmdFilePath = x }))
      "PATH"
      "JSON workflow definition."
  ]

intakeDummyMode :: Mode Cmd
intakeDummyMode = mode' "dummy" intakeDummy
  "Dummy mode."
  []


------------------------------------------------------------------------------
intakeParse = Parse
  { cmdFilePath = ""
  }

intakeDummy = Dummy


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
