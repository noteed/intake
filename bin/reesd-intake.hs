{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | This should be a re-implementation of the Reesd workflow system. It uses
-- Lovelace and Haskell channels instead of a real queue. It should be merged
-- with/replace intake.hs.
module Main (main) where

import System.Console.CmdArgs.Explicit

import Intake.Commands (intakeModes, processCmd)


------------------------------------------------------------------------------
main :: IO ()
main = processArgs intakeModes >>= processCmd
