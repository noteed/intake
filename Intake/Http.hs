{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Intake.Http
-- Copyright   : (c) 2013 Vo Minh Thu,
--
-- License     : BSD-style
-- Maintainer  : thu@hypered.be
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides the HTTP interface to Intake. It is similar to
-- tibbe's ekg package (and contains some of its code).
--
-- Example client:
--
-- > curl -H "Accept: text/plain" http://127.0.0.1:7001/workflows
-- > curl -H "Accept: application/json" http://127.0.0.1:7001/workflows
-- > curl -H "Accept: text/plain" http://127.0.0.1:7001/workflows/sleep2 -d ''
-- > curl -H "Accept: application/json" http://127.0.0.1:7001/workflows/sleep2 -d ''
module Intake.Http where

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function (on)
import Data.List (sortBy)
import Data.Word (Word8)

import Snap.Core (MonadSnap, Request, getHeaders, getParam, getRequest,
                  method, Method(GET, POST), pass, route,
                  writeBS, writeLBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Conf

import Intake.Core (inspect, status, WorkflowId(..), WorkflowIdPrefix(..), WorkflowName(..))
import Intake.Engine (Command(..))
import Intake.Process (backend)

serve :: Chan Command -> IO ()
serve chan = httpServe conf $ handler chan
  where
  conf = Conf.setPort 7001 $
         Conf.setBind "127.0.0.1" $
         Conf.defaultConfig

handler :: MonadSnap m => Chan Command -> m ()
handler chan = do
  route
    [ ("workflows", method GET (format "text/plain" getWorkflows))
    , ("workflows", method GET (format "application/json" getWorkflowsJSON))
    , ("workflows/:name", method POST (format "text/plain" $ instanciate chan))
    , ("workflows/:name", method POST (format "application/json" $ instanciateJSON chan))
    , ("instances/:id/status", method GET (format "application/json" statusJSON))
    ]

-- TODO must be sorted.
getWorkflows :: MonadSnap m => m ()
getWorkflows = mapM_ (writeBS . S8.pack . (++ "\n")) ["a", "ab"]

getWorkflowsJSON :: MonadSnap m => m ()
getWorkflowsJSON = writeLBS . encode $ map WorkflowName ["a", "ab"]

instanciate :: MonadSnap m => Chan Command -> m ()
instanciate chan = do
  mname <- getParam "name"
  case mname of
    Just name -> do
      WorkflowId i <- liftIO $ instanciate' chan name
      writeBS . S8.pack $ i ++ "\n"
    _ -> pass

instanciateJSON :: MonadSnap m => Chan Command -> m ()
instanciateJSON chan = do
  mname <- getParam "name"
  case mname of
    Just name -> do
      i <- liftIO $ instanciate' chan name
      writeLBS $ encode i
    _ -> pass

instanciate' :: Chan Command -> S8.ByteString -> IO WorkflowId
instanciate' chan name = do
  mvar <- newEmptyMVar
  writeChan chan $
    Instanciate (Right . WorkflowName $ S8.unpack name) [] mvar
  readMVar mvar

statusJSON :: MonadSnap m => m ()
statusJSON = do
  mid <- getParam "id"
  case mid of
    Just i -> do
      e <- liftIO $ inspect backend (WorkflowIdPrefix $ S8.unpack i)
      writeLBS . encode $ status e
    _ -> pass

----------------------------------------------------------------------
-- From tibbe's ekg package.
----------------------------------------------------------------------

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = map fst
                . sortBy (rcompare `on` snd)
                . map grabQ
                . S.split 44 -- comma
  where
    rcompare :: Double -> Double -> Ordering
    rcompare = flip compare
    grabQ s =
        let (s', q) = breakDiscard 59 s -- semicolon
            (_, q') = breakDiscard 61 q -- equals sign
         in (trimWhite s', readQ $ trimWhite q')
    readQ s = case reads $ S8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
    trimWhite = S.dropWhile (== 32) -- space

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | The Accept header of the request.
acceptHeader :: Request -> Maybe S.ByteString
acceptHeader req = S.intercalate "," <$> getHeaders "Accept" req

-- | Runs a Snap monad action only if the request's Accept header
-- matches the given MIME type.
format :: MonadSnap m => S.ByteString -> m a -> m a
format fmt action = do
    req <- getRequest
    let acceptHdr = (head . parseHttpAccept) <$> acceptHeader req
    case acceptHdr of
        Just hdr | hdr == fmt -> action
        _ -> pass
