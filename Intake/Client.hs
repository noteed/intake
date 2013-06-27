{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Intake.Client where

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.JSONClient (apiGet, apiPost)

import Intake.Core (WorkflowId(..), WorkflowName(..))

-- | Execute a GET agains the specified URI (e.g. `/workflow`) using the
-- supplied parameters.
get :: FromJSON a => B.ByteString
  -> [(B.ByteString, Maybe B.ByteString)] -> IO (Maybe a)
get = apiGet Nothing "http://127.0.0.1:7001"

post :: B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> LB.ByteString
  -> IO (Maybe WorkflowId)
post = apiPost Nothing "http://127.0.0.1:7001"

-- | Return the list of defined workflows.
workflows :: IO [WorkflowName]
workflows = do
  m <- get "/workflows" []
  return $ maybe (error "GET /worklfows") id m

-- | Instanciate a workflow.
instanciate :: (Either String WorkflowName) -> IO WorkflowId
instanciate n =
  case n of
    Right (WorkflowName name) -> do
      m <- post ("/workflows/" `B.append` B.pack name) [] ""
      return $ maybe (error "POST /workflows") id m
    Left command -> do -- TODO it is not yet implemented on the server.
      m <- post ("/commands/" `B.append` B.pack command) [] ""
      return $ maybe (error "POST /commands") id m
