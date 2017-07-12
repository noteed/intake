{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | This module makes it possible to have user-defined workflows:
-- activity types are provided by Intake but their wiring and triggers can be
-- configured by users.
-- Workflows are defined using JSON.
-- Currently this module is used to attempt to match JSON data to
-- Reesd/Lovelace workflows.
module Intake.Workflow where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as H
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V

import Lovelace hiding (run)


------------------------------------------------------------------------------
data Tagged = Tagged
  { fromTagger :: T.Text
  }

instance FromJSON Tagged where
  parseJSON (Object v) = Tagged <$> v .: "tag"
  parseJSON _ = mzero


------------------------------------------------------------------------------
data RToken = RToken Value -- Include a "tag".
  deriving Show

instance Token RToken String where
  tag (RToken v) =
    case fromJSON v of
      Error _ -> "error"
      Success t -> T.unpack (fromTagger t)

data RTask =
    Barrier
  | BuildImageTask
  | CloneTask
  | RunImage
  | SendMailTask
  | WaitGetRequest
  deriving Show


------------------------------------------------------------------------------
toWorkflow :: Def -> Either String (Workflow Value RTask RToken String)
toWorkflow Def{..} = do
  activities_ <- mapM mkActivity defNodes
  let activities = activities_ ++ [done]
  transitions <- mapM (mkTransition activities) defTransitions
  start <- mkStart activities
  let finals = filter (isFinal transitions) activities

  return (Workflow defName start transitions finals)

  where

  mkActivity n = do
    act <- case aType n of
      -- Tasks
      "barrier" -> return (Task Barrier)
      "build" -> return (Task BuildImageTask)
      "clone" -> return (Task CloneTask)
      "run" -> return (Task RunImage)
      "wait-get" -> return (Task WaitGetRequest)
      "send-mail" -> return (Task SendMailTask)

      -- Emit its argument to the next activity.
      "input" -> case aArgs n of
        Nothing -> fail "Activity 'input' with no args"
        Just k -> return (Pure (\state _ -> (state, RToken k)))

      -- Emit its input shadowed by its argument to the next activity.
      "override" -> case aArgs n of
        Nothing -> fail "Activity 'override' with no args"
        Just (Object k) -> return (Pure (\state input -> case input of
          RToken (Object k_) -> (state, RToken (Object (k `H.union` k_)))
          _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          ))
        Just _ -> fail "Activity 'override' args is not a dictionary"

      -- Emit the current state to the next activity, with possible overrides.
      "state" -> case aArgs n of
        Nothing -> return (Pure (\state _ -> case state of
          Object _ -> (state, RToken state)
          _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          ))
        Just (Object k) -> return (Pure (\state _ -> case state of
          Object s -> (state, RToken (Object (k `H.union` s)))
          _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          ))
        Just _ -> fail "Activity 'state' args is not a dictionary"

      -- Emit its input shadowed by some fields from the state.
      -- Each field is mapped to a new name (so the args is an object,
      -- not an array).
      "reset" -> case aArgs n of
        Nothing -> fail "Activity 'reset' with no args"
        Just (Array k) -> return (Pure (\state input -> case input of
          RToken (Object k_) -> case state of
            Object s ->
              let k' = (H.fromList . mapMaybe (\[String a, String b] ->
                    if a == "tag"
                    then Just (a, String b)
                    else (b,) <$> (H.lookup a s)) . map (\(Array a) -> V.toList a) . V.toList) k
              in (state, RToken (Object (k' `H.union` k_)))
            _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          ))
        Just _ -> fail "Activity 'reset' args is not a dictionary"

      -- Emit its input with its context key shadowed by some fields from the
      -- state.
      -- Each field is mapped to a new name (so the args is an object,
      -- not an array).
      "reset-context" -> case aArgs n of
        Nothing -> fail "Activity 'reset' with no args"
        Just (Array k) -> return (Pure (\state input -> case input of
          RToken (Object k_) -> case state of
            Object s ->
              let k' = (H.fromList . mapMaybe (\[String a, String b] ->
                    if a == "tag"
                    then Just (a, String b)
                    else (b,) <$> (H.lookup a s)) . map (\(Array a) -> V.toList a) . V.toList) k
                  mcontext_ = H.lookup "context" k_
                  mcontext = case mcontext_ of
                    Just (Array a) -> Just a
                    Nothing -> Nothing
                    _ -> Nothing -- TODO or fail ?
                  context_ = maybe k' ((k' `H.union`) . H.fromList . map (\[String a, String b] -> (a, String b)) . map  (\(Array a) -> V.toList a) . V.toList) mcontext
                  context = (Array . V.fromList) (map (\(a, b) -> Array (V.fromList [String a, b])) (H.toList context_))
              in (state, RToken (Object (H.insert "context" context k_)))
            _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          ))
        Just _ -> fail "Activity 'reset' args is not a dictionary"

      -- Implements a "if files `elem` state.touched-files" activity.
      "in-touched-files" -> case aArgs n of
        Nothing -> fail "Activity 'in-touched-files' with no args"
        Just (Array k) -> return (Pure (\state _ -> case state of
          Object s ->
            case H.lookup "touched-files" s of
              -- same as touched-files being an empty list
              Nothing -> (state, RToken (object ["tag" .= ("failure" :: String)]))
              Just (Array touchedFiles) ->
                if any (`elem` (V.toList touchedFiles)) (V.toList k)
                then (state, RToken (object ["tag" .= ("success" :: String)]))
                else (state, RToken (object ["tag" .= ("failure" :: String)]))
              _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          _ -> (state, RToken (object ["tag" .= ("error" :: String)]))
          ))
        Just _ -> fail "Activity 'in-touched-files' args is not a list"

      _ -> fail "Activity type unknown"
    return (Activity (aName n) "" act)

  nameActivity a = (activityName a, a)

  mkStart as = lkp (startName defStart) as

  lkp name as = case lookup name (map nameActivity as) of
    Just x -> return x
    Nothing -> fail ("Activity " ++ name ++ " unknown")

  mkTransition as t = do
    a <- lkp (trFrom t) as
    b <- lkp (trTo t) as
    return ((a, trTag t), b)

  isFinal [] _ = True
  isFinal (((b, _), _) : ts) a
    | activityName a == activityName b = False
    | otherwise = isFinal ts a

  -- TODO The done activity should be explicit in workflow definitions to
  -- account for concurrent activities and define how to join their result.
  done = Activity "done" "" (Pure $ \o k -> (o, k))


------------------------------------------------------------------------------
data Start = Start
  { startName :: String
  }
  deriving Show

data Transition = Transition
  { trFrom :: String
  , trTo :: String
  , trTag :: String
  , trArgs :: Maybe Value
  }
  deriving Show

data Node = Node
  { aName :: String
  , aType :: String
  , aArgs :: Maybe Value
  }
  deriving Show

data Def = Def
  { defName :: String
  , defStart :: Start
  , defTransitions :: [Transition]
  , defNodes :: [Node]
  }
  deriving Show


instance FromJSON Start where
  parseJSON (Object v) = do
    (name :: String) <- v .: "name"
    return (Start name)
  parseJSON _ = mzero

instance ToJSON Start where
  toJSON Start{..} = object $
    [ "name" .= startName
    ]

instance FromJSON Transition where
  parseJSON (Object v) = do
    from <- v .: "after"
    to <- v .: "activity"
    tag <- v .: "on"
    args <- v .:? "args"
    return (Transition from to tag args)
  parseJSON _ = mzero

instance ToJSON Transition where
  toJSON Transition{..} = object $
    [ "after" .= trFrom
    , "activity" .= trTo
    , "on" .= trTag
    ] ++
    (maybe [] (\a -> ["args" .= a]) trArgs)

instance FromJSON Node where
  parseJSON (Object v) = do
    name <- v .: "name"
    typ <- v .: "type"
    args <- v .:? "args"
    return (Node name typ args)
  parseJSON _ = mzero

instance ToJSON Node where
  toJSON Node{..} = object $
    [ "name" .= aName
    , "type" .= aType
    ] ++
    (maybe [] (\a -> ["args" .= a]) aArgs)

instance FromJSON Def where
  parseJSON (Object v) = do
    name <- v .: "name"
    start <- v .: "activity"
    transitions <- v .:? "transitions"
    nodes <- v .:? "activities"
    return (Def name start (maybe [] id transitions) (maybe [] id nodes))
  parseJSON _ = mzero

instance ToJSON Def where
  toJSON Def{..} = object $
    [ "name" .= defName
    , "activity" .= defStart
    , "transitions" .= defTransitions
    , "activities" .= defNodes
    ]


------------------------------------------------------------------------------
exampleTrigger = LB.pack
  "{\
  \  \"type\": \"github-push\",\
  \  \"args\": {\
  \    \"repository\": \"hypered/reesd-hello\"\
  \  }\
  \}"

exampleStart = LB.pack
  "{\
  \  \"name\": \"build\",\
  \  \"args\": {\
  \    \"repository\": \"hypered/reesd-hello\",\
  \    \"image\": \"images.reesd.com/reesd/reesd/hello\"\
  \  }\
  \}"

exampleStart' = LB.pack
  "{\
  \  \"name\": \"build\"\
  \}"

exampleTransition = LB.pack
  "{\
  \  \"after\": \"build\",\
  \  \"on\": \"success\",\
  \  \"activity\": \"run\",\
  \  \"args\": {\
  \    \"image\": \"images.reesd.com/reesd/reesd/hello\"\
  \  }\
  \}"

exampleNode = LB.pack
  "{\
  \  \"name\": \"build\",\
  \  \"type\": \"build\",\
  \  \"args\": {\
  \    \"cache\": \"disable\",\
  \    \"image\": \"images.reesd.com/reesd/hello\"\
  \  }\
  \}"
