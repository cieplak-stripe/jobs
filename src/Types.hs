{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Control.Lens
import Data.Aeson
import GHC.Generics
import Prelude ()
import Protolude
--------------------------------------------------------------------------------
-- |
data Job
  = Job
  { __id         :: Int
  , __title      :: Text
  , __code       :: Text
  , __start_time :: Text
  , __frequency  :: Text
  } deriving (Show, Eq, Generic)
--------------------------------------------------------------------------------
-- |
data Task
  = Task
  { __id            :: Int
  , __job_id        :: Int
  , __staged_at     :: Text
  , __scheduled_for :: Text
  , __state         :: TaskState
  } deriving (Show, Eq, Generic)

data TaskState
  = Staged
  | Ready
  | Running
  | Complete
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
--------------------------------------------------------------------------------
-- |
data Execution
  = Execution
  { __id          :: Int
  , __task_id     :: Int
  , __started_at  :: Text
  , __finished_at :: Maybe Text
  , __state       :: ExecutionState
  , __stdout      :: Maybe Text
  , __stderr      :: Maybe Text
  } deriving (Show, Eq, Generic)

data ExecutionState
  = Started
  | Succeeded
  | Failed
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
--------------------------------------------------------------------------------
-- |
data Alert
  = Alert
  { __id           :: Int
  , __execution_id :: Int
  , __message      :: Value
  } deriving (Show, Eq, Generic)
--------------------------------------------------------------------------------
--                                                                        Lenses
makeFieldsNoPrefix ''Job
makeFieldsNoPrefix ''Task
makeFieldsNoPrefix ''Execution
makeFieldsNoPrefix ''Alert
--------------------------------------------------------------------------------
--                                                                JSON Instances

instance ToJSON   Job       where toJSON    = genericToJSON    defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON Job       where parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON   Task      where toJSON    = genericToJSON    defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON Task      where parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON   Execution where toJSON    = genericToJSON    defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON Execution where parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON   Alert     where toJSON    = genericToJSON    defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON Alert     where parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}
--------------------------------------------------------------------------------
