--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Data.Lens
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
  deriving (Show, Eq, FromJSON, ToJSON)
--------------------------------------------------------------------------------
-- |
data Execution
  = Execution
  { __id          :: Int
  , __task_id     :: Int
  , __started_at  :: Timestamp
  , __finished_at :: Maybe Text
  , __state       :: ExecutionState
  , __stdout      :: Maybe Text
  , __stderr      :: Maybe Text
  }

data ExecutionState
  = Started
  | Succeeded
  | Failed
  deriving (Show, Eq, FromJSON, ToJSON)
--------------------------------------------------------------------------------
-- |
data Alert
  = Alert
  { __id           :: Int
  , __execution_id :: Int
  , __message      :: Value
  } deriving (Show, Eq, Generic)
--------------------------------------------------------------------------------
