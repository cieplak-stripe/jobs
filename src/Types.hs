{-#
LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  ConstraintKinds,
  ExtendedDefaultRules,
  FlexibleContexts,
  FlexibleInstances,
  TypeFamilies,
  TypeOperators,
  OverloadedStrings,
  OverloadedLabels
#-}
--------------------------------------------------------------------------------
module Types where
--------------------------------------------------------------------------------
import Control.Lens
import Data.Aeson
import GHC.Generics
import Prelude ()
import Protolude
import SuperRecord hiding ((&), get)
--------------------------------------------------------------------------------
type Job =
  '[ "id"         := Int
   , "title"      := Text
   , "code"       := Text
   , "start_time" := Text
   , "frequency"  := Text
   ]
--------------------------------------------------------------------------------
type Task =
  '[ "id"            := Int
   , "job_id"        := Int
   , "staged_at"     := Text
   , "scheduled_for" := Text
   , "state"         := TaskState
   ]

data TaskState
  = STAGED
  | READY
  | RUNNING
  | COMPLETE
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
--------------------------------------------------------------------------------
type Execution =
  '[ "id"          := Int
   , "task_id"     := Int
   , "started_at"  := Text
   , "finished_at" := Maybe Text
   , "state"       := ExecutionState
   , "stdout"      := Maybe Text
   , "stderr"      := Maybe Text
   ]

data ExecutionState
  = STARTED
  | SUCCEEDED
  | FAILED
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
--------------------------------------------------------------------------------
type Alert =
  '[ "id"           := Int
   , "execution_id" := Int
   , "message"      := Value
   ]
--------------------------------------------------------------------------------
