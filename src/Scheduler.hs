{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
--------------------------------------------------------------------------------
module Scheduler where
--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Exception
import           Prelude ()
import           Protolude
import qualified System.Directory as Dir
import           System.Process (readProcessWithExitCode)
--------------------------------------------------------------------------------
import qualified Database as DB
import           Types
--------------------------------------------------------------------------------
default (Text)
--------------------------------------------------------------------------------
schedulerRoutine :: IO ()
schedulerRoutine = do
  syncJobs
  return ()
  -- updateReadyTasks
  -- tasks <- getReadyTasks
  -- execute tasks
--------------------------------------------------------------------------------
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
--------------------------------------------------------------------------------
run :: IO ()
run = forever $ do
  result <- catchAny schedulerRoutine $ \e -> return ()
  let microsecondsInSecond = 1000000
  threadDelay (1 * microsecondsInSecond)
--------------------------------------------------------------------------------
syncJobs :: IO ()
syncJobs = do
  try (Dir.createDirectory "scripts") :: IO (Either (IOException) ())
  jobs <- DB.get "/jobs" :: IO [Job]
  mapM_ syncJob jobs
  return ()
--------------------------------------------------------------------------------
syncJob :: Job -> IO ()
syncJob job = do
  -- let filepath = scriptPath (show (job ^. _id))
  -- let content  = job ^. _code
  -- writeFile filepath content
  -- chmod "a+x" filepath
  return ()
--------------------------------------------------------------------------------
scriptPath :: Text -> Text
scriptPath number = "jobs" <> "/" <> "job" <> "." <> number
--------------------------------------------------------------------------------
updateReadyTasks :: IO ()
updateReadyTasks = do
  -- DB.post "/rpc/update_ready_tasks" (object [])
  return ()
--------------------------------------------------------------------------------
getReadyTasks :: IO [Task]
getReadyTasks = undefined
  -- tasks <- DB.get "/tasks?state=eq.ready" :: IO [Task]
  -- return tasks
--------------------------------------------------------------------------------
execute :: Task -> IO ()
execute task = undefined
  -- let startForm = object ["task_id" .= (task ^. _id)]
  -- maybeExecution <- DB.post "/rpc/checkout_task" startForm :: IO (Maybe Execution)
  -- case maybeExecution of
  --   Nothing -> do
  --     -- log error
  --     return ()
  --   Just execution -> do
  --     let scriptPath = scriptPath (show (task ^. _job_id))
  --     let args       = []
  --     let stdin      = ""
  --     (exitCode, stdout, stderr) <- readProcessWithExitCode scriptPath args stdin
  --     let executionStatus =
  --           case exitCode of
  --             0 -> Succeeded
  --             _ -> Failed
  --     let finishForm = object
  --                      [ "execution_id" .= (execution ^. _id)
  --                      , "state"        .= executionStatus
  --                      , "stdout"       .= (stdout)
  --                      , "stderr"       .= (stderr)
  --                      ]
  --     DB.post "/rpc/finish_task_execution" finishForm
  --     return ()
--------------------------------------------------------------------------------
