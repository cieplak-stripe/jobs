{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
--------------------------------------------------------------------------------
module Scheduler where
--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Exception
import           Data.Aeson
import           Data.Text (unpack)
import           Prelude ()
import           Protolude
import qualified System.Directory as Dir
import           System.Posix.Files (accessModes, setFileMode)
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
  scheduleTasks
  updateReadyTasks
  tasks <- getReadyTasks
  mapM_ execute tasks
  return ()

run :: IO ()
run = forever $ do
  result <- catchAny schedulerRoutine $ \e -> do
    putStrLn (show e :: Text)
    return ()
  let microsecondsInSecond = 1000000
  threadDelay (1 * microsecondsInSecond)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
--------------------------------------------------------------------------------
syncJobs :: IO ()
syncJobs = do
  try (Dir.createDirectory "scripts") :: IO (Either (IOException) ())
  jobs <- DB.get "/jobs" :: IO [Job]
  mapM_ syncJob jobs
  return ()

syncJob :: Job -> IO ()
syncJob job = do
  let filepath = scriptPath (show (job ^. _id))
  let content  = job ^. _code
  writeFile filepath content
  setFileMode filepath accessModes -- "chmod a+rwx"
  return ()

scriptPath :: Text -> FilePath
scriptPath number = unpack ("scripts" <> "/" <> "job" <> "." <> number)
--------------------------------------------------------------------------------
scheduleTasks :: IO ()
scheduleTasks = do
  DB.post "/rpc/schedule_tasks" (object []) :: IO Value
  return ()
--------------------------------------------------------------------------------
updateReadyTasks :: IO ()
updateReadyTasks = do
  DB.post "/rpc/update_ready_tasks" (object []) :: IO Value
  return ()
--------------------------------------------------------------------------------
getReadyTasks :: IO [Task]
getReadyTasks = do
  tasks <- DB.get "/tasks?state=eq.READY" :: IO [Task]
  return tasks
--------------------------------------------------------------------------------
execute :: Task -> IO ()
execute task = do
  let startForm = object ["task" .= (task ^. _id)]
  executions <- DB.post "/rpc/checkout_task" startForm :: IO [Execution]
  let maybeExecution = head executions
  case maybeExecution of
    Nothing -> do
      putStrLn (show executions :: Text)
      return ()
    Just execution -> do
      let path  = scriptPath (show (task ^. _job_id))
      let args  = []
      let stdin = ""
      putStrLn ("running " <> "path")
      (exitCode, stdout, stderr) <- readProcessWithExitCode path args stdin
      let executionStatus =
            case exitCode of
              _ -> SUCCEEDED
              -- _ -> FAILED
      let finishForm = object
                       [ "execution_id" .= (execution ^. _id)
                       , "state"        .= executionStatus
                       , "stdout"       .= (stdout)
                       , "stderr"       .= (stderr)
                       ]
      putStrLn (encode finishForm)
      DB.post "/rpc/finish_task_execution" finishForm :: IO Value
      return ()
--------------------------------------------------------------------------------
