--------------------------------------------------------------------------------
module Scheduler where
--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Exception (catch)
import           System.Process (readProcessWithExitCode)
--------------------------------------------------------------------------------
import qualified DB
import           Types
--------------------------------------------------------------------------------
schedulerRoutine :: IO ()
schedulerRoutine = do
  syncJobs
  updateReadyTasks
  tasks <- getReadyTasks
  execute tasks
--------------------------------------------------------------------------------
run :: IO ()
run = do
  result <- catch schedulerRoutine $ \e -> do
    putStrLn $ "Got an exception: " ++ show e
    putStrLn "Returning dummy value of -1"
    return (-1)
  print result
  let microsecondsInSecond = 1000000
  threadDelay (1 * microsecondsInSecond)
  run
--------------------------------------------------------------------------------
syncJobs :: IO ()
syncJobs = do
  case stat "jobs" of
    False -> mkdir "jobs"
    _     -> ()
  jobs <- DB.get "/jobs" :: IO [Job]
  map syncJob jobs
  return ()
--------------------------------------------------------------------------------
syncJob :: IO ()
syncJob job = do
  let filepath = scriptPath (show (job ^. _id))
  let content  = job ^. _code
  writeFile filepath content
  chmod "a+x" filepath
  return ()
--------------------------------------------------------------------------------
scriptPath :: Text -> Text
scriptPath number = "jobs" <> "/" <> "job" <> "." <> number
--------------------------------------------------------------------------------
updateReadyTasks :: IO ()
updateReadyTasks = do
  DB.post "/rpc/update_ready_tasks" (object [])
  return ()
--------------------------------------------------------------------------------
getReadyTasks :: IO [Task]
getReadyTasks = do
  tasks <- DB.get "/tasks?state=eq.ready" :: IO [Task]
  return tasks
--------------------------------------------------------------------------------
execute :: Task -> IO ()
execute task = do
  let startForm = object ["task_id" .= (task ^. _id)]
  maybeExecution <- DB.post "/rpc/checkout_task" startForm :: IO (Maybe Execution)
  case maybeExecution of
    Nothing -> do
      -- log error
      return ()
    Just execution -> do
      let scriptPath = scriptPath (show (task ^. _job_id))
      let args       = []
      let stdin      = ""
      (exitCode, stdout, stderr) <- readProcessWithExitCode scriptPath args stdin
      let executionStatus =
            case exitCode of
              0 -> Succeeded
              _ -> Failed
      let finishForm = object
                       [ "execution_id" .= (execution ^. _id)
                       , "state"        .= executionStatus
                       , "stdout"       .= (stdout)
                       , "stderr"       .= (stderr)
                       ]
      DB.post "/rpc/finish_task_execution" finishForm
      return ()
--------------------------------------------------------------------------------
