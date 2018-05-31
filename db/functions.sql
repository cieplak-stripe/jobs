-- -----------------------------------------------------------------------------
\connect jobs
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION schedule_tasks () RETURNS BIGINT AS $$

WITH
  scheduled_tasks AS (
    SELECT
      id AS job_id,
      generate_series(
        start_time,
        (now() + frequency) :: timestamp,
        frequency
      ) AS scheduled_for
     FROM jobs
   ),
  count_tasks_created AS (
    INSERT INTO tasks (job_id, scheduled_for)
    SELECT job_id, scheduled_for FROM scheduled_tasks
    ON CONFLICT DO NOTHING
    RETURNING 1
  )
SELECT count(*) from count_tasks_created

$$ LANGUAGE 'sql';
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION update_ready_tasks () RETURNS void AS $$

UPDATE tasks
SET    state = 'READY'
WHERE  state = 'STAGED' AND
       now() > scheduled_for

$$ LANGUAGE 'sql';
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION checkout_task (task INT)
RETURNS TABLE
( id          INT
, task_id     INT
, owner       TEXT
, started_at  TIMESTAMP
, finished_at TIMESTAMP
, state       execution_state
, stdout      TEXT
, stderr      TEXT
)
AS $$

BEGIN

  UPDATE tasks
  SET    state       = 'RUNNING'
  WHERE  tasks.id    = task
    AND  tasks.state = 'READY';

  IF NOT FOUND THEN
    RAISE EXCEPTION 'Task is not ready';
  END IF;

  RETURN QUERY INSERT INTO executions (task_id, owner)
  VALUES (task, 'localhost')
  RETURNING *;


END;

$$ LANGUAGE plpgsql;
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION finish_task_execution (
  execution_id INT,
  state        execution_state,
  stdout       TEXT,
  stderr       TEXT
) RETURNS VOID AS $$

DECLARE task_id_ int;
        state_  execution_state = state;
        stdout_ TEXT            = stdout;
        stderr_ TEXT            = stderr;
BEGIN

  UPDATE executions
  SET    finished_at = now(),
         state       = state_,
         stdout      = stdout_,
         stderr      = stderr_
  WHERE id = execution_id
  RETURNING task_id INTO task_id_;

  IF state = 'SUCCEEDED' THEN
    UPDATE tasks SET state = 'COMPLETE' where id = task_id_;
  ELSE
    UPDATE tasks SET state = 'READY'     where id = task_id_;
  END IF;
END;

$$ LANGUAGE plpgsql;
-- -----------------------------------------------------------------------------

