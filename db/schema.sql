-- -----------------------------------------------------------------------------
-- Postgres schema for jobs service
-- -----------------------------------------------------------------------------
CREATE DATABASE jobs;
-- -----------------------------------------------------------------------------
\connect jobs
-- -----------------------------------------------------------------------------
CREATE TYPE task_state
  AS ENUM
  ( 'staged'
  , 'ready'
  , 'running'
  , 'complete'
  );
-- -----------------------------------------------------------------------------
CREATE TYPE execution_state
  AS ENUM
  ( 'started'
  , 'succeeded'
  , 'failed'
  );
-- -----------------------------------------------------------------------------
CREATE TABLE jobs
  ( id         SERIAL    PRIMARY KEY
  , title      TEXT      NOT NULL
  , code       TEXT      NOT NULL
  , start_time TIMESTAMP NOT NULL
  , frequency  INTERVAL  NOT NULL
  );
-- -----------------------------------------------------------------------------
CREATE TABLE tasks
  ( id            SERIAL     PRIMARY KEY
  , job_id        INT        REFERENCES jobs (id) NOT NULL
  , staged_at     TIMESTAMP  NOT NULL DEFAULT now()
  , scheduled_for TIMESTAMP  NOT NULL
  , state         task_state NOT NULL DEFAULT 'staged'
  , unique (job_id, scheduled_for)
  );
-- -----------------------------------------------------------------------------
CREATE TABLE executions
  ( id          SERIAL          PRIMARY KEY
  , task_id     INT             REFERENCES tasks (id) NOT NULL
  , owner       TEXT            NOT NULL
  , started_at  TIMESTAMP       NOT NULL DEFAULT now()
  , finished_at TIMESTAMP
  , state       execution_state NOT NULL DEFAULT 'started'
  , stdout      TEXT
  , stderr      TEXT
  );
-- -----------------------------------------------------------------------------
CREATE TABLE alerts
  ( id           SERIAL PRIMARY KEY
  , execution_id INT    REFERENCES executions (id) NOT NULL
  , message      JSONB  NOT NULL
  );
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
SET    state = 'ready'
WHERE  state = 'staged' AND
       now() > scheduled_for

$$ LANGUAGE 'sql';
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION checkout_task (task_id_ int) RETURNS RECORD AS $$

BEGIN

  UPDATE tasks
  SET    state = 'running'
  WHERE  id = task_id_
    AND  state = 'ready';

  IF NOT FOUND THEN
    RAISE EXCEPTION 'Task is not ready';
  END IF;

  INSERT INTO executions (task_id)
  VALUES (task_id_, 'localhost')
  RETURNING *;

END;

$$ LANGUAGE plpgsql;
-- -----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION finish_task_execution (
  execution_id int,
  state_       execution_state,
  stdout_      text,
  stderr_      text
) RETURNS void AS $$

DECLARE task_id_ int;
BEGIN

  UPDATE executions
  SET    finished_at = now(),
         state       = state_,
         stdout      = stdout_,
         stderr      = stderr_
  WHERE id = execution_id
  RETURNING task_id INTO task_id_;

  IF state_ = 'succeeded' THEN
    UPDATE tasks SET state = 'completed' where id = task_id_;
  ELSE
    UPDATE tasks SET state = 'ready'     where id = task_id_;
  END IF;
END;

$$ LANGUAGE plpgsql;
-- -----------------------------------------------------------------------------
