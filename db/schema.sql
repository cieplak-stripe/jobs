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

