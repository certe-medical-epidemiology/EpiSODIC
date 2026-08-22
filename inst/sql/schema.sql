-- ===================================================================== --
--  An R package by Certe:                                               --
--  https://github.com/certe-medical-epidemiology                        --
--                                                                       --
--  Licensed as GPL-v2.0.                                                --
--                                                                       --
--  Developed at non-profit organisation Certe Medical Diagnostics &     --
--  Advice, department of Medical Epidemiology.                          --
--                                                                       --
--  This R package is free software; you can freely use and distribute   --
--  it for both personal and commercial purposes under the terms of the  --
--  GNU General Public License version 2.0 (GNU GPL-2), as published by  --
--  the Free Software Foundation.                                        --
--                                                                       --
--  We created this package for both routine data analysis and academic  --
--  research and it was publicly released in the hope that it will be    --
--  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              --
-- ===================================================================== --

-- EpiSODIC database schema, written in SQLite dialect and used verbatim
-- for SQLite connections. For a MariaDB/MySQL EPISODIC_DB,
-- episodic_db_schema_statements() rewrites this same file at load time
-- (AUTOINCREMENT -> AUTO_INCREMENT, the PRAGMA line dropped, and the four
-- TEXT columns carrying a UNIQUE constraint given a bounded VARCHAR) -
-- there is deliberately only one schema file to keep in sync.
--
-- Type mapping used throughout, for reference against a more general
-- relational type system:
--   BIGINT AUTO_INCREMENT PRIMARY KEY -> INTEGER PRIMARY KEY AUTOINCREMENT
--   ENUM(...)                         -> TEXT with CHECK (col IN (...))
--   JSON                              -> TEXT holding JSON
--   DATETIME, DATE                    -> TEXT, ISO 8601
--   TINYINT(1)                        -> INTEGER 0 or 1
--   DECIMAL(p,s)                      -> REAL
--
-- Write ownership (cron vs. app) is documented per table; the app itself
-- only ever inserts, never updates or deletes.

PRAGMA foreign_keys = ON;

-- ---------------------------------------------------------------------
-- 5.1 Streams (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_stream (
  stream_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_key      TEXT NOT NULL UNIQUE CHECK (length(stream_key) = 40),
  level           TEXT NOT NULL CHECK (level IN (
                    'pathogen_ward', 'pathogen_institution', 'pathogen_area',
                    'pathogen_province', 'pathogen_region')),
  pathogen        TEXT NOT NULL,  -- raw lab-provided string, deliberately unconstrained free text
  care_line       TEXT CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  region_code     TEXT,
  institution_id  INTEGER REFERENCES episodic_institution(institution_id),
  ward            TEXT,          -- optional finer level than institution, may be NULL
  denominator     TEXT NOT NULL DEFAULT 'none' CHECK (denominator IN (
                    'none', 'tests', 'population', 'patient_days')),
  severity_weight REAL NOT NULL DEFAULT 1.00,
  is_active       INTEGER NOT NULL DEFAULT 1 CHECK (is_active IN (0, 1)),
  first_seen      TEXT NOT NULL,
  last_seen       TEXT NOT NULL,
  created_at      TEXT NOT NULL
);

CREATE INDEX idx_episodic_stream_pathogen ON episodic_stream(pathogen);
CREATE INDEX idx_episodic_stream_active ON episodic_stream(is_active);

-- ---------------------------------------------------------------------
-- 5.2 Mutes (app)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_stream_mute (
  mute_id     INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_id   INTEGER NOT NULL REFERENCES episodic_stream(stream_id),
  muted_from  TEXT NOT NULL,
  muted_until TEXT NOT NULL,
  reason      TEXT NOT NULL CHECK (reason IN (
                'seasonal', 'screening_campaign', 'method_change',
                'known_source', 'other')),
  note        TEXT,
  user_id     INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at  TEXT NOT NULL,
  revoked_at  TEXT
);

CREATE INDEX idx_episodic_stream_mute_stream ON episodic_stream_mute(stream_id);

-- ---------------------------------------------------------------------
-- 5.3 Runs (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_detection_run (
  run_id            INTEGER PRIMARY KEY AUTOINCREMENT,
  host              TEXT NOT NULL,
  account           TEXT NOT NULL,
  started_at        TEXT NOT NULL,
  finished_at       TEXT,
  status            TEXT NOT NULL CHECK (status IN (
                      'running', 'success', 'failed', 'partial')),
  attempt_no        INTEGER NOT NULL DEFAULT 1,
  n_streams         INTEGER,
  n_detections      INTEGER,
  n_signals_new     INTEGER,
  n_signals_updated INTEGER,
  -- What each feed actually delivered. A run that skipped rows finishes
  -- 'partial', never 'success': silently dropping an operator's data is
  -- the one failure mode a green run must not be able to hide.
  n_cases_supplied       INTEGER,
  n_cases_deduplicated   INTEGER,
  n_cases_inserted       INTEGER,
  n_denominators_written INTEGER,
  n_activity_supplied    INTEGER,
  n_activity_written     INTEGER,
  n_activity_skipped     INTEGER,
  code_version      TEXT,
  pkg_versions      TEXT,
  config_hash       TEXT CHECK (config_hash IS NULL OR length(config_hash) = 40),
  config_snapshot   TEXT,
  error_text        TEXT
);

-- ---------------------------------------------------------------------
-- 5.4.1 Institutions (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_institution (
  institution_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  institution_key  TEXT NOT NULL UNIQUE CHECK (length(institution_key) = 40),
  display_name     TEXT NOT NULL,
  institution_type TEXT NOT NULL CHECK (institution_type IN (
                      'hospital', 'ltc_institution', 'gp_municipality',
                      'ooh_service', 'other')),
  care_line        TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  municipality     TEXT,
  pc               TEXT,
  n_beds           INTEGER,
  is_monitored     INTEGER NOT NULL DEFAULT 0 CHECK (is_monitored IN (0, 1)),
  is_active        INTEGER NOT NULL DEFAULT 1 CHECK (is_active IN (0, 1))
);

-- ---------------------------------------------------------------------
-- 5.4.2 Hospital activity (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_institution_activity (
  institution_id INTEGER NOT NULL REFERENCES episodic_institution(institution_id),
  period_start   TEXT NOT NULL,
  period_end     TEXT NOT NULL,
  patient_days   INTEGER,
  admissions     INTEGER,
  n_beds         INTEGER,
  source         TEXT,
  PRIMARY KEY (institution_id, period_start)
);

-- ---------------------------------------------------------------------
-- 5.4.3 Pathogen configuration (shipped defaults, overridable per instance;
-- not written by either process at runtime, loaded from CSV)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_pathogen_config (
  pathogen        TEXT NOT NULL PRIMARY KEY,  -- matches episodic_case.pathogen exactly
  episode_days    INTEGER NOT NULL DEFAULT 30,
  incub_min_days  REAL,
  incub_max_days  REAL,
  case_free_days  INTEGER NOT NULL DEFAULT 14,
  cooldown_days   INTEGER,
  rt_applicable   INTEGER NOT NULL DEFAULT 0 CHECK (rt_applicable IN (0, 1)),
  si_mean_days    REAL,
  si_sd_days      REAL,
  si_dist         TEXT CHECK (si_dist IS NULL OR si_dist IN ('gamma', 'lognormal', 'weibull')),
  mem_applicable  INTEGER NOT NULL DEFAULT 0 CHECK (mem_applicable IN (0, 1)),
  severity_weight REAL NOT NULL DEFAULT 1.00,
  source_ref      TEXT
);

-- ---------------------------------------------------------------------
-- 5.4 Cases (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_case (
  case_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  source_key     TEXT NOT NULL UNIQUE,
  patient_key    TEXT NOT NULL,
  sample_date    TEXT NOT NULL,
  receipt_date   TEXT,
  pathogen       TEXT NOT NULL,  -- raw lab-provided string, used verbatim
  care_line      TEXT NOT NULL DEFAULT 'unknown' CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  institution_id INTEGER REFERENCES episodic_institution(institution_id),
  ward           TEXT,
  specialism     TEXT,
  pc             TEXT,
  sex            TEXT CHECK (sex IS NULL OR sex IN ('M', 'F', 'U')),
  age            INTEGER,
  first_seen_run INTEGER NOT NULL REFERENCES episodic_detection_run(run_id)
);

CREATE INDEX idx_episodic_case_sample_date ON episodic_case(sample_date);
CREATE INDEX idx_episodic_case_pathogen ON episodic_case(pathogen);
CREATE INDEX idx_episodic_case_patient ON episodic_case(patient_key);
CREATE INDEX idx_episodic_case_institution ON episodic_case(institution_id);

-- ---------------------------------------------------------------------
-- 5.5 Detections and clusters (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_detection (
  detection_id INTEGER PRIMARY KEY AUTOINCREMENT,
  run_id       INTEGER NOT NULL REFERENCES episodic_detection_run(run_id),
  stream_id    INTEGER NOT NULL REFERENCES episodic_stream(stream_id),
  cluster_id   INTEGER REFERENCES episodic_cluster(cluster_id),
  detector     TEXT NOT NULL CHECK (detector IN (
                 'farrington', 'ears', 'mem', 'rare_trigger', 'same_place')),
  first_day    TEXT NOT NULL,
  last_day     TEXT NOT NULL,
  n_cases      INTEGER NOT NULL,
  expected     REAL,
  upperbound   REAL,
  params       TEXT NOT NULL,
  created_at   TEXT NOT NULL
);

CREATE INDEX idx_episodic_detection_run ON episodic_detection(run_id);
CREATE INDEX idx_episodic_detection_stream ON episodic_detection(stream_id);
CREATE INDEX idx_episodic_detection_cluster ON episodic_detection(cluster_id);

CREATE TABLE episodic_cluster (
  cluster_id               INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_id                INTEGER NOT NULL REFERENCES episodic_stream(stream_id),
  first_day                TEXT NOT NULL,
  last_day                 TEXT NOT NULL,
  n_cases                  INTEGER NOT NULL,
  expected                 REAL,
  excess                   REAL,
  ratio                    REAL,
  priority_score           REAL NOT NULL,
  detector_agreement       INTEGER NOT NULL,
  opened_at                TEXT NOT NULL,
  last_detected_run        INTEGER NOT NULL REFERENCES episodic_detection_run(run_id),
  runs_since_detected      INTEGER NOT NULL DEFAULT 0,
  changed_since_assessment INTEGER NOT NULL DEFAULT 0 CHECK (changed_since_assessment IN (0, 1)),
  suppressed_by            INTEGER REFERENCES episodic_cluster(cluster_id),
  merged_into              INTEGER REFERENCES episodic_cluster(cluster_id)
);

-- Note what this table does NOT carry: verdict, state, closed_at,
-- snooze_until. All four are authored by the app, which is insert-only,
-- and are therefore derived at read time from episodic_assessment_event.

CREATE INDEX idx_episodic_cluster_stream ON episodic_cluster(stream_id);

CREATE TABLE episodic_cluster_case (
  cluster_id INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  case_id    INTEGER NOT NULL REFERENCES episodic_case(case_id),
  PRIMARY KEY (cluster_id, case_id)
);

CREATE INDEX idx_episodic_cluster_case_case ON episodic_cluster_case(case_id);

-- ---------------------------------------------------------------------
-- 5.6 Assessments (app, append-only)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_assessment_event (
  event_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id     INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  user_id        INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at     TEXT NOT NULL,
  verdict        TEXT CHECK (verdict IS NULL OR verdict IN (
                   'artefact', 'expected_variation', 'cluster_not_yet',
                   'possible_epidemic', 'confirmed_epidemic')),
  rationale      TEXT NOT NULL,
  wpg_notifiable INTEGER CHECK (wpg_notifiable IS NULL OR wpg_notifiable IN (0, 1)),
  ggd_informed   INTEGER CHECK (ggd_informed IS NULL OR ggd_informed IN (0, 1)),
  ggd_note       TEXT,
  snooze_until   TEXT,
  supersedes     INTEGER REFERENCES episodic_assessment_event(event_id)
);

CREATE INDEX idx_episodic_assessment_event_cluster ON episodic_assessment_event(cluster_id);

-- ---------------------------------------------------------------------
-- 5.7 Reporting triangle (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_reporting_triangle (
  stream_id   INTEGER NOT NULL REFERENCES episodic_stream(stream_id),
  sample_date TEXT NOT NULL,
  run_date    TEXT NOT NULL,
  n_cases     INTEGER NOT NULL,
  PRIMARY KEY (stream_id, sample_date, run_date)
);

-- ---------------------------------------------------------------------
-- Weekly Farrington trend points (cron). The multi-year trend panel needs
-- a continuous expected/upperbound band across many weeks, and the app
-- only ever performs cheap reads - it cannot recompute farringtonFlexible()
-- at render time. One row per stream per evaluated week;
-- episodic_detection/episodic_cluster remain the only tables that drive
-- reconciliation, this is purely a chart data cache.
-- ---------------------------------------------------------------------
CREATE TABLE episodic_stream_trend (
  stream_id  INTEGER NOT NULL REFERENCES episodic_stream(stream_id),
  week_start TEXT NOT NULL,
  n_cases    INTEGER NOT NULL,
  expected   REAL,
  upperbound REAL,
  PRIMARY KEY (stream_id, week_start)
);

-- ---------------------------------------------------------------------
-- 5.7.1 Denominators / positivity metadata (cron)
--
-- Deliberately supplied by the operator as pre-aggregated counts, not as a
-- raw per-test linelist. Optional entirely - a
-- site that cannot produce it (e.g. culture-only, no closed target list to
-- report negatives against) simply never writes rows here, and positivity
-- panels stay blank for its streams. Keyed on `pathogen` directly rather
-- than on a lab `determination` code, so the determination-to-pathogen
-- mapping (which pathogens a given test method can detect) is the
-- operator's own transform-time knowledge, not something EpiSODIC encodes
-- (episodic_mo_determination from earlier drafts is dropped).
-- ---------------------------------------------------------------------
CREATE TABLE episodic_denominator (
  pathogen      TEXT NOT NULL,
  sample_date   TEXT NOT NULL,
  care_line     TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  area_code     TEXT,
  n_tests       INTEGER NOT NULL,
  PRIMARY KEY (pathogen, sample_date, care_line, area_code)
);

-- ---------------------------------------------------------------------
-- 6.4 State trajectory (cron and app, append-only)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_cluster_state (
  state_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  state      TEXT NOT NULL CHECK (state IN (
               'new', 'assessing', 'monitoring', 'closable', 'closed', 'reassess')),
  entered_at TEXT NOT NULL,
  left_at    TEXT,
  trigger    TEXT NOT NULL CHECK (trigger IN (
               'detection', 'assessment', 'case_free', 'new_case', 'closure', 'system')),
  event_id   INTEGER REFERENCES episodic_assessment_event(event_id),
  user_id    INTEGER REFERENCES episodic_app_user(user_id)
);

CREATE INDEX idx_episodic_cluster_state_cluster ON episodic_cluster_state(cluster_id);

-- ---------------------------------------------------------------------
-- 5.8 Reports and users
-- ---------------------------------------------------------------------
CREATE TABLE episodic_app_user (
  user_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  username      TEXT NOT NULL UNIQUE,
  full_name     TEXT NOT NULL,
  email         TEXT NOT NULL,
  password_hash TEXT NOT NULL,
  role          TEXT NOT NULL CHECK (role IN ('assessor', 'admin')),
  is_active     INTEGER NOT NULL DEFAULT 1 CHECK (is_active IN (0, 1)),
  must_change   INTEGER NOT NULL DEFAULT 1 CHECK (must_change IN (0, 1)),
  last_login_at TEXT,
  created_at    TEXT NOT NULL
);

-- Account bookkeeping that would otherwise need an UPDATE (a fresh
-- password_hash on change, a bumped last_login_at on every login) is kept
-- insert-only instead, the same event-sourced pattern episodic_cluster_state
-- already uses for cluster state: the "current" password hash is the most
-- recent password_change event's, falling back to episodic_app_user's own
-- initial password_hash if no such event exists yet; "current" last_login_at
-- is the most recent login event.
CREATE TABLE episodic_app_user_event (
  event_id      INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id       INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at    TEXT NOT NULL,
  event_type    TEXT NOT NULL CHECK (event_type IN ('login', 'password_change')),
  password_hash TEXT  -- set only for password_change events
);

CREATE INDEX idx_episodic_app_user_event_user ON episodic_app_user_event(user_id);

-- app, and cron for pre-renders
CREATE TABLE episodic_report_render (
  report_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id  INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  user_id     INTEGER REFERENCES episodic_app_user(user_id),
  rendered_at TEXT NOT NULL,
  file_path   TEXT NOT NULL,
  file_sha256 TEXT NOT NULL CHECK (length(file_sha256) = 64),
  params      TEXT NOT NULL,
  case_ids    TEXT NOT NULL,
  version_no  INTEGER NOT NULL
);

CREATE INDEX idx_episodic_report_render_cluster ON episodic_report_render(cluster_id);
