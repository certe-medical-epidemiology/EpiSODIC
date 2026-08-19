-- EpiSODE database schema, SQLite dialect.
--
-- Canonical source: ARCHITECTURE.md section 5. Type mapping used throughout
-- (see ARCHITECTURE.md section 5 table):
--   BIGINT AUTO_INCREMENT PRIMARY KEY -> INTEGER PRIMARY KEY AUTOINCREMENT
--   ENUM(...)                         -> TEXT with CHECK (col IN (...))
--   JSON                              -> TEXT holding JSON
--   DATETIME, DATE                    -> TEXT, ISO 8601
--   TINYINT(1)                        -> INTEGER 0 or 1
--   DECIMAL(p,s)                      -> REAL
--
-- One dialect only: there is no MySQL in this system. Write ownership
-- (cron vs. app) is documented per table; the app is insert-only, see
-- ARCHITECTURE.md section 5.0.

PRAGMA foreign_keys = ON;

-- ---------------------------------------------------------------------
-- 5.1 Streams (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_stream (
  stream_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_key      TEXT NOT NULL UNIQUE CHECK (length(stream_key) = 40),
  level           TEXT NOT NULL CHECK (level IN (
                    'pathogen_ward', 'pathogen_institution', 'pathogen_area',
                    'pathogen_province', 'pathogen_region')),
  mo_code         TEXT NOT NULL,
  mo_name         TEXT NOT NULL,
  mo_rank         TEXT NOT NULL,
  care_line       TEXT CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  region_code     TEXT,
  institution_id  INTEGER REFERENCES episode_institution(institution_id),
  ward            TEXT,          -- see QUESTIONS.md item 20: not in ARCHITECTURE.md section 5.1
  denominator     TEXT NOT NULL DEFAULT 'none' CHECK (denominator IN (
                    'none', 'tests', 'population', 'patient_days')),
  severity_weight REAL NOT NULL DEFAULT 1.00,
  is_active       INTEGER NOT NULL DEFAULT 1 CHECK (is_active IN (0, 1)),
  first_seen      TEXT NOT NULL,
  last_seen       TEXT NOT NULL,
  created_at      TEXT NOT NULL
);

CREATE INDEX idx_episode_stream_mo ON episode_stream(mo_code);
CREATE INDEX idx_episode_stream_active ON episode_stream(is_active);

-- ---------------------------------------------------------------------
-- 5.2 Mutes (app)
-- ---------------------------------------------------------------------
CREATE TABLE episode_stream_mute (
  mute_id     INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_id   INTEGER NOT NULL REFERENCES episode_stream(stream_id),
  muted_from  TEXT NOT NULL,
  muted_until TEXT NOT NULL,
  reason      TEXT NOT NULL CHECK (reason IN (
                'seasonal', 'screening_campaign', 'method_change',
                'known_source', 'other')),
  note        TEXT,
  user_id     INTEGER NOT NULL REFERENCES episode_app_user(user_id),
  created_at  TEXT NOT NULL,
  revoked_at  TEXT
);

CREATE INDEX idx_episode_stream_mute_stream ON episode_stream_mute(stream_id);

-- ---------------------------------------------------------------------
-- 5.3 Runs (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_detection_run (
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
  code_version      TEXT,
  pkg_versions      TEXT,
  config_hash       TEXT CHECK (config_hash IS NULL OR length(config_hash) = 40),
  config_snapshot   TEXT,
  error_text        TEXT
);

-- ---------------------------------------------------------------------
-- 5.4.1 Institutions (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_institution (
  institution_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  institution_key  TEXT NOT NULL UNIQUE CHECK (length(institution_key) = 40),
  display_name     TEXT NOT NULL,
  institution_type TEXT NOT NULL CHECK (institution_type IN (
                      'hospital', 'ltc_institution', 'gp_municipality',
                      'ooh_service', 'other')),
  care_line        TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  municipality     TEXT,
  pc4              TEXT,
  n_beds           INTEGER,
  is_monitored     INTEGER NOT NULL DEFAULT 0 CHECK (is_monitored IN (0, 1)),
  is_active        INTEGER NOT NULL DEFAULT 1 CHECK (is_active IN (0, 1))
);

-- ---------------------------------------------------------------------
-- 5.4.2 Hospital activity (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_institution_activity (
  institution_id INTEGER NOT NULL REFERENCES episode_institution(institution_id),
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
CREATE TABLE episode_pathogen_config (
  mo_code         TEXT NOT NULL PRIMARY KEY,
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
CREATE TABLE episode_case (
  case_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  source_key     TEXT NOT NULL UNIQUE,
  patient_key    TEXT NOT NULL,
  sample_date    TEXT NOT NULL,
  receipt_date   TEXT,
  mo_code        TEXT NOT NULL,
  determination  TEXT,
  material       TEXT,
  care_line      TEXT NOT NULL DEFAULT 'unknown' CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  institution_id INTEGER REFERENCES episode_institution(institution_id),
  ward           TEXT,
  specialism     TEXT,
  pc4            TEXT,
  sex            TEXT CHECK (sex IS NULL OR sex IN ('M', 'F', 'U')),
  age            INTEGER,
  first_seen_run INTEGER NOT NULL REFERENCES episode_detection_run(run_id)
);

CREATE INDEX idx_episode_case_sample_date ON episode_case(sample_date);
CREATE INDEX idx_episode_case_mo ON episode_case(mo_code);
CREATE INDEX idx_episode_case_patient ON episode_case(patient_key);
CREATE INDEX idx_episode_case_institution ON episode_case(institution_id);

-- ---------------------------------------------------------------------
-- 5.5 Detections and clusters (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_detection (
  detection_id INTEGER PRIMARY KEY AUTOINCREMENT,
  run_id       INTEGER NOT NULL REFERENCES episode_detection_run(run_id),
  stream_id    INTEGER NOT NULL REFERENCES episode_stream(stream_id),
  cluster_id   INTEGER REFERENCES episode_cluster(cluster_id),
  detector     TEXT NOT NULL CHECK (detector IN (
                 'clusters', 'farrington', 'ears', 'mem', 'rare_trigger', 'same_place')),
  first_day    TEXT NOT NULL,
  last_day     TEXT NOT NULL,
  n_cases      INTEGER NOT NULL,
  expected     REAL,
  upperbound   REAL,
  params       TEXT NOT NULL,
  created_at   TEXT NOT NULL
);

CREATE INDEX idx_episode_detection_run ON episode_detection(run_id);
CREATE INDEX idx_episode_detection_stream ON episode_detection(stream_id);
CREATE INDEX idx_episode_detection_cluster ON episode_detection(cluster_id);

CREATE TABLE episode_cluster (
  cluster_id               INTEGER PRIMARY KEY AUTOINCREMENT,
  stream_id                INTEGER NOT NULL REFERENCES episode_stream(stream_id),
  first_day                TEXT NOT NULL,
  last_day                 TEXT NOT NULL,
  n_cases                  INTEGER NOT NULL,
  expected                 REAL,
  excess                   REAL,
  ratio                    REAL,
  priority_score           REAL NOT NULL,
  detector_agreement       INTEGER NOT NULL,
  opened_at                TEXT NOT NULL,
  last_detected_run        INTEGER NOT NULL REFERENCES episode_detection_run(run_id),
  runs_since_detected      INTEGER NOT NULL DEFAULT 0,
  changed_since_assessment INTEGER NOT NULL DEFAULT 0 CHECK (changed_since_assessment IN (0, 1)),
  suppressed_by            INTEGER REFERENCES episode_cluster(cluster_id),
  merged_into              INTEGER REFERENCES episode_cluster(cluster_id)
);

-- Note what this table does NOT carry: verdict, state, closed_at,
-- snooze_until. All four are authored by the app, which is insert-only,
-- and are therefore derived at read time from episode_assessment_event.
-- See ARCHITECTURE.md sections 5.0 and 6.1.

CREATE INDEX idx_episode_cluster_stream ON episode_cluster(stream_id);

CREATE TABLE episode_cluster_case (
  cluster_id INTEGER NOT NULL REFERENCES episode_cluster(cluster_id),
  case_id    INTEGER NOT NULL REFERENCES episode_case(case_id),
  PRIMARY KEY (cluster_id, case_id)
);

CREATE INDEX idx_episode_cluster_case_case ON episode_cluster_case(case_id);

-- ---------------------------------------------------------------------
-- 5.6 Assessments (app, append-only)
-- ---------------------------------------------------------------------
CREATE TABLE episode_assessment_event (
  event_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id     INTEGER NOT NULL REFERENCES episode_cluster(cluster_id),
  user_id        INTEGER NOT NULL REFERENCES episode_app_user(user_id),
  created_at     TEXT NOT NULL,
  verdict        TEXT CHECK (verdict IS NULL OR verdict IN (
                   'artefact', 'expected_variation', 'cluster_not_yet',
                   'possible_epidemic', 'confirmed_epidemic')),
  rationale      TEXT NOT NULL,
  wpg_notifiable INTEGER CHECK (wpg_notifiable IS NULL OR wpg_notifiable IN (0, 1)),
  ggd_informed   INTEGER CHECK (ggd_informed IS NULL OR ggd_informed IN (0, 1)),
  ggd_note       TEXT,
  snooze_until   TEXT,
  supersedes     INTEGER REFERENCES episode_assessment_event(event_id)
);

CREATE INDEX idx_episode_assessment_event_cluster ON episode_assessment_event(cluster_id);

-- ---------------------------------------------------------------------
-- 5.7 Reporting triangle (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_reporting_triangle (
  stream_id   INTEGER NOT NULL REFERENCES episode_stream(stream_id),
  sample_date TEXT NOT NULL,
  run_date    TEXT NOT NULL,
  n_cases     INTEGER NOT NULL,
  PRIMARY KEY (stream_id, sample_date, run_date)
);

-- ---------------------------------------------------------------------
-- 5.7.1 Denominators (cron)
-- ---------------------------------------------------------------------
CREATE TABLE episode_denominator (
  determination TEXT NOT NULL,
  sample_date   TEXT NOT NULL,
  care_line     TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'other', 'unknown')),
  area_code     TEXT,
  n_tests       INTEGER NOT NULL,
  PRIMARY KEY (determination, sample_date, care_line, area_code)
);

CREATE TABLE episode_mo_determination (
  mo_code       TEXT NOT NULL,
  determination TEXT NOT NULL,
  PRIMARY KEY (mo_code, determination)
);

-- ---------------------------------------------------------------------
-- 6.4 State trajectory (cron and app, append-only)
-- ---------------------------------------------------------------------
CREATE TABLE episode_cluster_state (
  state_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id INTEGER NOT NULL REFERENCES episode_cluster(cluster_id),
  state      TEXT NOT NULL CHECK (state IN (
               'new', 'assessing', 'monitoring', 'closable', 'closed', 'reassess')),
  entered_at TEXT NOT NULL,
  left_at    TEXT,
  trigger    TEXT NOT NULL CHECK (trigger IN (
               'detection', 'assessment', 'case_free', 'new_case', 'closure', 'system')),
  event_id   INTEGER REFERENCES episode_assessment_event(event_id),
  user_id    INTEGER REFERENCES episode_app_user(user_id)
);

CREATE INDEX idx_episode_cluster_state_cluster ON episode_cluster_state(cluster_id);

-- ---------------------------------------------------------------------
-- 5.8 Reports and users
-- ---------------------------------------------------------------------
CREATE TABLE episode_app_user (
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

-- app, and cron for pre-renders
CREATE TABLE episode_report_render (
  report_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id  INTEGER NOT NULL REFERENCES episode_cluster(cluster_id),
  user_id     INTEGER REFERENCES episode_app_user(user_id),
  rendered_at TEXT NOT NULL,
  file_path   TEXT NOT NULL,
  file_sha256 TEXT NOT NULL CHECK (length(file_sha256) = 64),
  params      TEXT NOT NULL,
  case_ids    TEXT NOT NULL,
  version_no  INTEGER NOT NULL
);

CREATE INDEX idx_episode_report_render_cluster ON episode_report_render(cluster_id);
