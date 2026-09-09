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
-- (AUTOINCREMENT -> AUTO_INCREMENT, the PRAGMA line dropped, and every
-- TEXT column that MySQL rejects as-is - one carrying a UNIQUE or
-- PRIMARY KEY constraint, a DEFAULT value, or used in a CREATE INDEX or
-- composite PRIMARY KEY - given a bounded VARCHAR instead) - there is
-- deliberately only one schema file to keep in sync.
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
  care_line       TEXT CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown')),
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
  created_at  TEXT NOT NULL
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
  -- The run's own business date, as passed to episodic_run_cron(). Nearly
  -- always the date part of started_at, but not by construction: a
  -- backfill or a replay names the date it is standing in for, and the
  -- reporting-delay calculation reads this, not the wall clock.
  run_date          TEXT NOT NULL,
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
  care_line        TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown')),
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
  lab_number     TEXT NOT NULL,  -- the lab's own specimen/culture number; not unique, unlike source_key
  patient_key    TEXT NOT NULL,
  sample_date    TEXT NOT NULL,
  receipt_date   TEXT,
  pathogen       TEXT NOT NULL,  -- raw lab-provided string, used verbatim
  care_line      TEXT NOT NULL DEFAULT 'unknown' CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown')),
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
-- Serves episodic_db_last_case_dates(), the per-run lookup that lets an
-- operator send only a recent window of positives instead of full history.
CREATE INDEX idx_episodic_case_patient_pathogen ON episodic_case(patient_key, pathogen, sample_date);
-- Finds every other result from the same culture (e.g. two isolates of
-- the same pathogen reported separately with different antibiograms).
CREATE INDEX idx_episodic_case_lab_number ON episodic_case(lab_number);

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
  -- NULL only for origin = 'manual': a cluster added through
  -- episodic_add_manual_cluster() was never produced by a detection run,
  -- so there is no run to reference here.
  last_detected_run        INTEGER REFERENCES episodic_detection_run(run_id),
  runs_since_detected      INTEGER NOT NULL DEFAULT 0,
  changed_since_assessment INTEGER NOT NULL DEFAULT 0 CHECK (changed_since_assessment IN (0, 1)),
  suppressed_by            INTEGER REFERENCES episodic_cluster(cluster_id),
  merged_into              INTEGER REFERENCES episodic_cluster(cluster_id),
  -- 'detected': produced by episodic_reconcile_stream() from real
  -- detector output, the only kind that existed before this column.
  -- 'manual': added directly through episodic_add_manual_cluster(), for
  -- output from another algorithm/system, never connected to this
  -- instance's own episodic_case data. Manual clusters are excluded from
  -- reconciliation matching and from automatic staleness/closure
  -- (both in R/reconcile.R); their case-level detail
  -- (if any) lives in episodic_cluster_manual_case, never in
  -- episodic_case/episodic_cluster_case.
  origin                   TEXT NOT NULL DEFAULT 'detected' CHECK (origin IN ('detected', 'manual'))
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

-- Case-level detail for origin = 'manual' clusters only: enough to drive
-- the epi curve/demography/geo panels (sample_date, pc, sex, age), and
-- deliberately nothing else - no patient_key, lab_number or source_key,
-- so a manual cluster can never be joined back to a real patient. Kept
-- fully separate from episodic_case/episodic_cluster_case so this data
-- never reaches denominators, line lists, or patient search.
CREATE TABLE episodic_cluster_manual_case (
  manual_case_id INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id     INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  sample_date    TEXT NOT NULL,
  pc             TEXT,
  sex            TEXT CHECK (sex IS NULL OR sex IN ('M', 'F', 'U')),
  age            INTEGER
);

CREATE INDEX idx_episodic_cluster_manual_case_cluster ON episodic_cluster_manual_case(cluster_id);

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
-- 5.6.1 Cluster notes (app, append-only)
--
-- A free-text scratchpad per cluster, open to any signed-in role (not
-- gated on epidemiologist, unlike the assessment form) - distinct from
-- episodic_assessment_event, which is a formal verdict trail, not a
-- place for informal working notes. Event-sourced like every other
-- app-authored table: "current" is the most recent row per cluster_id.
-- note_text is always the raw markdown source, rendered to HTML only at
-- display time, never stored as HTML.
-- ---------------------------------------------------------------------
CREATE TABLE episodic_cluster_note (
  note_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  user_id    INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at TEXT NOT NULL,
  note_text  TEXT NOT NULL
);

CREATE INDEX idx_episodic_cluster_note_cluster ON episodic_cluster_note(cluster_id);

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
--
-- area_code is nullable by design - a site with a single catchment
-- supplies no stratum at all (episodic_validate_denominators() does not
-- require it) - so it cannot sit in the PRIMARY KEY: MySQL implicitly
-- forces every PRIMARY KEY column NOT NULL (silently, at CREATE TABLE
-- time), which would later reject any such row with "column 'area_code'
-- cannot be null". A surrogate PRIMARY KEY plus a UNIQUE constraint on
-- the natural key gets the same one-row-per-stratum guarantee in both
-- SQLite and MySQL, since UNIQUE (unlike PRIMARY KEY) treats multiple
-- NULLs as distinct in both dialects.
--
-- That last point cuts both ways, and is why episodic_denominators_load()
-- matches an incoming row against what is on file in R rather than letting
-- the database decide with an upsert on this constraint: NULL is not equal
-- to NULL, so no conflict would ever be raised for a row without an area
-- stratum, and every one of them would insert afresh on every run.
-- ---------------------------------------------------------------------
CREATE TABLE episodic_denominator (
  denominator_id INTEGER PRIMARY KEY AUTOINCREMENT,
  pathogen       TEXT NOT NULL,
  sample_date    TEXT NOT NULL,
  care_line      TEXT NOT NULL CHECK (care_line IN ('first', 'second', 'third', 'other', 'unknown')),
  area_code      TEXT,
  n_tests        INTEGER NOT NULL,
  UNIQUE (pathogen, sample_date, care_line, area_code)
);

-- ---------------------------------------------------------------------
-- 6.4 State trajectory (cron and app, append-only)
-- ---------------------------------------------------------------------
CREATE TABLE episodic_cluster_state (
  state_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  state      TEXT NOT NULL CHECK (state IN (
               'new', 'assessing', 'monitoring', 'closed', 'reassess')),
  -- Append-only, so a state's end is the next row's entered_at; there is
  -- deliberately no left_at to keep in step with it.
  entered_at TEXT NOT NULL,
  `trigger`  TEXT NOT NULL CHECK (`trigger` IN (
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
  role          TEXT NOT NULL CHECK (role IN ('epidemiologist', 'viewer')),
  -- Additive to `role`: an admin is still either an epidemiologist or a
  -- viewer for read/write purposes, and separately may see the Settings
  -- screen (episodic_user_is_admin()). Mutated the same insert-only way as
  -- role/is_active below, never via UPDATE.
  is_admin      INTEGER NOT NULL DEFAULT 0 CHECK (is_admin IN (0, 1)),
  is_active     INTEGER NOT NULL DEFAULT 1 CHECK (is_active IN (0, 1)),
  must_change   INTEGER NOT NULL DEFAULT 1 CHECK (must_change IN (0, 1)),
  created_at    TEXT NOT NULL
);

-- Account bookkeeping that would otherwise need an UPDATE (a fresh
-- password_hash on change, a new last-login time on every login, a role or
-- admin flag or active-state change made from the Settings screen) is kept
-- insert-only instead, the same event-sourced pattern episodic_cluster_state
-- already uses for cluster state: the "current" password hash/role/is_admin/
-- is_active is the most recent matching event's, falling back to
-- episodic_app_user's own initial column value if no such event exists yet,
-- and the last login is the most recent login event. There is deliberately
-- no last_login_at column on episodic_app_user for the same value to drift
-- out of step with.
CREATE TABLE episodic_app_user_event (
  event_id      INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id       INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at    TEXT NOT NULL,
  event_type    TEXT NOT NULL CHECK (event_type IN (
                  'login', 'password_change', 'role_change',
                  'admin_change', 'active_change')),
  actor_user_id INTEGER REFERENCES episodic_app_user(user_id),  -- who made the change; NULL for login/password_change (self-service)
  password_hash TEXT,                                            -- set only for password_change events
  new_role      TEXT CHECK (new_role IS NULL OR new_role IN ('epidemiologist', 'viewer')), -- set only for role_change events
  new_is_admin  INTEGER CHECK (new_is_admin IS NULL OR new_is_admin IN (0, 1)),  -- set only for admin_change events
  new_is_active INTEGER CHECK (new_is_active IS NULL OR new_is_active IN (0, 1)) -- set only for active_change events
);

CREATE INDEX idx_episodic_app_user_event_user ON episodic_app_user_event(user_id);

-- ---------------------------------------------------------------------
-- 5.9 In-app settings: notification/config overrides (app, append-only)
--
-- The YAML overlay (EPISODIC_CONFIG) remains the authoritative source of
-- detection configuration and stays fully supported; this table is an
-- additional, higher-precedence overlay that lets an is_admin account
-- change certain sections (currently: notifications) from the Settings
-- screen without SSH/file access. episodic_config_resolve() applies the
-- most recent event per section on top of the YAML-resolved config. Every
-- change is kept (never updated/deleted) for the audit trail the Settings
-- screen shows; config_json for 'notifications' may contain secrets
-- (SMTP passwords, webhook URLs) and is masked in the UI on display, never
-- on storage - the database is already the trust boundary every other
-- secret-bearing table in this schema relies on.
-- ---------------------------------------------------------------------
CREATE TABLE episodic_app_config_event (
  event_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id     INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at  TEXT NOT NULL,
  section     TEXT NOT NULL CHECK (section IN ('notifications')),
  config_json TEXT NOT NULL,
  note        TEXT
);

CREATE INDEX idx_episodic_app_config_event_section ON episodic_app_config_event(section, created_at);

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

-- app. Event-sourced, same shape as episodic_cluster_note: an
-- epidemiologist "sets" a schedule (an every-N-days cadence plus a
-- recipient list, for colleagues with no EpiSODIC account and no need
-- for one - see episodic_scheduled_reports_dispatch()) or "cancels" one;
-- the current schedule for a cluster is always the latest row for that
-- cluster_id, exactly like episodic_app_config_latest()'s pattern for
-- notifications. recipients/channel/interval_days are NULL on a
-- 'cancel' row - there is nothing left to schedule. channel names one
-- already-configured, email-capable entry under
-- notifications.channels (smtp, sendmail or microsoft365): scheduled
-- reports are delivered through the same channel machinery
-- episodic_notify() uses, never a separate credential store.
CREATE TABLE episodic_report_subscription_event (
  event_id         INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id       INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  user_id          INTEGER NOT NULL REFERENCES episodic_app_user(user_id),
  created_at       TEXT NOT NULL,
  action           TEXT NOT NULL CHECK (action IN ('set', 'cancel')),
  interval_days    INTEGER,
  recipients       TEXT,
  channel          TEXT,
  include_linelist INTEGER NOT NULL DEFAULT 0 CHECK (include_linelist IN (0, 1))
);

CREATE INDEX idx_episodic_report_subscription_event_cluster ON episodic_report_subscription_event(cluster_id, created_at);

-- cron. One row per attempted send of a scheduled report - "attempted"
-- because a failed send (channel misconfigured, SMTP unreachable) is
-- still logged, never silently dropped, so an epidemiologist can see why
-- a colleague never received an update. subscription_event_id pins the
-- send to the exact schedule settings in effect at the time (recipients
-- may since have changed); recipients is a snapshot of who it actually
-- went to. `final` marks the one closure/suppression send that ends a
-- schedule automatically (see episodic_scheduled_reports_dispatch()).
CREATE TABLE episodic_report_subscription_send (
  send_id               INTEGER PRIMARY KEY AUTOINCREMENT,
  cluster_id            INTEGER NOT NULL REFERENCES episodic_cluster(cluster_id),
  subscription_event_id INTEGER NOT NULL REFERENCES episodic_report_subscription_event(event_id),
  run_id                INTEGER REFERENCES episodic_detection_run(run_id),
  sent_at               TEXT NOT NULL,
  report_id             INTEGER REFERENCES episodic_report_render(report_id),
  recipients            TEXT NOT NULL,
  status                TEXT NOT NULL CHECK (status IN ('sent', 'failed')),
  error_text            TEXT,
  final                 INTEGER NOT NULL DEFAULT 0 CHECK (final IN (0, 1))
);

CREATE INDEX idx_episodic_report_subscription_send_cluster ON episodic_report_subscription_send(cluster_id, sent_at);

-- ---------------------------------------------------------------------
-- 5.11 Failed sign-in attempts (app, append-only)
--
-- Successful sign-ins are an account event and live in
-- episodic_app_user_event ('login'); a failed one may have no account at
-- all - a username nobody has, typed by somebody probing or by a
-- colleague who mistyped - and episodic_app_user_event.user_id is NOT
-- NULL by design, so it cannot hold them. Hence a table of its own,
-- rather than an event type that would need a fictional user_id.
--
-- episodic_auth_login() deliberately tells the visitor nothing about
-- WHY a sign-in failed (unknown username, wrong password and deactivated
-- account are one generic outcome to them). That is a statement about
-- the response, not about the record: which of the three it was is
-- exactly what the operator reading the audit trail needs, so it is
-- recorded here in full.
--
-- `username` is stored as typed and may therefore be anything a visitor
-- can send. It is written by parameterised query, truncated at write
-- time, and rendered as text, never as markup.
-- ---------------------------------------------------------------------
CREATE TABLE episodic_app_login_failure (
  failure_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  attempted_at TEXT NOT NULL,
  username     TEXT NOT NULL,   -- as typed; may match no account
  -- Filled when the username does name an account, NULL when it does not.
  user_id      INTEGER REFERENCES episodic_app_user(user_id),
  reason       TEXT NOT NULL CHECK (reason IN (
                 'unknown_username', 'wrong_password', 'inactive_account'))
);

CREATE INDEX idx_episodic_app_login_failure_at ON episodic_app_login_failure(attempted_at);

-- ---------------------------------------------------------------------
-- 5.12 Schema version (created by episodic_db_create(), advanced by
-- episodic_db_migrate())
--
-- EpiSODIC is deployed at laboratories that then upgrade the package,
-- and a schema is not a fixed thing: every column added in a future
-- release has to reach a database that already holds years of
-- surveillance data. Without a recorded version there is no way to tell
-- which shape a given database is in, so the upgrade fails as an
-- unexplained SQL error on the first run after the update - or worse,
-- does not fail, and reads a column that means something else now.
--
-- Append-only: the current version is the highest row present, and the
-- table doubles as the record of when each step was applied and by which
-- package version. A database created fresh records only the version it
-- was built at (the intermediate ones were never applied to it); one
-- brought forward by episodic_db_migrate() gains a row per step.
-- episodic_db_connect() compares the highest against what the installed
-- package expects and refuses a mismatch by name rather than letting the
-- query layer discover it.
-- ---------------------------------------------------------------------
CREATE TABLE episodic_schema_version (
  version    INTEGER PRIMARY KEY,
  applied_at TEXT NOT NULL,
  -- The package version that applied it, for the audit trail.
  applied_by TEXT
);
