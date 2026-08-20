# EpiSODIC

**Epidemiological Signal Observation, Detection, Identification and Classification**

**Architecture document, draft 6**
Author: Dr M.S. Berends, Department of Medical Epidemiology, Certe
Licence: GPL-2
Database tables are prefixed `episode_` throughout.
Status: topology, persistence, authentication, interface and data scope settled. Remaining open decisions in section 18. Superseded approaches are listed there under Considered and rejected and must not be reintroduced.

The name carries a useful double meaning: `AMR::get_episode()` is the function on which both detectors and the reconciliation algorithm depend.

---

## 1. Purpose

An automated surveillance system that detects aberrations in laboratory-confirmed infections across all pathogens, presents them to a small board of epidemiologists as a ranked list of dossiers, records their formal assessment with a full audit trail, and produces outbreak reports for clinical colleagues.

The system has two halves that must remain separable:

- **Engine.** Detection, reconciliation, statistics, plotting, report templates. Open source, no data, no Certe-specific configuration.
- **Instance.** Configuration, database, rendered reports. Operator-owned, never committed, never published.

The engine must run for a stranger who clones the repository, using bundled synthetic data and a SQLite backend, without access to Diver or any Certe system.

---

## 2. Fixed constraints

These are settled and drive everything downstream.

| Constraint | Value |
|---|---|
| Detection functions | `certestats::detect_disease_clusters()` and `certestats::detect_farrington()` |
| Data source | `certedb::get_diver_data()` against Diver cBases |
| Date anchor | Sample date (afnamedatum), which silently falls back to receipt date when unfilled |
| Case fields | Patient ID, sex, age, PC4, care line (1st/2nd), institution, ward, specialism |
| Denominators | Tests keyed on determination; patient-days and beds per hospital |
| Hospitals | Eight in the region, full microbiology performed by Certe |
| Detection host | Author's AVD session host, primary plus backup, existing `cron.R` with sequential retry |
| State backend | One SQLite file, path from the instance configuration |
| Assessors | Four epidemiologists: the author plus three medical board specialists |
| Assessor access | Thin client to `mijnwerkplek.in.certe.nl`, no AVD, no R, browser only |
| Delivery | Shiny app served from the AVD, confirmed reachable from thin clients on port 7788 |
| Detection scope | All pathogens, automatically |
| Lattice | L1 afdeling to L5 regio, ascending with aggregation, redundancy suppressed |
| Expected volume | Five to ten assessed clusters per month. The eligibility gate is tuned to this |
| Interface language | Dutch by default, full i18n, code and documentation in English |

---

## 3. Topology

```
                 Diver cBases
                      |
                      | certedb::get_diver_data()
                      v
        +-------------------------------+
        |  AVD session host (primary)   |
        |                               |
        |  cron.R  ->  detection        |
        |             reconciliation    |
        |             triangle update   |
        |             report pre-render |
        |                               |
        |  Shiny app (scheduled task,   |
        |  run whether logged on or not)|
        +---------------+---------------+
                        | HTTP(S) :7788
                        |
        +---------------+---------------+
        |               |               |
   Assessor 2      Assessor 3      Assessor 4
   (thin client)   (thin client)   (thin client)
                        |
                        v
              episode.sqlite
              (signals, assessments, cases)
```

Backup AVD runs the same `cron.R` under the existing retry chain. It does **not** serve the app; only the primary does, so the URL is stable.

### 3.1 Durability

A logoff terminates every process in the session, so an app started from the R console dies with it and colleagues get a refused connection until the next login. IT's five-day session limit therefore guarantees a recurring outage.

The fix is a Windows scheduled task with "Run whether user is logged on or not", which runs the process in session 0 where interactive session policy does not apply. Configuration traps, in order of how much time they cost when missed:

- **Settings, "Stop the task if it runs longer than", defaults to 3 days.** Untick it. Otherwise Task Scheduler kills the app on its own schedule, which is very hard to diagnose after the fact.
- Trigger "At startup", plus "If the task fails, restart every 1 minute" and "If the task is already running: do not start a new instance".
- Untick "Start the task only if the computer is on AC power" and all idle conditions.
- The account needs the "Log on as a batch job" right, and its password is stored with the task, so a routine password change breaks the app silently.
- **Session 0 has no mapped drives.** `Z:` does not exist there. Use UNC paths throughout.
- A User DSN under the interactive profile is invisible to a task running under another account. The app needs only the SQLite file, but the cron's Diver DSN must be a System DSN.

A five-minute watchdog task that tests the port and restarts on failure is a few lines and removes the whole class of problem.

Running it as a Windows service through NSSM is the more robust alternative: automatic restart, start at boot, proper service logging. It needs a one-off install with administrator rights.

### 3.2 Transport

Plain HTTP on the internal network. Adequate for this deployment. TLS through a local Caddy reverse proxy is available if IT ever asks for it.

### 3.3 Concurrency

A single R process serves all sessions, so any slow operation blocks every user. Therefore the app performs **cheap reads only**. All detection, all Diver querying, all report rendering and all triangle maintenance happen in the cron. Where an interactive action must be expensive (an ad hoc re-render), it runs through `promises`/`future` on a separate worker.

---

## 4. Repository layout

```
EpiSODIC/
  DESCRIPTION                 GPL-2
  NAMESPACE
  R/
    db_*.R                    DBI repository layer, split by writer (cron / app)
    detect_*.R                wrappers around certestats
    reconcile_*.R             signal identity and merging
    lattice_*.R               stream enumeration and suppression
    institution_*.R           institution normalisation and allow-listing
    triangle_*.R              reporting triangle and truncation
    score_*.R                 priority scoring
    epi_*.R                   curve, demography, geography, Rt
    app_*.R                   Shiny modules
    i18n.R
    run_app.R
    run_cron.R
  inst/
    app/www/                  SCSS, logo, custom JS
    i18n/nl.json, en.json
    quarto/report.qmd
    sql/schema.sql            SQLite DDL
    config/default.yaml       shipped defaults, not instance configuration
    config/pathogen_config.csv
    demo/                     synthetic data, SQLite bootstrap
  tests/testthat/
  vignettes/
  man/
  NEWS.md
  README.md
  LICENSE
```

R package names are case-sensitive, so `library(EpiSODIC)` is valid and the branding survives into the code.

The instance lives entirely outside this tree and is located through `EPISODE_CONFIG`. See section 7.5. Nothing under the repository ever contains a credential, an instance parameter or a patient record.

---

## 5. Data model

One SQLite file. No server, no credentials, no client-server database anywhere in the system.

This was a MySQL design in earlier drafts, on the assumption that four assessors meant four writing processes. They do not: all four share the single Shiny process on the AVD, so there is exactly one interactive writer and one nightly cron, against a dataset of a few hundred clusters a year. SQLite is not a compromise at this scale, it is the right answer, and it removes a server, a credentials file and an IT dependency from the design.

The DDL below is written in MySQL-flavoured syntax for readability. The canonical schema ships as `inst/sql/schema.sql` in SQLite dialect, mapping as follows.

| Shown as | SQLite |
|---|---|
| `BIGINT AUTO_INCREMENT PRIMARY KEY` | `INTEGER PRIMARY KEY AUTOINCREMENT` |
| `ENUM(...)` | `TEXT` with a `CHECK (col IN (...))` constraint |
| `JSON` | `TEXT` holding JSON |
| `DATETIME`, `DATE` | `TEXT`, ISO 8601 |
| `TINYINT(1)` | `INTEGER` 0 or 1 |
| `DECIMAL(p,s)` | `REAL` |

Enable `PRAGMA journal_mode = WAL` and `PRAGMA busy_timeout = 5000`. WAL is available because the file sits on the local disk of the AVD that runs both the app and the cron.

### 5.0 Write ownership

**The cron owns the facts. The app owns the judgements.** The two processes never write the same table, so they never contend.

| Table | Written by |
|---|---|
| `episode_detection_run` | cron |
| `episode_case` | cron |
| `episode_reporting_triangle` | cron |
| `episode_stream` | cron |
| `episode_institution`, `episode_institution_activity` | cron |
| `episode_denominator` | cron |
| `episode_detection` | cron |
| `episode_cluster` | cron |
| `episode_cluster_case` | cron |
| `episode_assessment_event` | app |
| `episode_stream_mute` | app |
| `episode_cluster_state` | both, append only |
| `episode_report_render` | app, and cron for pre-renders |
| `episode_app_user` | app, rarely |

**The app only ever inserts.** It never updates and never deletes. This is what makes the whole concurrency question disappear: two people assessing the same cluster in the same minute produce two events, both visible in the timeline, latest effective by timestamp. That is also the more defensible record for a board of four, so optimistic locking and row versioning are dropped from the design entirely.

If the app must run on a different host from the cron, the file has to live on a share, WAL becomes unavailable and the risk calculus changes. Keep both on the same host.

### 5.0.1 Backup

The only irreplaceable data is the assessments. Cases, detections and clusters can be regenerated by re-running the cron over history. So the backup requirement is small: a dated copy of the SQLite file after each nightly run, plus a mirror of every assessment event appended to a plain JSONL file. Recovering from a corrupt database then means re-running detection and replaying one text file.

### 5.1 Streams

A stream is a monitored slice of the data. `stream_key` is a deterministic hash of the defining dimensions, so the same slice always receives the same identifier across runs and across hosts. The detector is **not** part of stream identity, because both detectors watch the same stream.

```sql
CREATE TABLE episode_stream (
  stream_id      BIGINT AUTO_INCREMENT PRIMARY KEY,
  stream_key     CHAR(40) NOT NULL UNIQUE,      -- sha1 of dimensions below
  level          ENUM('pathogen_ward',        -- L1
                      'pathogen_institution', -- L2
                      'pathogen_area',        -- L3
                      'pathogen_province',    -- L4
                      'pathogen_region')      -- L5
                 NOT NULL,
  mo_code        VARCHAR(24) NOT NULL,          -- AMR::as.mo() code
  mo_name        VARCHAR(128) NOT NULL,         -- denormalised for display
  mo_rank        VARCHAR(24)  NOT NULL,         -- species, genus, group
  care_line      ENUM('first','second','other','unknown') NULL,
  region_code    VARCHAR(16)  NULL,
  institution_id BIGINT       NULL,             -- hospital streams only
  denominator    ENUM('none','tests','population','patient_days') NOT NULL DEFAULT 'none',
  severity_weight DECIMAL(3,2) NOT NULL DEFAULT 1.00,
  is_active      TINYINT(1)   NOT NULL DEFAULT 1,
  first_seen     DATE NOT NULL,
  last_seen      DATE NOT NULL,
  created_at     DATETIME NOT NULL
);
```

Taxonomic stability matters: pathogens are keyed on `AMR::as.mo()` output rather than free-text names, and the rank is stored so that a rename upstream does not fracture a stream's history.

The `denominator` column records which normalisation applies, since it differs by level: patient-days for hospital streams, population for regional first-line streams, test counts where neither is available.

### 5.2 Mutes

Mutes are historical records, not a column, so that a past suppression remains visible in the audit trail.

```sql
CREATE TABLE episode_stream_mute (
  mute_id     BIGINT AUTO_INCREMENT PRIMARY KEY,
  stream_id   BIGINT NOT NULL,
  muted_from  DATE NOT NULL,
  muted_until DATE NOT NULL,
  reason      ENUM('seasonal','screening_campaign','method_change','known_source','other') NOT NULL,
  note        TEXT,
  user_id     BIGINT NOT NULL,
  created_at  DATETIME NOT NULL,
  revoked_at  DATETIME NULL,
  FOREIGN KEY (stream_id) REFERENCES episode_stream(stream_id),
  FOREIGN KEY (user_id)   REFERENCES episode_app_user(user_id)
);
```

### 5.3 Runs

```sql
CREATE TABLE episode_detection_run (
  run_id           BIGINT AUTO_INCREMENT PRIMARY KEY,
  host             VARCHAR(64) NOT NULL,
  account          VARCHAR(64) NOT NULL,
  started_at       DATETIME NOT NULL,
  finished_at      DATETIME NULL,
  status           ENUM('running','success','failed','partial') NOT NULL,
  attempt_no       TINYINT NOT NULL DEFAULT 1,
  n_streams        INT NULL,
  n_detections     INT NULL,
  n_signals_new    INT NULL,
  n_signals_updated INT NULL,
  code_version     VARCHAR(64) NULL,   -- installed EpiSODIC version
  pkg_versions     JSON NULL,          -- certestats, surveillance, AMR, EpiEstim
  config_hash      CHAR(40) NULL,      -- sha1 of the resolved configuration
  config_snapshot  JSON NULL,          -- the resolved configuration in full
  error_text       TEXT NULL
);
```

These columns are not decoration. `config_snapshot` holds the fully resolved configuration actually used by that run, defaults and overrides collapsed into one object. Any result is therefore explainable from the database alone, with no reference to a file, a folder or a repository. When a threshold shifts because someone edited the config in March or because `surveillance` changed a default, this table is the only place that can say so.

Each run writes its signals inside one transaction, so a retry after a partial failure never compounds the state. Reconciliation is keyed on `stream_key` and interval overlap rather than insertion order, which makes reruns idempotent by construction.

### 5.4 Cases

```sql
CREATE TABLE episode_case (
  case_id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  source_key     VARCHAR(64) NOT NULL UNIQUE,   -- stable LIS identifier
  patient_key    VARCHAR(64) NOT NULL,
  sample_date    DATE NOT NULL,
  receipt_date   DATE NULL,
  mo_code        VARCHAR(24) NOT NULL,
  determination  VARCHAR(32) NULL,              -- links to the denominator
  material       VARCHAR(32) NULL,
  care_line      ENUM('first','second','other','unknown') NOT NULL DEFAULT 'unknown',
  institution_id BIGINT NULL,                   -- normalised, see 5.4.1
  ward           VARCHAR(64) NULL,              -- second line only
  specialism     VARCHAR(64) NULL,
  pc4            CHAR(4) NULL,
  sex            ENUM('M','F','U') NULL,
  age            SMALLINT NULL,
  first_seen_run BIGINT NOT NULL,
  INDEX (sample_date), INDEX (mo_code), INDEX (patient_key), INDEX (institution_id)
);
```

Prefixing resolves an incidental irritation: `case` is a reserved word in SQL and would have required quoting everywhere. `episode_case` does not.

`first_seen_run` is what makes the reporting triangle possible without a separate ingestion log.

PC4 is stored as `CHAR(4)` rather than an integer, so it joins directly against `certegis` without coercion.

### 5.4.1 Institutions

The individual requester is not an analytical object. Diver carries named clinicians and those fields must never reach the database; the pull uses an explicit column allow-list rather than a drop-list, so a new upstream column cannot leak in silently.

What matters is the care-providing organisation, normalised on ingestion to the level that is epidemiologically useful and no finer:

| Source | Stored as |
|---|---|
| Hospital | Institution name, first-class entity |
| Long-term care institution | Institution name |
| General practice | Municipality of the practice |
| Out-of-hours GP service | Service name |
| Other or unknown | `NULL` |

The asymmetry is deliberate and worth stating in the published documentation. A hospital or care home is a transmission unit, so its identity is the analytical object. A single-handed GP practice is not a transmission unit; it is a proxy for a small population, and its name adds identifiability without adding epidemiological information. The municipality is both safer and more useful.

```sql
CREATE TABLE episode_institution (
  institution_id   BIGINT AUTO_INCREMENT PRIMARY KEY,
  institution_key  CHAR(40) NOT NULL UNIQUE,     -- sha1 of source identifier
  display_name     VARCHAR(128) NOT NULL,
  institution_type ENUM('hospital','ltc_institution','gp_municipality',
                        'ooh_service','other') NOT NULL,
  care_line        ENUM('first','second','other','unknown') NOT NULL,
  municipality     VARCHAR(64) NULL,
  pc4              CHAR(4) NULL,
  n_beds           INT NULL,                     -- hospitals
  is_monitored     TINYINT(1) NOT NULL DEFAULT 0,-- gets its own streams
  is_active        TINYINT(1) NOT NULL DEFAULT 1
);
```

Keying on a hash of the source identifier rather than on the display name means an institutional rename does not fracture the history, which matters given how often Dutch care organisations merge.

`is_monitored` is what makes hospital-level detection tractable. The eight regional hospitals are flagged and receive their own streams. Everything else does not, and is covered by the rule-based detector in section 7.2.

### 5.4.2 Hospital activity

```sql
CREATE TABLE episode_institution_activity (
  institution_id BIGINT NOT NULL,
  period_start   DATE NOT NULL,
  period_end     DATE NOT NULL,
  patient_days   INT NULL,
  admissions     INT NULL,
  n_beds         INT NULL,
  source         VARCHAR(64) NULL,
  PRIMARY KEY (institution_id, period_start)
);
```

Patient-days are the correct denominator for hospital streams, and their availability changes what the system can claim. Counts alone confound occupancy with incidence: a summer dip in admissions lowers case counts without any change in transmission, and a capacity expansion raises them without any either. Cases per 1,000 patient-days is the standard unit in nosocomial surveillance and is also what makes the eight hospitals comparable with one another.

Beds are a weaker proxy, useful as a fallback and as context on the signal detail, but occupancy varies too much for beds to substitute for patient-days.

### 5.4.3 Deduplication

One isolate per patient per episode. A patient with three positive cultures during one illness is one case, and eleven isolates from eight patients is a different cluster from eleven patients.

Deduplication uses `AMR::get_episode()` at ingestion, with the episode length taken per organism rather than fixed: thirty days is reasonable for *Campylobacter*, a year is closer to right for MRSA carriage. The length lives in `episode_pathogen_config` alongside the serial interval, incubation period and cool-down.

```sql
CREATE TABLE episode_pathogen_config (
  mo_code         VARCHAR(24) NOT NULL PRIMARY KEY,
  episode_days    INT NOT NULL DEFAULT 30,   -- deduplication window
  incub_min_days  DECIMAL(5,2) NULL,
  incub_max_days  DECIMAL(5,2) NULL,
  case_free_days  INT NOT NULL DEFAULT 14,   -- 14 or 21 depending on organism
  cooldown_days   INT NULL,                  -- silence after closure
  rt_applicable   TINYINT(1) NOT NULL DEFAULT 0,
  si_mean_days    DECIMAL(6,2) NULL,
  si_sd_days      DECIMAL(6,2) NULL,
  si_dist         ENUM('gamma','lognormal','weibull') NULL,
  mem_applicable  TINYINT(1) NOT NULL DEFAULT 0,
  severity_weight DECIMAL(3,2) NOT NULL DEFAULT 1.00,
  source_ref      VARCHAR(256) NULL
);
```

This supersedes `episode_serial_interval` from draft 2: one table per organism carrying every pathogen-specific constant, since they are curated together and always read together. Both the case count and the deduplicated patient count are stored on the signal, and the interface shows both.

### 5.5 Detections and signals

This is the core distinction. A **detection** is what an algorithm produced in one run. A **cluster** is the persistent real-world entity an epidemiologist assesses. Neither `certestats` function emits a stable identity, so the mapping between the two is the system's central algorithm.

```sql
CREATE TABLE episode_detection (
  detection_id BIGINT AUTO_INCREMENT PRIMARY KEY,
  run_id       BIGINT NOT NULL,
  stream_id    BIGINT NOT NULL,
  cluster_id    BIGINT NULL,                     -- assigned by reconciliation
  detector     ENUM('clusters','farrington','ears','mem','rare_trigger','same_place') NOT NULL,
  first_day    DATE NOT NULL,
  last_day     DATE NOT NULL,
  n_cases      INT NOT NULL,
  expected     DECIMAL(10,3) NULL,
  upperbound   DECIMAL(10,3) NULL,
  params       JSON NOT NULL,                   -- attributes of the certestats object
  created_at   DATETIME NOT NULL
);

CREATE TABLE episode_cluster (
  cluster_id        BIGINT AUTO_INCREMENT PRIMARY KEY,
  stream_id        BIGINT NOT NULL,
  first_day        DATE NOT NULL,
  last_day         DATE NOT NULL,
  n_cases          INT NOT NULL,
  expected         DECIMAL(10,3) NULL,
  excess           DECIMAL(10,3) NULL,
  ratio            DECIMAL(10,3) NULL,
  priority_score   DECIMAL(5,2) NOT NULL,
  detector_agreement TINYINT NOT NULL,          -- how many detectors fired
  opened_at        DATETIME NOT NULL,
  last_detected_run BIGINT NOT NULL,
  runs_since_detected INT NOT NULL DEFAULT 0,
  changed_since_assessment TINYINT(1) NOT NULL DEFAULT 0,
  suppressed_by    BIGINT NULL,                 -- lattice redundancy
  merged_into      BIGINT NULL                  -- reconciliation merge
);

-- Note what this table does NOT carry: verdict, state, closed_at, snooze_until.
-- All four are authored by the app, which is insert-only, and are therefore
-- derived at read time from episode_assessment_event. See sections 5.0 and 6.1.

CREATE TABLE episode_cluster_case (
  cluster_id BIGINT NOT NULL,
  case_id   BIGINT NOT NULL,
  PRIMARY KEY (cluster_id, case_id)
);
```

### 5.6 Assessments

Append-only, and the only table the app writes on classification.

Note what is **not** on `episode_cluster`: there is no `state` column. State is a pure function of the assessment events, the case-free clock and the changed flag, computed at read time. Caching it would require the app to update a row the cron owns, which would break the write-ownership rule in section 5.0 for no benefit at this data volume.

```sql
CREATE TABLE episode_assessment_event (
  event_id     BIGINT AUTO_INCREMENT PRIMARY KEY,
  cluster_id    BIGINT NOT NULL,
  user_id      BIGINT NOT NULL,
  created_at   DATETIME NOT NULL,
  verdict      ENUM('artefact','expected_variation','cluster_not_yet',
                    'possible_epidemic','confirmed_epidemic') NULL,
  rationale    TEXT NOT NULL,                   -- mandatory, free text
  wpg_notifiable TINYINT(1) NULL,
  ggd_informed   TINYINT(1) NULL,
  ggd_note       TEXT NULL,
  snooze_until DATE NULL,
  supersedes   BIGINT NULL
);
```

The two axes are deliberate. The verdict answers "what was it"; the derived state answers "does this still need attention". A single "irrelevant" option could not separate a detector artefact from a true but unimportant rise, and that distinction is exactly what section 10 needs to evaluate the system's own performance in two years.

### 5.7 Reporting triangle

```sql
CREATE TABLE episode_reporting_triangle (
  stream_id   BIGINT NOT NULL,
  sample_date DATE NOT NULL,
  run_date    DATE NOT NULL,
  n_cases     INT NOT NULL,                     -- cumulative as visible on run_date
  PRIMARY KEY (stream_id, sample_date, run_date)
);
```

Because afnamedatum silently becomes ontvangstdatum when the physician leaves it blank, the reporting delay cannot be recovered from the date columns. It can be measured from observed accrual: how many cases with sample date D were visible on D, D+1, D+2. That is also the quantity the truncation correction actually requires.

### 5.7.1 Denominators

Tests are counted independently of streams, because a determination is not pathogen-specific: one faeces culture can yield several organisms and none. The denominator is therefore keyed on determination, and a mapping table connects determinations to the pathogens they can detect.

```sql
CREATE TABLE episode_denominator (
  determination VARCHAR(32) NOT NULL,
  sample_date   DATE NOT NULL,
  care_line     ENUM('first','second','other','unknown') NOT NULL,
  area_code     VARCHAR(16) NULL,
  n_tests       INT NOT NULL,
  PRIMARY KEY (determination, sample_date, care_line, area_code)
);

CREATE TABLE episode_mo_determination (
  mo_code       VARCHAR(24) NOT NULL,
  determination VARCHAR(32) NOT NULL,
  PRIMARY KEY (mo_code, determination)
);
```

Positivity for a stream is then positives over the summed tests of all determinations capable of detecting that organism, restricted to the stream's own strata. This is shown alongside counts in the triage view. Detection itself remains count-based, since a positivity-based detector would need its own baseline model, but an assessor seeing a count rise with flat positivity is looking at test volume, which is the commonest false alarm in the system.

### 5.7.2 Pathogen constants

Superseded by `episode_pathogen_config` in section 5.4.3, which carries the serial interval, incubation range, deduplication window, closure threshold, cool-down, `rt_applicable`, `mem_applicable` and severity weight in one table. There is no separate serial interval table.

### 5.8 Reports and users

```sql
CREATE TABLE episode_report_render (
  report_id   BIGINT AUTO_INCREMENT PRIMARY KEY,
  cluster_id   BIGINT NOT NULL,
  user_id     BIGINT NULL,                      -- NULL for cron pre-renders
  rendered_at DATETIME NOT NULL,
  file_path   VARCHAR(512) NOT NULL,
  file_sha256 CHAR(64) NOT NULL,
  params      JSON NOT NULL,
  case_ids    JSON NOT NULL,                    -- exact cases included
  version_no  INT NOT NULL
);

CREATE TABLE episode_app_user (
  user_id       BIGINT AUTO_INCREMENT PRIMARY KEY,
  username      VARCHAR(64) NOT NULL UNIQUE,
  full_name     VARCHAR(128) NOT NULL,
  email         VARCHAR(128) NOT NULL,
  password_hash VARCHAR(255) NOT NULL,          -- sodium::password_store(), Argon2id
  role          ENUM('assessor','admin') NOT NULL,
  is_active     TINYINT(1) NOT NULL DEFAULT 1,
  must_change   TINYINT(1) NOT NULL DEFAULT 1,
  last_login_at DATETIME NULL,
  created_at    DATETIME NOT NULL
);
```

There is no `viewer` role and no viewer row. Read access is anonymous: the app opens read-only for anyone who reaches it, and a row in this table exists only for someone who classifies. See section 12.

Reports are versioned, never overwritten. What was handed to a microbiologist on a given morning must remain exactly recoverable, including which cases it contained.

---

## 6. Cluster reconciliation

Run per stream, after detection, inside the run transaction.

1. Collect today's detections for the stream across all detectors.
2. Merge detections from different detectors whose intervals overlap into a single candidate episode, recording `detector_agreement`.
3. For each candidate, find open clusters in the same stream whose `[first_day, last_day]` interval overlaps the candidate, or lies within `case_free_days` of it.
   - **No match**: create a new cluster.
   - **One match**: update it. Extend the interval, recount cases, recompute score. If the signal already carries an assessment and its interval or case count changed, set `changed_since_assessment = 1`.
   - **Multiple matches**: merge. The oldest signal survives; the others receive `merged_into`, retain their assessment history, and disappear from the queue. Merging is never destructive.
4. Open clusters in the stream with no candidate this run get `runs_since_detected + 1`.
5. Auto-closure applies only to clusters with no assessment at all, or assessed as `artefact` or `expected_variation`. When `runs_since_detected` exceeds `close_after_runs` (default 14) and no assessment exists, close with `expected_variation` and a system-authored rationale. Anything classified `cluster_not_yet` or above is never auto-closed; it is flagged as no longer detected and left to a person. See section 6.5.

Because the anchor is sample date, late-arriving cases legitimately change signals retrospectively. Detection therefore re-runs over a rolling window rather than only on today, with the window width taken from the triangle's empirical completion curve.

### 6.1 Derived state

State is computed, never chosen, and never stored on the cluster row. An assessor records a classification and a rationale; the state follows from the classification, the case-free clock and whether the cluster has changed since it was last assessed.

| State | Condition |
|---|---|
| Nieuw | No assessment event exists |
| In beoordeling | An event exists but no classification yet, or the cluster is snoozed |
| Monitoring | Classified as possible, confirmed or ongoing epidemic, case-free threshold not reached |
| Af te sluiten | Classified as an epidemic and the closure criterion for that stream has fired. A prompt only; closure remains available at any time |
| Afgesloten | Classified as artefact or normal variation; or any other classification explicitly closed |
| Herbeoordeling nodig | Classified, but the underlying data changed since that assessment |

Closure is an act rather than a verdict, so it does not become a seventh classification: the classification records what the cluster was, and closure records that it is over.

Closure is always available to an assessor on any classified cluster. Af te sluiten is a prompt, not a gate. The non-epidemic classifications close themselves; an epidemic closes when a person says so, whether or not any criterion has fired.

### 6.2 Classification vocabulary

| # | Classification | Terminal | Meaning |
|---|---|---|---|
| 1 | Artefact | yes | Detection error or registration effect |
| 2 | Normale variatie | yes | Within expectation for the season |
| 3 | Cluster, nog geen epidemie | no | Real clustering, watch it, no outbreak measures |
| 4 | Mogelijke epidemie | no | Follow-up and source investigation started |
| 5 | Bevestigde epidemie | no | Outbreak control started, notification duty assessed |

Two changes from earlier drafts. The terminal boundary sits between 2 and 3, not between 3 and 4: an assessor choosing "real cluster, not an epidemic" almost always means "not yet", and closing it at that moment removes it from view exactly when watching matters. Only artefacts and normal variation close themselves.

The former "Lopende epidemie" is gone. A confirmed epidemic that is still running is a confirmed epidemic in the Monitoring state, which the derived state already expresses. Carrying it as a separate classification duplicated the state machine and made the archive harder to count.

The distinction between 3 and 4 is not descriptive but operational: 4 triggers source investigation, 3 does not. That is what the hint text on each option says, because a distinction the interface does not state will be applied inconsistently across four people.

### 6.3 Which criterion prompts closure

The prompt is per stream, because the criterion differs by the kind of epidemic.

| Stream | Closure criterion |
|---|---|
| L1 and L2 clusters | Case-free interval: `case_free_days`, or two maximum incubation periods for a confirmed epidemic |
| Point-source clusters | Case-free interval |
| Seasonal streams flagged `mem_applicable` | Counts falling back below the MEM post-epidemic threshold |
| Everything else | Case-free interval |

A seasonal influenza epidemic at L4 or L5 runs for months and will never satisfy a fourteen-day case-free rule while still being over in every meaningful sense. Applying the ward criterion to it would leave it open until spring. The MEM post-epidemic threshold is the instrument built for exactly this question, which is a second reason to run MEM beyond detection.

### 6.4 State trajectory

Every transition is written to its own table, so a cluster's history is queryable rather than recomputed.

```sql
CREATE TABLE episode_cluster_state (
  state_id   BIGINT AUTO_INCREMENT PRIMARY KEY,
  cluster_id  BIGINT NOT NULL,
  state      ENUM('new','assessing','monitoring','closable','closed','reassess') NOT NULL,
  entered_at DATETIME NOT NULL,
  left_at    DATETIME NULL,
  trigger    ENUM('detection','assessment','case_free','new_case','closure','system') NOT NULL,
  event_id   BIGINT NULL,      -- the assessment event that caused it, if any
  user_id    BIGINT NULL
);
```

This is what makes the trajectory renderable as a dated band on the dossier, and it supplies the timeliness figures on the Prestatie screen directly rather than by reconstruction from event timestamps.

### 6.5 Closure and cool-down

Auto-closure on `runs_since_detected` is too crude on its own. The epidemiological criterion is a case-free interval: no new case for `case_free_days` (14 or 21 depending on organism), and for clusters classified as an epidemic, two maximum incubation periods.

Every open cluster therefore carries a visible countdown: days since the last case against the threshold that would let it be declared over. This converts closure from a decision that is perpetually deferred into a prompt with a date on it. The countdown resets whenever a late case arrives with a sample date inside the window, which is the correct behaviour and another reason the reporting triangle matters.

After closure a cool-down applies: `cooldown_days` during which the same stream does not raise a new cluster, so that the tail of a resolved outbreak does not immediately reopen as a fresh one.

The cool-down needs an escape hatch, or it becomes a blind spot. If a stream closed as artefact or normal variation subsequently exceeds the excess on which that judgement was made by a material margin (a ratio increase of half again, or crossing a higher threshold), the closed cluster reopens as Herbeoordeling nodig rather than being suppressed. Without this rule, classifying a rise as normal variation on Monday would hide a genuine escalation on Wednesday for the length of the cool-down, which is the worst failure the system could produce.

`changed_since_assessment` drives a visible diff and a re-confirmation prompt in the interface. Silent mutation of an assessed signal is the fastest way to lose the assessors' trust.

---

## 7. Lattice and suppression

Streams are enumerated automatically from the data. Enumeration happens per run, so a newly appearing pathogen creates its streams without configuration.

Levels are numbered ascending with aggregation, matching the multilevel modelling convention in which level 1 is the finest unit. Escalation therefore reads as an increase: a cluster that spreads from an institution to an area has become an L3.

| Level | Dutch | Unit | Denominator |
|---|---|---|---|
| L1 | Afdeling | Ward or specialism | Ward patient-days if obtainable |
| L2 | Instelling | Hospital or care home | Patient-days |
| L3 | Gebied | Contiguous PC4 grouping | PC4 population |
| L4 | Provincie | Groningen, Fryslân, Drenthe | Provincial population |
| L5 | Regio | Full Certe catchment, Noord-Nederland | Catchment population or tests |

Each level is a strict partition of the one above it, which is what makes suppression meaningful: afdeling within instelling within gebied within provincie within regio.

**Care line is not a level.** First and second line do not nest inside a province or an area; they cut across every rung. Care line is therefore a filter applicable at any level, not a rung of its own, and the earlier `pathogen_careline` level is dropped.

**Two cautions on the labels.** "Regio" here means the whole catchment, not a sub-provincial area, which is why L3 is Gebied. And the letter must stay L rather than anything resembling *fase*: SO-ZI/AMR already assigns an ascending fase 0 to 5 to outbreak severity, with fase 4 triggering notification to the IGJ, and colleagues will otherwise conflate the two.

L2 is tractable precisely because Certe performs the full microbiology for all eight regional hospitals. The denominator is complete, the catchment is unambiguous, and years of baseline exist per institution. A norovirus rise in one hospital is a first-class cluster at its own level, not something inferred from a scoring component.

Suppression is computed after reconciliation:

- A child cluster (the lower number) suppresses its parent when it accounts for at least 70% of the parent's excess. The rise is local; showing the wider view adds nothing.
- A parent suppresses its children when no child reaches 50% and two or more children are flagged. The rise is diffuse; showing five separate area clusters is noise.
- The surviving cluster carries the suppressed ones as attached context, always visible on the detail screen, never discarded.

One nesting caveat: an institution draws patients across area and provincial boundaries, so L2 does not sit cleanly inside a single L3. Suppression between L2 and L3 is therefore computed on shared cases rather than on geography.

### 7.1 Normalisation at L1 and L2

Institution streams use `farringtonFlexible()` with a population offset of patient-days rather than raw counts. This matters more than it might appear: occupancy varies seasonally and with capacity changes, so a count-based baseline will alarm on a busy February and stay silent through a quiet August when transmission per patient-day is identical.

Implementation note requiring verification: `surveillance::farringtonFlexible()` accepts `populationOffset` in its control list and reads the `population` slot of the `sts` object. Whether `certestats::detect_farrington()` currently exposes that argument, and whether `linelist2sts()` populates the population slot, both need checking against the source. If not, this is a small local wrapper or a modest upstream addition to `certestats`.

The signal detail should present both the count curve and the incidence-density curve. Clinicians think in cases; infection prevention thinks in cases per 1,000 patient-days; the assessment needs both in view.

### 7.2 Institutions without their own streams

Long-term care institutions are numerous and individually sparse, so they do not receive streams. They are covered by a rule-based detector needing no baseline at all: *n* or more cases of the same organism, at the same institution, within *k* days. Defaults of n = 3 and k = 14, tightened per organism for the ones that matter (*C. difficile*, MRSA, VRE, carbapenemase producers, norovirus, *Legionella*).

Inside hospitals the rule runs on ward rather than institution, which is the real transmission unit and the level at which infection prevention acts. It runs as `detector = 'same_place'` and reconciles into the same signal table as everything else. It finds outbreaks Farrington structurally cannot, because a care home with three cases in a fortnight has no baseline to exceed. It should also run alongside L2 for the hospitals, where it will occasionally fire first: three cases in four days can satisfy the rule before a weekly aggregation has produced a point for Farrington to test.

---

### 7.3 Seasonal pathogens

Farrington answers whether counts exceed expectation. For influenza and RSV the question is different and closer to the statutory duty: has the epidemic started. The Moving Epidemic Method supplies pre-epidemic, epidemic and post-epidemic thresholds plus intensity levels from historical seasons, and runs as `detector = 'mem'` for organisms flagged `mem_applicable`.

### 7.4 Where detection is configured

All detection parameters live in the instance configuration read by the cron: thresholds, baseline lengths, aggregation, MEM seasons, eligibility gates, `same_place` rules. None of it is editable from the app.

**The configuration is not part of the software and never enters the repository.** The repository is a piece of software that anyone may clone; the configuration is operational data belonging to whoever runs it. Committing thresholds would mean a public commit for every operational change, repository write access for whoever operates the system, and a fork arriving with somebody else's parameters. The package ships `inst/config/default.yaml` as documented defaults, which is software. What an instance actually uses is not.

Where the instance configuration lives is the operator's decision. EpiSODIC locates it through a single environment variable, `EPISODE_CONFIG`, and cares about nothing else. A network drive, a SharePoint project folder, a local directory beside the cron script: all equivalent to the software.

Reproducibility is achieved in the database rather than in version control. Every run writes `config_hash` and `config_snapshot`, so the exact parameters behind any result are recoverable from `episode_detection_run` alone, whatever happened to the file afterwards. This is stronger than a commit reference, because it records the resolved configuration rather than a document that has to be reconstructed.

The Streams screen displays the configuration from the most recent run's snapshot, not from the file, so what an assessor sees is what actually ran.

### 7.5 Instance layout

The instance is a folder containing the cron script, the configuration and the reference tables. It is not versioned by EpiSODIC and its shape is up to the operator.

```
<instance>/
  cron.R                  operator's own scheduling entry point
  config.yaml             detection parameters, paths, thresholds
  pathogen_config.csv     operator's overrides of the shipped defaults
  institutions.csv        local institution mapping
  reports/                rendered outbreak reports
```

At Certe this folder sits in the SharePoint project directory alongside the existing detection script. Elsewhere it may be anywhere at all.

**One warning that follows.** The SQLite database must not live in a synchronising folder. SharePoint and OneDrive sync will corrupt or fork a live SQLite file, and the failure is silent until it is not. Configuration in SharePoint is fine, since it is small, read-only at runtime and rarely changed. The database belongs on the local disk of the host running the app and the cron, with its dated backup copies written wherever the operator likes, including SharePoint.

### 7.6 Baseline feedback into detection

Periods classified as a confirmed epidemic are excluded from the baseline of subsequent runs for that stream.

`farringtonFlexible()` downweights past aberrations statistically, but a human verdict is better evidence than a residual. Without this exclusion, last winter's outbreak silently raises this winter's threshold and the system becomes progressively less able to detect recurrence of exactly the events it was built to find. No off-the-shelf tool does this, because no off-the-shelf tool has the verdicts.

The exclusion is derived from `episode_assessment_event`, so it updates automatically when a verdict is revised, and the excluded windows are listed on the Streams screen so that a baseline is never quietly different from what an assessor expects.

---

---

## 8. Volume control

Stricter thresholds are the wrong instrument, since they discard exactly the small early signals the system exists to find. The queue is controlled by a layered gate and by ranking.

1. **Eligibility gate, before statistics.** A stream needs sufficient baseline history, a minimum median weekly count, and non-zero counts in a reasonable share of baseline weeks. Streams failing this fall through to EARS C2 or to the rare-pathogen path.
2. **Rare-but-serious path.** A single case of *N. meningitidis*, diphtheria or a carbapenemase producer is notable on count, not on aberration statistics. These use a curated trigger list. Running them through Farrington produces only silence.
3. **Effect-size floor.** Both statistical alarm and meaningful excess are required: observed minus upperbound of at least 3 cases, or observed over expected above 1.5. This removes most "7 against 6.2 expected" noise.
4. **Automatic ageing-out.** Section 6, step 5.
5. **Priority ranking, not exclusion.** Assessors work top down.
6. **Mute with reason.** Section 5.2.

FDR correction across streams is deliberately rejected as a primary control. It makes today's alarm set depend on the whole batch, so a signal can appear or vanish because of unrelated pathogens, which is indefensible to a microbiologist and corrupts the audit trail.

### 8.1 Priority score

Weights configurable in `default.yaml`, defaults given.

```
score = 100 * weighted_mean(
  excess_component    = rescale(log1p(observed - upperbound)),        w = 0.25
  ratio_component     = rescale(min(observed / expected, 5)),         w = 0.15
  severity_component  = episode_stream.severity_weight,               w = 0.20
  growth_component    = slope of last 3 aggregation periods,          w = 0.15
  agreement_component = detector_agreement / n_detectors,             w = 0.10
  density_component   = incidence per 1,000 patient-days vs baseline, w = 0.10
  spatial_component   = concentration across PC4 (Gini or top-1),     w = 0.05
)
```

Weights sum to 1.00. The density component applies only where a patient-day denominator exists, so for L3 to L5 streams the remaining weights renormalise rather than the score being depressed. Without that, hospital signals would systematically outrank community ones for a purely structural reason.

Severity weights derive from a curated table: Wpg notification group, AMR relevance, outbreak propensity. That table ships with the package with sensible defaults and is overridable per instance.

### 8.2 Denominator drift

The commonest false alarm is rising test volume, not rising incidence. Section 5.7.1 defines the denominator model. Positivity is shown beside counts on the rail and in the dossier, and a large count rise with flat positivity should be dispositionable as `artefact` in seconds.

---

## 9. Epidemiological outputs

Per cluster, computed in the cron and cached, rendered by the app and the report.

- **Epi curve.** Counts by sample date at the stream's aggregation period, with the Farrington threshold overlaid and the incomplete recent window shaded, width from the triangle. The shading is mandatory, not optional: without it a colleague reads a falling tail as a receding outbreak when it is only unreported specimens.
- **Rt.** `EpiEstim` on sample-date incidence, using the pathogen's serial interval from `episode_pathogen_config`, sliding weekly windows, credible intervals shown, estimates within the truncated window withheld rather than captioned. Suppressed entirely where `rt_applicable` is false. This is the time-varying effective reproduction number, not R0, and the report should say so.
- **Demography.** Age-sex pyramid for the cluster against the stream's historical baseline, so that a shift in affected age group is visible rather than merely the count.
- **Geography.** PC4 choropleth via `certegis`, cluster cases against baseline expectation, with small-count suppression configurable for reports leaving the department. A second panel breaks the cluster down by institution, which for second-line and long-term care is usually more informative than the map.
- **Curve shape.** Whether all cases fall within a single maximum incubation period distinguishes a point source from propagated transmission, and that is the first question in any outbreak investigation because it redirects the enquiry from person-to-person spread towards a common exposure. Derived from the case date distribution against `incub_max_days` and stated in the Duiding, with an intermediate verdict where the evidence is ambiguous.
- **Case-free countdown.** Days since the last case against the organism's closure threshold, shown on every open cluster.
- **Unique patients against isolates.** Both counts, always, so that a cluster inflated by repeat sampling is visible at a glance.
- **Line list.** Sortable table, exportable, restricted to the stored fields: patient key, sample date, sex, age, PC4, care line, institution, ward, specialism. Hidden entirely for anonymous viewers, along with its export.
- **Detector internals.** The `sts` object, thresholds, parameters, and the raw detector output. This tab exists so an assessor can distinguish a real rise from a modelling artefact, which is the single most common assessment question.

---

## 10. Interface

Shiny with `bslib`, packaged as `run_app()`, styled with an organisation-configurable colour palette (a shipped, organisation-neutral default, overridable per instance).

**This is a dossier, not a triage queue.** Earlier drafts optimised for a ten-minute morning routine through a long ranked list, on the assumption that detecting across all pathogens would produce high daily volume. At five to ten assessed clusters a month that assumption is wrong. Each cluster earns a full page with generous space, several plots and written interpretation. The list is a narrow rail, not the product.

**Rail.** Open clusters only; anything reaching Afgesloten leaves immediately for the archive. Per entry: organism, level, place, case count, observed over expected, derived state, classification if any, and a changed flag. A count and a link to the archive sit at the top.

**Dossier.** Header with organism, level, place, derived state and classification. Then the figures strip, the status trajectory band, the Duiding, and the analytical panels in order: epidemic curve, multi-year trend, denominator and positivity, age and sex, geography, institution or ward, phenotypic resistance profile, comparable earlier clusters, line list, detection settings.

**Assessment rail.** Fixed on the right, never scrolling out of view: the timeline of what was decided and by whom, then the classification control. Classification must never require scrolling.

**Status strip.** Last run status and reporting-triangle completeness, always visible, because a silently failed cron is the main systemic risk and must never be something one has to go looking for.

**Streams.** Read-only. The lattice, the mute registry with expiry, and the configuration as recorded in the latest run's `config_snapshot`, not as currently on disk.

**Runs.** Per-host run history, durations, row counts, errors, package and code versions, configuration hash.

**Archive, Activiteit, Prestatie.** Separate views reached from the top navigation. Archive holds closed clusters, searchable by organism and place, because last winter's assessment is the best prior for this winter's cluster.

### 10.1 Screens

**Vergelijkbare clusters.** A panel on the dossier surfacing the three most similar assessed clusters from the archive, matched on organism, level, size, season and duration, each showing what was decided and why. At roughly ten clusters a month there will be two hundred assessed precedents within two years, which makes the archive a decision aid rather than a filing cabinet.

**Zoeken.** Free search across current and archived clusters, and in particular every cluster ever raised for a given organism. "Show me all *Klebsiella* clusters since 2024" is the question that gets asked in a medical board meeting, and it should not require a database client.

**Handmatig cluster.** A signed-in user can raise a cluster the algorithms did not, and run an ad-hoc check of a suspicion against the data. A microbiologist will telephone about four VRE isolates, and if there is nowhere for that to land the assessment happens outside the system and the record is incomplete. The same screen answers "why did this not fire", which is the question that most erodes trust when it cannot be answered.

**Prestatie.** Positive predictive value per detector per organism computed from the stored verdicts, time from first case to detection, and time from detection to first assessment. This is how tuning decisions get justified, how the eligibility gate gets calibrated towards the target of roughly ten clusters a month, and it is publishable in its own right.

### 10.2 Interaction

- Keyboard shortcuts for moving between clusters and setting a classification. Useful, not load-bearing at this volume.
- No locking. The app appends assessment events and never overwrites, so two people assessing the same cluster on the same morning produce two entries in the timeline rather than one silently lost one.
- Autosave of in-progress rationale text, so an app restart does not discard typing.
- Snooze as a first-class action, distinct from both mute and close.
- Assessments rendered as an append-only timeline, never overwritten.

---

## 11. Reporting

A parameterised Quarto template rendered to self-contained HTML. The cron pre-renders for every cluster with a verdict of `possible_epidemic` or above; assessors can re-render on demand, producing a new version.

Files are written to an authorised folder and registered in `episode_report_render`. Send the file, not a link into the app: the medical staff have neither R nor an account, and a static artefact is also the defensible record of what was communicated on that date. Distribution through SharePoint or Teams is licensed and available.

---

## 12. Authentication

**Read access is anonymous.** The app opens read-only for anyone who reaches it, with one exception: the line list panel and its export are hidden until someone signs in. It is the only panel carrying patient-level rows, so the split needs no partial masking anywhere else.

Four accounts, hashed with `sodium::password_store()` and checked with `sodium::password_verify()`. No lockout, no backoff, no TLS requirement: access already requires being in the building on a Certe thin client.

The login exists to attribute assessments, not to defend against attackers. Since a single R process serves all sessions, `Sys.info()[["user"]]` returns the host account rather than the assessor, so the login is the only available identity source. Every assessment event carries `user_id`.

The audit trail, by contrast, is not negotiable, and its justification is scientific rather than defensive: when a confirmed outbreak is reviewed afterwards by the medical board, the GGD or a reviewer, the question of who judged it an epidemic, when, and on what grounds must be answerable. Hence the append-only `episode_assessment_event` table.

---

## 13. Internationalisation

Flat JSON per language under `inst/i18n/`, dotted keys rather than English source strings as keys, so Dutch wording can be revised without touching code.

```json
{ "rail.title": "Openstaande clusters", "dossier.priority": "Prioriteit" }
```

`tr("queue.title")` with fallback chain: instance override, then session language, then `en`, then the key itself rendered visibly so missing translations are obvious in testing rather than silently blank. Language is a user attribute, defaulting to `nl`.

---

## 14. Retention

Cases not linked to any cluster with verdict `possible_epidemic` or above are purged after a configurable horizon, retaining the aggregate counts on `episode_cluster` and the full assessment record. Retention is a first-class scheduled function with a dry-run mode and a log, not a manual cleanup script.

---

## 15. Demo mode

Synthetic data, no Diver, no credentials, launched with a single call. This determines whether anyone outside Certe ever adopts the project. The synthetic generator should produce a realistic seasonal baseline plus injected outbreaks, so that the detectors visibly fire on first run.

---

## 16. Testing

- Reconciliation is the highest-risk component and needs property-based tests: extension, split, merge, backfill, rerun idempotence, out-of-order runs.
- Golden-file tests on the `certestats` wrappers, so an upstream default change surfaces as a failing test rather than as a year of artefactual alarms.
- A synthetic outbreak library with known ground truth for measuring sensitivity and timeliness.

---

## 17. Adoption order for a new instance

Reconciliation is the load-bearing component: schema, ingestion, the lattice, the detectors, and reconciliation itself must be correct before anything built on top of them (the read-only dossier, assessment workflow, reporting, or the Prestatie screen's calibration tools) can be trusted. Running the interface for several weeks against real signal before turning on classification workflows is advisable: the priority score and gate thresholds will need recalibration against real volume (see QUESTIONS.md), and that is far easier before colleagues have formed expectations of the tool.

---

## 18. Open decisions

### Considered and rejected

- **Co-circulation panel.** What else rose in the same week. Not now.
- **National context.** RIVM data is not reliably machine-retrievable and NIVEL figures are pathogen-bound and scraped by hand. Not automatable at acceptable cost.
- **Teams or webhook notification.** Not how the team works.
- **MySQL or any client-server database.** Superseded once one Shiny process was settled as serving all four assessors. See section 5.
- **Analyst role.** Dropped; the data analysts are shown the system rather than given a distinct permission set.
- **Optimistic locking and row versioning.** Unnecessary once the app became insert-only.
- **A daily triage queue interface.** Superseded by the dossier once monthly volume was known. See section 10.
- **Suppressing Rt below a case threshold.** Rejected: wide credible intervals are themselves informative and explainable to the audience, so intervals are shown rather than estimates withheld.

### Still open

Resolved since draft 2: hospitals as a lattice level, ward and specialism, deduplication per episode, baseline feedback, closure criterion, MEM, performance metrics, SQLite as the only backend, configuration outside the repository.

Resolved since draft 1: package name (EpiSODIC), table prefix (`episode_`), postcode granularity (PC4), institution and care line availability, denominator model including patient-days, hospitals as a first-class lattice level, per-pathogen serial intervals.

1. **Serial interval values.** Which literature estimates to ship, and for which organisms. This is the one item requiring genuine epidemiological curation rather than a decision, and it can be built incrementally: ship `rt_applicable = 0` for everything, then populate the organisms that matter as sources are agreed.
2. **Severity weight table.** Source and curation: Wpg notification groups, ECDC priority lists, or local judgement. Affects ranking only, so a rough first pass is acceptable.
3. **`same_place` thresholds per organism.** Defaults of n = 3 within 14 days, tightened for *C. difficile*, MRSA, VRE, carbapenemase producers, norovirus and *Legionella*. Needs a first pass from real data.
4. **Retention horizon.** Months before unlinked cases are purged.
5. **Baseline artefact check.** Before trusting any baseline, plot the proportion of records where afnamedatum equals ontvangstdatum across the full historic series. A step change from a LIS migration or a changed request form will shift sample dates systematically, and Farrington will faithfully flag it as an aberration for a whole year afterwards. This is a half-hour query that could invalidate every baseline in the system, and it should be done before M1.
6. **Institution type mapping.** Whether Diver carries a usable type field, or whether hospital, GP practice and long-term care must be derived from identifier ranges.
7. **Patient-day cadence.** Hospitals typically supply activity monthly. If so, weekly detection needs interpolation onto the aggregation grid, and the interpolation method should be stated rather than implicit.
8. **Ward-level denominators.** Ward and specialism are available. Whether patient-days are obtainable per ward, or only per hospital, decides whether L1 can ever be more than rule-based.
9. **Which machine runs the app long term.** The author's personal AVD is not a managed service: no patching window, no backup, no monitoring, and it disappears with the author. A small IT-managed VM or a designated persistent session host is the durable answer. Worth asking IT, not worth waiting for. An AVD RemoteApp was considered and is not it: publishing a browser locked to a URL is a bookmark, and starting R per user would restore the concurrency problem SQLite removed.
10. **Upstream fixes to `certestats`.** The stray `print(n_cl)` debug call in `print.farrington_clusters()`, and exposure of `populationOffset` plus the `sts` population slot needed for patient-day normalisation at L1 and L2.
