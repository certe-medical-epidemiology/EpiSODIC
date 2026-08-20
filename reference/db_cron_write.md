# Cron-side writers

The cron owns the facts and may upsert. These functions are the only
place in the package that write to `episode_stream`,
`episode_institution`, `episode_institution_activity`, `episode_case`,
`episode_reporting_triangle`, `episode_denominator`,
`episode_detection`, `episode_cluster`, `episode_cluster_case`,
`episode_detection_run` and (for pre-renders) `episode_report_render`.
See `R/db_app_write.R` for the insert-only counterparts.

## Arguments

- con:

  A
  [DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

- pathogen_config:

  A data frame matching `inst/config/pathogen_config.csv`.

- institution_id:

  An `episode_institution` id.

- period_start, period_end:

  Activity period bounds (dates).

- patient_days, admissions, n_beds:

  Activity counts, or `NA`.

- source:

  Free-text provenance, or `NA`.

- stream_key:

  A 40-character `stream_key`.

- level:

  One of the five lattice levels.

- pathogen:

  The raw lab-provided pathogen string, used verbatim as the stream's
  identity; no taxonomy is resolved against it.

- care_line:

  One of `"first"`, `"second"`, `"other"`, `"unknown"`, or `NA`.

- region_code:

  A region code, or `NA`.

- pc:

  A PC postcode, or `NA`.

- ward:

  A ward, for `pathogen_ward` streams, or `NA`.

- denominator:

  One of `"none"`, `"tests"`, `"population"`, `"patient_days"`.

- severity_weight:

  A severity weight, 0-1.

- observed_date:

  The date this stream was observed on, updates
  `first_seen`/`last_seen`.

- cases:

  A data frame of cases to insert (`episode_ingest_run()`'s deduplicated
  batch).

- run_id:

  A `run_id`.

- stream_id:

  A `stream_id`.

- sample_date, run_date:

  Reporting-triangle dates.

- n_cases:

  A case count.

- n_tests:

  A test count, for the optional positivity metadata table.

- area_code:

  An area code, for the optional positivity metadata table.

- detector:

  One of the detector enum values.

- first_day, last_day:

  A detection or cluster interval.

- expected, upperbound:

  Statistical detector output, or `NA`.

- params_json:

  JSON-serialised detector attributes.

- cluster_id:

  A `cluster_id`, or `NA`.

- detection_id:

  A `detection_id`.

- excess, ratio:

  Cluster statistics, or `NA`.

- priority_score:

  A priority score, 0-100.

- detector_agreement:

  Count of distinct detectors that fired.

- changed_since_assessment:

  Logical, or `NULL` to leave unchanged.

- merged_into:

  The surviving `cluster_id` this cluster merged into.

- case_id:

  A `case_id`.

- host, account:

  Recorded on `episode_detection_run`.

- attempt_no:

  The run's attempt number.

- status:

  One of `episode_detection_run.status`.

- n_streams, n_detections, n_signals_new, n_signals_updated:

  Run summary counts.

- code_version:

  The installed EpiSODIC version.

- pkg_versions:

  JSON-serialised package versions.

- config_hash, config_snapshot:

  The resolved configuration's hash and snapshot.

- error_text:

  Error text for a failed run, or `NA`.
