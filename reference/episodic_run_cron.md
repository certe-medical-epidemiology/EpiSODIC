# Run one detection cycle

The cron entry point. Ingests, enumerates streams, detects, reconciles
and persists, all inside one transaction, so a partially failed run
leaves no partial state and a retry is always safe.

## Usage

``` r
episodic_run_cron(
  db_path,
  ingest_source_fn = episodic_ingest_source_synthetic,
  denominator_source_fn = NULL,
  institution_activity_source_fn = NULL,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
  host = Sys.info()[["nodename"]],
  account = Sys.info()[["user"]],
  run_date = Sys.Date()
)
```

## Arguments

- db_path:

  Path to a SQLite database file, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Created if it does not exist yet (SQLite) or is an empty database
  (MariaDB/MySQL).

- ingest_source_fn:

  A zero-argument function returning a data frame satisfying the
  ingestion interface, or that data frame itself (an operator who has
  already extracted and transformed their data has no reason to wrap it
  in a function just to satisfy this parameter - see
  [`episodic_resolve_source()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_source.md)).
  Defaults to the bundled synthetic generator, the only ingestion source
  shipped in this package.

- denominator_source_fn:

  An optional zero-argument function returning the pre-aggregated
  positivity metadata data frame (`pathogen`, `sample_date`,
  `care_line`, `area_code`, `n_tests`), that data frame itself, or
  `NULL` (the default) if the operator has none to supply.

- institution_activity_source_fn:

  An optional one-argument function (`institutions`, the current
  `episodic_db_institutions()` data frame, so a real implementation can
  key its own hospital system's export by the same institutions this run
  already knows about) returning weekly patient-days (`institution_key`,
  `period_start`, `period_end`, `patient_days`); that data frame itself
  (the `institutions` argument is simply not passed in that case); or
  `NULL` (the default) if the operator has none to supply. Powers L2
  patient-day normalisation; without it, L1/L2 Farrington detection uses
  raw counts, unnormalised.

- episodic_config_path:

  Passed to
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md).

- host, account:

  Recorded on `episodic_detection_run`; default to the process's own
  host/account. This is not an identity source for assessors, only for
  the run record.

- run_date:

  The date to treat as "today" for closure/eligibility calculations;
  defaults to the system date, injectable for tests.

## Value

Invisibly, the `run_id` of the completed run.

## Details

Configuration is read from `EPISODIC_CONFIG`; the resolved
configuration's hash and full snapshot are written to
`episodic_detection_run` so that any result is explainable from the
database alone.

`certedb`/Diver access is deliberately never called from inside this
package: `get_diver_data()` is the operator's own step, run before
`episodic_run_cron()`, transforming Diver's columns into
`episodic_ingest_columns`. See `README.md` for the raw data contract.

## Examples

``` r
# \donttest{
db_path <- tempfile(fileext = ".sqlite")
run_id <- episodic_run_cron(
  db_path,
  ingest_source_fn = function() episodic_ingest_source_synthetic(
    start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
  )
)
file.remove(db_path)
#> [1] TRUE
# }
```
