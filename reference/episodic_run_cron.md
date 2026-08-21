# Run one surveillance detection cycle

This is the function you schedule to run regularly (e.g. daily, via
cron): it pulls in new laboratory data, checks every monitored stream
for statistical aberrations with the configured detectors, reconciles
the results into cluster dossiers for the board to assess, and records
everything in the database. A run either completes in full or leaves no
trace at all - it runs inside a single database transaction, so a failed
run is always safe to simply retry.

## Usage

``` r
episodic_run_cron(
  db_path,
  ingest_source = episodic_ingest_source_synthetic,
  denominator_source = NULL,
  institution_activity_source = NULL,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
  host = Sys.info()[["nodename"]],
  account = Sys.info()[["user"]],
  run_date = Sys.Date()
)
```

## Arguments

- db_path:

  Path to the EpiSODIC database: a SQLite file (created automatically if
  it does not exist yet) or a MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).

- ingest_source:

  Your laboratory data: a data frame in the shape
  [episodic_ingest_columns](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_interface.md)
  describes, or a zero-argument function that returns one. Defaults to
  the bundled synthetic generator, useful for demos and testing but not
  real surveillance.

- denominator_source:

  Optional: your testing-volume data, in the same data-frame-or-function
  form as `ingest_source` (see
  [`episodic_denominator_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_denominator_source_synthetic.md)
  for the expected shape). Leave as `NULL` (the default) if you have
  none to supply - positivity panels simply stay blank.

- institution_activity_source:

  Optional: your hospital patient-days data (see
  [`episodic_synthetic_institution_activity_source()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_institution_activity_source.md)
  for the expected shape), either as a data frame or as a function
  taking the current institutions table. Leave as `NULL` (the default)
  if you have none - detection falls back to raw case counts.

- episodic_config_path:

  Passed to
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md).

- host, account:

  Recorded with the run for audit purposes; default to the current
  machine and account.

- run_date:

  The date to treat as "today". Defaults to the system date; mainly
  useful to override in tests.

## Value

Invisibly, the `run_id` of the completed run.

## Details

EpiSODIC never connects to your laboratory system directly. You extract
and transform your own data beforehand, and hand it over as a plain data
frame: `ingest_source` for case data (see
[episodic_ingest_columns](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_interface.md)
for the required shape), and optionally `denominator_source` and
`institution_activity_source` for testing volume and hospital activity.
If producing the data only makes sense at run time (a live database
query, for instance), pass a zero-argument function that returns it
instead - see
[`episodic_resolve_source()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_source.md).

The exact detection settings used are recorded with the run (see
[`episodic_config_hash()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_hash.md)),
so any past result can always be traced back to the configuration that
produced it.

## Examples

``` r
# \donttest{
db_path <- tempfile(fileext = ".sqlite")
cases <- episodic_ingest_source_synthetic(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
run_id <- episodic_run_cron(db_path, ingest_source = cases)
file.remove(db_path)
#> [1] TRUE
# }
```
