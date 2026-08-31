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
  cases,
  denominators = NULL,
  institution_activity = NULL,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
  db_path = Sys.getenv("EPISODIC_DB"),
  host = Sys.info()[["nodename"]],
  account = Sys.info()[["user"]],
  run_date = Sys.Date(),
  debug = FALSE
)
```

## Arguments

- cases:

  Your laboratory data: a data frame or `tibble` in the shape
  [episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  describes, or a zero-argument function that returns one. Defaults to
  the bundled synthetic generator, useful for demos and testing but not
  real surveillance.

- denominators:

  Optional: your testing-volume data, in the same form as `cases` -
  normally a data set, a function if it has to be produced at run time
  (see
  [`episodic_synthetic_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_denominators.md)
  for the expected shape). Leave as `NULL` (the default) if you have
  none to supply - positivity panels simply stay blank.

- institution_activity:

  Optional: your hospital patient-days data (see
  [`episodic_synthetic_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_institution_activity.md)
  for the expected shape), normally as a data set, or as a function
  taking the current institutions table. Leave as `NULL` (the default)
  if you have none - detection falls back to raw case counts.

- episodic_config_path:

  Passed to
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md).

- db_path:

  Path to the EpiSODIC database: a SQLite file (created automatically if
  it does not exist yet) or a MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).

- host, account:

  Recorded with the run for audit purposes; default to the current
  machine and account.

- run_date:

  The date to treat as "today". Defaults to the system date; mainly
  useful to override in tests.

- debug:

  If `TRUE`, print a lot more than the phase-by-phase progress this
  function always writes:
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html), the
  versions of every package a fatal (non-catchable) crash is most likely
  to originate in, memory snapshots, per-stream detail inside the
  detection loop, and - for the calls implicated so far in a known
  MariaDB-only crash (`episodic_app_density()`, the population-vector
  lookup, the trend/detection writes, the assessment-event lookups, and
  `episodic_spatial_concentration()`'s own input) - the exact SQL and
  every bound parameter's value, class and encoding immediately before
  each such call runs, not only once it returns. Meant for chasing
  exactly the kind of failure that leaves no R-level error behind at
  all - a crashed session, a run that silently never returns - where the
  normal progress trace does not narrow things down enough on its own.
  Noisy; leave off for routine scheduled runs.

## Value

Invisibly, the `run_id` of the completed run. The run's row in
`episodic_detection_run` holds its status, the per-feed load counts, and
`error_text` if it failed. Case data that does not satisfy the contract
throws instead of returning - the run row is still written, with
`status = "failed"` and the same message in `error_text`.

## Details

EpiSODIC never connects to your laboratory system directly. You extract
and transform your own data beforehand, and hand it over as a plain data
frame or `tibble`: `cases` for the laboratory results themselves (see
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for the required columns and their allowed values), and optionally
`denominators` and `institution_activity` for testing volume and
hospital activity. A data set is the normal case; if producing the data
only makes sense at run time (a live database query, for instance), a
zero-argument function returning one is accepted just as well - see
[`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md).

The exact detection settings used are recorded with the run (see
[`episodic_config_hash()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_hash.md)),
so any past result can always be traced back to the configuration that
produced it.

So is what each feed delivered. Before the run writes anything, your
case data goes through
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md).
Structural problems - a missing column, a value outside the allowed set,
a date that does not read as a date - stop the run with an error naming
every offending column, its values and the rows they are in, and are
recorded on the run as well, so the reason is visible both where the run
was started and in the dashboard's activity screen. Advisory findings
are mentioned once and the run proceeds. Run
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
on your extract yourself to see all of it without starting a run at all.
A run that fails later, for any other reason, records the reason and
warns rather than returning quietly. Rows that are merely unmatched are
counted rather than dropped in silence: institution activity whose
`institution_key` matches no known institution is skipped with a
warning, its count recorded, and the run finishes `"partial"` instead of
`"success"`. Both are complete runs the dashboard reads from;
`"partial"` says go and look at why rows were skipped.
`episodic_detection_run` carries the counts (`n_cases_supplied`,
`n_cases_inserted`, `n_activity_skipped`, and the rest).

## Check your data before you run anything

Do not find out from an empty dashboard. Run
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
on your extract - it needs no database, changes nothing, and reports
every problem it finds at once, with the rows and values involved and
what to do about each:

    cases <- my_extract_and_transform_function()
    episodic_check_cases(cases)

It also reports what is merely worth a look: one pathogen spelled two
ways (two streams instead of one), no `ward` on any hospital row (no
ward-level detection), a `patient_key` that never repeats (no
deduplication), postcodes the map cannot place, sample dates in the
future.
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
runs the same checks but throws, for use in a script;
`episodic_run_cron()` runs it for you before every run, and refuses to
start on data it cannot use, naming what to fix.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
to see what EpiSODIC makes of your extract before you schedule anything,
and
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for the contract it checks against.

## Examples

``` r
# \donttest{
db_path <- tempfile(fileext = ".sqlite")
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
run_id <- episodic_run_cron(db_path = db_path, cases = cases)
#> 2026-08-31 14:42:00.485 | episodic_run_cron() starting (host=runnervmgx7h7, account=runner)
#> 2026-08-31 14:42:00.486 | Resolving configuration
#> 2026-08-31 14:42:00.491 | Configuration resolved (hash f198adfd3d37)
#> 2026-08-31 14:42:00.491 | Connecting to database
#> 2026-08-31 14:42:00.491 | No existing database found - creating one
#> 2026-08-31 14:42:00.508 | Database connected (dialect: sqlite)
#> 2026-08-31 14:42:00.509 | Run 1 started
#> 2026-08-31 14:42:00.509 | Resolving and checking case data
#> 2026-08-31 14:42:00.522 | Case data checked: 419 rows, 0 problems, 0 advisory finding(s)
#> 2026-08-31 14:42:00.522 | Beginning transaction
#> 2026-08-31 14:42:00.523 | Loading pathogen configuration
#> 2026-08-31 14:42:00.525 | Pathogen configuration loaded (23 pathogen(s))
#> 2026-08-31 14:42:00.525 | Loading case data into the database
#> 2026-08-31 14:42:00.635 | Case data loaded: supplied=419, deduplicated=410, inserted=410
#> 2026-08-31 14:42:00.635 | Fetching all known cases and institutions
#> 2026-08-31 14:42:00.637 | Enumerating lattice streams
#> 2026-08-31 14:42:00.673 | Running same-place detector
#> 2026-08-31 14:42:00.744 | Same-place detector found 4 detection(s)
#> 2026-08-31 14:42:00.745 | Running rare-trigger detector
#> 2026-08-31 14:42:00.748 | Rare-trigger detector found 1 detection(s)
#> 2026-08-31 14:42:00.749 | Farrington owes 8 week(s) this run
#> 2026-08-31 14:42:00.751 | Reconciling 393 stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)
#> 2026-08-31 14:42:01.336 | Stream reconciliation done: 5 detection(s), 5 new signal(s), 0 updated signal(s)
#> 2026-08-31 14:42:01.336 | Suppressing lattice
#> 2026-08-31 14:42:01.341 | Committing transaction
#> 2026-08-31 14:42:01.342 | Finishing run 1 (status: success)
#> 2026-08-31 14:42:01.344 | episodic_run_cron() finished in 0.9s (status: success)
file.remove(db_path)
#> [1] TRUE
# }
```
