# Run One Surveillance Detection Cycle

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
  backfill = NULL,
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
  taking the current institutions table. Its `institution_key` is the
  same identifier your case data uses - EpiSODIC hashes both on load, so
  a key taken from the institutions table this passes a function is
  already hashed and will match nothing. Leave as `NULL` (the default)
  if you have none - detection falls back to raw case counts.

- episodic_config_path:

  The config path.

- db_path:

  Path to the EpiSODIC database: a SQLite file (created automatically if
  it does not exist yet) or a MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).

- host, account:

  Recorded with the run for audit purposes; default to the current
  machine and account.

- run_date:

  The date to treat as "today". Defaults to the system date. Every
  detector anchors on it: `same_place` and `rare_trigger` report only
  hits inside their `lookback_days` of it, and Farrington tests only
  weeks that are complete on or before it. Detection against a
  historical extract is therefore one run per date with a `run_date`
  inside the extract's own window, not one run dated today holding the
  whole archive - a run dated today reports what is current, and the
  archive is not.
  [`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)
  replays exactly that way.

- backfill:

  Whether this run reports the whole case history it is given rather
  than only what falls inside the detectors' lookback windows. `NULL`,
  the default, decides from the database: a run with no completed run
  behind it is a backfill, so a first import of years of history arrives
  as years of clusters (the settled ones closed in that same run by
  `reconciliation.stale_open_days`, and so straight into the Archive)
  rather than as an empty dashboard. Every run after it is bounded
  again, so nothing is ever reported twice. Pass `FALSE` to bound the
  first run too, which is what a prospective replay wants:
  [`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)
  measures detection delay, and a first replay run that reported its
  whole baseline at once would measure the import instead. `TRUE` forces
  it on, for a database whose history was imported by something other
  than a run.

- debug:

  If `TRUE`, print a good deal more than the phase-by-phase progress
  this function always writes:
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html), the
  versions of every package whose own behaviour a run depends on, memory
  snapshots at the start and end, and per-stream detail inside the
  detection loop - which stream, how many cases, whether it cleared the
  eligibility gate, what each detector did with it. Useful when a run's
  *results* are not what an operator expects and the question is which
  stream, or which phase, they diverged at. Noisy; leave off for routine
  scheduled runs.

## Value

Invisibly, the `run_id` of the completed run. The run's row in
`episodic_detection_run` holds its status, the per-feed load counts, and
`error_text` if it failed. Case data that does not satisfy the
requirements throws instead of returning - the run row is still written,
with `status = "failed"` and the same message in `error_text`.

## Details

EpiSODIC never connects to your laboratory system directly. You extract
and transform your own data beforehand, and hand it over as a plain data
frame or `tibble`: `cases` for the laboratory results themselves (see
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for the required columns and their allowed values), and optionally
`denominators` and `institution_activity` for testing volume and
hospital activity. A data set is the normal case; if producing the data
only makes sense at run time (a live database query, for instance), a
zero-argument function returning one is accepted just as well.

The exact detection settings used are recorded with the run, so any past
result can always be traced back to the configuration that produced it.

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
future. `episodic_check_cases(..., stop_on_problem = TRUE)` runs the
same checks but throws an error, for use in a script.
`episodic_run_cron()` runs that before every run, and refuses to start
on data it cannot use, naming what to fix.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
to see what EpiSODIC makes of your extract before you schedule anything,
and
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for the requirements it checks against.

## Examples

``` r
# \donttest{
db_path <- tempfile(fileext = ".sqlite")
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
run_id <- episodic_run_cron(db_path = db_path, cases = cases)
#> 2026-09-12 21:50:43.145 | episodic_run_cron() starting (host=runnervmlun5p, account=runner)
#> 2026-09-12 21:50:43.145 | Resolving configuration
#> 2026-09-12 21:50:43.150 | Configuration resolved (hash 3caee38bc163)
#> 2026-09-12 21:50:43.151 | Connecting to database
#> 2026-09-12 21:50:43.151 | No existing database found - creating one
#> 2026-09-12 21:50:43.174 | Database connected (dialect: sqlite)
#> 2026-09-12 21:50:43.176 | Run 1 started
#> 2026-09-12 21:50:43.176 | Resolving and checking case data
#> 2026-09-12 21:50:43.189 | Case data checked: 419 rows, 0 problems, 0 advisory finding(s)
#> 2026-09-12 21:50:43.189 | Beginning transaction
#> 2026-09-12 21:50:43.190 | Loading pathogen configuration
#> 2026-09-12 21:50:43.192 | Pathogen configuration loaded (23 pathogen(s))
#> 2026-09-12 21:50:43.193 | Loading case data into the database
#> 2026-09-12 21:50:43.293 | Case data loaded: supplied=419, deduplicated=410, inserted=410
#> 2026-09-12 21:50:43.294 | Fetching all known cases and institutions
#> 2026-09-12 21:50:43.296 | First run on this database: reporting the whole case history rather than only what falls inside the detectors' lookback windows. Clusters whose last case is more than 60 day(s) before this run's date close in this same run and go straight to the Archive; the rest open for assessment. Every run after this one is bounded again, so nothing here is reported twice.
#> 2026-09-12 21:50:43.298 | Case history on file spans 2025-01-01 to 2025-03-31, ending 530 day(s) before this run's date (2026-09-12)
#> 2026-09-12 21:50:43.299 | Enumerating lattice streams
#> ! 2026-09-12 21:50:43.328 | no province could be resolved for any of the 106 postcode values in this run - province-level (L4) detection has nothing to run on. EPISODIC_PC_PROVINCE_MAP is unset, and there is no built-in rule to fall back on - deriving a province from a postcode is country-specific. Point it at your own pc/province_code CSV, or leave it unset and the province level stays empty.
#> 2026-09-12 21:50:43.335 | Running same-place detector
#> 2026-09-12 21:50:43.417 | Same-place detector found 6 detection(s)
#> 2026-09-12 21:50:43.417 | Running rare-trigger detector
#> 2026-09-12 21:50:43.422 | Rare-trigger detector found 1 detection(s)
#> 2026-09-12 21:50:43.422 | Farrington tests every week its streams can carry this run, rather than the 8-week catch-up cap: there is no earlier run to catch up to
#> 2026-09-12 21:50:43.424 | Reconciling 368 stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)
#> ! 2026-09-12 21:50:43.928 | MEM: declined stream Bordetella pertussis/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.933 | MEM: declined stream Campylobacter/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.938 | MEM: declined stream Clostridioides difficile/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.942 | MEM: declined stream Giardia lamblia/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.948 | MEM: declined stream Influenza A/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.953 | MEM: declined stream MRSA/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.958 | MEM: declined stream Neisseria meningitidis/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.963 | MEM: declined stream Norovirus/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.968 | MEM: declined stream RSV/pathogen_region, insufficient history for climatology
#> ! 2026-09-12 21:50:43.973 | MEM: declined stream Salmonella/pathogen_region, insufficient history for climatology
#> 2026-09-12 21:50:44.020 | Stream reconciliation done: 7 detection(s), 7 new signal(s), 0 updated signal(s)
#> 2026-09-12 21:50:44.021 | Backfill: 7 cluster(s) opened from the case history, 7 of them already closed by the system and in the Archive, 0 left open for assessment
#> 2026-09-12 21:50:44.021 | Suppressing lattice
#> 2026-09-12 21:50:44.026 | Committing transaction
#> 2026-09-12 21:50:44.029 | Finishing run 1 (status: success)
#> 2026-09-12 21:50:44.031 | episodic_run_cron() finished in 0.9s (status: success)
file.remove(db_path)
#> [1] TRUE
# }
```
