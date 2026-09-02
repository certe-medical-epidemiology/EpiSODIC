# Try EpiSODIC with synthetic outbreak data

The fastest way to see what EpiSODIC does: this single call creates a
fresh database, generates several years of synthetic laboratory data,
runs detection over it, creates a demo epidemiologist account, and opens
the dashboard - all without needing access to any real laboratory system
or an instance configuration file. Everything used here is a shipped
default, so it works right after installing the package.

## Usage

``` r
episodic_demo(
  db_path = tempfile(fileext = ".sqlite"),
  username = "demo",
  full_name = "Demo User",
  email = "demo@example.org",
  password = "demo",
  launch = TRUE,
  run_date = episodic_synthetic_week_end(),
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  cases = function() episodic_synthetic_cases(end_date = run_date),
  denominators = function() episodic_synthetic_denominators(end_date = run_date)
)
```

## Arguments

- db_path:

  Path to the SQLite database to create. Defaults to a temporary file,
  so repeated calls never collide and nothing is left behind once the R
  session ends.

- username, full_name, email, password:

  Credentials for the demo epidemiologist account this creates, so you
  can sign in and classify a cluster right away. These are placeholder
  values - change them for anything beyond a local demo.

- launch:

  If `TRUE` (default), opens the dashboard afterwards (see
  [`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md));
  this call blocks until you close it. Set to `FALSE` to only build the
  demo database and return its path, e.g. for scripting or screenshots.

- run_date:

  The date to run detection as of. Defaults to the end of the last
  complete week, so the week the statistical detectors test is a full
  one however far into the week you happen to run the demo - which is
  how surveillance reads its own weeks anyway.

- lang:

  Dashboard language when `launch = TRUE`: `"en"`, `"ar"`, `"nl"`,
  `"fr"`, `"de"`, `"hi"`, `"zh"`, or `"es"`. Defaults to the
  `EPISODIC_LANGUAGE` environment variable, falling back to `"en"` if
  that is unset.

- cases, denominators:

  The data to generate the demo from - normally data frames (or
  tibbles), passed on unchanged to
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md).
  Default to several years of synthetic data; generate a narrower date
  range yourself (see
  [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md))
  and pass it here for a quicker demo. Trying the demo with your own
  extract is a good way to see EpiSODIC work end to end: `cases` is
  checked against the
  [episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
  contract first, so data it cannot use stops here with an explanation
  of what to fix, rather than opening a dashboard with nothing in it.
  Run
  [`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
  on your extract yourself to see the same findings, plus the advisory
  ones.

## Value

Invisibly, `db_path`.

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
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
runs it for you before every run, and refuses to start on data it cannot
use, naming what to fix.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
to see what EpiSODIC makes of your own extract first, and
[episodic_case_data](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_case_data.md)
for the shape it expects.

## Examples

``` r
if (FALSE) { # \dontrun{
# launches a blocking, interactive Shiny session against several years
# of freshly-generated synthetic data
episodic_demo()
} # }

# \donttest{
# non-interactive: populate a database and stop there, e.g. for scripting
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
db_path <- episodic_demo(launch = FALSE, cases = cases, denominators = NULL)
#> Creating synthetic cases...
#> 2026-09-02 12:03:04.674 | episodic_run_cron() starting (host=runnervmgx7h7, account=runner)
#> 2026-09-02 12:03:04.675 | Resolving configuration
#> 2026-09-02 12:03:04.679 | Configuration resolved (hash f198adfd3d37)
#> 2026-09-02 12:03:04.679 | Connecting to database
#> 2026-09-02 12:03:04.680 | No existing database found - creating one
#> 2026-09-02 12:03:04.695 | Database connected (dialect: sqlite)
#> 2026-09-02 12:03:04.697 | Run 1 started
#> 2026-09-02 12:03:04.697 | Resolving and checking case data
#> 2026-09-02 12:03:04.710 | Case data checked: 419 rows, 0 problems, 0 advisory finding(s)
#> 2026-09-02 12:03:04.710 | Beginning transaction
#> 2026-09-02 12:03:04.711 | Loading pathogen configuration
#> 2026-09-02 12:03:04.714 | Pathogen configuration loaded (23 pathogen(s))
#> 2026-09-02 12:03:04.719 | Loading case data into the database
#> 2026-09-02 12:03:04.807 | Case data loaded: supplied=419, deduplicated=410, inserted=410
#> 2026-09-02 12:03:04.808 | Fetching all known cases and institutions
#> 2026-09-02 12:03:04.810 | Enumerating lattice streams
#> 2026-09-02 12:03:04.850 | Running same-place detector
#> 2026-09-02 12:03:04.913 | Same-place detector found 4 detection(s)
#> 2026-09-02 12:03:04.914 | Running rare-trigger detector
#> 2026-09-02 12:03:04.917 | Rare-trigger detector found 1 detection(s)
#> 2026-09-02 12:03:04.918 | Farrington owes 8 week(s) this run
#> 2026-09-02 12:03:04.920 | Reconciling 393 stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)
#> 2026-09-02 12:03:05.576 | Stream reconciliation done: 5 detection(s), 5 new signal(s), 0 updated signal(s)
#> 2026-09-02 12:03:05.576 | Suppressing lattice
#> 2026-09-02 12:03:05.581 | Committing transaction
#> 2026-09-02 12:03:05.584 | Finishing run 1 (status: success)
#> 2026-09-02 12:03:06.755 | episodic_run_cron() finished in 2.1s (status: success)
#> OK
#> ===========================================================================
#> 
#>   EpiSODIC demo account (admin) - username: demo, password: demo
#> 
#> ===========================================================================
file.remove(db_path)
#> [1] TRUE
# }
```
