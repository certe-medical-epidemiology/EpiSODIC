# Try EpiSODIC With Synthetic Outbreak Data

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
  run_date = NULL,
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  cases = NULL,
  denominators = NULL,
  overwrite = FALSE,
  ...
)
```

## Arguments

- db_path:

  Path to the SQLite database to create. Defaults to a temporary file,
  so repeated calls never collide and nothing is left behind once the R
  session ends. A database that already exists is refused, and a
  MariaDB/MySQL DSN is refused outright: this call generates synthetic
  cases and runs detection over them, which is contamination anywhere
  but a throwaway database.

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

  The date to run detection as of. Left `NULL` (the default) it is
  chosen for you, and which way depends on whose data this is: for the
  bundled synthetic data, the end of the last complete week, so the week
  the statistical detectors test is a full one however far into the week
  you happen to run the demo; for a `cases` extract you supply yourself,
  the last day that extract covers, and the demo says so when it does
  it.

  That second rule is the demo's alone. Detection is bounded to a
  lookback window around `run_date`, so a run dated today against an
  extract from last year correctly finds nothing - which is right for a
  scheduled run and useless for someone trying the system out on a
  historical export.
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  therefore keeps dating its runs from the system date, as a real
  surveillance run must.

- lang:

  Dashboard language when `launch = TRUE`: `"en"`, `"ar"`, `"nl"`,
  `"fr"`, `"de"`, `"hi"`, `"zh"`, or `"es"`, or a regional variant of
  one (`"en-US"`, `"es-419"`). Defaults to the `EPISODIC_LANGUAGE`
  environment variable, falling back to `"en"` if that is unset.

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
  requirements first, so data it cannot use stops here with an
  explanation of what to fix, rather than opening a dashboard with
  nothing in it. Run
  [`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
  on your extract yourself to see the same findings, plus the advisory
  ones.

- overwrite:

  If `TRUE`, an existing demo database at `db_path` and the two
  configuration files beside it are deleted and rebuilt. `FALSE` (the
  default) refuses instead. This deletes whatever is at that path, so it
  is deliberately not something the demo decides for you.

- ...:

  Arguments passed on to
  [`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md).

## Value

Invisibly, `db_path`.

## What the demo writes beside the database

EpiSODIC has no built-in geography - no default map, and no built-in
rule for turning a postcode into a province - because a default for
either would be a default for one country. The demo therefore configures
its own, exactly the way a real deployment does: two files named after
`db_path` (`<name>-config.yaml` and `<name>-pc-province.csv`), plus the
Netherlands postcode geometry bundled with the package.

They are left in place, so a database built with `launch = FALSE` can be
re-opened later with the same geography by setting `EPISODIC_DB`,
`EPISODIC_CONFIG`, `EPISODIC_PC_PROVINCE_MAP` and `EPISODIC_GEO_DATA`
back to them - the paths are printed when the demo finishes. Without
them the database still opens; it simply shows no map and no provinces.

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
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
runs that before every run, and refuses to start on data it cannot use,
naming what to fix.

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
#> Running detection as of 2025-03-31, the last day your case data covers.
#> Creating synthetic cases...
#> 2026-09-26 13:23:05.865 | episodic_run_cron() starting (host=runnervmtr4k5, account=runner)
#> 2026-09-26 13:23:05.865 | Resolving configuration
#> 2026-09-26 13:23:05.872 | Configuration resolved (hash eec18d51a9ba)
#> 2026-09-26 13:23:05.872 | Connecting to database
#> 2026-09-26 13:23:05.873 | No existing database found - creating one
#> 2026-09-26 13:23:05.899 | Database connected (SQLite 3.53.3)
#> 2026-09-26 13:23:05.900 | Run 1 started
#> 2026-09-26 13:23:05.900 | Resolving and checking case data
#> 2026-09-26 13:23:05.916 | Case data checked: 419 rows, 0 problems, 0 advisory finding(s)
#> 2026-09-26 13:23:05.916 | Beginning transaction
#> 2026-09-26 13:23:05.918 | Loading pathogen configuration
#> 2026-09-26 13:23:05.921 | Pathogen configuration loaded (23 pathogen(s))
#> 2026-09-26 13:23:05.921 | Loading case data into the database
#> 2026-09-26 13:23:06.039 | Case data loaded: supplied=419, deduplicated=410, inserted=410
#> 2026-09-26 13:23:06.040 | Fetching all known cases and institutions
#> 2026-09-26 13:23:06.043 | First run on this database: reporting the whole case history rather than only what falls inside the detectors' lookback windows. Clusters whose last case is more than 60 day(s) before this run's date close in this same run and go straight to the Archive; the rest open for assessment. Every run after this one is bounded again, so nothing here is reported twice.
#> 2026-09-26 13:23:06.045 | Case history on file spans 2025-01-01 to 2025-03-31, ending 0 day(s) before this run's date (2025-03-31)
#> 2026-09-26 13:23:06.046 | Enumerating lattice streams
#> 2026-09-26 13:23:06.088 | Running same-place detector
#> 2026-09-26 13:23:06.172 | Same-place detector found 6 detection(s)
#> 2026-09-26 13:23:06.173 | Running rare-trigger detector
#> 2026-09-26 13:23:06.179 | Rare-trigger detector found 1 detection(s)
#> 2026-09-26 13:23:06.179 | Farrington tests every week its streams can carry this run, rather than the 8-week catch-up cap: there is no earlier run to catch up to
#> 2026-09-26 13:23:06.181 | Reconciling 393 stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)
#> ! 2026-09-26 13:23:06.550 | MEM: declined stream Bordetella pertussis/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.559 | MEM: declined stream Campylobacter/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.563 | MEM: declined stream Campylobacter/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.568 | MEM: declined stream Campylobacter/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.576 | MEM: declined stream Clostridioides difficile/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.580 | MEM: declined stream Clostridioides difficile/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.585 | MEM: declined stream Clostridioides difficile/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.592 | MEM: declined stream Giardia lamblia/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.597 | MEM: declined stream Giardia lamblia/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.605 | MEM: declined stream Influenza A/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.610 | MEM: declined stream Influenza A/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.615 | MEM: declined stream Influenza A/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.623 | MEM: declined stream MRSA/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.628 | MEM: declined stream MRSA/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.632 | MEM: declined stream MRSA/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.640 | MEM: declined stream Neisseria meningitidis/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.648 | MEM: declined stream Norovirus/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.653 | MEM: declined stream Norovirus/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.658 | MEM: declined stream Norovirus/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.666 | MEM: declined stream RSV/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.670 | MEM: declined stream RSV/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.682 | MEM: declined stream RSV/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.690 | MEM: declined stream Salmonella/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-26 13:23:06.694 | MEM: declined stream Salmonella/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.698 | MEM: declined stream Salmonella/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-26 13:23:06.703 | MEM: declined stream Bordetella pertussis/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.708 | MEM: declined stream Campylobacter/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.713 | MEM: declined stream Clostridioides difficile/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.718 | MEM: declined stream Giardia lamblia/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.724 | MEM: declined stream Influenza A/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.729 | MEM: declined stream MRSA/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.734 | MEM: declined stream Neisseria meningitidis/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.739 | MEM: declined stream Norovirus/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.745 | MEM: declined stream RSV/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-26 13:23:06.750 | MEM: declined stream Salmonella/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> 2026-09-26 13:23:06.795 | Stream loop time by stage: cases 0.1s, mem 0.2s, eligibility 0.0s, farrington 0.0s, trend 0.0s, detections 0.0s, reconciliation 0.1s, total 0.4s (393 stream(s))
#> 2026-09-26 13:23:06.796 | Stream reconciliation done: 7 detection(s), 7 new signal(s), 0 updated signal(s)
#> 2026-09-26 13:23:06.797 | Backfill: 7 cluster(s) opened from the case history, 2 of them already closed by the system and in the Archive, 5 left open for assessment
#> 2026-09-26 13:23:06.799 | Suppressing lattice: weighing 7 cluster(s) across 4 pathogen(s)
#> 2026-09-26 13:23:06.801 | Suppressing lattice: 40 case link(s) read for 7 cluster(s), 0 assessed cluster(s) exempt
#> 2026-09-26 13:23:06.806 | Suppressing lattice done: 0 cluster(s) suppressed (0 parent(s) behind a dominant child, 0 child(ren) behind a diffuse parent)
#> 2026-09-26 13:23:06.806 | Committing transaction
#> 2026-09-26 13:23:06.810 | Finishing run 1 (status: success)
#> 2026-09-26 13:23:08.197 | episodic_run_cron() finished in 2.3s (status: success)
#> OK
#> ===========================================================================
#> 
#>   EpiSODIC demo account (admin) - username: demo, password: demo
#> 
#>   To re-open this demo later, with its geography:
#>     Sys.setenv(EPISODIC_DB = "/tmp/Rtmpc6XboC/file1e016c36a940.sqlite",
#>                EPISODIC_CONFIG = "/tmp/Rtmpc6XboC/file1e016c36a940-config.yaml",
#>                EPISODIC_PC_PROVINCE_MAP = "/tmp/Rtmpc6XboC/file1e016c36a940-pc-province.csv")
#>     episodic_run_app()
#> 
#> ===========================================================================
file.remove(db_path)
#> [1] TRUE
# }
```
