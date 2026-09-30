# Try EpiSODIC With Synthetic Outbreak Data

The fastest way to see what EpiSODIC does: this single call creates a
fresh database, generates several years of synthetic laboratory data,
runs detection over it, creates a demo epidemiologist account, and opens
the dashboard - all without needing access to any real laboratory system
or an instance configuration file. It needs nothing but the package: the
shipped defaults, plus the few files it writes beside the database (see
below).

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
  replay_days = 14L,
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

- replay_days:

  For the bundled synthetic data, how many of its last days are replayed
  as daily runs, each loading only the cases received by its date, after
  a first run over everything before them. A scheduled instance learns
  its reporting delays this way, one run at a time, and the delays are
  what the reporting completeness, the nowcast and the end-of-outbreak
  probability are computed from: with a single run there are none to
  learn from. Each replayed day adds a few seconds; `0` builds the demo
  in one run, without them. A `cases` extract you supply is always run
  once, as of `run_date`: its receipt dates are not necessarily the
  dates its cases were reported.

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

  If `TRUE`, an existing demo database at `db_path` and the three
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
Netherlands postcode geometry bundled with the package. A third,
`<name>-pathogens.csv`, gives the transmissible pathogens in the
synthetic data an offspring distribution (`end_r`, `end_k`), which ships
empty, so the outbreak dossier can show its end-of-outbreak probability.
Its values are illustrative, chosen for the demo, not estimates for any
setting.

They are left in place, so a database built with `launch = FALSE` can be
re-opened later with the same geography by setting `EPISODIC_DB`,
`EPISODIC_CONFIG`, `EPISODIC_PC_PROVINCE_MAP`,
`EPISODIC_PATHOGEN_CONFIG` and `EPISODIC_GEO_DATA` back to them - the
paths are printed when the demo finishes. Without them the database
still opens; it simply shows no map and no provinces.

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
#> Running detection...
#> 2026-09-30 10:06:15.843 | episodic_run_cron() starting (host=runnervmtr4k5, account=runner)
#> 2026-09-30 10:06:15.843 | Resolving configuration
#> 2026-09-30 10:06:15.851 | Configuration resolved (hash f374468968d9)
#> 2026-09-30 10:06:15.851 | Connecting to database
#> 2026-09-30 10:06:15.852 | No existing database found - creating one
#> 2026-09-30 10:06:15.881 | Database connected (SQLite 3.53.3)
#> 2026-09-30 10:06:15.883 | Run 1 started
#> 2026-09-30 10:06:15.883 | Resolving and checking case data
#> 2026-09-30 10:06:16.127 | Case data checked: 421 rows, 0 problems, 1 advisory finding(s)
#> episodic_check_cases() has 1 advisory finding about this case data, starting with: `pc` has no area in EPISODIC_GEO_DATA in 165 of 421 rows. Run episodic_check_cases() on it to see them all.
#> 2026-09-30 10:06:16.128 | Beginning transaction
#> 2026-09-30 10:06:16.130 | Loading pathogen configuration
#> 2026-09-30 10:06:16.133 | Pathogen configuration overlaid from /tmp/RtmpcD5Ya8/file1e953556ec4c-pathogens.csv
#> 2026-09-30 10:06:16.136 | Pathogen configuration loaded (39 pathogen(s))
#> 2026-09-30 10:06:16.136 | Loading case data into the database
#> 2026-09-30 10:06:16.515 | Case data loaded: supplied=421, deduplicated=403, inserted=403
#> 2026-09-30 10:06:16.515 | Fetching all known cases and institutions
#> 2026-09-30 10:06:16.518 | First run on this database: reporting the whole case history rather than only what falls inside the detectors' lookback windows. Clusters whose last case is more than 60 day(s) before this run's date close in this same run and go straight to the Archive; the rest open for assessment. Every run after this one is bounded again, so nothing here is reported twice.
#> 2026-09-30 10:06:16.520 | Case history on file spans 2025-01-01 to 2025-03-31, ending 0 day(s) before this run's date (2025-03-31)
#> 2026-09-30 10:06:16.521 | Enumerating lattice streams
#> 2026-09-30 10:06:16.567 | Running same-place detector
#> 2026-09-30 10:06:16.639 | Same-place detector found 5 detection(s)
#> 2026-09-30 10:06:16.640 | Running rare-trigger detector
#> 2026-09-30 10:06:16.645 | Rare-trigger detector found 1 detection(s)
#> 2026-09-30 10:06:16.646 | Farrington tests every week its streams can carry this run, rather than the 8-week catch-up cap: there is no earlier run to catch up to
#> 2026-09-30 10:06:16.648 | Reconciling 342 stream(s) (Farrington/MEM detection, triangle update, cluster reconciliation)
#> ! 2026-09-30 10:06:17.008 | MEM: declined stream Bordetella pertussis/pathogen_province/PROV_FRYSLAN, disabled by mem_mode
#> ! 2026-09-30 10:06:17.018 | MEM: declined stream Campylobacter/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-30 10:06:17.023 | MEM: declined stream Campylobacter/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.028 | MEM: declined stream Campylobacter/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.036 | MEM: declined stream Clostridioides difficile/pathogen_province/PROV_DRENTHE, disabled by mem_mode
#> ! 2026-09-30 10:06:17.041 | MEM: declined stream Clostridioides difficile/pathogen_province/PROV_FRYSLAN, disabled by mem_mode
#> ! 2026-09-30 10:06:17.045 | MEM: declined stream Clostridioides difficile/pathogen_province/PROV_GRONINGEN, disabled by mem_mode
#> ! 2026-09-30 10:06:17.054 | MEM: declined stream Giardia lamblia/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.059 | MEM: declined stream Giardia lamblia/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.068 | MEM: declined stream Influenza A/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-30 10:06:17.074 | MEM: declined stream Influenza A/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.080 | MEM: declined stream Influenza A/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.088 | MEM: declined stream MRSA/pathogen_province/PROV_DRENTHE, disabled by mem_mode
#> ! 2026-09-30 10:06:17.093 | MEM: declined stream MRSA/pathogen_province/PROV_GRONINGEN, disabled by mem_mode
#> ! 2026-09-30 10:06:17.101 | MEM: declined stream Neisseria meningitidis/pathogen_province/PROV_FRYSLAN, disabled by mem_mode
#> ! 2026-09-30 10:06:17.109 | MEM: declined stream Norovirus/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-30 10:06:17.115 | MEM: declined stream Norovirus/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.121 | MEM: declined stream Norovirus/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.130 | MEM: declined stream RSV/pathogen_province/PROV_DRENTHE, insufficient history for climatology
#> ! 2026-09-30 10:06:17.144 | MEM: declined stream RSV/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.150 | MEM: declined stream RSV/pathogen_province/PROV_GRONINGEN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.158 | MEM: declined stream Salmonella/pathogen_province/PROV_FRYSLAN, insufficient history for climatology
#> ! 2026-09-30 10:06:17.163 | MEM: declined stream Bordetella pertussis/pathogen_region/NORTHERN_NETHERLANDS, disabled by mem_mode
#> ! 2026-09-30 10:06:17.168 | MEM: declined stream Campylobacter/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-30 10:06:17.173 | MEM: declined stream Clostridioides difficile/pathogen_region/NORTHERN_NETHERLANDS, disabled by mem_mode
#> ! 2026-09-30 10:06:17.178 | MEM: declined stream Giardia lamblia/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-30 10:06:17.184 | MEM: declined stream Influenza A/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-30 10:06:17.189 | MEM: declined stream MRSA/pathogen_region/NORTHERN_NETHERLANDS, disabled by mem_mode
#> ! 2026-09-30 10:06:17.194 | MEM: declined stream Neisseria meningitidis/pathogen_region/NORTHERN_NETHERLANDS, disabled by mem_mode
#> ! 2026-09-30 10:06:17.199 | MEM: declined stream Norovirus/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-30 10:06:17.204 | MEM: declined stream RSV/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> ! 2026-09-30 10:06:17.209 | MEM: declined stream Salmonella/pathogen_region/NORTHERN_NETHERLANDS, insufficient history for climatology
#> 2026-09-30 10:06:17.240 | Stream loop time by stage: cases 0.1s, mem 0.2s, eligibility 0.0s, farrington 0.0s, trend 0.0s, detections 0.0s, reconciliation 0.0s, total 0.4s (342 stream(s))
#> 2026-09-30 10:06:17.240 | Stream reconciliation done: 6 detection(s), 6 new signal(s), 0 updated signal(s)
#> 2026-09-30 10:06:17.241 | Backfill: 6 cluster(s) opened from the case history, 1 of them already closed by the system and in the Archive, 5 left open for assessment
#> 2026-09-30 10:06:17.244 | Suppressing lattice: weighing 6 cluster(s) across 5 pathogen(s)
#> 2026-09-30 10:06:17.245 | Suppressing lattice: 42 case link(s) read for 6 cluster(s), 0 assessed cluster(s) exempt
#> 2026-09-30 10:06:17.252 | Suppressing lattice done: 0 cluster(s) suppressed (0 parent(s) behind a dominant child, 0 child(ren) behind a diffuse parent)
#> 2026-09-30 10:06:17.269 | Nowcast: 0 of 5 stream(s) with an open cluster nowcast, 5 without enough reported yet to nowcast from, 0 failed
#> 2026-09-30 10:06:17.270 | Epidemic direction: no open epidemic
#> 2026-09-30 10:06:17.272 | Epidemic outlook: no open epidemic
#> 2026-09-30 10:06:17.298 | Outbreak-end forecast: 0 of 4 open outbreak(s) of a transmissible pathogen computed, 1 point source, 3 without the parameters or reporting history, 0 failed
#> 2026-09-30 10:06:17.298 | Committing transaction
#> 2026-09-30 10:06:17.301 | Finishing run 1 (status: success)
#> 2026-09-30 10:06:18.647 | episodic_run_cron() finished in 2.8s (status: success)
#> OK
#> ===========================================================================
#> 
#>   EpiSODIC demo account (admin) - username: demo, password: demo
#> 
#>   To re-open this demo later, with its geography:
#>     Sys.setenv(EPISODIC_DB = "/tmp/RtmpcD5Ya8/file1e953556ec4c.sqlite",
#>                EPISODIC_CONFIG = "/tmp/RtmpcD5Ya8/file1e953556ec4c-config.yaml",
#>                EPISODIC_PC_PROVINCE_MAP = "/tmp/RtmpcD5Ya8/file1e953556ec4c-pc-province.csv",
#>                EPISODIC_PATHOGEN_CONFIG = "/tmp/RtmpcD5Ya8/file1e953556ec4c-pathogens.csv")
#>     episodic_run_app()
#> 
#> ===========================================================================
file.remove(db_path)
#> [1] TRUE
# }
```
