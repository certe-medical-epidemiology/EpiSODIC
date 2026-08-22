# Try EpiSODIC with synthetic outbreak data

The fastest way to see what EpiSODIC does: this single call creates a
fresh database, generates several years of synthetic laboratory data,
runs detection over it, creates a demo assessor account, and opens the
dashboard - all without needing access to any real laboratory system or
an instance configuration file. Everything used here is a shipped
default, so it works right after installing the package.

## Usage

``` r
episodic_demo(
  db_path = tempfile(fileext = ".sqlite"),
  username = "demo",
  full_name = "Demo User",
  email = "demo@example.org",
  password = "episodic-demo",
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

  Credentials for the demo assessor account this creates, so you can
  sign in and classify a cluster right away. These are placeholder
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

  Dashboard language when `launch = TRUE`: `"nl"`, `"en"`, `"es"`,
  `"fr"`, `"de"`, `"zh"`, `"hi"`, or `"ar"`. Defaults to the
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
#> EpiSODIC demo account - username: demo, password: episodic-demo
file.remove(db_path)
#> [1] TRUE
# }
```
