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
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  cases = episodic_synthetic_cases,
  denominators = episodic_synthetic_denominators
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
  and pass it here for a quicker demo.

## Value

Invisibly, `db_path`.

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
