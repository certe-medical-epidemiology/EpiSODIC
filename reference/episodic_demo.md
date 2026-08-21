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
  ingest_source = episodic_ingest_source_synthetic,
  denominator_source = episodic_denominator_source_synthetic
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

- ingest_source, denominator_source:

  The data sources to generate the demo from, passed on to
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md).
  Default to several years of synthetic data; pass a narrower date range
  (see
  [`episodic_ingest_source_synthetic()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_source_synthetic.md))
  for a quicker demo.

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
cases <- episodic_ingest_source_synthetic(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
db_path <- episodic_demo(launch = FALSE, ingest_source = cases, denominator_source = NULL)
#> EpiSODIC demo account - username: demo, password: episodic-demo
file.remove(db_path)
#> [1] TRUE
# }
```
