# Open the EpiSODIC dashboard

Launches the Shiny dashboard against a database already populated by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md):
an overview of monitored surveillance streams and each cluster's
dossier, with charts, its narrative interpretation, and an assessment
form. Anyone can browse the dashboard; signing in is only required to
record an assessment or classify a cluster.

## Usage

``` r
episodic_run_app(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  ...
)
```

## Arguments

- db_path:

  Path to the EpiSODIC database: a SQLite file, or a MariaDB/MySQL DSN
  (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

- lang:

  Dashboard language, fixed for the whole running app - there is no
  in-app language switcher. One of `"en"`, `"ar"`, `"nl"`, `"fr"`,
  `"de"`, `"hi"`, `"zh"`, or `"es"`. Defaults to the `EPISODIC_LANGUAGE`
  environment variable, falling back to `"en"` if that is unset.

- ...:

  Passed on to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `port` or `host`.

## Value

Invisible; called for its side effect of starting the app. This call
blocks until the app is stopped.

## Details

If you just want to explore EpiSODIC without setting anything up first,
use
[`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
instead - it populates a database with synthetic data and calls this
function for you.

## Examples

``` r
if (FALSE) { # \dontrun{
# opens a blocking, interactive dashboard session; see episodic_demo()
# for a one-call version that also creates a populated demo database
episodic_run_app("/path/to/episodic.sqlite")
} # }
```
