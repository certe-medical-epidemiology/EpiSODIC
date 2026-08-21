# Run the EpiSODIC Shiny app

Read-only for anonymous visitors: signing in is only required to
classify a cluster. Serves the cluster dossier and the Streams overview
against a database already populated by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md).

## Usage

``` r
episodic_run_app(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  lang = "nl",
  ...
)
```

## Arguments

- db_path:

  Path to a SQLite database, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

- lang:

  Default session language, `"nl"` (default) or `"en"`.

- ...:

  Passed on to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port`, `host`).

## Value

Invisible; called for its side effect of starting the app.

## Examples

``` r
if (FALSE) { # \dontrun{
# opens a blocking, interactive Shiny session - see episodic_demo() for a
# one-call, populated version of this same example
episodic_run_app("/path/to/episodic.sqlite")
} # }
```
