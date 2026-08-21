# Open a connection to `db_path`, or the `EPISODIC_DB` environment variable

A thin wrapper around
[`episodic_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_connect.md)
for entry points that take `db_path` rather than an already-open
connection (e.g.
[`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md)) -
centralises the `EPISODIC_DB` default and the "neither was given" error
in one place, rather than duplicating the resolve-then-connect logic in
every such entry point.

## Usage

``` r
episodic_db_open(db_path = Sys.getenv("EPISODIC_DB", unset = NA))
```

## Arguments

- db_path:

  Path to an existing SQLite database, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

## Value

An open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html);
the caller is responsible for disconnecting it.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)
Sys.setenv(EPISODIC_DB = db_path)
con <- episodic_db_open()
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
