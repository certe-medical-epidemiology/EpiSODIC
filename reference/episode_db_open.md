# Open a connection to `db_path`, or the `EPISODE_DB` environment variable

A thin wrapper around
[`episode_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_db_connect.md)
for entry points that take `db_path` rather than an already-open
connection (e.g.
[`episode_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_provision_user.md)) -
centralises the `EPISODE_DB` default and the "neither was given" error
in one place, rather than duplicating the resolve-then-connect logic in
every such entry point.

## Usage

``` r
episode_db_open(db_path = Sys.getenv("EPISODE_DB", unset = NA))
```

## Arguments

- db_path:

  Path to an existing SQLite database. Defaults to the `EPISODE_DB`
  environment variable.

## Value

An open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html);
the caller is responsible for disconnecting it.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episode_db_create(db_path)
DBI::dbDisconnect(con)
Sys.setenv(EPISODE_DB = db_path)
con <- episode_db_open()
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
