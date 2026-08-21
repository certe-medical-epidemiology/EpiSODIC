# Connect using the `EPISODIC_DB` environment variable

Like
[`episodic_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_connect.md),
but falls back to the `EPISODIC_DB` environment variable when you don't
pass a path explicitly - handy for one-off console use, e.g.
[`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md)
uses it internally so provisioning an account needs only a username and
password, not a connection you build yourself first.

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
you are responsible for disconnecting it.

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
