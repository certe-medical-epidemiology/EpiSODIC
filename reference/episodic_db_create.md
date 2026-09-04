# Set Up a New EpiSODIC Database

Run this once, when setting up a new EpiSODIC instance: it creates a new
database at `path` and builds all the required tables. Refuses to run
against a database that already has tables in it, so it cannot
accidentally overwrite existing surveillance data - use
[`episodic_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_connect.md)
to open a database you have already set up.

## Usage

``` r
episodic_db_create(path, overwrite = FALSE)
```

## Arguments

- path:

  Path to a SQLite file to create, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md))
  pointing at an empty MariaDB/MySQL database.

- overwrite:

  If `TRUE`, delete an existing SQLite file (or drop all tables in an
  existing MariaDB/MySQL database) first. Use with care - this destroys
  any data already there.

## Value

(Invisibly) an open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
to the new database. You are responsible for disconnecting it (with
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html))
when done.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
