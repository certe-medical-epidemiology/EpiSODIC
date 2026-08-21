# Create a fresh EpiSODIC database

Creates a new, empty database at `path` and applies the canonical schema
shipped as `inst/sql/schema.sql`. Always builds from scratch; refuses to
run against a database that already contains tables so that it cannot
silently clobber existing data.

## Usage

``` r
episodic_db_create(path, overwrite = FALSE)
```

## Arguments

- path:

  Path to a SQLite file, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md))
  pointing at an empty MariaDB/MySQL database. For SQLite, must not
  already exist, or must be an empty/non-EpiSODIC file.

- overwrite:

  If `TRUE`, delete an existing SQLite file (or drop all tables in an
  existing MariaDB/MySQL database) first.

## Value

(Invisibly) an open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
to the new database. The caller is responsible for disconnecting it.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
