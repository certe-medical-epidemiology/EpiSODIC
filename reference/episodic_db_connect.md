# Connect to an Existing EpiSODIC Database

Opens a connection to a database you have already set up with
[`episodic_db_create()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_create.md),
with the settings EpiSODIC needs enabled (for SQLite: WAL journal mode,
a busy timeout, and foreign key enforcement). Remember to disconnect
with
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
when you are done.

## Usage

``` r
episodic_db_connect(path)
```

## Arguments

- path:

  Path to an existing SQLite file, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md))
  pointing at an existing MariaDB/MySQL database.

## Value

An open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)
con <- episodic_db_connect(db_path)
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
