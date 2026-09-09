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
episodic_db_connect(path, check_schema_version = TRUE)
```

## Arguments

- path:

  Path to an existing SQLite file, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md))
  pointing at an existing MariaDB/MySQL database.

- check_schema_version:

  Whether to refuse a database whose schema version is not the one this
  build of EpiSODIC expects, with an error naming
  [`episodic_db_migrate()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_migrate.md)
  as the fix. `TRUE` (the default) everywhere except inside
  [`episodic_db_migrate()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_migrate.md)
  itself, which by definition has to open a database that is out of
  date.

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
