# Connect to an existing EpiSODIC SQLite database

Opens a connection with the pragmas required by the architecture (WAL
journal mode, a busy timeout, and foreign key enforcement).

## Usage

``` r
episode_db_connect(path)
```

## Arguments

- path:

  Path to an existing SQLite file.

## Value

An open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episode_db_create(db_path)
DBI::dbDisconnect(con)
con <- episode_db_connect(db_path)
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
