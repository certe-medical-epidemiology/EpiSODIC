# Create a fresh EpiSODIC SQLite database

Creates a new SQLite database file at `path` and applies the canonical
schema shipped as `inst/sql/schema.sql`. Always builds from scratch;
refuses to run against a file that already contains EpiSODIC tables so
that it cannot silently clobber existing data.

## Usage

``` r
episode_db_create(path, overwrite = FALSE)
```

## Arguments

- path:

  Path to the SQLite file to create. Must not already exist, or must be
  an empty/non-EpiSODIC SQLite file.

- overwrite:

  If `TRUE`, delete an existing file at `path` first.

## Value

(Invisibly) an open
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
to the new database. The caller is responsible for disconnecting it.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episode_db_create(db_path)
DBI::dbDisconnect(con)
file.remove(db_path)
#> [1] TRUE
```
