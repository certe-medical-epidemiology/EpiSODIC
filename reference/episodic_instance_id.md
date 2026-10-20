# This Instance's Identifier

Every EpiSODIC database carries an identifier of its own: ten
hexadecimal characters, generated once from a cryptographic random
source when the database is created (or brought up to schema version 15)
and never changed afterwards. It is what an export file names its sender
by, and what an importing instance prefixes the sender's `source_key`,
`patient_key` and `lab_number` with, so that two laboratories' keys can
never be mistaken for one another. Give it to a partner together with
your public keys; the Info screen of the dashboard shows it too.

## Usage

``` r
episodic_instance_id(db_path = Sys.getenv("EPISODIC_DB", unset = NA))
```

## Arguments

- db_path:

  Path to an existing SQLite database, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

## Value

A single string of ten lowercase hexadecimal characters.

## See also

[`vignette("import-export")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.md)

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
DBI::dbDisconnect(episodic_db_create(db_path))
episodic_instance_id(db_path)
#> [1] "abeb3997fb"
file.remove(db_path)
#> [1] TRUE
```
