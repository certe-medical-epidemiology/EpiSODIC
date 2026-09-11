# Bring an Existing Database up to the Current Schema

EpiSODIC records which version of its schema a database was built with,
and refuses to open one it does not recognise rather than failing later
with an unexplained SQL error (or, worse, not failing and reading a
column that has since come to mean something else). When you upgrade the
package and its schema has moved on, this is what brings your existing
database along - in one transaction per step, so a failed migration
leaves the database exactly as it was.

## Usage

``` r
episodic_db_migrate(db_path = Sys.getenv("EPISODIC_DB", unset = NA))
```

## Arguments

- db_path:

  Path to an existing SQLite database, or a MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

## Value

Invisibly, the schema version the database is at afterwards.

## Details

Nothing is ever dropped or rewritten: migrations add tables, add
columns, and backfill values. Your surveillance history, your
assessments and your audit trail are carried forward untouched. Take a
backup first anyway - that advice does not stop being good because the
code is careful.

Each step runs inside a transaction, which under SQLite covers the
schema changes themselves. MariaDB and MySQL commit implicitly on every
`CREATE`/`ALTER`, so there a failed step can leave its schema change in
place with no version row recorded; every migration is written to skip
what it finds already done, so simply running this again is the correct
response.

A database created by EpiSODIC 0.12.x or earlier carries no version at
all, since the version table postdates it. Such a database is adopted at
version 1 (which is the shape it already has) rather than refused.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)

# already current: reports so and changes nothing
episodic_db_migrate(db_path)
#> Database is at schema version 5.

file.remove(db_path)
#> [1] TRUE
```
