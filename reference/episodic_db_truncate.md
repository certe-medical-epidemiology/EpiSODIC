# Empty every EpiSODIC table, keeping the schema itself

A hard reset back to "freshly created, no data" - every row in every
EpiSODIC table is deleted, but the tables, indexes and constraints stay
exactly as
[`episodic_db_create()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_create.md)
built them, so the database is immediately ready for a new
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
without a schema migration. This includes `episodic_app_user`: dashboard
accounts are data too, and are deleted along with everything else -
there is nothing this function leaves behind to sign in with afterwards.

## Usage

``` r
episodic_db_truncate(path)
```

## Arguments

- path:

  Path to an existing SQLite file, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md))
  pointing at an existing MariaDB/MySQL database.

## Value

Invisibly, the character vector of tables that were truncated -
`character(0)` if you cancelled, or if the database had no EpiSODIC
tables to begin with.

## Details

Deliberately hard to trigger by accident:

- Interactive only:

  Refuses outright under
  [`interactive()`](https://rdrr.io/r/base/interactive.html) `FALSE` - a
  script, a cron job, or any other unattended context can never reach
  the confirmation prompt, so it can never reach the deletion either.

- Typed confirmation:

  Prints exactly what is about to be deleted (every table, and its row
  count) and then requires you to type the database's own name back at a
  prompt. A bare yes/no is answerable on autopilot; typing the name back
  is not.

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_db_truncate(db_path)
} # }
```
