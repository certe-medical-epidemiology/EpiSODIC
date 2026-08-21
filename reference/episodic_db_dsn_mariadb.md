# Build a MariaDB/MySQL DSN for `EPISODIC_DB`

`EPISODIC_DB` accepts either a filesystem path (a SQLite database, the
default) or a `mysql://user:password@host:port/dbname` DSN pointing at a
MariaDB or MySQL server instead - every function that takes `db_path`
(or falls back to `EPISODIC_DB`) dispatches on which of the two it was
given. This helper builds that DSN string from its parts and URL-encodes
`user`/`password`, so credentials containing `:`, `@` or `/` do not
break the DSN. `episodic_db_dsn_mysql()` is an alias for the same
function - the DSN and everything downstream of it is identical either
way, so use whichever name matches the server you actually run.

## Usage

``` r
episodic_db_dsn_mariadb(host, dbname, user, password, port = 3306L)

episodic_db_dsn_mysql(host, dbname, user, password, port = 3306L)
```

## Arguments

- host:

  Server hostname or IP address.

- dbname:

  Database (schema) name.

- user, password:

  Credentials.

- port:

  TCP port. Defaults to `3306`.

## Value

A single string, ready to pass as `db_path` or to
`Sys.setenv(EPISODIC_DB = ...)`.

## Examples

``` r
episodic_db_dsn_mariadb(
  host = "db.internal", dbname = "episodic",
  user = "episodic_app", password = "s3cr3t!"
)
#> [1] "mysql://episodic_app:s3cr3t%21@db.internal:3306/episodic"
```
