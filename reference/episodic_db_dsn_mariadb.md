# Connect EpiSODIC to a MariaDB or MySQL Server

EpiSODIC stores its data in either a SQLite file (the default, and all
you need for a single-server deployment) or a MariaDB/MySQL database.
This function builds the connection string ("DSN") for the latter, so
you never have to hand-assemble one or worry about special characters in
your password breaking it. Use the resulting string as `db_path`
anywhere EpiSODIC expects one, or store it in the `EPISODIC_DB`
environment variable. `episodic_db_dsn_mysql()` is an identical alias -
use whichever name matches the server you run.

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
