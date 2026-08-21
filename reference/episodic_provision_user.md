# Provision an assessor account

There is deliberately no in-app account management screen - "four
accounts" are provisioned outside the app, by whoever administers the
database, not created by assessors themselves. This is that provisioning
step: hashes `password` with
[`sodium::password_store()`](https://docs.ropensci.org/sodium/reference/password.html)
and inserts the account, `must_change = 1` by default so the first real
sign-in forces a password of the account holder's own choosing (see
`episodic_auth_change_password()`).

## Usage

``` r
episodic_provision_user(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  username,
  full_name,
  email,
  password,
  role = "assessor"
)
```

## Arguments

- db_path:

  Path to an existing SQLite database, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

- username, full_name, email:

  The new account's fields.

- password:

  An initial plaintext password (hashed here, never stored or logged as
  plaintext) - a temporary one the holder is expected to change at first
  sign-in.

- role:

  One of `"assessor"`, `"admin"`.

## Value

Invisibly, the new `user_id`.

## Details

Takes `db_path` rather than an open connection - opened and closed here
via
[`episodic_db_open()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_open.md) -
so provisioning an account is one call at the console, without first
having to construct a `con` by hand.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)
episodic_provision_user(
  db_path, username = "jdoe", full_name = "Jane Doe",
  email = "jane@example.org", password = "temporary-password"
)
file.remove(db_path)
#> [1] TRUE
```
