# Create an account for a new assessor or administrator

There is no self-service registration and no in-app account management
screen: whoever administers the database creates accounts with this
function, typically once per new board member. The password you supply
is temporary - the new account is flagged to require a password change,
so the account holder chooses their own password the first time they
sign in.

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

  Path to the EpiSODIC database: an existing SQLite file, or a
  MariaDB/MySQL DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

- username, full_name, email:

  The new account's sign-in name, display name, and email address.

- password:

  A temporary plaintext password, never stored or logged as-is: it is
  hashed before it reaches the database, and the account holder is
  required to replace it on first sign-in.

- role:

  Either `"assessor"` (records assessments) or `"admin"` (assessor
  privileges plus dossier reconciliation and archiving).

## Value

Invisibly, the new account's `user_id`.

## Details

You only need to run this once per person. It opens the database, adds
the account, and closes the connection again, so it is meant to be run
interactively at the R console rather than from application code.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)

user_id <- episodic_provision_user(
  db_path, username = "jdoe", full_name = "Jane Doe",
  email = "jane@example.org", password = "temporary-password"
)
user_id
#> [1] 1

file.remove(db_path)
#> [1] TRUE
```
