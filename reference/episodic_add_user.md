# Create an Account for a New Epidemiologist or Viewer

There is no self-service registration: an account is added either from
the Settings screen (by an `is_admin` account) or with this function at
the R console. The password you supply is temporary - the new account is
flagged to require a password change, so the account holder chooses
their own password the first time they sign in.

## Usage

``` r
episodic_add_user(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  username,
  full_name,
  email,
  password,
  role = "epidemiologist",
  is_admin = FALSE
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

  Either `"epidemiologist"` (can classify and close clusters, in
  addition to everything a viewer can do) or `"viewer"` (read-only - can
  see everything a signed-in epidemiologist sees, including
  patient-level line lists, but cannot record an assessment). Both roles
  require sign-in; there is no anonymous access to patient detail.

- is_admin:

  Whether the new account may also see the Settings screen (manage
  notification channels, other accounts, and export the configuration).
  Independent of `role` - an admin is still either an epidemiologist or
  a viewer for everything outside Settings.

## Value

Invisibly, the new account's `user_id`.

## Details

At the console, you only need to run this once per person. It opens the
database, adds the account, and closes the connection again, so it is
meant to be run interactively rather than from application code.

## Examples

``` r
db_path <- tempfile(fileext = ".sqlite")
con <- episodic_db_create(db_path)
DBI::dbDisconnect(con)

user_id <- episodic_add_user(
  db_path,
  username = "jdoe", full_name = "Jane Doe",
  email = "jane@example.org", password = "temporary-password"
)
user_id
#> [1] 1

file.remove(db_path)
#> [1] TRUE
```
