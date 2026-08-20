# Authentication

A handful of accounts, hashed with
[`sodium::password_store()`](https://docs.ropensci.org/sodium/reference/password.html)
and checked with
[`sodium::password_verify()`](https://docs.ropensci.org/sodium/reference/password.html).
No lockout, no backoff, and no TLS is implemented *by this package*: the
login exists to attribute assessments, not to defend against attackers,
and network-level access control (VPN, internal network, a reverse proxy
terminating TLS) is the deploying operator's own responsibility, not
something this package provides or assumes. Since a single R process
serves every session, `Sys.info()[["user"]]` returns the host account
rather than the assessor, so the login is the only available identity
source.

## Details

Password changes and login timestamps are the one bit of per-user
*mutable* state in the schema, yet the app never issues an `UPDATE`.
Resolved the same way `episode_cluster_ state` already resolves it for
cluster state: `episode_app_user_event` is an append-only log, and the
"current" password hash / login time is derived from it at read time
(see `episode_auth_password_hash()`, `episode_auth_last_login()`),
falling back to `episode_app_user`'s own initial values when no event
has been recorded yet.
