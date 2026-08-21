# How sign-in works

EpiSODIC's dashboard is used by a small board of epidemiologists who
sign in with a username and password to record their assessments under
their own name. There is no self-service registration: an administrator
creates each account with
[`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md),
and the new user sets their own password on first sign-in.

## Details

Passwords are hashed (never stored in plain text) and login history is
kept for audit purposes. This login exists to attribute who assessed
what, not to defend the system against attackers: EpiSODIC does not
implement TLS, account lockout, or rate limiting, so it should always be
deployed behind your own organisation's network controls (VPN, internal
network, or a reverse proxy that terminates TLS).
