# How sign-in works

EpiSODIC's dashboard is used by a small board of epidemiologists (the
`"epidemiologist"` role - by definition, they assess clusters and
classify them) and by `"viewer"` accounts for anyone who needs to see
cluster detail, including patient-level data, without classifying
anything themselves. Both roles sign in with a username and password;
there is no self-service registration - an administrator creates each
account with
[`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md),
and the new user sets their own password on first sign-in.

## Details

Passwords are hashed (never stored in plain text). Both outcomes are
recorded: a successful sign-in as a `login` event on the account, a
refused one - with which of unknown username, wrong password or
deactivated account it was, and the username as typed - in
`episodic_app_login_failure`. Both appear on the dashboard's Activity
screen, to signed-in readers only, where the sign-in category can be
filtered to on its own.

That record is an audit trail, not a defence. This login exists to
attribute who assessed what, not to keep an attacker out: EpiSODIC does
not implement TLS, account lockout, or rate limiting, so it should
always be deployed behind your own organisation's network controls (VPN,
internal network, or a reverse proxy that terminates TLS). What the
failure log gives you is the ability to *notice* - a burst of refused
attempts on one account is visible on the Activity screen the same day
it happens.
