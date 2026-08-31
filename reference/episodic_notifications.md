# How notifications work

When
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
detects new clusters or fails, it can notify your team through one or
more channels: ntfy, SMTP, sendmail, Microsoft 365, Teams (Power
Automate Workflow webhook), or Slack. Notifications are configured in
the instance YAML configuration file (the same file `EPISODIC_CONFIG`
points at), under a `notifications` key. The notification settings are
deliberately excluded from the detection `config_hash`, so changing them
never alters reproducibility and secrets never reach the
`config_snapshot` stored on each run.

## Details

Notifications are dispatched *after* the detection transaction has
committed: a failed notification never rolls back a successful run.
Errors during dispatch are logged via `episodic_trace()` and never stop
the run from finishing.
