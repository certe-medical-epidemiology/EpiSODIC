# How scheduled reports work

A scheduled report is a standing subscription on one cluster: every
`interval_days`,
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
emails the outbreak report - the same self-contained HTML
[`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
produces on demand - to a list of addresses, as an attachment. It exists
for people who need to follow a cluster without an EpiSODIC account, or
with only a `viewer` one: a ward manager, a department head, a referring
physician. This is deliberately not the same thing as
[episodic_notifications](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_notifications.md):
those alert *epidemiologists* that something needs assessing; a
scheduled report keeps *everyone else* informed once it is being
watched, on a cadence they chose, without needing to open the dashboard
at all.

## Details

An `epidemiologist` sets a schedule from the cluster's Reports panel: an
interval in days, one or more recipient addresses, which
already-configured, email-capable notification channel to send through
(`smtp`, `sendmail` or `microsoft365` - see
[episodic_notifications](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_notifications.md)),
and whether to include the case line list (excluded by default, since
these recipients are often outside the organisation - see
[`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)
for how a custom Quarto template can include it anyway). Every setting
is stored as an event, the same event-sourced shape as a cluster note:
setting a new schedule (even just changing the recipient list) starts
its cadence over, and the very next cron run sends the first report
under the new settings so the change is confirmed immediately rather
than silently taking effect days later.

Each cron run checks every cluster's current schedule and sends
whichever are due - due the first time under a schedule's current
settings, or `interval_days` after the last *successful* send under
those settings. A failed send (a broken SMTP relay, say) does not
postpone the next attempt: the very next cron run tries again, and every
attempt, sent or failed, is logged to
`episodic_report_subscription_send`. A schedule stops automatically,
after one final report, the run a cluster closes, merges, or is
suppressed - further updates about an inactive cluster would be noise
rather than news. An epidemiologist can also cancel a schedule outright
at any time.
