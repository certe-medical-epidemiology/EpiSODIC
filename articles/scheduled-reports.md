# Scheduled reports

[`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md)
is for your epidemiologists: it alerts them the moment a cluster needs
assessing. Scheduled reports are for everyone else who needs to follow a
cluster without ever opening the dashboard - a ward manager, a
department head, a referring physician, or any colleague with no
EpiSODIC account, or only a `viewer` one. An epidemiologist puts a
cluster on a recurring schedule from its dossier; from then on,
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
emails the outbreak report to a list of addresses, every N days, for as
long as the cluster stays active.

## Setting up a schedule

Open a cluster’s dossier and scroll to the Reports panel. Below the
render-on-demand button, a signed-in `epidemiologist` sees a “Scheduled
reports” section:

- **Send every** - the cadence, in days.
- **Recipient email addresses** - one or more, separated by a comma or a
  new line.
- **Send via** - which already-configured, email-capable notification
  channel to deliver through: `smtp`, `sendmail` or `microsoft365` (see
  [`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md)).
  Only channels with `enabled: true` in your instance configuration
  appear here; if none do, the section tells you to configure one first.
- **Include the case line list** - off by default. These recipients are
  often outside your own organisation, so patient-level detail is opt-in
  per schedule rather than on by default.

Saving replaces any previous schedule for that cluster - there is only
ever one active schedule per cluster, and setting a new one (even just
to change the recipient list) resets its cadence: the very next cron run
sends a report under the new settings, confirming the change took effect
rather than leaving you to wait out the old interval.

An epidemiologist can cancel a schedule at any time from the same panel.
Every change is recorded as an event, the same way a cluster note is -
nothing is ever silently overwritten.

## Cadence and delivery

Each cron run checks every cluster’s current schedule:

- **Due for the first time** under a schedule’s current settings (no
  successful send logged against it yet) - sent immediately.
- **Due again** once `interval_days` have passed since the last
  *successful* send under those settings. A failed attempt (an
  unreachable SMTP relay, say) does not push this back - the very next
  cron run tries again, so a transient outage costs at most one missed
  day, not a full interval.
- **Stops automatically**, after one final report, the run a cluster
  closes, merges into another cluster, or is suppressed behind one.
  Further updates about a cluster that is no longer live would be noise,
  not news.

The report is delivered as a self-contained HTML file attached to the
email - the same file
[`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
produces on demand, opened directly in a browser, needing neither a
dashboard login nor a network connection back to your instance. Every
send, sent or failed, is logged; the Reports panel shows the most recent
attempt so an epidemiologist can see at a glance whether a schedule is
actually reaching anyone.

## What changed since last time

Every report - scheduled or rendered on demand - shows what changed
since the previous version, in a callout box near the top: new cases
since then, and how the priority score, the observed/expected ratio, and
the case period moved. The very first report for a cluster has nothing
to compare against, so the box is simply omitted.

For a scheduled report specifically, the email body itself also mentions
the new-case count, so a reader can judge from the inbox alone whether
opening the attachment is urgent.

## Custom report templates and patient-level detail

Scheduled reports render through the same
[`inst/report/episodic_default_report.qmd`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/report/episodic_default_report.qmd)
template (or your own, via `EPISODIC_QUARTO_REPORT` - see
[`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)’s
“Custom report templates” section) as an on-demand render. The “include
the case line list” checkbox controls only whether `d$linelist` is
populated for that particular schedule; everything else in
`params$data_path` is always present, snapshot metrics and
version-to-version diff included.

If you need a scheduled report to carry more patient-level detail than
the shipped template shows even with the line list included - a
different set of columns, say, or per-case highlighting of what is new
since the previous version - write your own `.qmd` reading from the same
`report_data.rds` shape documented in
[`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md),
and point `EPISODIC_QUARTO_REPORT` at it. The line list itself
(`d$linelist`) already carries everything `episodic_case` records short
of the raw source key: patient key, lab number, sample date, sex, age,
postcode, ward and specialism - a custom template is free to show all of
it, or to compute its own per-case “new since last time” flag from
`d$diff` and the sample dates in `d$linelist`.

## See also

- [`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md)
  for configuring the email-capable channels (`smtp`, `sendmail`,
  `microsoft365`) a schedule sends through.
- [`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)
  for custom Quarto report templates.
- [`?episodic_scheduled_reports`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_scheduled_reports.md)
  for the API-level documentation.
- [`?episodic_report_render`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
  for what a single render produces.
