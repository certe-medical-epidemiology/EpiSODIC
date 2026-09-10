# Deployment

This vignette is for standing up a real instance. If you only want to
see the system working, run
[`EpiSODIC::episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
instead - no data, no credentials, no configuration required.

## The two things you provide

EpiSODIC never connects to a laboratory information system, data
warehouse, or any other data source itself - that step is deliberately
yours, run before EpiSODIC, so the package stays reusable by any
laboratory rather than tied to one:

``` r

cases <- my_extract_and_transform_function()   # a data frame or tibble

episodic_run_cron(
  db_path = "/path/to/episodic.sqlite",
  cases = cases,
  denominators = NULL  # optional
)
```

`cases` (and `denominators`, `institution_activity`) is normally a plain
data frame or tibble, as above - that is what these arguments are
written for. If producing the data only makes sense at run time (a live
database query, for instance), a zero-argument function returning one is
accepted just as well - see `episodic_resolve_data()`.
[`vignette("data-format")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.md)
documents the exact columns each of the four data sources (cases,
positivity metadata, institution activity, geographic reference data)
expects; only cases are mandatory.

Scope `my_extract_and_transform_function()` to a recent window - the
last few weeks, with a couple of weeks of overlap as a margin against a
missed run - rather than a patient’s full history on every schedule. A
scheduled job should stay fast and light regardless of how large the
archive behind it grows. This is safe: deduplication checks every
incoming result against what is already stored for that patient and
pathogen, so a result that reappears inside your overlap window, or one
that continues an episode whose earlier result is not in this batch at
all, is still recognised correctly rather than counted twice.

Check your extract against those requirements before you schedule
anything:

``` r

episodic_check_cases(cases)
episodic_check_denominators(denominators)                  # if you supply positivity data
episodic_check_institution_activity(institution_activity)  # if you supply activity data
```

All three need no database and change nothing, and report everything
wrong with the data at once - the column, the number of rows affected,
which rows those are, the offending values, and what to do about each -
along with what is merely worth a look (one pathogen spelled two ways,
no `ward` on any hospital row, a `patient_key` that never repeats).
Setting `episodic_check_cases(cases, stop_on_problem = TRUE)` makes the
checks return an error if there is at least one problem, for use in a
script.
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
runs that before every run.

Schedule
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
however your environment normally schedules R jobs (cron, a Windows
scheduled task, a CI pipeline) - there is nothing EpiSODIC-specific
about the scheduling itself. It validates the case data itself before
writing anything: a run on data that does not satisfy the requirements
stops with that same message, records it on the run row, and leaves the
database untouched, so a scheduled job fails visibly (a non-zero exit,
an error in the job’s log) rather than completing over data it could not
read. The recorded message is what the dashboard’s status strip and
activity screen show, so whoever notices the empty dashboard first
learns why without going near a log file.

## Configuration lives outside the repository

Detection thresholds, baseline lengths, `same_place` rules and MEM
seasons are *operational data*, not software, and are never committed to
this repository.
[`inst/config/episodic_default_config.yaml`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/config/episodic_default_config.yaml)
ships the documented defaults; point `EPISODIC_CONFIG` at a YAML file
with only the keys you want to override, and it is merged key-by-key on
top of the shipped defaults. Every run records the resolved
configuration’s hash and full snapshot on `episodic_detection_run`, so
the exact parameters behind any past result stay recoverable from the
database alone, regardless of what has since changed on disk.

The UI’s colour palette and typography work the same way, deliberately
through a *separate* environment variable (`EPISODIC_STYLE`): colour and
font choice are display concerns, never part of the
detection-reproducibility guarantee `EPISODIC_CONFIG`’s hash provides.

## Accounts

By default the app is **closed**: an anonymous visitor gets a sign-in
prompt and nothing else. Accounts are never created by users themselves;
either an `is_admin` account provisions them from the in-app Settings
screen, or whoever administers the database runs
[`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md)
at the console. Signing in unlocks everything the account’s role allows,
patient-level detail (the line list) included, for both roles below.

### Opening the app to anonymous visitors

An instance already behind network controls that make anonymous
reachability impossible may prefer to let colleagues read the aggregate
screens - clusters, streams, the archive - without signing in, leaving
sign-in needed only for the line list and for writing. Set
`access.require_login` to `false` in your `EPISODIC_CONFIG` YAML:

``` yaml
access:
  require_login: false
```

Weigh that deliberately rather than by habit. A cluster row names a
pathogen, a ward or an institution, and a date range; in a small
population that combination is often enough to identify who, even
without the line list. The shipped default is `true` for the same
reason: a surveillance system distributed to laboratories worldwide has
to be safe in the state it arrives in, because the deployment that goes
wrong is the one where nobody read this file.

With `require_login: true` (the default):

An anonymous visitor then gets a sign-in prompt and nothing else: no
navigation, no status strip, not one row of data. This is enforced on
the **server**, not in the browser - none of those screens is rendered
for a session that has not signed in, so nothing they would contain is
ever serialised into the page. There is correspondingly nothing for a
browser’s developer tools to uncover by removing the prompt: what is not
sent cannot be inspected. The same gate covers inputs a client can set
for itself, not only the links the navigation offers.

This is a YAML-only setting on purpose, with no override on the Settings
screen: a login wall an admin account can switch off from inside the app
is a login wall that falls with that one account. Turning it off takes
file access to the machine. The Settings screen does *report* which way
it is set, so an admin can confirm it without SSH access.

It is excluded from `config_hash` alongside `notifications`: it governs
how the instance is operated, never what a detection run computes, so
turning it on does not make historic runs look incomparable with current
ones.

There are exactly two roles:

- `"epidemiologist"` - by definition, assesses clusters and classifies
  them. Can do everything a viewer can, plus classify, close, and mute
  clusters, and re-render reports on demand.
- `"viewer"` - read-only. Sees exactly what a signed-in epidemiologist
  sees, including patient-level detail, but cannot record an assessment.

Independent of role, an account can additionally be flagged `is_admin`,
which unlocks the Settings screen: managing notification channels and
recipients, creating accounts and changing their role/admin/active
state, and exporting the resolved configuration. Detection parameters
themselves stay read-only there deliberately - changing them is still a
`EPISODIC_CONFIG` YAML change, so every past result stays traceable to
the exact configuration that produced it (see “Configuration” above).

``` r

Sys.setenv(EPISODIC_DB = "/path/to/episodic.sqlite")  # or pass db_path explicitly below

episodic_add_user(
  username = "jdoe",
  role = "epidemiologist",  # or "viewer"
  full_name = "Dr Jane Doe",
  email = "j.doe@example.org",
  password = "a-temporary-password"
)
```

[`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md)
takes `db_path` (defaulting to `EPISODIC_DB`, see
[`vignette("environment-variables")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.md)),
not an open connection - it opens and closes its own via
[`episodic_db_open()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_open.md),
so adding an account is one call at the console. The account is created
with `must_change = TRUE`, so its first real sign-in forces the holder
to set their own password before continuing. Pass `is_admin = TRUE` to
give the new account Settings-screen access as well - you need at least
one such account to manage anything from the dashboard itself; every
account after that can be added either way.

### Watching sign-ins

Both outcomes are recorded. A successful sign-in becomes a `login` event
on the account; a refused one is written to `episodic_app_login_failure`
with the username as typed and which of three reasons it was - no
account with that username, wrong password, or account deactivated. The
sign-in screen itself still says none of that to whoever is typing:
unknown username and wrong password are one generic refusal to them, so
that a stranger cannot use the login form to find out which usernames
exist. The distinction is for the operator, in the record, afterwards.

Both appear on the **Activity** screen, where the filter chips at the
top narrow the log to sign-ins alone. A run of refused attempts on one
account, or a series of usernames nobody has, is then visible the same
day it happens.

Sign-in rows are the one part of the Activity screen withheld from a
reader who has not signed in - on an instance running with
`require_login: false`, “who has an account here, and which usernames
somebody has been trying” is precisely what not to hand a visitor.

This is a record, not a defence: EpiSODIC has no account lockout and no
rate limiting, and deliberately so - a surveillance dashboard that locks
an epidemiologist out mid-outbreak because somebody else mistyped their
username is worse than the attack it prevents. Keeping attackers off the
port is your network’s job (see the top of this vignette); noticing that
one got as far as the login form is this log’s.

## Where the database lives

`EPISODIC_DB` (and every `db_path` argument that falls back to it) works
against two backends:

- **SQLite** (the default): a path to a local file. It must sit on local
  disk, not in a synchronising folder (SharePoint, OneDrive, Dropbox) -
  background sync can corrupt or fork a live SQLite file without any
  immediate warning. Configuration files are safe in a synced location,
  since they are small and read-only at runtime; the database itself is
  not.
- **MariaDB/MySQL**: a `mysql://user:password@host:port/dbname` DSN,
  built with
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)
  rather than assembled by hand. Requires the `RMariaDB` package. This
  is the option for a deployment that already runs a database server and
  would rather point EpiSODIC at it than manage a SQLite file on disk.

``` r

Sys.setenv(EPISODIC_DB = episodic_db_dsn_mariadb(
  host = "db.internal", dbname = "episodic",
  user = "episodic_app", password = "s3cr3t!"
))
```

Connecting to MariaDB/MySQL requires the `RMariaDB` package
(`install.packages("RMariaDB")`); it is a `Suggests` dependency, not
installed automatically, so SQLite-only deployments never need it. The
schema
([`inst/sql/schema.sql`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/sql/schema.sql))
is written once, in SQLite syntax, and adapted at load time for what
differs under MariaDB/MySQL - there is no separate schema file to keep
in sync.

### Which servers this is tested against

Every release runs the full test suite against a real **MariaDB 11** and
a real **MySQL 8** server, not only against SQLite, and a schema change
either server rejects fails the build. The suite has also been run by
hand against MariaDB 10.11.

Both are tested because they are not the same server. MariaDB and MySQL
disagree about how a foreign key written on the column rather than at
the end of the table should be treated - MariaDB creates it, MySQL
parses it and discards it - and EpiSODIC’s schema is written in the form
SQLite wants. It is translated for these servers so that the constraints
exist on all three, and the suite counts them on a live server rather
than assuming.

`CHECK` constraints are enforced from MariaDB 10.2.1 / MySQL 8.0.16
onwards; on older servers they are accepted but silently ignored.

### Sharing a schema with something else

EpiSODIC does not need a database of its own. Its tables can sit in a
schema alongside another application’s, which is a common way to be
given a database by an infrastructure team, and every operation that
enumerates tables - creating the schema, `overwrite = TRUE`, and
[`episodic_db_truncate()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_truncate.md) -
is scoped to EpiSODIC’s own tables and cannot touch a co-tenant’s.

### If you already have a MariaDB or MySQL database

**One created by EpiSODIC 0.15.x or earlier has no foreign keys.** They
were declared in the schema and the server discarded them without saying
so, so such a database has been accepting rows that reference nothing.
There is no migration that adds them, because a constraint cannot be
added to a table that already violates it: recreate the database with
`episodic_db_create(overwrite = TRUE)` and load your case data again.
SQLite databases are unaffected and always had them.

To check what your server actually created:

``` sql
SELECT COUNT(*) FROM information_schema.REFERENTIAL_CONSTRAINTS
 WHERE CONSTRAINT_SCHEMA = DATABASE()
   AND TABLE_NAME LIKE 'episodic\_%';
```

Anything other than 38 means the schema and the server disagree, and the
suite’s own live tests check that same number against a live server on
every run.

## Upgrading EpiSODIC

The database records which version of EpiSODIC’s schema it was built
with, and
[`episodic_db_connect()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_connect.md)
refuses one it does not recognise rather than failing later with an
unexplained SQL error - or, worse, not failing and reading a column that
has since come to mean something else. So when you update the package
and its schema has moved on, the next run stops with an error naming the
fix:

``` r

episodic_db_migrate()   # or episodic_db_migrate(db_path)
```

Migrations are additive: they add tables, add columns and backfill
values, in one transaction per step, and never drop or rewrite anything.
Your surveillance history, your assessments and your audit trail are
carried forward untouched. Take a backup first anyway - that advice does
not stop being good because the code is careful.

A database built by EpiSODIC 0.12.x or earlier carries no version at
all, since the version table postdates it.
[`episodic_db_migrate()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_migrate.md)
adopts such a database at version 1 rather than refusing it, then brings
it forward from there. Run it once, on each instance, after upgrading.

## Running the app

``` r

Sys.setenv(EPISODIC_DB = "/path/to/episodic.sqlite")
episodic_run_app()
```

[`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md)’s
`db_path` argument defaults to `EPISODIC_DB` exactly like
[`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md)’s
does, so a systemd unit or Docker container can configure everything
through environment variables alone, with no R code to edit between
instances.

## Custom report templates

[`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
renders the shipped
[`inst/report/episodic_default_report.qmd`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/report/episodic_default_report.qmd)
(embedded as self-contained HTML via Quarto) unless
`EPISODIC_QUARTO_REPORT` points at an operator’s own `.qmd` file, in
which case that is used instead - for an organisation that wants its own
letterhead, section order, or house style. A custom template only needs
to `readRDS(params$data_path)` and read from the same list the shipped
one does (`obj`, `epi_curve`, `trend`, `linelist`, `timeline`,
`similar`, `diff`, `small_count_threshold`, `rendered_at`, `lang`,
`package_version`); see the shipped template for the exact shape,
including how it calls `episodic_tr(..., lang = d$lang)` to render in
any of the dashboard’s supported languages. `diff` is `NULL` for a
cluster’s first-ever render and otherwise holds what changed since the
previous version - see
[`vignette("scheduled-reports")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/scheduled-reports.md)’s
“What changed since last time” section for what it contains.

## Scheduled reports

A cluster can be put on a recurring email schedule from its dossier -
every N days, to a list of colleagues without an EpiSODIC account. This
reuses the report template and the email-capable notification channels
(`smtp`, `sendmail`, `microsoft365`) described in
[`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md);
see
[`vignette("scheduled-reports")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/scheduled-reports.md)
for the full setup guide, including the cadence and automatic-closure
rules and how to extend a custom template with more patient-level detail
than the shipped one shows.

## Optional pieces, and their fallbacks

Every optional integration point degrades to a documented fallback
rather than failing when it is absent:

| Feature | Needs | Fallback |
|----|----|----|
| Rt estimation | `EpiEstim` | Panel omitted |
| MEM seasonal thresholds | `mem` | Detector skipped for `mem_applicable` organisms |
| Outbreak reports | `quarto` R package + the separate Quarto CLI | Render errors clearly instead of silently producing nothing |
| Choropleth map | `sf` + geographic reference data | Plain bar breakdown by PC value |
| Notifications | `httr2`, `curl`, `Microsoft365R` + `AzureGraph` + `AzureAuth` (depending on channel) | No alerts; review clusters through the dashboard only |

`AMR` is a hard dependency, not an optional integration: episode
deduplication (`episodic_cases_deduplicate()`) calls
[`AMR::get_episode()`](https://amr-for-r.org/reference/get_episode.html)
directly, and pathogen-name italicisation
([`episodic_ui_italicise_taxon()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md))
reads
[`AMR::microorganisms`](https://amr-for-r.org/reference/microorganisms.html).
There is no fallback for either, by design - see the DESCRIPTION for the
published methods this package is built on.

House-style colours and typography are not an optional-dependency
concern at all: the app always ships an organisation-neutral default
palette
([`inst/config/episodic_default_style.yaml`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/config/episodic_default_style.yaml)),
overridable per instance by pointing `EPISODIC_STYLE` at an
organisation’s own YAML file with its real colours and font - no package
dependency involved either way. A webfont named there still needs
delivering (self-hosted, or linked from its own provider), typically via
a custom `www/episodic.css`; a system font needs no such link at all.

None of these are required to run the demo, the detection engine, or the
interface - each is additive.

## Notifications

EpiSODIC can notify your team, through ntfy, email, Teams, or Slack,
whenever new clusters are detected or a cron run fails. Notifications
are configured in the same YAML file as detection thresholds
(`EPISODIC_CONFIG`), under a `notifications` key. See
[`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md)
for the full setup guide, with step-by-step instructions for every
supported channel.

## See also

- [`vignette("data-format")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.md)
  for the case data requirements this vignette’s
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  calls expect.
- [`vignette("environment-variables")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.md)
  for the full `EPISODIC_*` reference table, including every variable
  named above.
- [`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md)
  for setting up alerts on new clusters and run failures.
- [`vignette("scheduled-reports")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/scheduled-reports.md)
  for emailing a recurring outbreak report to colleagues without a
  dashboard account.
- [`vignette("faq")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/faq.md)
  for hosting choices, account roles, and other operational questions.
