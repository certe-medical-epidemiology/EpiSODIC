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
accepted just as well - see
[`episodic_resolve_data()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_resolve_data.md).
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

Check your extract against that contract before you schedule anything:

``` r

episodic_check_cases(cases)
episodic_check_denominators(denominators)              # if you supply positivity data
episodic_check_institution_activity(institution_activity)  # if you supply activity data
```

All three need no database and change nothing, and report everything
wrong with the data at once - the column, the number of rows affected,
which rows those are, the offending values, and what to do about each -
along with what is merely worth a look (one pathogen spelled two ways,
no `ward` on any hospital row, a `patient_key` that never repeats).
[`episodic_validate_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_cases.md)
runs the same checks as
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
and throws instead, for a script that should stop.

Schedule
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
however your environment normally schedules R jobs (cron, a Windows
scheduled task, a CI pipeline) - there is nothing EpiSODIC-specific
about the scheduling itself. It validates the case data itself before
writing anything: a run on data that does not satisfy the contract stops
with that same message, records it on the run row, and leaves the
database untouched, so a scheduled job fails visibly (a non-zero exit,
an error in the job’s log) rather than completing over data it could not
read. The recorded message is what the dashboard’s status strip and
activity screen show, so whoever notices the empty dashboard first
learns why without going near a log file.

## Configuration lives outside the repository

Detection thresholds, baseline lengths, `same_place` rules and MEM
seasons are *operational data*, not software, and are never committed to
this repository. `inst/config/default.yaml` ships the documented
defaults; point `EPISODIC_CONFIG` at a YAML file with only the keys you
want to override, and it is merged key-by-key on top of the shipped
defaults. Every run records the resolved configuration’s hash and full
snapshot on `episodic_detection_run`, so the exact parameters behind any
past result stay recoverable from the database alone, regardless of what
has since changed on disk.

The UI’s colour palette works the same way, deliberately through a
*separate* environment variable (`EPISODIC_PALETTE_CONFIG`): colour is a
display concern, never part of the detection-reproducibility guarantee
`EPISODIC_CONFIG`’s hash provides.

## Accounts

Aggregate data is anonymous - the app opens read-only for anyone who
reaches it. Signing in unlocks patient-level detail (the line list) for
both roles below. Accounts are never created by users themselves; either
an `is_admin` account provisions them from the in-app Settings screen,
or whoever administers the database runs
[`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md)
at the console.

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

episodic_provision_user(
  username = "jdoe",
  role = "epidemiologist",  # or "viewer"
  full_name = "Dr Jane Doe",
  email = "j.doe@example.org",
  password = "a-temporary-password"
)
```

[`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md)
takes `db_path` (defaulting to `EPISODIC_DB`, see
[`vignette("environment-variables")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.md)),
not an open connection - it opens and closes its own via
[`episodic_db_open()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_open.md),
so provisioning an account is one call at the console. The account is
created with `must_change = TRUE`, so its first real sign-in forces the
holder to set their own password before continuing. Pass
`is_admin = TRUE` to give the new account Settings-screen access as
well - you need at least one such account to manage anything from the
dashboard itself; every account after that can be provisioned either
way.

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
  would rather point EpiSODIC at it than manage a SQLite file on disk -
  the schema and every query work identically against either backend.

``` r

Sys.setenv(EPISODIC_DB = episodic_db_dsn_mariadb(
  host = "db.internal", dbname = "episodic",
  user = "episodic_app", password = "s3cr3t!"
))
```

Connecting to MariaDB/MySQL requires the `RMariaDB` package
(`install.packages("RMariaDB")`); it is a `Suggests` dependency, not
installed automatically, so SQLite-only deployments never need it. The
schema (`inst/sql/schema.sql`) is written once, in SQLite syntax, and
adapted at load time for the handful of tokens that differ under
MariaDB/MySQL - there is no separate schema file to keep in sync.
`CHECK` constraints are enforced from MariaDB 10.2.1 / MySQL 8.0.16
onwards; on older servers they are accepted but silently ignored.

## Running the app

``` r

Sys.setenv(EPISODIC_DB = "/path/to/episodic.sqlite")
episodic_run_app()
```

[`episodic_run_app()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_app.md)’s
`db_path` argument defaults to `EPISODIC_DB` exactly like
[`episodic_provision_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_provision_user.md)’s
does, so a systemd unit or Docker container can configure everything
through environment variables alone, with no R code to edit between
instances.

## Custom report templates

[`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
renders `inst/report/cluster_report.qmd` (embedded as self-contained
HTML via Quarto) unless `EPISODIC_QUARTO_REPORT` points at an operator’s
own `.qmd` file, in which case that is used instead - for an
organisation that wants its own letterhead, section order, or house
style. A custom template only needs to `readRDS(params$data_path)` and
read from the same list the shipped one does (`obj`, `epi_curve`,
`trend`, `linelist`, `timeline`, `similar`, `small_count_threshold`,
`rendered_at`, `lang`, `package_version`); see the shipped template for
the exact shape, including how it calls
`episodic_tr(..., lang = d$lang)` for a bilingual report.

## Optional pieces, and their fallbacks

Every optional integration point degrades to a documented fallback
rather than failing when it is absent:

| Feature | Needs | Fallback |
|----|----|----|
| Rt estimation | `EpiEstim` | Panel omitted |
| MEM seasonal thresholds | `mem` | Detector skipped for `mem_applicable` organisms |
| Outbreak reports | `quarto` R package + the separate Quarto CLI | Render errors clearly instead of silently producing nothing |
| Choropleth map | `sf` + geographic reference data | Plain bar breakdown by PC value |
| Notifications | `httr2`, `curl`, `Microsoft365R` + `AzureGraph` (depending on channel) | No alerts; review clusters through the dashboard only |

`AMR` is a hard dependency, not an optional integration: episode
deduplication (`episodic_cases_deduplicate()`) calls
[`AMR::get_episode()`](https://amr-for-r.org/reference/get_episode.html)
directly, and pathogen-name italicisation
([`episodic_ui_italicise_taxon()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ui_italicise_taxon.md))
reads
[`AMR::microorganisms`](https://amr-for-r.org/reference/microorganisms.html).
There is no fallback for either, by design - see the DESCRIPTION for the
published methods this package is built on.

House-style colours are not an optional-dependency concern at all: the
app always ships an organisation-neutral default palette
(`inst/config/palette.yaml`), overridable per instance by pointing
`EPISODIC_PALETTE_CONFIG` at an organisation’s own YAML file with its
real colours - no package dependency involved either way.

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
  for the case data contract this vignette’s
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
  calls expect.
- [`vignette("environment-variables")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.md)
  for the full `EPISODIC_*` reference table, including every variable
  named above.
- [`vignette("notifications")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.md)
  for setting up alerts on new clusters and run failures.
- [`vignette("faq")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/faq.md)
  for hosting choices, account roles, and other operational questions.
