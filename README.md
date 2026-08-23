# EpiSODIC: Epidemiological Signal Observation, Detection, Identification, and Classification

[![R-CMD-check](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml)

EpiSODIC is an automated outbreak cluster detection and assessment
system for infectious disease epidemiologists. It runs in R, and reads laboratory-confirmed
infections, detects statistical and rule-based aberrations, reconciles them
into persistent clusters, and gives epidemiologists a
dossier to assess each one, with a full audit trail and outbreak reports
for clinical colleagues (e.g. clinical microbiologists) and infection
prevention nurses.

The dashboard and its outbreak reports are available in Dutch, English,
Spanish, French, German, Mandarin Chinese, Hindi, and (Modern Standard)
Arabic.

Ready to automate your epi-analyses? Be sure to read our [frequently asked
questions](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/faq.html).

This software is free; this repository is open-source software with no data and no site-specific configuration.

<!--
Screenshots below live in man/figures/ (ships with the package, so
they also render on the CRAN page), regenerated against episodic_demo().
-->
<p align="center">
  <img src="man/figures/main_screen.png" alt="A cluster dossier: case stats, status trajectory, an automatically generated plain-language interpretation, the epidemic curve, and the classification panel" width="800">
  <br>
  <em>A cluster dossier - case stats, status trajectory, an automatically generated interpretation of the evidence, the epidemic curve, and the classification panel, alongside the rail of open clusters.</em>
</p>
<p align="center">
  <img src="man/figures/performance_screen.png" alt="The Performance screen: detection timeliness and positive predictive value per detector and pathogen" width="800">
  <br>
  <em>The Performance screen - detection timeliness and positive predictive value per detector and pathogen, computed from the stored verdicts (both fill in as clusters get assessed).</em>
</p>

## Capabilities

The detection engine, interface, assessment and authentication
workflow, reporting, and analytical panels are all implemented and
tested, including a Performance screen for evidence-based tuning.
Detection thresholds and priority score weights are configurable per
instance (`inst/config/default.yaml`, `EPISODIC_CONFIG`), so an organisation
can tune them against its own signal volume as its evidence base grows.

### Two altitudes

The **Clusters** screen is operational: a ranked queue of discrete
signals, each one something a person has to make and record a decision
about. That is the right unit for triage and the wrong unit for
surveillance - "is influenza A unusual this season, and where in the
season are we" is not a question about any one cluster, and cannot be
answered by reading several dossiers in turn.

The **Pathogen** screen is the other altitude. Pick an pathogen and a
period - a surveillance season (ISO week 40 to week 20), the last twelve
months, the last five years, or an exact date range - and it describes
that pathogen across the whole catchment over that period:

- weekly incidence, with the Moving Epidemic Method's pre- and
  post-epidemic thresholds and its medium/high/very high intensity bands
  drawn on it, fitted only on the seasons *before* the one being looked
  at;
- the same period laid over earlier seasons (or, for a non-seasonal
  pathogen, earlier calendar years) on a shared week-within-period axis,
  so "earlier", "later", "bigger", "smaller" can be read directly;
- Rt for the pathogen as a whole rather than per cluster - which is the
  population the renewal model assumes it is seeing - conditioned on case
  history from before the selected period;
- testing volume and positivity, age and sex against the pathogen's own
  long-run distribution, geography, and care-line and institution
  breakdowns;
- and the clusters that were raised during the period, with the verdict
  each one received, as the way back to the operational view.

## Installation

```r
# install.packages("remotes")
remotes::install_github("certe-medical-epidemiology/EpiSODIC")
```

### Demo

No data, no credentials, no configuration - one call runs the whole
system against bundled synthetic data:

```r
EpiSODIC::episodic_demo()
```

This creates a temporary database, runs one detection cycle, provisions
a demo assessor account (printed to the console), and opens the app.
Pass `launch = FALSE` to skip opening the app and just get a populated
database path back, e.g. for scripting.

### Bringing your own data? Check it first

Before the demo, before a scheduled run, before anything: hand your own
extract to `episodic_check_cases()`. No database, no configuration, and
nothing is changed - it just tells you what EpiSODIC makes of your data.

```r
EpiSODIC::episodic_check_cases(my_cases)
```

It reports every problem at once - the column, how many rows are
affected, which rows those are, what the offending values look like, and
what to do about each - along with what is merely worth a look (one
pathogen spelled two ways, no `ward` on any hospital row, a `patient_key`
that never repeats). See "Check your data first" under "Data format"
below for what its output looks like.

EpiSODIC's detection engine has **no dependency on any laboratory system
or data warehouse**. Every dependency is a CRAN-hosted package
(`surveillance` for Farrington); no private, organisation-specific
package is required or even referenced. House colours
(`episodic_palette()`) come from a shipped, organisation-neutral default,
overridable per instance by pointing `EPISODIC_PALETTE_CONFIG` at a YAML
file with an organisation's own colours - the same mechanism used for
detection configuration, a custom report template, and geographic
reference data. Geography (the choropleth panel) is not tied to any one
organisation or country either - see "Geographic reference data" below.

## Data format

EpiSODIC never queries a laboratory information system, data warehouse, or
any other data source itself. That is deliberately your own step, run
before EpiSODIC: extract from wherever your data lives, transform into the
shape below, then call `episodic_run_cron()` with the result as a plain
data frame (a tibble works just as well). This keeps the engine reusable
by any laboratory.

```r
cases <- my_extract_and_transform_function()

episodic_check_cases(cases)  # what is wrong with it, before anything runs

episodic_run_cron(
  db_path = "/path/to/episodic.sqlite",
  cases = cases,
  denominators = NULL  # optional, see "Positivity metadata" below
)
```

A data set is the normal case, and what these arguments are written for.
If producing the data only makes sense at run time (e.g. a live database
query), a zero-argument function returning one is accepted just as well -
EpiSODIC resolves either.

### Cases (mandatory)

One row per confirmed-positive laboratory result. This is the complete,
allow-listed column set (`episodic_case_columns`); a data set with any
column outside this list, or missing one from it, is rejected. Run
`episodic_check_cases()` on your extract to check all of this - columns,
allowed values, dates, and `source_key` uniqueness - before you schedule
anything. The three fixed value sets are available as
`episodic_care_lines`, `episodic_institution_types` and
`episodic_sex_codes`, so your transform step can map onto them directly:

| Column | Type and allowed values | Meaning |
|---|---|---|
| `source_key` | character, required, unique | A unique identifier for the row in your own source system, so re-running the same extract later cannot create duplicate cases. |
| `patient_key` | character, required | A stable, pseudonymised patient identifier. This is what deduplication and episode grouping key on: without it, EpiSODIC cannot tell that two isolates belong to the same patient, and every isolate would be treated as its own case. Never displayed as-is in the interface. |
| `sample_date` | `Date` or `"YYYY-MM-DD"`, required | The anchor date every detector, trend, and report is built against. If your system falls back to a receipt date when sample date is unfilled, that fallback should already have happened before this row reaches EpiSODIC. |
| `receipt_date` | `Date` or `"YYYY-MM-DD"`, `NA` allowed | When the result was received. Stored for provenance/audit, kept separate from `sample_date` - EpiSODIC deliberately does not use it to measure reporting delay, since a lab's own receipt-date field can itself silently be a stand-in for a missing sample date; reporting completeness is instead measured empirically, from how a stream's case counts change across successive detection runs. |
| `pathogen` | character, required, free text | The pathogen as your lab reports it, as free text. **Not** resolved against any taxonomy, since EpiSODIC has to detect clusters of anything a lab reports. The same underlying isolate can appear more than once under different `pathogen` values when that is epidemiologically useful - an ETEC isolate reported as both `"Escherichia coli"` and `"ETEC"`, so each is watched on its own. This is your transform step's decision, not EpiSODIC's. |
| `care_line` | `first`, `second`, `other`, `unknown` - `NA` allowed | Which part of the health system the case came from: `first` is primary care, `second` secondary care. `NA` is read as `unknown` and stored that way - an empty value, an R `NA` and a database `NULL` all mean the same thing here. |
| `institution_key` | character, required | A stable identifier for the institution, hashed internally so a later rename does not fracture history. |
| `institution_display_name` | character, required | The human-readable name shown in the interface. |
| `institution_type` | `hospital`, `ltc_institution`, `gp_municipality`, `ooh_service`, `other` - required | How the institution is handled downstream; see the list below the table. |
| `municipality` | character, `NA` allowed | The institution's municipality, used for `gp_municipality`-type rows (see below) and as a coarse geographic fallback. |
| `ward` | character, `NA` allowed | Only meaningful (and only used) for hospitals: this is what the `same_place` detector watches at ward level. |
| `specialism` | character, `NA` allowed | The treating specialism, shown on the line list for context; not itself a detection dimension. |
| `pc` | character, `NA` allowed | The patient's postcode (or equivalent - see "Geographic reference data" below for how coarse or fine this can be). Drives the geography panel and the choropleth, and the concentration measure ("how localised is this cluster") that feeds the priority score. |
| `sex` | `M`, `F`, `U` - `NA` allowed | The patient's sex. Feeds the demography panel's age/sex pyramid, one of the interpretation engine's own evidence dimensions (a cluster's demography shifting from an organisation's usual baseline is itself a signal worth surfacing). |
| `age` | integer (whole years), `NA` allowed | The patient's age at sample date. Same role as `sex`: demography panel and interpretation, not a detection input. |

`institution_type` values, beyond the self-explanatory `hospital`:

- `ltc_institution` — a long-term care institution (nursing home, residential
  care), kept as a first-class institution like a hospital.
- `gp_municipality` — a general practice. Stored as the municipality the
  practice is in, not the practice's own identity: a single-handed GP
  practice is not a transmission unit, and its name adds identifiability
  without adding epidemiological information.
- `ooh_service` — an out-of-hours GP service (*huisartsenpost*), kept as a
  first-class institution like a hospital.
- `other` — anything that doesn't fit the above; institution identity is
  dropped (stored as `NULL`) for this category.

**Deduplication is EpiSODIC's job, not yours.** Send every positive result;
EpiSODIC collapses isolates for the same patient and pathogen into one case
per episode, using the episode length configured per pathogen in
`inst/config/pathogen_config.csv`.

**Do not send negative results here.** This feed drives every detector; it
is deliberately positives-only so an operator never has to ship a
multi-year, per-test linelist just to run detection.

### Check your data first

Before you schedule anything, run `episodic_check_cases()` on your
extract. It needs no database, changes nothing, and reports everything it
finds in one go - which column is wrong, how many rows are affected, which
rows those are, what the offending values look like, and what to do about
each:

```r
cases <- my_extract_and_transform_function()

episodic_check_cases(cases)
#> -- EpiSODIC case data check ------------------------------------------
#>    4,312 rows, 15 columns
#>    sample_date from 2024-01-02 to 2024-06-30
#>    12 pathogens, 7 institutions, 3,981 patients
#>
#> x 2 problems - a detection run refuses to start until these are fixed:
#>
#>   1. `sample_date` has 4312 of 4312 rows that are not a Date and do
#>      not read as YYYY-MM-DD.
#>      values: 02-01-2024, 03-01-2024, 04-01-2024 (and 178 more)
#>      rows:   1, 2, 3, 4, 5 (and 4307 more)
#>      fix:    These look day-first (e.g. 31-12-2025): convert with
#>              as.Date(x, format = "%d-%m-%Y"). EpiSODIC accepts a Date
#>              column, or text in ISO 8601 YYYY-MM-DD form, and nothing
#>              else [...]
#>
#>   2. `sex` has 2106 of 4312 rows with a value outside the allowed set
#>      (M, F, U, or NA).
#>      values: male, female
#>      [...]
#>
#> ! 2 things worth a look - a run proceeds regardless:
#>
#>   1. 1 pathogen name(s) appear in more than one spelling, differing
#>      only in capitalisation or spacing: "Influenza A" / "influenza a".
#>      [...]
```

It separates two kinds of finding on purpose. **Problems** are things
EpiSODIC cannot work around, and a run refuses to start while any of them
stand. **Advice** covers what is allowed but rarely intended and quietly
costs you signal: one pathogen spelled two ways (two streams instead of
one), no `ward` on any hospital row (nothing for ward-level detection to
group on), a `patient_key` that never repeats (nothing for deduplication
to do), postcodes the map cannot place, sample dates in the future.

`episodic_validate_cases()` runs the same checks and throws instead, for
use in a script. `episodic_run_cron()` runs it for you before every run:
data it cannot use stops the run with the same message, before anything is
written, and that message is recorded against the run so it is also
visible in the dashboard's status strip and activity screen. A failed run
never passes in silence.

### Positivity metadata (optional)

If, and only if, you can produce it: a small, pre-aggregated table of test
counts, used purely as an interpretive aid on the dossier (a rising case
count with flat test volume is a much weaker signal than the same rise with
stable positivity) and never as a detection input.

| Column        | Meaning                                                                  |
|---------------|--------------------------------------------------------------------------|
| `pathogen`    | Matches the cases feed.                                                  |
| `sample_date` | A period start (e.g. week start); this is aggregate data, not per-test.  |
| `care_line`   | `first`, `second`, `other`, or `unknown`.                                |
| `area_code`   | Optional geographic stratum.                                             |
| `n_tests`     | Total tests run for this pathogen/period/stratum, positive and negative. |

This is realistic to produce for a multiplex PCR panel testing a fixed list
of targets (your LIS can report "we ran 40 GI panels this week" trivially).
It is not meaningful for open-ended culture results, where there is no
closed list of things a negative result could have been - if that is your
situation, simply leave `denominators` at `NULL`, and positivity
panels stay blank for your streams. See `episodic_synthetic_denominators()` for a worked
example.

### Institution activity (optional)

If, and only if, you can produce it: weekly patient-days per hospital,
used to normalise L2 (institution-level) Farrington detection by
occupancy rather than raw counts - a busy February and a quiet August at
identical transmission-per-patient-day should not read as different
signal strengths. Also optional; without it, L1/L2 detection uses raw
counts exactly as it always has.

| Column                       | Meaning                                                    |
|------------------------------|------------------------------------------------------------|
| `institution_key`            | Matches the cases feed.                                    |
| `period_start`, `period_end` | The activity period this row covers (typically a week).    |
| `patient_days`               | Total patient-days across the institution for that period. |

Rows whose `institution_key` does not match a known institution are
skipped, not an error - an activity feed and a case feed need not be
perfectly synchronised. Skipped rows are never silent, though: the load
warns and names the unmatched keys, the count is recorded against the
run, and the run finishes `partial` rather than `success` (see "What a
run reports" below). See
`episodic_synthetic_institution_activity()` for a worked example.
There is no ward-level (L1) equivalent in the schema, so L1 detection is
never normalised, only L2.

### Geographic reference data (optional)

The dossier's geography panel shows a choropleth when both the `sf`
package and a geographic reference dataset are available; otherwise it
falls back to a plain bar breakdown by PC value, exactly as if this
feature did not exist. EpiSODIC ships a Netherlands postcode default
(`inst/extdata/geo_postcodes4_nl.rds`, geometry only, sourced from
`certegis` under the same GPL-2 licence - see `data-raw/
geo_postcodes4_nl.R` for provenance), but geography is not
Netherlands-specific: point `EPISODIC_GEO_DATA` at your own `.rds` file
holding an [`sf`](https://r-spatial.github.io/sf/) object with a `pc`
column (matching whatever your own `episodic_case.pc` values are -
postcodes, zip codes, municipality codes, anything) and a `geometry`
column, and it is used instead. See `R/geo_data.R` for the exact
contract.

A second, independent layer can be drawn on top for orientation - region
outlines (provinces, municipalities, whatever is useful), colour but no
fill, a thicker line than the choropleth itself. Point
`EPISODIC_GEO_DATA_OVERLAY` at an `.rds` file holding an `sf` object with
just a `geometry` column (no `pc` join needed, since it carries no case
counts of its own). No default is shipped for this one - unlike the
postcode default above, region boundaries are too jurisdiction-specific
to guess a sensible default for.

### Custom report templates (optional)

`episodic_report_render()` renders `inst/report/cluster_report.qmd`
(embedded as self-contained HTML via Quarto) unless `EPISODIC_QUARTO_REPORT`
points at an operator's own `.qmd` file, in which case that is used
instead - for an organisation that wants its own letterhead, section order,
or house style. A custom template only needs to `readRDS(params$data_path)`
and read from the same list the shipped one does (`obj`, `epi_curve`,
`trend`, `linelist`, `timeline`, `similar`, `small_count_threshold`,
`rendered_at`, `lang`, `package_version`); see the shipped template for
the exact shape, including how it calls `episodic_tr(..., lang = d$lang)`
for a bilingual report.

## What a run reports

Every `episodic_run_cron()` call records what each feed actually
delivered, so "did last night's extract arrive in full?" is answerable
without re-running anything. `episodic_detection_run` carries
`n_cases_supplied`, `n_cases_deduplicated`, `n_cases_inserted`,
`n_denominators_written`, `n_activity_supplied`, `n_activity_written` and
`n_activity_skipped` alongside the detection counts, and the dashboard's
Activity screen shows them under each run.

The run's `status` distinguishes three outcomes:

| Status | Meaning |
|---|---|
| `success` | Everything supplied was loaded. |
| `partial` | The run completed and its detections are valid, but rows of an optional feed were skipped - today that means institution activity whose `institution_key` matched no known institution. |
| `failed` | Nothing was written. The whole run is one transaction, so a failure leaves no partial state and is always safe to retry; `error_text` holds the reason. |

The distinction that matters: **structural problems fail the run, and
row-level facts are counted and reported, but never silently dropped.** A
malformed feed - a missing column, a value outside the allowed set, a
date that does not parse, a duplicate `source_key` - stops the run before
anything is written, and says which column and which values were wrong.
A run that quietly loaded 10% of your patient-days is the one outcome the
design refuses to report as green.

Both `success` and `partial` are usable runs, and the dashboard reads
from the most recent of either.

## Database backend

`EPISODIC_DB` (and every `db_path` argument that falls back to it) accepts
either of two things:

- a filesystem path, opened as a SQLite database (the default, and what
  `episodic_demo()` uses); or
- a `mysql://user:password@host:port/dbname` DSN, opened against a
  MariaDB or MySQL server instead. Build one with `episodic_db_dsn_mariadb()`
  (or its alias `episodic_db_dsn_mysql()`) rather than assembling the
  string by hand, so that credentials containing `:`, `@` or `/` are
  URL-encoded correctly:

```r
Sys.setenv(EPISODIC_DB = EpiSODIC::episodic_db_dsn_mariadb(
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

## Accounts

Read access is anonymous - the app opens read-only for anyone who reaches
it. Signing in is only needed to classify a
cluster, and there is deliberately no in-app account management screen:
the four assessor accounts are provisioned by whoever administers the
database, not created by assessors themselves or by the app.

```r
Sys.setenv(EPISODIC_DB = "/path/to/episodic.sqlite")  # or pass db_path explicitly below

episodic_provision_user(
  username = "jdoe", full_name = "Jane Doe", email = "j.doe@example.org",
  password = "a-temporary-password"
)
```

`episodic_provision_user()` takes `db_path` (defaulting to `EPISODIC_DB`,
see "Environment variables" below), not an open connection - it opens and
closes its own via `episodic_db_open()`, so provisioning an account is one
call at the console. The account is created with `must_change = TRUE`, so
its first real sign-in forces the holder to set their own password before
continuing.

## Environment variables

Every `EPISODIC_*` variable is optional; each entry point works fine with
the equivalent argument passed explicitly instead. Environment variables
exist so an operator can configure a running instance (a systemd unit, a
Docker container) without editing R code.

| Variable                    | Used by                                                              | Meaning                                                                                                                                                                                                                                                                              |
|-----------------------------|----------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `EPISODIC_DB`               | `episodic_run_app()`, `episodic_provision_user()` (`db_path` argument) | Path to the instance's SQLite database, or a `mysql://` DSN pointing at a MariaDB/MySQL database instead - see "Database backend" above.                                                                                                                                             |
| `EPISODIC_LANGUAGE`         | every `lang` argument - `episodic_run_app()`, `episodic_demo()`, `episodic_report_render()`, `episodic_tr()` and every renderer beneath them | Dashboard/report language: `nl`, `en`, `es`, `fr`, `de`, `zh`, `hi`, or `ar`. Defaults to `en` if unset. Fixed for the whole running app - there is no in-app language switcher.                                                                                                       |
| `EPISODIC_CONFIG`           | `episodic_run_cron()` (`episodic_config_path` argument)                | Path to an instance override of detection configuration (pathogen thresholds, `same_place`/`rare_trigger`/Farrington settings), overlaid key-by-key on `inst/config/default.yaml`'s shipped defaults.                                                                                |
| `EPISODIC_PALETTE_CONFIG`   | `episodic_palette()` (`palette_config_path` argument)                 | Path to an instance override of the UI colour palette, overlaid key-by-key on `inst/config/palette.yaml`'s shipped defaults. Deliberately separate from `EPISODIC_CONFIG`: colour is a display concern, never part of `episodic_config_hash()`'s detection-reproducibility guarantee. |
| `EPISODIC_GEO_DATA`         | `episodic_geo_source_resolve()` (`path` argument)                     | Path to an `.rds` file holding an operator's own geographic reference data (an `sf` object with `pc`/`geometry` columns), overriding the shipped Netherlands postcode default. See "Geographic reference data" above.                                                                |
| `EPISODIC_GEO_DATA_OVERLAY` | `episodic_geo_overlay_resolve()` (`path` argument)                    | Path to an `.rds` file holding an optional region-outline overlay (an `sf` object with just a `geometry` column), drawn on top of the choropleth. No default. See "Geographic reference data" above.                                                                                 |
| `EPISODIC_QUARTO_REPORT`    | `episodic_report_render()` (`qmd_path` argument)                      | Path to an operator's own Quarto report template, overriding the shipped `inst/report/cluster_report.qmd`. See "Custom report templates" above.                                                                                                                                      |

## Licence

GPL-2.
