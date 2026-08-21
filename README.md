# EpiSODIC

[![R-CMD-check](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml)

#### **Epi**demiological **S**ignal **O**bservation, **D**etection,
**I**dentification, and **C**lassification

EpiSODIC is an outbreak cluster detection and assessment system for a
department of medical epidemiology. It ingests laboratory-confirmed
infections, detects statistical and rule-based aberrations, reconciles them
into persistent clusters, and gives a small board of epidemiologists a
dossier to assess each one, with a full audit trail and outbreak reports for
clinical colleagues.

The engine and the instance it runs against are kept separate: this
repository is open-source software with no data and no site-specific
configuration.

<!--
Screenshots below are sourced from data-raw/screenshots/ (git-tracked,
.Rbuildignore'd so they never ship in the CRAN tarball) - see
data-raw/README.md for how to regenerate them against episodic_demo().
-->
<p align="center">
  <img src="data-raw/screenshots/dossier.png" alt="A cluster dossier: epi curve, trend, and an automatically generated plain-language interpretation" width="800">
  <br>
  <em>The cluster dossier - epi curve, multi-year trend, and an automatically generated interpretation of the evidence.</em>
</p>
<p align="center">
  <img src="data-raw/screenshots/performance.png" alt="The Performance screen: positive predictive value per detector and organism" width="800">
  <br>
  <em>The Performance screen - positive predictive value per detector and organism, and detection timeliness, from the stored verdicts.</em>
</p>

## Capabilities

The detection engine, interface, assessment and authentication
workflow, reporting, and analytical panels are all implemented and
tested, including a Performance screen for evidence-based tuning.
Detection thresholds and priority score weights are configurable per
instance (`inst/config/default.yaml`, `EPISODIC_CONFIG`), so a department
can tune them against its own signal volume as its evidence base grows.

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
any other data source itself. That is deliberately the operator's own step,
run before EpiSODIC: extract from wherever your data lives, transform into
the shape below, then call `episodic_run_cron()` with a function that returns
it. This keeps the engine reusable by any laboratory, not only Certe's.

```r
episodic_run_cron(
  db_path = "/path/to/episodic.sqlite",
  ingest_source_fn = function() my_extract_and_transform_function(),
  denominator_source_fn = NULL  # optional, see "Positivity metadata" below
)
```

### Cases (mandatory)

One row per confirmed-positive laboratory result. This is the complete,
allow-listed column set (`episodic_ingest_columns`); an ingestion source
returning any column outside this list, or missing one from it, is
rejected:

| Column                     | Meaning                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|----------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source_key`               | A unique identifier for the row in your own source system, so a later re-ingest of the same extract cannot create duplicate cases.                                                                                                                                                                                                                                                                                                                    |
| `patient_key`              | A stable, pseudonymised patient identifier. This is what deduplication and episode grouping key on: without it, EpiSODIC cannot tell that two isolates belong to the same patient, and every isolate would be treated as its own case. Never displayed as-is in the interface.                                                                                                                                                                        |
| `sample_date`              | The anchor date every detector, trend, and report is built against. If your system falls back to a receipt date when sample date is unfilled, that fallback should already have happened before this row reaches EpiSODIC.                                                                                                                                                                                                                            |
| `receipt_date`             | When the result was received. Stored for provenance/audit, kept separate from `sample_date` - EpiSODIC deliberately does not use it to measure reporting delay, since a lab's own receipt-date field can itself silently be a stand-in for a missing sample date; reporting completeness is instead measured empirically, from how a stream's case counts change across successive detection runs.                                                    |
| `pathogen`                 | The organism as your lab reports it, as free text. **Not** resolved against any taxonomy, since EpiSODIC has to detect clusters of anything a lab reports. The same underlying isolate can appear more than once under different `pathogen` values when that is epidemiologically useful - an ETEC isolate reported as both `"Escherichia coli"` and `"ETEC"`, so each is watched on its own. This is your transform step's decision, not EpiSODIC's. |
| `care_line`                | `first`, `second`, `other`, or `unknown` - which part of the health system the case came from.                                                                                                                                                                                                                                                                                                                                                        |
| `institution_key`          | A stable identifier for the institution, hashed internally so a later rename does not fracture history.                                                                                                                                                                                                                                                                                                                                               |
| `institution_display_name` | The human-readable name shown in the interface.                                                                                                                                                                                                                                                                                                                                                                                                       |
| `institution_type`         | One of `hospital`, `ltc_institution`, `gp_municipality`, `ooh_service`, `other`.                                                                                                                                                                                                                                                                                                                                                                      |
| `municipality`             | The institution's municipality, used for `gp_municipality`-type rows (see below) and as a coarse geographic fallback.                                                                                                                                                                                                                                                                                                                                 |
| `ward`                     | Only meaningful (and only used) for hospitals: this is what the `same_place` detector watches at ward level.                                                                                                                                                                                                                                                                                                                                          |
| `specialism`               | The treating specialism, shown on the line list for context; not itself a detection dimension.                                                                                                                                                                                                                                                                                                                                                        |
| `pc`                       | The patient's postcode (or equivalent - see "Geographic reference data" below for how coarse or fine this can be). Drives the geography panel and the choropleth, and the concentration measure ("how localised is this cluster") that feeds the priority score.                                                                                                                                                                                      |
| `sex`                      | The patient's sex. Feeds the demography panel's age/sex pyramid, one of the interpretation engine's own evidence dimensions (a cluster's demography shifting from a department's usual baseline is itself a signal worth surfacing).                                                                                                                                                                                                                  |
| `age`                      | The patient's age at sample date. Same role as `sex`: demography panel and interpretation, not a detection input.                                                                                                                                                                                                                                                                                                                                     |

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
situation, simply never call this, and positivity panels stay blank for
your streams. See `episodic_denominator_source_synthetic()` for a worked
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
perfectly synchronised. See
`episodic_synthetic_institution_activity_source()` for a worked example.
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
instead - for a department that wants its own letterhead, section order,
or house style. A custom template only needs to `readRDS(params$data_path)`
and read from the same list the shipped one does (`obj`, `epi_curve`,
`trend`, `linelist`, `timeline`, `similar`, `small_count_threshold`,
`rendered_at`, `lang`, `package_version`); see the shipped template for
the exact shape, including how it calls `episodic_tr(..., lang = d$lang)`
for a bilingual report.

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
| `EPISODIC_CONFIG`           | `episodic_run_cron()` (`episodic_config_path` argument)                | Path to an instance override of detection configuration (pathogen thresholds, `same_place`/`rare_trigger`/Farrington settings), overlaid key-by-key on `inst/config/default.yaml`'s shipped defaults.                                                                                |
| `EPISODIC_PALETTE_CONFIG`   | `episodic_palette()` (`palette_config_path` argument)                 | Path to an instance override of the UI colour palette, overlaid key-by-key on `inst/config/palette.yaml`'s shipped defaults. Deliberately separate from `EPISODIC_CONFIG`: colour is a display concern, never part of `episodic_config_hash()`'s detection-reproducibility guarantee. |
| `EPISODIC_GEO_DATA`         | `episodic_geo_source_resolve()` (`path` argument)                     | Path to an `.rds` file holding an operator's own geographic reference data (an `sf` object with `pc`/`geometry` columns), overriding the shipped Netherlands postcode default. See "Geographic reference data" above.                                                                |
| `EPISODIC_GEO_DATA_OVERLAY` | `episodic_geo_overlay_resolve()` (`path` argument)                    | Path to an `.rds` file holding an optional region-outline overlay (an `sf` object with just a `geometry` column), drawn on top of the choropleth. No default. See "Geographic reference data" above.                                                                                 |
| `EPISODIC_QUARTO_REPORT`    | `episodic_report_render()` (`qmd_path` argument)                      | Path to an operator's own Quarto report template, overriding the shipped `inst/report/cluster_report.qmd`. See "Custom report templates" above.                                                                                                                                      |

## Licence

GPL-2.
