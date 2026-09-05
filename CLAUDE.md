# EpiSODIC

EpiSODIC stands for: Epidemiological Signal Observation, Detection,
Identification, and Classification.

This is an R package that detects aberrations in laboratory-confirmed
infections, reconciles them into persistent clusters, and gives
epidemiologists a Shiny dashboard to assess each one, with a full audit
trail and outbreak reports.

## What this package is for

EpiSODIC is a complete and automated outbreak detection and assessment
system. It is designed to run at any laboratory, in any country, against
any set of pathogens, with no dependency on any one laboratory
information system or data warehouse. Therefore, the aimed quality
standard admits no concessions. Every component, from data ingestion to
signal detection to reporting, must be production-grade, and must remain
so after every change:

- No shortcuts, no placeholder logic, no “good enough for now”. If a
  proper implementation is more effort than a shortcut, implement it
  properly or check with the user.
- No silent failures. Every error path must be handled explicitly and
  must fail loudly, never fail quietly and produce a plausible-looking
  wrong result.
- No hidden assumptions about a specific laboratory’s data structure,
  coding system, or naming convention. Anything laboratory-specific must
  be configurable, not hardcoded.
- No untested code paths merged into main. Every function that touches
  detection logic, data transformation, or reporting must have
  accompanying tests before it is considered complete, and must be
  placed in a separate branch WITH a PR, so every change regarding these
  items must automatically be put into a PR.
- No inconsistent interfaces. Function signatures, argument naming,
  return types, and error conventions must be uniform across the entire
  codebase, as if written by a single disciplined author, not accreted
  piecemeal.

The operator provides case data in a documented format; EpiSODIC handles
everything from statistical detection through cluster reconciliation to
dashboard presentation and outbreak reporting.

The dashboard and reports are available in English, Arabic, Dutch,
French, German, Hindi, Mandarin Chinese, and Spanish.

## Architecture

### Pipeline

    case data (data frame)
      -> episodic_run_cron()          # scheduled detection run
        -> validate + deduplicate
        -> detect (4 detectors, per stream)
        -> reconcile (match detections to persistent clusters)
        -> score priority
        -> suppress lattice duplicates
        -> notify (if configured)
      -> episodic_run_app()           # Shiny dashboard
        -> epidemiologist assesses clusters
        -> outbreak reports rendered

### Detectors

Four independent detectors, each producing detections per stream:

| Detector | Method | File |
|----|----|----|
| Farrington | Improved Farrington (surveillance::farringtonFlexible) | `R/detect_farrington.R` |
| same_place | Rule-based: N cases at one location within K days | `R/detect_same_place.R` |
| rare_trigger | Single-case alert for curated rare pathogens | `R/detect_rare_trigger.R` |
| MEM | Moving Epidemic Method seasonal threshold (mem::memmodel) | `R/detect_mem.R` |

### Streams and the lattice

A “stream” is a unique surveillance unit: a (pathogen, level, location)
tuple. Levels form a geographic lattice from finest to coarsest:

- `pathogen_ward` (finest)
- `pathogen_institution`
- `pathogen_area`
- `pathogen_province`
- `pathogen_region` (coarsest)

Detection runs against every eligible stream independently. After
detection, lattice suppression removes redundant signals (a
hospital-level cluster that is entirely explained by a ward-level
cluster in the same hospital).

Stream keys are SHA-1 hashes of (pathogen, level, institution_id, ward,
region_code), computed in `R/lattice_stream_key.R`.

### Database

Single schema in `inst/sql/schema.sql`, written in SQLite dialect.
Adapted at load time for MariaDB/MySQL (there is no separate schema
file). Key tables:

| Table | Owner | Purpose |
|----|----|----|
| `episodic_stream` | cron | Surveillance units |
| `episodic_detection_run` | cron | One row per cron invocation |
| `episodic_detection` | cron | Individual detector firings |
| `episodic_cluster` | cron | Persistent clusters (reconciled) |
| `episodic_cluster_case` | cron | Cases assigned to clusters |
| `episodic_cluster_state` | cron | Derived state (open/closed/stale) |
| `episodic_assessment_event` | app | Epidemiologist assessments (append-only) |
| `episodic_app_user` | app | Dashboard accounts |
| `episodic_case` | cron | Deduplicated case records |
| `episodic_institution` | cron | Institution reference data |
| `episodic_cluster_note` | app | Per-cluster free-text notes (append-only) |
| `episodic_cluster_manual_case` | [`episodic_add_manual_cluster()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_manual_cluster.md) | Case-level detail for `origin = 'manual'` clusters only |

Write ownership is strict: cron-owned tables are written only by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
app-owned tables only by the Shiny app. The app never updates or
deletes, only inserts (event-sourced).

One deliberate exception:
[`episodic_add_manual_cluster()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_manual_cluster.md)
(in `R/cluster_manual.R`) is a third, console-invoked write path into
`episodic_stream`, `episodic_cluster` and `episodic_cluster_manual_case`
(all otherwise cron-owned), for clusters detected by another algorithm
or system rather than by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
itself - exactly the same kind of exception
[`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md)
already is for the app-owned `episodic_app_user` table. Such a cluster
gets `origin = 'manual'` on `episodic_cluster` and is excluded from
`episodic_reconcile_stream()`’s matching and auto-closure (see
`episodic_db_clusters_for_stream()`); its case-level detail, if any,
lives in `episodic_cluster_manual_case`, never in `episodic_case`, so it
can never reach denominators, line lists or patient search.

### Configuration

YAML-based, with recursive merge.
`inst/config/episodic_default_config.yaml` ships documented defaults. An
operator’s instance config (pointed at by `EPISODIC_CONFIG`) overlays
key-by-key. The resolved configuration is hashed (SHA-1 over canonical
JSON) and stored on every run for reproducibility. `notifications` and
`access` are deliberately excluded from the hash (see
`episodic_config_unhashed_sections`): both govern how the instance is
operated rather than what a run computes, and notification settings
additionally contain secrets that must never reach `config_snapshot`.

Key config sections: `reconciliation`, `eligibility`,
`effect_size_floor`, `same_place`, `farrington`, `mem`, `rare_trigger`,
`priority_score`, `notifications`, `suppression`, `access`.

Pathogen-specific parameters (episode length, serial interval, severity
weight) live in `inst/config/episodic_default_pathogen_config.csv`.

### Notifications

Configured under the `notifications` key in the instance YAML. Six
channels: ntfy, SMTP, sendmail, Microsoft 365, Teams, Slack. Dispatched
after the detection transaction commits, never inside it. Errors are
caught and logged, never propagated. Implementation split across
`R/notify.R` (dispatcher, message building) and `R/notify_channels.R`
(per-channel send functions).

### Roles

Two roles for dashboard access:

- `epidemiologist`: read + write (assess clusters, classify, close,
  mute, render reports)
- `viewer`: read-only (sees everything including patient-level detail,
  but cannot record assessments)

Accounts are added via
[`episodic_add_user()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_add_user.md)
at the R console; there is also in-app account management.

## File layout

    R/
      run_cron.R          # cron entry point, episodic_trace(), the full pipeline
      run_app.R           # Shiny app entry point
      config.R            # YAML config: resolve, merge, canonicalise, hash
      cases.R             # case data requirements and deduplication entry
      cases_check.R       # episodic_check_cases() validation
      cases_dedup.R       # episode deduplication (via AMR::get_episode)
      cases_load.R        # loading cases into the database
      reconcile.R         # match detections to persistent clusters
      reconcile_closure.R # auto-close stale clusters
      reconcile_suppress.R # lattice suppression
      cluster_manual.R    # episodic_add_manual_cluster() - clusters from other systems
      detect_*.R          # the four detectors
      notify.R            # notification dispatcher and message building
      notify_channels.R   # per-channel send functions (ntfy, smtp, etc.)
      score_priority.R    # composite priority score
      db_cron_write.R     # all cron-side DB writes
      db_read.R           # all DB reads (shared by cron and app)
      db_app_write.R      # all app-side DB writes (append-only)
      schema_migrate.R    # schema creation and migration
      app_server.R        # Shiny server
      app_server_notes.R  # wires the cluster notes save button
      app_ui.R            # Shiny UI
      app_dossier.R       # cluster dossier (the main assessment screen)
      app_pathogen.R      # pathogen overview panel
      app_charts.R        # reusable chart components
      app_widgets.R       # reusable UI widgets
      app_read.R          # data loading for the dashboard
      i18n.R              # translation system (episodic_tr)
      interpretation.R    # AI/template-based narrative summaries
      report_render.R     # Quarto outbreak report rendering
    inst/
      config/episodic_default_config.yaml       # shipped detection defaults
      config/episodic_default_style.yaml       # shipped colour palette defaults
      config/episodic_default_pathogen_config.csv # per-pathogen parameters
      sql/schema.sql            # database schema (SQLite dialect)
      app/                      # Shiny app assets (CSS, JS)
      i18n/                     # translation JSON files (en, nl, de, fr, es, ar, hi, zh)
      report/                   # Quarto report template
    tests/testthat/             # ~480 test blocks across 40 files
    vignettes/                  # 7 vignettes

## Development

If R is available, find the appropriate functions below. If not, consult
the user on whether the user will run the functions themselves on their
computers instead.

### Running tests

``` bash
Rscript -e 'devtools::test()'
```

All tests run against temporary SQLite databases created in
`helper-db.R`. No external services or credentials are needed.

### Building documentation

``` bash
Rscript -e 'devtools::document()'
# for linting:
Rscript -e 'styler::style_pkg()'
```

This regenerates `man/` and `NAMESPACE` from roxygen2 comments. The
package uses `Authors@R` in DESCRIPTION (not separate Author/Maintainer
fields).

### R CMD check

``` bash
R CMD build . && R CMD check EpiSODIC_*.tar.gz
```

There is a pre-existing NOTE about Author/Maintainer fields because the
package uses `Authors@R`, which requires building first. This is not a
real problem.

### NEWS.md

Keep entries the way they already are: one version heading, then
`## New`/`## Changed`/`## Fixed` sections, each a flat list of
single-line bullets. No sub-bullets, no elaboration, no multi-sentence
entries - one short line per change, stating what changed, not why.

### pkgdown site

Do not regenerate the site; a GitHub Action will do that, available in
.github/workflows.

Yet, `_pkgdown.yml` groups every exported topic into a section. When
adding a new exported function, it must be added to the appropriate
section in `_pkgdown.yml` or the build will fail with “topics missing
from index”.

### Code style

- File header: the standard Certe GPL-2 banner (17-line comment block)
  goes at the top of every R file.
- Multi-line function signatures: use hanging-indent, not
  single-indent - the first argument stays on the same line as
  `function(`, continuation lines align under it, and `) {` shares the
  line with the last argument (never on its own line). Both are
  permitted by the tidyverse style guide
  (style.tidyverse.org/functions.html), but styler \>= 1.11.0 defaults
  to single-indent with `) {` isolated, which this project does not
  want. Styler cannot be configured to prefer one over the other (raised
  and declined upstream: github.com/r-lib/styler/pull/1235) - it detects
  the shape per function from the source itself
  (`is_single_indent_function_declaration()` in styler’s
  `R/rules-indention.R`: first argument on a new line after `function(`
  means single-indent; first argument sharing that line, with
  continuation lines indented more than `2 * indent_by` i.e. more than 4
  spaces by default, means hanging-indent) and preserves whichever you
  wrote, so writing it this way is stable across `styler::style_pkg()`
  runs regardless of styler version. One real cost: renaming a function
  invalidates every continuation line’s alignment in its own signature,
  since styler will not recompute it for you - realign by hand when that
  happens.
- Internal functions: use `@keywords internal` and `@noRd` for functions
  that should not have a man page. Also, don’t reference functions as
  `[some_function()]` there, but use \`some_function()\` instead, as
  otherwise roxygen2 gives a warning.
- Logging: use `episodic_trace()` (defined in `run_cron.R`) for
  cron-side logging, [`message()`](https://rdrr.io/r/base/message.html)
  for interactive functions.
- Database: all SQL is inline (no ORM). Parameterised queries
  (`DBI::dbGetQuery(con, sql, params = ...)`) throughout, never string
  interpolation of user values.
- Dependencies: hard dependencies in Imports, optional integrations in
  Suggests. Gate optional packages at runtime with
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  or [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).
  Exception: `bslib`, `cli`, `commonmark`, `htmltools` and `jsonlite`
  are used unconditionally but live in Suggests, not Imports - `shiny`
  (a hard Import) already Imports every one of them, so they are
  guaranteed present whenever EpiSODIC is, without EpiSODIC also
  declaring them. Always call them with `pkg::fun()`, never
  `@importFrom`, since no NAMESPACE import backs them. `rlang` itself
  stays a real Import, not this exception: `R/app_charts.R` imports its
  `.data` pronoun (`@importFrom rlang .data`), which
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  only recognises as a data-mask pronoun when referenced by the bare
  symbol `.data` - `rlang::.data$col` is a different expression to
  `aes()`’s NSE and errors with “Can’t subset `.data` outside of a data
  mask context”. Every other `rlang::` call in the codebase
  (`check_installed()`, `cnd_message()`, etc.) is an ordinary function
  call and would have been fine moved to Suggests; the `.data` import is
  the one thing forcing the whole package to stay in Imports.
- Config hash: the keys in `episodic_config_unhashed_sections`
  (`notifications`, `access`) are stripped before hashing. Any new
  config section that contains secrets or is operationally irrelevant to
  detection should be added there.
- Anonymous access: `access.require_login` closes the app to visitors
  who have not signed in. It is enforced server-side, by not rendering -
  `episodic_app_access_granted()` gates every output and every
  data-bearing observer, so nothing reaches the page to be uncovered
  client-side. Any new output or observer that reads surveillance data
  must go behind the same gate. It is YAML-only, with no Settings-screen
  override, on purpose.
- Error handling: notification and report-rendering errors must not
  propagate to the cron pipeline. Wrap in `tryCatch` and log via
  `episodic_trace()`.
- Event sourcing: the app only inserts into `episodic_assessment_event`,
  never updates or deletes. Current state is derived from the event
  stream.

### Environment variables

| Variable | Purpose |
|----|----|
| `EPISODIC_DB` | Database path (SQLite) or DSN (MariaDB) |
| `EPISODIC_CONFIG` | Instance detection + notification config YAML |
| `EPISODIC_STYLE` | Instance colour palette YAML |
| `EPISODIC_LANGUAGE` | Dashboard/report language (en, ar, nl, fr, de, hi, zh, es) |
| `EPISODIC_GEO_DATA` | Geographic reference data (.rds, sf object) |
| `EPISODIC_GEO_DATA_OVERLAY` | Optional region-outline overlay (.rds) |
| `EPISODIC_PC_PROVINCE_MAP` | Postcode-to-province CSV mapping |
| `EPISODIC_QUARTO_REPORT` | Custom Quarto report template path |

See
[`vignette("environment-variables")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.md)
for the full reference.

### Key invariants

- Detection runs are transactional: either the full run commits or
  nothing does.
- Notifications fire after commit, never inside the transaction.
- `config_hash` is deterministic: same config in, same hash out,
  regardless of key order or platform.
- Stream keys are deterministic: same (pathogen, level, institution_id,
  ward, region_code) always produces the same SHA-1 key.
- Cluster IDs are database-assigned (AUTOINCREMENT); cluster identity is
  the stream + the case-free-days gap logic, not the ID.
- The app never writes to cron-owned tables; the cron never writes to
  app-owned tables.
