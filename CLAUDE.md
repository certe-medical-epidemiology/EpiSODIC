# EpiSODIC

EpiSODIC stands for: Epidemiological Signal Observation, Detection, Identification, and Classification.

This is an R package that detects aberrations in laboratory-confirmed infections, reconciles them into persistent outbreaks and epidemics, and gives epidemiologists a Shiny dashboard to assess each one, with a full audit trail and outbreak reports.

**Status: not released, and not in production anywhere.** EpiSODIC is in development. It has not been shipped to any laboratory, no epidemiologist relies on it, and no outbreak decision is taken on its output. The instances that exist, including the maintainer's own with real case data, are development and test instances. So:

- A change in what the dashboard shows affects nobody's work. Do not frame changes in terms of users, colleagues or deployments that would be affected; there are none.
- Schema migrations are still required and must still work (`episodic_db_migrations()`, never removing old ones), because the product is built for the day it is released and every instance after it.
- The quality standard below applies in full regardless: the package is built to production grade for when it is released, not to "beta grade" because it is not yet.

## Quality standard

EpiSODIC is designed to run at any laboratory, in any country, against any set of pathogens, with no dependency on any one laboratory information system or data warehouse:

- No shortcuts, no placeholder logic, no "good enough for now". If a proper implementation is more effort than a shortcut, implement it properly or check with the user.
- No silent failures. Every error path must be handled explicitly and must fail loudly, never fail quietly and produce a plausible-looking wrong result.
- **Absence of a measurement is never a measurement of zero.** This is the failure mode this codebase is most prone to. A zero is a legal value of the quantity, so substituting one for "cannot be computed" produces a number that reads as a finding. Watch `%||%` in particular: this package's own (`R/interpretation.R`) swallows `NA` as well as `NULL`, so `x %||% 0` turns an unmeasured quantity into a measured zero. When a quantity cannot be computed, drop the component, skip the fragment, or return `NULL`, never substitute a zero and carry on.
- No hidden assumptions about a specific laboratory's data structure, coding system, or naming convention. Anything laboratory-specific must be configurable, not hardcoded.
- No untested code paths merged into main. Every function that touches detection logic, data transformation, or reporting must have accompanying tests before it is considered complete, and must be placed in a separate branch WITH a PR.
- No inconsistent interfaces. Function signatures, argument naming, return types, and error conventions must be uniform across the entire codebase.

## Architecture

### Pipeline

```
case data (data frame)
  -> episodic_run_cron()          # scheduled detection run
    -> validate + deduplicate
    -> detect (4 detectors, per stream)
    -> reconcile (match detections to persistent outbreaks/epidemics)
    -> route on scale (outbreak at L1-L3, epidemic at L4-L5)
    -> score priority
    -> link outbreaks to concurrent epidemics
    -> suppress lattice duplicates (continuous across the scale boundary)
    -> notify (if configured)
  -> episodic_run_app()           # Shiny dashboard
    -> epidemiologist assesses outbreaks (Outbreaks screen)
    -> epidemiologist assesses epidemics and records declarations (Epidemics screen)
    -> outbreak reports rendered
```

### Detectors

Four independent detectors, each producing detections per stream:

| Detector | Method | File |
|---|---|---|
| Farrington | Improved Farrington (surveillance::farringtonFlexible) | `R/detect_farrington.R` |
| same_place | Rule-based: N cases at one location within K days | `R/detect_same_place.R` |
| rare_trigger | Single-case alert for curated rare pathogens | `R/detect_rare_trigger.R` |
| MEM | Moving Epidemic Method seasonal threshold (mem::memmodel), derived anchor and eligibility | `R/detect_mem.R` |

Both rule-based detectors and Farrington are bounded by configured lookback windows and report only hits whose most recent case falls inside. Unbounded, they re-emit the whole case history on every run, resetting `runs_since_detected` and making `reconciliation.close_after_runs` unreachable.

All bounds are lifted for exactly one run: the first against a database (`episodic_run_is_backfill()`). A backfilled cluster carries `opened_in_backfill` as a flag, deliberately not a third `origin`, because `episodic_db_clusters_for_stream()` and `episodic_db_clusters_for_suppression()` select on `origin = 'detected'` and a third value would exclude them from reconciliation and suppression.

MEM derives its season anchor and seasonality eligibility from each stream's own data rather than from per-pathogen configuration. Seasons are full-year (52 or 53 weeks) with no off-season gap. MEM runs at L4 and L5 (configurable via `mem.levels`).

Each detector's config section carries `enabled`, read through `episodic_detector_enabled()`. It lives in config rather than as an argument because which detectors ran changes what a run computes, and so must be inside `config_hash`.

### Streams and the lattice

A "stream" is a unique surveillance unit: a (pathogen, level, location) tuple. Levels form a geographic lattice from finest to coarsest: `pathogen_ward`, `pathogen_institution`, `pathogen_area`, `pathogen_province`, `pathogen_region`.

A configurable boundary (`scale.epidemic_levels`) splits the lattice: streams at or above the boundary produce **epidemics**, those below produce **outbreaks**. Suppression is continuous across the scale boundary. Outbreaks are linked to concurrent epidemics via `episodic_cluster_link`, defined on pathogen, time overlap and geographic nesting, never on case-set containment.

Stream keys are SHA-1 hashes computed in `R/lattice_stream_key.R`.

The geography a run uses is resolved once from that run's own config and passed down. Nothing is hardcoded to one country: L5 and L3 come from `config$geography`; L4 is an operator-supplied postcode-to-province CSV (`EPISODIC_PC_PROVINCE_MAP`); `EPISODIC_GEO_DATA` has no default.

### Database

Single schema in `inst/sql/schema.sql` (SQLite dialect), adapted at load time for MariaDB/MySQL. The MariaDB adapter derives table-level `FOREIGN KEY` clauses from inline `REFERENCES` (MySQL silently discards inline references), and applies the schema with `FOREIGN_KEY_CHECKS = 0`.

The schema is versioned (`episodic_schema_version` in `R/schema_migrate.R`). `episodic_run_cron()` auto-migrates via `episodic_run_cron_connect()` unless `database.auto_migrate` is `false`. Any change to `inst/sql/schema.sql` means bumping the version constant and adding a matching entry to `episodic_db_migrations()`, a function `(con, dialect)` that is idempotent, runs inside a transaction, and never drops or rewrites data. Exception: a column holding no data on any row may be dropped via `episodic_db_drop_empty_column()`. Never remove an old migration.

Write ownership is strict: cron-owned tables are written only by `episodic_run_cron()`, app-owned tables only by the Shiny app (append-only, event-sourced). Two deliberate exceptions: `episodic_add_manual_cluster()` writes to cron-owned tables for `origin = 'manual'` clusters, and `episodic_add_user()` writes to `episodic_app_user`.

### Configuration

YAML-based with recursive merge. `inst/config/episodic_default_config.yaml` ships defaults; an operator's config (`EPISODIC_CONFIG`) overlays key-by-key. The resolved config is hashed (SHA-1 over canonical JSON) and stored per run. `episodic_config_unhashed_sections` (`notifications`, `access`, `report`, `database`) are stripped before hashing.

Validation (`episodic_config_validate()`) checks against shipped defaults: unknown keys, wrong types, or nulls where values are needed stop the run. Adding a new key means adding it to the defaults, or to `episodic_config_open_sections` for operator-named children. A nullable setting must be listed in `episodic_config_nullable_keys`.

`EPISODIC_CONFIG`, `EPISODIC_PATHOGEN_CONFIG` and `EPISODIC_QUARTO_REPORT` set to a nonexistent path are errors. `EPISODIC_STYLE` is the exception: a missing or invalid palette is announced (Info screen, log warning) but falls back to shipped defaults, because refusing would stop the dashboard over a colour.

The same palette styles everything: dashboard, reports, emails. A colour written literally into an email or report is a bug. Emails are styled inline from `episodic_mail_style()`.

Pathogen-specific parameters live in `inst/config/episodic_default_pathogen_config.csv`. An operator's CSV (`EPISODIC_PATHOGEN_CONFIG`) overlays row-by-row: non-NA values override, unlisted pathogens keep shipped defaults.

### Internationalisation

Eight languages (en, ar, nl, fr, de, hi, zh, es) plus regional variants. The CSS uses logical properties throughout (`margin-inline-start`, not `margin-left`); a physical `left`/`right` in `episodic.css` is a bug.

Every number a reader sees goes through `episodic_format_number()`, driven by per-language `misc.decimal.mark`/`misc.thousands.*` keys, never `options(OutDec)` or the system locale. A `format(x, big.mark = ",")` reaching a screen is a bug; `episodic_css_pct()` is the one exception (machine-read CSS value). Identifiers (`O-123`, `E-45`, page numbers, versions) are rendered as-is, never grouped.

Regional variants carry **only what differs** from their base language. `en` is British English, `es` is Spain's Spanish; `en-GB`/`es-ES` are aliases. Unshipped regions (`nl-BE`) resolve to their language.

Every language file lists keys sorted by byte order (`sort(method = "radix")`); `test-i18n.R` enforces this. A new key goes at its sorted place in every file it belongs to.

### Keys that must agree across feeds

- **`institution_key`** is hashed by `episodic_institution_key_hash()` before storage; every feed must supply the raw key and have it hashed to match.
- **The episode key** is built by `episodic_case_group_key()` with a control-character separator. `episodic_cases_deduplicate()` and `episodic_db_last_case_dates()` must agree on it.

### Notifications

Six channels: ntfy, SMTP, sendmail, Microsoft 365, Teams, Slack. Dispatched after the detection transaction commits, never inside it. Errors caught and logged, never propagated.

Email bodies can be operator Quarto templates (`EPISODIC_MAIL_TEMPLATE_NEW_CLUSTERS`, `EPISODIC_MAIL_TEMPLATE_REPORT`); only `.qmd` accepted. A template that fails falls back to the built-in body and logs a `danger` line.

### Roles

- `epidemiologist`: read + write (assess, classify, close, mute, declare, render reports)
- `viewer`: read-only (sees everything but cannot record assessments or declarations)

`access.require_login` ships `false`; fails closed on anything it cannot read as `false`. Both sign-in outcomes are recorded (success as event, failure in `episodic_app_login_failure`). The auth endpoint reveals nothing about which failure reason applied.

### Measuring detection

`episodic_validate_detection()` replays synthetic cases week by week against a throwaway SQLite database, matching clusters to seeded outbreaks **on case sets, in both directions**. Ground truth comes from `episodic_synthetic_ground_truth()`, not naming conventions.

Rules: a metric with no denominator is `NA`, never 0; an undetected outbreak is right-censored, never dropped; a threshold sweep re-matches a finished replay (`episodic_validate_rethreshold()`) rather than re-running detection. If a result is unflattering, it is reported.

The multi-seed study and operating-point sweep live in `data-raw/validation/run_study.R` (in `.Rbuildignore`): nothing that takes minutes may reach the suite, CI, an example or a vignette.

## Development

If R is available, find the appropriate functions below. If not, consult the user on whether the user will run the functions themselves on their computers instead.

### Running tests

```bash
Rscript -e 'devtools::test()'
```

All tests run against temporary SQLite databases (`helper-db.R`). `test-mariadb_live.R` skips unless `EPISODIC_TEST_MARIADB_DSN` is set.

### Building documentation

```bash
Rscript -e 'devtools::document()'
Rscript -e 'styler::style_pkg()'
```

### R CMD check

```bash
R CMD build . && R CMD check EpiSODIC_*.tar.gz
```

There is a pre-existing NOTE about Author/Maintainer fields because the package uses `Authors@R`. This is not a real problem.

### NEWS.md

One version heading, then `## New`/`## Changed`/`## Fixed` sections, each a flat list of single-line bullets. No sub-bullets, no elaboration, no multi-sentence entries.

### pkgdown

Do not regenerate the site; a GitHub Action does that. But `_pkgdown.yml` groups every exported topic, so new exports must be added to it or the build fails.

### Code style

- **File header**: the standard Certe GPL-2 banner (17-line comment block) at the top of every R file.
- **Multi-line function signatures**: hanging-indent, not single-indent. First argument on the same line as `function(`, continuation lines align under it, `) {` on the last argument's line. This is stable across `styler::style_pkg()` runs. Renaming a function means realigning its continuation lines by hand.
- **Comments describe the code as it is, never as it was.** No "this used to", "was wrong until", "the exact bug this fixes". Name the alternative and its cost, present tense. Test comments state the invariant, not the regression. History belongs in the commit message, PR and `NEWS.md`.
- **Internal functions**: use `@keywords internal` and `@noRd`. Reference functions as \`some_function()\`, not `[some_function()]`.
- **Logging**: `episodic_trace()` for cron-side, `message()` for interactive. Severity (`plain`, `warn`, `danger`) is for structural problems only, never phase headings, counts or decoration.
- **Database**: all SQL inline, parameterised, through `episodic_db_get_query()`/`episodic_db_execute()` only, never `DBI::dbGetQuery()`/`DBI::dbExecute()` directly (MariaDB re-entrancy kills the R process). `test-db_reentrancy.R` enforces this.
- **Dependencies**: hard dependencies in Imports, optional in Suggests. `bslib`, `cli`, `commonmark`, `htmltools`, `jsonlite` live in Suggests (guaranteed by `shiny`'s own Imports) but must be called as `pkg::fun()`, never `@importFrom`. `rlang` stays in Imports because `R/app_charts.R` imports `.data` for `ggplot2::aes()`.
- **Config hash**: `episodic_config_unhashed_sections` strips secrets and operationally irrelevant sections before hashing. New sections with secrets or no detection relevance must be added there.
- **Anonymous access**: `episodic_app_access_granted()` gates every output and data-bearing observer server-side. Any new output reading surveillance data must go behind the same gate.
- **Error handling**: notification and report-rendering errors must not propagate to the cron pipeline. Wrap in `tryCatch` and log via `episodic_trace()`.
- **Event sourcing**: the app only inserts into `episodic_assessment_event`, never updates or deletes.

### Environment variables

See `vignette("environment-variables")` for the full reference.

### Key invariants

- Detection runs are transactional: full commit or nothing.
- Notifications fire after commit, never inside the transaction.
- `config_hash` is deterministic regardless of key order or platform.
- Stream keys are deterministic SHA-1 hashes.
- Cluster IDs are database-assigned; cluster identity is stream + case-free-days gap logic. Rendered as `O-{id}` / `E-{id}`.
- The scale boundary is configuration, not a constant.
- A seasonal epidemic closes on post-epidemic threshold or trough backstop; non-seasonal on case-free-days. Declarations are human acts, never automatic.
- The app never writes to cron-owned tables; the cron never writes to app-owned tables.
