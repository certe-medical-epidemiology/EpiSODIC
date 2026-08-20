# EpiSODE 0.1.0 (development)

First public development version: a full outbreak cluster detection and
assessment system, from raw laboratory results to a signed-off outbreak
report, running entirely on open-source, CRAN-hosted packages plus one
optional Certe house-style dependency.

## Detection engine

- SQLite schema and migration runner; a DBI repository layer split by
  writer (the cron may upsert, the app only ever inserts).
- Automatic lattice enumeration (ward, institution, geographic area,
  province, catchment) from the data itself, no configuration required
  for a newly appearing pathogen.
- Five detectors: `farrington` (`surveillance::farringtonFlexible()`,
  with patient-day normalisation at ward/institution level where
  activity data is supplied), `same_place` (rule-based, no baseline
  needed - covers long-term care institutions and fast hospital
  clusters alike), `rare_trigger` (a single occurrence of a curated
  organism is itself the signal), and `mem` (Moving Epidemic Method
  seasonal thresholds for influenza/RSV-like organisms).
- Cluster reconciliation across repeated runs: extension, split, merge,
  backfill, idempotent reruns, out-of-order convergence, all inside one
  transaction per run so a partial failure never leaves partial state.
- Suppression across the lattice so one real outbreak reads as one
  cluster, not five restatements of itself at every level.
- Baseline feedback: a stream's own confirmed-epidemic history is
  excluded from what `farringtonFlexible()` sees on later runs, so a
  past outbreak never silently raises the next season's threshold.
- Derived cluster state as a pure function of classification history,
  the case-free clock, and whether the cluster changed since it was
  last assessed - never stored, always recomputed.

## Interface

- A Shiny app, read-only for anonymous visitors; classifying, closing,
  or muting a stream requires signing in.
- Bilingual throughout (Dutch and English) via a small `episode_tr()`
  translation layer, including Dutch number/singular-plural agreement.
- Dossier view with epi curve, multi-year trend, denominator/positivity,
  demography, geography (choropleth when both `sf` and geographic
  reference data are available, plain bar breakdown otherwise), line
  list (hidden for anonymous viewers), Rt estimation, historical
  analogue matching, and an automatically generated narrative
  ("Duiding") explaining the evidence in plain language.
- Full account, classification and audit-trail model: an append-only
  assessment timeline, an Archief of closed clusters, and an Activiteit
  log of every recorded action with system runs visually distinct.
- Prestatie (Performance) screen: positive predictive value per detector
  per organism, the classification distribution, and timeliness figures
  (first case to detection, detection to first assessment, detection to
  classification) - the evidence base for tuning decisions once an
  instance has run against real data for a while.

## Reporting

- Parameterised, versioned outbreak reports rendered via Quarto to
  self-contained HTML, with configurable small-count disclosure
  suppression for reports leaving the department. An operator can
  supply their own report template (`EPISODE_QUARTO_REPORT`) instead of
  the shipped one.

## Optional, always-guarded integrations

Every non-CRAN dependency is optional, with a documented fallback when
it is absent: `AMR` (episode deduplication), `EpiEstim` (Rt), `mem`
(seasonal thresholds), `quarto` + the Quarto CLI (report rendering),
`sf` (geographic choropleth). None of them are required to run the
detection engine, the interface, or the bundled demo. House colours are
a shipped, organisation-neutral default, overridable per instance via
`EPISODE_PALETTE_CONFIG` - no organisation-specific package involved.

## Data contract

Cases are the only mandatory input, deduplicated internally per
patient/pathogen/episode; positivity metadata, institution activity
(patient-days), and geographic reference data are all optional,
additive inputs with their own documented shape (see the README).
`episode_run_cron()`'s data-source arguments accept either a function
that produces the data at run time, or the data frame itself.

## Getting started

`EpiSODE::episode_demo()` runs the whole system in one call against
bundled synthetic data - no laboratory system, data warehouse,
credentials, or configuration required.

See `QUESTIONS.md` for the judgement calls made along the way, and
`ARCHITECTURE.md` for the full design this implementation follows.
