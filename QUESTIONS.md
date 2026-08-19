# EpiSODE, open questions

Assumptions adopted provisionally where a decision was needed and no answer
was available. Update this file whenever a new assumption is made; do not
delete resolved entries, mark them resolved instead so the history of
decisions survives.

## Carried over from ARCHITECTURE.md section 18 ("Still open")

1. **Serial interval values.** Ship `rt_applicable = 0` for every organism in
   M1's `pathogen_config.csv` unless a specific literature source was found
   during curation. See the `source_ref` column.
2. **Severity weight table.** M1 ships a flat default of 1.00 for every
   organism pending curation; not scored against literature yet.
3. **`same_place` thresholds per organism.** M1 ships the architecture's
   stated defaults (n = 3 within 14 days) and per-organism overrides only
   for the six organisms the architecture names explicitly (*C. difficile*,
   MRSA, VRE, carbapenemase producers, norovirus, *Legionella*). Values are
   provisional pending real data.
4. **Retention horizon.** Not implemented until M... (retention is not in
   scope for M1). Left for a later milestone.
5. **Baseline artefact check** (afnamedatum == ontvangstdatum proportion
   over time). Cannot be run without real Diver data; deferred until an
   instance has real data to check. Flagged here so it is not forgotten
   before the first production run.
6. **Institution type mapping** (whether Diver carries a usable type field).
   Unknown without Diver access. The M1 ingestion interface assumes the
   synthetic generator supplies `institution_type` directly; the real
   `get_diver_data()` implementation will need to resolve this against
   actual Diver columns when it is written.
7. **Patient-day cadence.** Assumed monthly, per the architecture's own
   caveat. M1's synthetic `episode_institution_activity` generator produces
   monthly rows; no interpolation onto a weekly detection grid is
   implemented yet (that is M5, patient-day normalisation).
8. **Ward-level denominators.** Assumed unavailable for M1; ward/specialism
   patient-days are not modelled in the synthetic generator. L1 remains
   rule-based (`same_place`) only, consistent with the architecture's own
   fallback.
9. **Which machine runs the app long term.** Not an M1 concern.
10. **Upstream fixes to `certestats`** (`print(n_cl)` debug call,
    `populationOffset` exposure). Deferred to M5 per the milestone note;
    M1's `certestats` wrapper is guarded with `requireNamespace()` and is
    not exercised unless the package is installed, so this cannot be
    verified in this environment. Flagged for verification once
    `certestats` is available.

## New in M1

11. **Diver column names.** `certedb::get_diver_data()` is unavailable in
    this environment. `R/ingest_interface.R` defines the ingestion contract
    (an allow-listed column set matching architecture section 5.4/5.4.1) and
    `R/ingest_synthetic.R` is the only implementation shipped. No Diver
    column name is invented or assumed; a real `get_diver_data()` adapter
    is left as a documented extension point (`episode_ingest_source`).
12. **`AMR::get_episode()` availability.** `AMR` is listed in `Suggests`.
    Deduplication in `R/ingest_dedup.R` calls it via `requireNamespace()`
    and falls back to a simple documented equivalent (group by patient and
    organism, collapse isolates within `episode_days` of the previous one
    in the group) if `AMR` is not installed, so the demo works without it.
    This fallback is a documented approximation, not a replacement; flagged
    for review once `AMR::get_episode()` behaviour can be compared directly.
13. **SQLite `ENUM` mapping.** Architecture section 5 maps `ENUM(...)` to
    `TEXT` with a `CHECK` constraint. Implemented literally in
    `inst/sql/schema.sql`.
14. **`stream_key` hash algorithm.** Not specified beyond "deterministic
    hash of the defining dimensions". Adopted SHA-1 of a canonical
    pipe-joined string of the dimension values (level, mo_code, care_line,
    region_code, institution_id), matching the `CHAR(40)` column width
    implied by other `_key` columns in the schema (e.g.
    `institution_key`). Implemented in `R/lattice_stream_key.R`.
15. **`config_hash` algorithm.** Adopted SHA-1 of the canonicalised
    (alphabetically-keyed) JSON serialisation of the resolved configuration,
    for the same `CHAR(40)` reason as above.
16. **Eligibility gate thresholds (section 8, item 1).** "Sufficient
    baseline history", "a minimum median weekly count", "a reasonable share
    of baseline weeks" are not numerically specified anywhere. Adopted as
    configurable values in `inst/config/default.yaml`
    (`eligibility.min_baseline_weeks = 52`,
    `eligibility.min_median_weekly_count = 1`,
    `eligibility.min_nonzero_week_share = 0.2`), overridable per instance,
    calibration deferred to M6 per the milestone's own text.
17. **`case_free_days` default per organism where not curated.** Schema
    default is 14 (per column default). Used as the fallback in
    `pathogen_config.csv` rows that do not warrant a specific override.
18. **`mo_code` values in `pathogen_config.csv`.** The architecture keys
    streams on `AMR::as.mo()` output (section 5.1) but `AMR` cannot be
    exercised in this environment to generate real codes. The shipped
    `inst/config/pathogen_config.csv` uses placeholder codes of the shape
    `<kingdom>_<NAME>` (e.g. `B_SALMONELLA`, `V_NOROVIRUS`) that are
    deliberately **not** presented as real `AMR` codes. `R/mo_lookup.R`
    resolves these through `AMR::as.mo()` at ingestion time when `AMR` is
    installed, replacing the placeholder with the real code and rank, and
    falls back to using the placeholder verbatim (with `mo_rank` taken from
    the config file) when `AMR` is unavailable, so the demo still runs.
    This needs revisiting against real `AMR::as.mo()` output once available.
19. **`certestats::detect_disease_clusters()` / `detect_farrington()` return
    shape.** `R/detect_certestats.R` guesses a plausible return shape (a
    list of cluster objects with `first_day`/`last_day`/`n_cases` for the
    former; a data frame with `alarm`/`date`/`observed`/`expected`/
    `upperbound` columns for the latter, matching the `sts`-like
    conventions of the `surveillance` package `certestats` builds on).
    Neither function can be installed or inspected in this environment, so
    this is unverified and marked clearly as such; both call sites are
    guarded by `requireNamespace("certestats")` and return zero detections
    when the package is absent, so the demo and the test suite never
    exercise this code path. Must be corrected against the real source
    before this wrapper is trusted in production, per MILESTONES.md M1
    step 8 and `ARCHITECTURE.md` section 7.1's own verification note.

19b. **Priority score `rescale()` function.** Section 8.1 names
    `rescale(...)` for several components but does not define it. Adopted
    `x / (x + 1)` (bounded to `[0, 1)`, monotonic, no fixed reference
    range needed) in `R/score_priority.R`. Calibration is explicitly M6
    scope; this is a placeholder shape, not a calibrated function.

20. **`episode_stream` has no `ward` column.** ARCHITECTURE.md section 5.1's
    `episode_stream` DDL carries `level`, `mo_code`, `mo_name`, `mo_rank`,
    `care_line`, `region_code`, `institution_id`, `denominator`,
    `severity_weight`. It does not carry a `ward` column, yet section 7's
    lattice table defines L1 (`pathogen_ward`) as "Ward or specialism" and
    section 7.2 says the `same_place` rule "runs on ward rather than
    institution" for hospitals. Without a `ward` column, two different
    wards in the same hospital with the same pathogen would collapse into
    a single stream (since `stream_key` is a hash of the stream's
    dimensions, and ward would not be one of them), which is exactly the
    kind of silent identity collision reconciliation exists to prevent.
    This is flagged in the "architecture concerns" section below as a
    genuine inconsistency; the adopted fix, pending a decision, is to add
    a nullable `ward TEXT` column to `episode_stream` in
    `inst/sql/schema.sql` and include it in the `stream_key` hash
    dimensions for `pathogen_ward`-level streams only (see
    `R/lattice_stream_key.R`, `R/lattice_enumerate.R`,
    `R/detect_same_place.R`). This is a schema change relative to the
    literal DDL in the architecture document and should be reviewed.

21. **`dbplyr` dropped from `Imports` for now.** The standing brief (section
    4, "Dependencies") names `dbplyr` alongside `dplyr` as a lean-Imports
    baseline. Nothing in M1 uses lazy `tbl()`/`dbplyr` queries yet (the
    repository layer is plain parameterised `DBI::dbGetQuery()`/
    `dbExecute()`, per the standing brief's own "no SQL anywhere else in
    the package" rule, which the M1 order-of-work interprets as "keep the
    SQL in `R/db_*.R`", not "use dbplyr specifically"). `R CMD check`
    flags an Imports entry with no corresponding `::` call as a NOTE, so
    `dbplyr` was removed from `DESCRIPTION` rather than shipping an
    unused dependency; it should be added back in M2 when the Shiny app's
    read paths are built (ARCHITECTURE.md section 3.3, "cheap reads
    only" favours lazy `dbplyr` queries over materialising full tables).

## Architecture concerns raised during implementation

1. **`episode_stream` is missing a `ward` column** (see item 20 above).
   Section 5.1's DDL does not carry one, but section 7's own lattice table
   defines L1 as ward-level and section 7.2 explicitly says the
   `same_place` detector "runs on ward rather than institution" inside
   hospitals. Taken literally, the schema in section 5.1 cannot represent
   L1 at the granularity the rest of the document requires: any two wards
   in the same hospital with cases of the same organism would be
   indistinguishable as streams. I added a nullable `ward` column rather
   than silently working around it or leaving L1 broken, since the
   alternative (no ward tracking) contradicts the document's own stated
   design elsewhere. Flagging for review rather than treating this as
   settled.
