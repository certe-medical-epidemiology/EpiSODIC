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

## Architecture concerns raised during implementation

(none yet; this section is for anything that looked wrong or internally
inconsistent while implementing, per the standing brief's closing
instruction. Recorded prominently in the session summary as well.)
