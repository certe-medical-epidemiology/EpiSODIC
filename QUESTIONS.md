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
10. **RESOLVED, superseded by item 22.** `certestats` is no longer a
    dependency at all (its Farrington glue is now owned directly by
    EpiSODE on top of `surveillance`, CRAN). The `print(n_cl)` debug call
    and `populationOffset` exposure questions are moot for that reason;
    `populationOffset`/patient-day normalisation on the in-house Farrington
    wrapper is still M5 scope (`R/detect_farrington.R`'s `sts()` call does
    not yet pass a population offset).

## New in M1

11. **Diver column names / `certedb` boundary.** RESOLVED by item 22:
    `certedb::get_diver_data()` (and any other source system) is now
    explicitly and permanently outside this package's scope, not merely
    "unavailable in this environment". `R/ingest_interface.R` defines the
    ingestion contract; the operator's own transform step, run before
    `episode_run_cron()`, is the only thing that ever touches Diver.
12. **`AMR::get_episode()` availability.** RESOLVED: `AMR` is no longer a
    dependency at all (item 22 - `pathogen` is a free-text string `AMR`
    cannot resolve for viruses anyway). `R/ingest_dedup.R`'s own
    implementation of `AMR::get_episode()`'s documented default-case
    algorithm is now the only implementation, not a fallback.
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
18. **RESOLVED, superseded by item 22.** `mo_code`/`AMR::as.mo()` is gone
    entirely; `pathogen_config.csv` is keyed on the raw `pathogen` string.
19. **RESOLVED.** `certestats::detect_disease_clusters()` was inspected
    directly (its source was fetched and read in full) and retired rather
    than ported - see item 22 for the reasoning. `certestats::
    detect_farrington()`'s presumed shape no longer matters: EpiSODE calls
    `surveillance::farringtonFlexible()` directly now
    (`R/detect_farrington.R`), a real CRAN dependency this environment can
    install, run and test against directly rather than guess about.

19b. **Priority score `rescale()` function.** Section 8.1 names
    `rescale(...)` for several components but does not define it. Adopted
    `x / (x + 1)` (bounded to `[0, 1)`, monotonic, no fixed reference
    range needed) in `R/score_priority.R`. Calibration is explicitly M6
    scope; this is a placeholder shape, not a calibrated function.

20. **`episode_stream` has no `ward` column.** ARCHITECTURE.md section 5.1's
    `episode_stream` DDL carries `level`, `mo_code` (now `pathogen`, item
    22), `mo_name`, `mo_rank`, `care_line`, `region_code`,
    `institution_id`, `denominator`, `severity_weight`. It does not carry
    a `ward` column, yet section 7's
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

22. **Detection pipeline decoupled from every Certe package; `mo_code` ->
    `pathogen`.** Decided directly with the architecture's author
    mid-session (not a solo assumption; recorded here per the standing
    brief's instruction to log every deliberate deviation, decided or
    not). Four changes, made together:

    a. **`certestats` retired, not wrapped.** Its source was fetched and
       read in full
       (`https://raw.githubusercontent.com/certe-medical-epidemiology/certestats/main/R/detect_disease_clusters.R`
       and `moving_average.R`). `detect_disease_clusters()` is a
       moving-average-vs-historic-percentile heuristic (trailing 7-day
       moving average, threshold set as the 97.5th percentile of historic
       moving averages with optional boxplot-based outlier removal,
       `AMR::get_episode()` for episode grouping, several hand-tuned
       constants: `threshold_percentile`, `remove_outliers_coefficient`,
       `minimum_case_fraction_in_period`). Its author agreed it is
       "arbitrary and not great". Rather than port it, it is retired
       outright: what it was trying to catch (an unexpected rise against
       history) is what Farrington does with actual statistical grounding,
       and running both would be the same detection angle twice, not two
       angles. `certestats::detect_farrington()` was never inspected
       (assumed to be a thin linelist-to-`sts` wrapper around
       `surveillance::farringtonFlexible()`, which is a reasonable
       inference but unverified); it is replaced by EpiSODE owning that
       glue directly (`R/detect_farrington.R`), which also means it can
       finally be installed and tested in this environment, unlike the old
       wrapper which could only ever return zero detections here.
    b. **`AMR` dropped entirely**, not just made optional. `AMR::as.mo()`
       only resolves non-viral taxonomy, and this system must detect
       clusters of anything a lab reports (including viruses), so pathogen
       identity is now the raw lab-provided `pathogen` string, used
       verbatim, never resolved against any taxonomy. `R/mo_lookup.R` is
       deleted. `AMR::get_episode()`'s role (episode grouping for
       deduplication) is filled by `R/ingest_dedup.R`'s own direct
       implementation of that function's documented algorithm, which no
       longer needs to be called a "fallback" since it is the only
       implementation.
    c. **`certedb` was already outside the package** (the ingestion
       interface was designed that way from the start of M1), but the
       boundary is now explicit and permanent rather than an artefact of
       "unavailable in this sandbox": EpiSODE never calls a data source
       itself, on principle, so that labs/operators own extraction and
       transform and EpiSODE owns detection and assessment only. See
       `README.md`'s data format section.
    d. **Positivity/denominators decoupled from detection and made
       optional.** The `positive`/negative-test question was discussed at
       length: patient-day normalisation (L1/L2 incidence density) needs
       no negatives at all (`episode_case`, already deduplicated, is the
       numerator; `episode_institution_activity.patient_days` is the
       denominator, from hospital admin systems, unrelated to the lab
       feed). Test-level % positivity is a different, optional feature:
       ARCHITECTURE.md section 8.2 already scoped it as an interpretive
       aid, not a detection input, and a raw per-test linelist (including
       negatives) would mean millions of rows for something that is only
       ever a sanity check. Resolution: the mandatory ingestion contract
       (`episode_ingest_columns`) is positives-only, no `positive` column
       at all. `episode_denominator` (keyed on `pathogen` now, not
       `determination`) is fed by a *separate, optional, pre-aggregated*
       source (`R/ingest_denominator.R`,
       `episode_denominator_ingest_run()`) that an operator supplies only
       if they can produce it (trivial for multiplex PCR panels with a
       fixed target list; not meaningful for open-ended culture results).
       `episode_mo_determination` is dropped: the determination-to-
       organism mapping it encoded is lab-specific expert knowledge that
       now belongs in the operator's own transform step, not inside
       EpiSODE.

    Net effect on `DESCRIPTION`: `certestats`, `certedb` and `AMR` are gone
    from `Suggests`/`Remotes` entirely (not merely optional); `surveillance`
    moved from `Suggests` to a hard `Imports` (CRAN, no Certe dependency).
    `certegis`/`certeplot2`/`certestyle` are unaffected (M2+ interface
    concerns, not detection).

    Rare-but-serious detection (ARCHITECTURE.md section 8, item 2) was
    also implemented in this pass (`R/detect_rare_trigger.R`,
    `episode_detection.detector` already had `'rare_trigger'` in its enum
    from the original M1 schema but nothing produced it): a curated list
    in `inst/config/default.yaml`, matched case-insensitively against
    `pathogen`, any occurrence fires. `'clusters'` (the `detect_disease_
    clusters()` detector value) is removed from the `episode_detection.
    detector` CHECK constraint since nothing produces it any more.

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
