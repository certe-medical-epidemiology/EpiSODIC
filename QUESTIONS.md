# EpiSODE, open questions

Assumptions adopted provisionally where a decision was needed and no answer
was available. Update this file whenever a new assumption is made; do not
delete resolved entries, mark them resolved instead so the history of
decisions survives. Organised by subsystem rather than by when a decision
was made — a reader looking for "how does geography work" should not have
to know which point in the project's history that decision was made.

## Detection & reconciliation

1. **Serial interval values.** `pathogen_config.csv` ships `rt_applicable = 0`
   for every organism unless a specific literature source was found during
   curation. See the `source_ref` column.
2. **Severity weight table.** A flat default of 1.00 for every organism,
   pending curation against literature.
3. **`same_place` thresholds per organism.** The architecture's stated
   defaults (n = 3 within 14 days) ship, with per-organism overrides only
   for the six organisms the architecture names explicitly (*C. difficile*,
   MRSA, VRE, carbapenemase producers, norovirus, *Legionella*). Values are
   provisional pending real data.
4. **Baseline artefact check** (afnamedatum == ontvangstdatum proportion
   over time). Cannot be run without real data; deferred until an instance
   has real data to check. Flagged here so it is not forgotten before the
   first production run.
5. **Eligibility gate thresholds (ARCHITECTURE.md section 8, item 1).**
   "Sufficient baseline history", "a minimum median weekly count", "a
   reasonable share of baseline weeks" are not numerically specified
   anywhere. Adopted as configurable values in `inst/config/default.yaml`
   (`eligibility.min_baseline_weeks = 52`,
   `eligibility.min_median_weekly_count = 1`,
   `eligibility.min_nonzero_week_share = 0.2`), overridable per instance.
   Calibration against real signal volume needs an instance that has
   actually run for a while (see item 68 below).

6. **Detection pipeline decoupled from every Certe package; `mo_code` ->
   `pathogen`.** Decided directly with the architecture's author, not a
   solo assumption. Four changes, made together:

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
      glue directly (`R/detect_farrington.R`), which also means it can be
      installed and tested directly rather than always returning zero
      detections.
   b. **`AMR` dropped entirely**, not just made optional. `AMR::as.mo()`
      only resolves non-viral taxonomy, and this system must detect
      clusters of anything a lab reports (including viruses), so pathogen
      identity is now the raw lab-provided `pathogen` string, used
      verbatim, never resolved against any taxonomy. `R/mo_lookup.R` is
      deleted. `AMR::get_episode()`'s role (episode grouping for
      deduplication) is filled by `R/ingest_dedup.R`'s own direct
      implementation of that function's documented algorithm, which is
      the only implementation, not a fallback.
   c. **`certedb` was already outside the package** (the ingestion
      interface was designed that way from the start), but the boundary
      is now explicit and permanent rather than an artefact of "not
      installed here": EpiSODE never calls a data source itself, on
      principle, so that labs/operators own extraction and transform and
      EpiSODE owns detection and assessment only. See README.md's data
      format section.
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
      at all. `episode_denominator` (keyed on `pathogen`, not
      `determination`) is fed by a *separate, optional, pre-aggregated*
      source (`R/ingest_denominator.R`,
      `episode_denominator_ingest_run()`) that an operator supplies only
      if they can produce it (trivial for multiplex PCR panels with a
      fixed target list; not meaningful for open-ended culture results).
      `episode_mo_determination` is dropped: the determination-to-
      organism mapping it encoded is lab-specific expert knowledge that
      belongs in the operator's own transform step, not inside EpiSODE.

   Net effect on `DESCRIPTION`: `certestats`, `certedb` and `AMR` are gone
   from `Suggests`/`Remotes` entirely (not merely optional); `surveillance`
   moved from `Suggests` to a hard `Imports` (CRAN, no Certe dependency).
   `certegis`/`certeplot2`/`certestyle` are unaffected (interface concerns,
   not detection).

   Rare-but-serious detection (ARCHITECTURE.md section 8, item 2) was also
   implemented alongside this (`R/detect_rare_trigger.R`): a curated list
   in `inst/config/default.yaml`, matched case-insensitively against
   `pathogen`, any occurrence fires. `'clusters'` (the retired
   `detect_disease_clusters()` detector value) is removed from the
   `episode_detection.detector` CHECK constraint since nothing produces it
   any more.

7. **RESOLVED by item 6.** `mo_code`/`AMR::as.mo()` is gone entirely;
   `pathogen_config.csv` is keyed on the raw `pathogen` string.
8. **RESOLVED by item 6.** `certestats::detect_disease_clusters()` was
   inspected directly (its source was fetched and read in full) and
   retired rather than ported. EpiSODE calls `surveillance::
   farringtonFlexible()` directly (`R/detect_farrington.R`), a real CRAN
   dependency that can be installed, run and tested against directly
   rather than guessed about.
9. **Priority score `rescale()` function.** ARCHITECTURE.md section 8.1
   names `rescale(...)` for several components but does not define it.
   Adopted `x / (x + 1)` (bounded to `[0, 1)`, monotonic, no fixed
   reference range needed) in `R/score_priority.R`. This is a placeholder
   shape, not a calibrated function — calibration is real-volume-dependent,
   see item 68 below.

10. **The cool-down escape hatch (ARCHITECTURE.md section 6.5) was
    initially left unimplemented, then built once its own prerequisite gap
    was found.** While wiring `explicitly_closed`, `cooldown_days` turned
    out to have never been read anywhere in reconciliation at all, despite
    being a real schema/config column from the start — there was no
    cool-down suppression to build an escape hatch *onto*. Implemented
    both together: `episode_reconcile_find_cooldown_match()` only runs for
    a candidate that found zero ordinary matches (i.e. already outside
    `case_free_days`), looks one step further to `cooldown_days`, and
    absorbs it into a terminal-verdict (`artefact`/`expected_variation`)
    closed cluster rather than opening a new one — flagging
    `changed_since_assessment` only when the candidate's case count
    clears `cooldown_reopen_ratio` ("half again") against the closed
    cluster's own count. This also surfaced a second, independent latent
    bug: `episode_derive_state()` treated a terminal verdict as
    unconditionally "closed" regardless of `changed_since_assessment`, so
    even the *pre-existing* case-free-days-driven update path (which
    already set the flag correctly) had no way to surface it in the UI —
    fixed with a one-line change reusing the exact mechanism already built
    for non-terminal verdicts, rather than inventing new
    `episode_cluster_state` choreography.

11. **Baseline feedback (`episode_baseline_excluded_windows()`) excludes a
    stream's own confirmed-epidemic periods from what gets fed to
    Farrington**, both for detection and for the persisted trend series,
    and is listed on the Streams screen per ARCHITECTURE.md section 7.6's
    own requirement. The exclusion removes the affected case rows from the
    series entirely, covering the whole window regardless of which
    historical weeks `farringtonFlexible()`'s own `b`/`w`
    reference-selection actually reads for a given evaluated week —
    simpler than trying to track exactly which weeks Farrington would have
    referenced, and the plain-language architecture text ("excluded from
    the baseline") does not specify anything more precise than that.

12. **Curve-shape classification (`episode_classify_curve_shape()`) is a
    Duiding fragment, not a dossier panel** — ARCHITECTURE.md section 9
    says curve shape is "stated in the Duiding, with an intermediate
    verdict where the evidence is ambiguous", not that it gets its own
    visual panel the way Rt or geography do. The `2 × incub_max_days`
    boundary between "propagated" and "ambiguous" is not a value the
    architecture specifies numerically (only "the first question in any
    outbreak investigation" and "an intermediate verdict" are given) — a
    documented judgement call, flagged for calibration alongside the
    codebase's other provisional numeric thresholds (see item 68 below).

13. **MEM (`episode_detect_mem()`, the `mem` CRAN package - added to
    `Suggests`) runs only on `pathogen_region` (L5) streams**, for
    organisms flagged `mem_applicable` (Influenza A/B, RSV in the shipped
    `pathogen_config.csv`) — it needs a stable, population-level weekly
    series across several historical seasons to fit at all, which no ward
    or single institution has the volume to support, and ARCHITECTURE.md
    itself frames seasonal surveillance as a population-level question.
    `same_place` and Farrington are unaffected at every other level.
    Verified against the real `mem` package (installed and exercised
    directly, not guessed at from documentation alone):
    - **Season convention**: `mem`'s own bundled `flucyl` example data
      confirms the northern-hemisphere week-40-to-week-20 convention
      ARCHITECTURE.md section 7.3 describes, with row labels `"40".."52",
      "1".."20"` (33 weeks) and column labels `"YYYY/YYYY"`. Week 53 (some
      years have one) is folded into week 52 — `mem`'s own guidance
      ("accommodate week 53") describes a distinct-column treatment this
      does not attempt; a documented simplification.
    - **Threshold extraction**: `memmodel()`'s help text states "pre.post.
      intervals... Threhold is the upper limit of the confidence
      interval" — confirmed this means column 3 (not column 2, the point
      estimate) of the returned 2x3 matrix, via direct inspection of a
      fitted model's structure against that documentation.
    - Requires at least 2 complete prior seasons before firing at all
      (`min_seasons`), mirroring Farrington's own `b`-parameter baseline
      requirement; returns `NULL`/an empty detection record for an
      off-season date, no case data, `mem` not installed, or insufficient
      seasons, at every call site (`episode_detect_mem()`,
      `episode_mem_status()`, and the closure criterion via
      `episode_closure_criterion_met(mem_applicable = TRUE, mem_status =
      NULL)`) — a `mem_applicable` stream simply never closes via this
      criterion when MEM cannot be computed, rather than silently falling
      back to a case-free-days threshold that is not epidemiologically
      meaningful for a seasonal pathogen (ARCHITECTURE.md section 6.3).
    - End-to-end verified against a real cron run with realistic synthetic
      seasonal data: fires during peak season (mid-January), does not fire
      off-season or with only one season of history.

14. **Rt (`episode_compute_rt()`, `EpiEstim::estimate_R()`) uses
    `method = "parametric_si"` unconditionally** — `si_dist`
    (`gamma`/`lognormal`) is recorded in `episode_pathogen_config` but not
    yet used to select a distribution family, since `EpiEstim`'s
    parametric method only ever fits a Gamma-shaped serial interval
    (mean/sd parameterised) regardless of the literature source's own
    distributional assumption. A known simplification, not a silent one.
    Estimates whose window ends within `incomplete_days` (from
    `episode_app_completeness()`, the same reporting-triangle-derived
    figure the epi curve's own shading already uses) of the run date are
    withheld from the returned series entirely — not shown-and-captioned —
    since the trailing days are under-ascertained by construction and an
    Rt estimate ending there would read a reporting artefact as a real
    transmission change.

15. **Rt's empty-state message says *why*, distinguishing three different
    causes `episode_compute_rt()` previously collapsed into one generic
    "insufficient data" string** (the panel is only ever shown at all when
    `rt_applicable` is `TRUE`, so *something* has to explain the emptiness
    rather than nothing): a genuinely missing serial interval in the
    pathogen config (`no_serial_interval` — a data-entry gap worth
    fixing), the `EpiEstim` package not being installed
    (`epiestim_missing` — an environment gap), or simply not enough case
    history yet (`insufficient_history` — the common, expected case,
    needing no action). `episode_rt_unavailable_reason(pc)` decides which
    cheaply, without re-running `episode_compute_rt()`'s own computation;
    `episode_cluster_object()` carries this alongside `rt`. Most real
    cases of an empty Rt panel on an `rt_applicable` organism are simply
    "not enough cases in this specific cluster yet" — the previous message
    actively mislabelled that as a data problem.

16. **Cluster volume for endemic organisms at a single place — parked.**
    `same_place` has no eligibility/baseline gate by design (ARCHITECTURE.md
    section 7.2: it exists precisely to catch rare-pathogen bumps a
    baseline model can't see, so it needs none to fire). For an endemic
    organism like *Clostridioides difficile* at a busy institution, the
    default `n_cases: 3` / `k_days: 14` threshold gets crossed repeatedly
    as routine background noise — each bump becomes its own cluster once
    it is more than `case_free_days` past the last one, so the rail can
    fill with tens of small, quickly-resolved clusters of the same
    pathogen at the same place. This is not a reconciliation bug (nothing
    is being wrongly kept apart that `case_free_days` should have merged);
    it is `same_place` doing exactly what it is configured to do, at a
    volume that turned out to be a real workflow problem in testing.

    Two directions were discussed and deliberately parked rather than
    built, to see how the volume actually behaves in real use before
    committing to either:
    - **Raise the `same_place` threshold** for high-baseline organisms
      (e.g. `n_cases: 5` for *C. difficile*) — a one-line config change,
      cheapest to try first.
    - **A "series" grouping layered on top of clusters** (not a
      reconciliation-level merge, and not a change to `case_free_days`'s
      own closure semantics): clusters sharing a `stream_id` within a new,
      longer `series_gap_days` window would show as one row on the rail
      (aggregate case count and date span), expandable to each
      sub-cluster's own full timeline; classifying the open one would
      flag — not silently apply to — a later cluster that joins the same
      series while its last classification was `artefact`/
      `expected_variation`/`cluster_not_yet`, needing one click to
      confirm rather than a full re-assessment from zero. Real schema and
      UI work; not started.

17. **`episode_stream` needed a `ward` column not present in the
    architecture's literal DDL.** ARCHITECTURE.md section 5.1's
    `episode_stream` DDL carries `level`, `mo_code` (now `pathogen`, item
    6), `mo_name`, `mo_rank`, `care_line`, `region_code`, `institution_id`,
    `denominator`, `severity_weight` — no `ward` column, yet section 7's
    lattice table defines L1 (`pathogen_ward`) as "Ward or specialism" and
    section 7.2 says the `same_place` rule "runs on ward rather than
    institution" for hospitals. Without a `ward` column, two different
    wards in the same hospital with the same pathogen would collapse into
    a single stream (since `stream_key` is a hash of the stream's
    dimensions, and ward would not be one of them), which is exactly the
    kind of silent identity collision reconciliation exists to prevent.
    Taken literally, the schema as originally specified cannot represent
    L1 at the granularity the rest of the document requires. Rather than
    silently work around it or leave L1 broken, a nullable `ward TEXT`
    column was added to `episode_stream` in `inst/sql/schema.sql` and
    included in the `stream_key` hash dimensions for `pathogen_ward`-level
    streams only (see `R/lattice_stream_key.R`, `R/lattice_enumerate.R`,
    `R/detect_same_place.R`). This is a schema change relative to the
    literal DDL in the architecture document; flagged for review rather
    than treated as settled.

## Data model, schema & ingestion

18. **Retention horizon.** Not implemented; left for a later phase of
    work.
19. **Institution type mapping** (whether the source system carries a
    usable type field). Unknown without direct access to a real source
    system. The ingestion interface assumes the synthetic generator
    supplies `institution_type` directly; a real transform implementation
    will need to resolve this against the actual source columns when it is
    written.
20. **Patient-day cadence.** Assumed monthly, per the architecture's own
    caveat. The synthetic `episode_institution_activity` generator
    produces monthly rows; no interpolation onto a weekly detection grid
    is implemented.
21. **Ward-level denominators.** Assumed unavailable; ward/specialism
    patient-days are not modelled in the synthetic generator. L1 remains
    rule-based (`same_place`) only, consistent with the architecture's own
    fallback. Patient-day normalisation (ARCHITECTURE.md section 7.1) is
    implemented at L2 (institution) only, never L1 (ward) — there is no
    ward-level activity table in the schema, and section 7 itself only
    promises ward patient-days "if obtainable"; they are not. L1 detection
    is unaffected (still raw counts).
    `episode_synthetic_institution_activity_source()` is a new, entirely
    optional data source (mirroring `episode_denominator_source_synthetic()`'s
    own established pattern) — `episode_run_cron()`'s
    `institution_activity_source_fn` parameter defaults to `NULL`, so no
    existing caller's behaviour changes unless it opts in. Confirmed by
    direct inspection that `surveillance::sts()` accepts a `population`
    argument and `farringtonFlexible()`'s control list accepts
    `populationOffset`, which is what makes this possible now that
    `surveillance::farringtonFlexible()` is called directly (item 6
    above) rather than through a wrapper of unknown shape.
22. **`ENUM` mapping.** Architecture section 5 maps `ENUM(...)` to `TEXT`
    with a `CHECK` constraint. Implemented literally in
    `inst/sql/schema.sql`.
23. **`stream_key` hash algorithm.** Not specified beyond "deterministic
    hash of the defining dimensions". Adopted SHA-1 of a canonical
    pipe-joined string of the dimension values (level, mo_code, care_line,
    region_code, institution_id), matching the `CHAR(40)` column width
    implied by other `_key` columns in the schema (e.g.
    `institution_key`). Implemented in `R/lattice_stream_key.R`.
24. **`config_hash` algorithm.** Adopted SHA-1 of the canonicalised
    (alphabetically-keyed) JSON serialisation of the resolved
    configuration, for the same `CHAR(40)` reason as above.
25. **`case_free_days` default per organism where not curated.** Schema
    default is 14 (per column default). Used as the fallback in
    `pathogen_config.csv` rows that do not warrant a specific override.
26. **Source column names / source-system boundary.** RESOLVED by item 6:
    a source system's own extraction/transform layer is explicitly and
    permanently outside this package's scope, not merely "unavailable in
    this environment". `R/ingest_interface.R` defines the ingestion
    contract; the operator's own transform step, run before
    `episode_run_cron()`, is the only thing that ever touches the source
    system.
27. **`AMR::get_episode()` availability.** RESOLVED by item 6: `AMR` is no
    longer a dependency at all — `pathogen` is a free-text string `AMR`
    cannot resolve for viruses anyway. `R/ingest_dedup.R`'s own
    implementation of `AMR::get_episode()`'s documented default-case
    algorithm is the only implementation, not a fallback.
28. **`dbplyr` dropped from `Imports`.** A lean-Imports baseline pairs
    `dplyr` with `dbplyr` for lazy `tbl()` queries, but the repository
    layer uses plain parameterised `DBI::dbGetQuery()`/`dbExecute()`
    throughout, keeping all SQL inside `R/db_*.R`. `R CMD check` flags an
    Imports entry with no corresponding `::` call as a NOTE, so `dbplyr`
    was removed from `DESCRIPTION` rather than shipping an unused
    dependency. Revisit if a future read path favours lazy `dbplyr`
    queries over materialising full tables (ARCHITECTURE.md section 3.3,
    "cheap reads only").

## Interface

29. **Static `ggplot2` charts rather than an interactive htmlwidgets
    stack.** ARCHITECTURE.md section 3.3 is explicit that the app must
    only ever do cheap reads, never recompute anything expensive at
    render time. `ggplot2` is already a near-certain transitive dependency
    of the R ecosystem this runs in, needs no extra JS toolchain, and its
    output is deterministic and easy to test outside a browser
    (`expect_s3_class(..., "ggplot")`, as `test-app_ui.R` does). An
    htmlwidgets library (plotly, echarts4r) would get closer to
    interactive hover/zoom but adds a dependency and a browser-only
    testing story for comparatively little gain in a read-only interface.
    Revisit if a future need asks for interactive drill-down on the
    charts themselves.

30. **`bslib` is used only for the Bootstrap/CSS reset and font-loading
    helper (`bslib::page_fluid()` + `bslib::bs_theme()`), not for its
    component library.** All visual design (header, nav, panels, chips,
    bars, pyramid) is bespoke `shiny::tags` styled by
    `inst/app/www/episode.css`, matching the intended house-style layout
    precisely rather than bslib's own Bootstrap look.

31. **`episode_cluster_object()`'s `concentration` field carries a full
    per-PC breakdown** (`concentration$rows`, a `data.frame(label, n)`
    covering every PC value the cluster's cases touch), not only the
    dominant PC's label/count/share. The geography panel
    (`episode_ui_geo_panel()`) needs the whole distribution to draw a bar
    chart; only ever exposing the single dominant row would have made
    that panel structurally unable to show anything but one bar.

32. **The classification form's "close cluster" confirmation passes the
    translated prompt through a `data-confirm` HTML attribute** (escaped
    by `shiny::tags` the same way any other attribute value is) rather
    than interpolating it into the `onclick` JS string directly — the
    earlier draft did that and would have broken on any future i18n
    string containing an apostrophe.

33. **Every detection-algorithm identifier surfaced to the user
    (`same_place`, `rare_trigger`, `farringtonFlexible`) renders in
    `<code>`, absolutely everywhere it appears, no exceptions** — these
    are names from the codebase, not prose, and reading as plain text
    made them look like typos or garbled Dutch rather than deliberate
    identifiers. Implemented as `episode_ui_code_join()`, escaping first
    so it is always safe to pass through `shiny::HTML()`. Covers the
    dossier's "detected by" line, the settings panel, the empty-timeline
    notice, the trend panel's notes, the Performance screen's PPV table
    (`episode_ui_code_join(row$detector)`), and the outbreak report's
    "Detected by" row (`` `r episode_ui_code_join(obj$detectors)` ``).
    `episode_ui_code_join()` and `episode_ui_italicise_taxon()` are
    `@export`ed rather than `@noRd` internal, since the Quarto report
    template runs in its own fresh `library(EpiSODE)` session where only
    exported functions are attached — a bare (unexported) reference from
    the `.qmd` would simply fail to resolve. An operator's own custom
    template (`EPISODE_QUARTO_REPORT`) has both helpers available for the
    same reason. One caller (`episode_ui_settings_panel()`'s row list) had
    to switch from `c(label, value)` to `list(label, value)`, since `c()`
    silently strips an `shiny::HTML()` value's class when combined with a
    plain string, which would have made that one row's `<code>` tags
    render as visible literal text instead of applying.

34. **The classification and mute-reason pickers were rebuilt as
    colour-coded button lists**, replacing plain `<select>` elements — a
    dropdown hides the mapping between an option and its meaning behind a
    click, where a labelled, coloured button per option puts option,
    colour and hint text in view at once. `episode_ui_picker()` is a
    small reusable building block (plain inline `onclick`, consistent
    with the rest of the app's forms — no new JS file). Added a one-line
    `<p>` above the mute picker explaining what muting actually does,
    since "Stream dempen" alone does not say why or when to use it.

35. **Two real bugs found via user testing**: the rail list's ratio line
    rendered literally "ratio NA" for any cluster detected by a
    rule-based detector (`same_place`, `rare_trigger`), which carries no
    `expected`/`ratio` at all — fixed by omitting the ratio segment when
    it is `NA` rather than formatting it anyway. And
    `episode_ui_format_datetime()` parsed a stored UTC timestamp with
    `as.POSIXct(iso, tz = "UTC")` but then called `format()` without an
    explicit `tz`, which uses the *parsed object's own* `tzone` attribute
    (still `"UTC"`) rather than the system's — every displayed time
    (status strip, assessment timeline, Activity/Archive tabs) was
    silently shown in UTC labelled as if local. Fixed by passing an
    explicit `tz` argument (defaulting to `Sys.timezone()`, exposed as a
    parameter rather than hardcoded so tests can pass a fixed zone —
    `Sys.timezone()` caches its result for the R session, so
    `Sys.setenv(TZ = ...)` after the first call has no effect on it).

36. **Several dossier/interpretation wordings tightened on user
    feedback**: the top metrics card's "Waargenomen"/"Observed" label was
    ambiguous next to "verwacht"/"expected" (found vs. lab-confirmed vs.
    statistically expected all read plausibly) — renamed to
    "Vastgesteld"/"Confirmed". The case-free stat showed two raw numbers
    separated by a slash with no indication which was which
    (`"1462 / 21 d"`); split into a value ("1462 d") and a sub-label
    ("drempel 21 d"/"threshold 21 d") using the existing stat-tile
    pattern. "PC4" was relabelled "PC" everywhere user-facing (the
    underlying `pc4` column and ingestion contract are unchanged — this
    is a label-only change, not a granularity change, so an operator
    supplying coarser or finer postcodes is not misled by a label that
    implies exactly four digits). The concentration interpretation
    fragments named a PC value bare (`"de grootste locatie, 7228,
    omvat..."`, ambiguous with an actual named place) — now prefixed
    "postcode" everywhere a PC value appears in prose. "Locatie" was also
    dropped from the diffuse-concentration fragment's own wording
    (Certe's own jargon uses "locatie" for a physical centre/institution,
    a different concept from a PC-based geographic area) in favour of
    "het hoogst epidemische gebied"/"the area with the most cases".

37. **`episode_format_date_range()`**, collapsing a cluster's
    `first_day`/`last_day` into `"7-15 jan. 2025"` rather than repeating
    the month and year per endpoint, falling back a step at a time as the
    range widens (same month, same year, different years) and collapsing
    to a single date for a one-day cluster. Used on the rail list, which
    previously showed no date range at all.

38. **Bug: "the condition has length > 1" on the Activiteit tab.**
    `episode_app_activity_log()` built the "action" column for detection
    runs via `episode_tr(paste0("activity.action_run_", runs$status), ...)`
    — `episode_tr()` is a scalar helper (`if (key %in% names(table))`
    inside it), so passing it a vector of keys only worked by accident
    when exactly one run existed. Any real instance, with more than one
    cron run recorded, hit this immediately. Fixed with `vapply()` over
    `runs$status`, matching how every other per-row `action`/`actor`
    label in the same function is already built. Regression test in
    `test-app_assessment_read.R` seeds a second run with a different
    status to catch this class of bug reappearing.

39. **Bug: the rail count and the Archief screen never noticed a write
    unless `view()` happened to change too.** First investigated against a
    *fresh* synthetic instance, where zero closed clusters is genuinely
    correct (nothing had been assessed yet) — that led to wrongly closing
    this out as "nothing to fix". A real closed cluster later showed the
    rail's "N clusters nieuw of actief" count not changing and Archief
    staying empty, which is a real bug: `open_clusters` (the rail's data
    source) and `output$archive_screen` both read straight from the
    database with no Shiny reactive dependency that a write action ever
    touched. `episode_app_server_assessment_actions()`'s `refresh()` only
    re-triggered `selected_cluster_id()`, which invalidates the
    dossier/assessment panes (both keyed on that value) but nothing else —
    so closing a cluster without ever navigating away from the "clusters"
    view left both permanently stale, and even navigating into Archief did
    not help, since `output$archive_screen` is a standalone Shiny output
    whose own dependencies (`archive_query()`, `lang`) `view()` is not one
    of — unlike the inline "streams"/"activity" branches inside
    `output$main_view`, which *do* recompute on every `view()` change
    because they live directly inside that renderUI. Fixed with a
    session-wide `db_version` `reactiveVal`, bumped by `refresh()`, that
    `open_clusters` and `output$archive_screen` both now read. Caught with
    a `shiny::testServer()` regression test (`test-app_server.R`) that
    closes a cluster via `assess_close` without ever touching `nav_view`,
    and asserts both the rail and Archief actually reflect it.

40. **Signaleringsreeksen (Streams) screen was slow to load on a real
    instance — fixed at the read-model level, with pagination as the
    user-facing shape.** The actual bottleneck was not HTML row count:
    `episode_app_streams_screen()` computed `baseline_excluded` for
    *every* stream unconditionally, and that computation is one DB round
    trip per stream (`episode_baseline_excluded_windows()`) plus one more
    per cluster within it (`episode_db_assessment_events()`) — an
    N+1(+M) query pattern invisible on a small fixture database, real on
    hundreds of real streams. `episode_app_streams_screen(con, page,
    page_size)` now slices to the requested page *before* that loop runs,
    so the cost is bounded by `page_size` (default 50) regardless of
    total stream count. Prev/Next controls (`streams_page_select` input)
    only render when there is more than one page. The screen's shared CSS
    class (`.episode-streams-screen`, used by Streams, Archief, Activiteit,
    Prestatie and Info) had no height/overflow of its own, unlike
    `.episode-body`'s children which scroll internally within a
    fixed-height flex parent — added `height: calc(100vh - 44px);
    overflow-y: auto; box-sizing: border-box;` to match `.episode-body`'s
    own pattern, so pagination controls are reachable by scroll on all
    five screens.

41. **The status trajectory band shows classifications, not the derived
    state.** Previously a single bar labelled with the current derived
    state (new/assessing/monitoring/...), which cannot answer "was this
    ever thought to be a possible epidemic before being confirmed, or was
    it artefact from the start" — exactly the question this band exists
    to answer at a glance. Rebuilt from `episode_app_assessment_timeline()`'s
    verdict-*setting* events only (note-only assessments and closures
    don't start a new segment, since neither changes what the cluster was
    judged to be): the period before the first classification is always
    labelled "Onbeoordeeld"/"Unassessed" (`statusverloop.unassessed`,
    replacing the unused `statusverloop.now`), not the generic
    "Nieuw"/"New" state label, which reads ambiguously as "recently
    created" rather than what it actually means here — nobody has looked
    at it yet. Segments are equal-width (not time-proportional, to avoid
    unreadable slivers for near-simultaneous events) with each segment's
    own start date shown instead. `episode_app_assessment_timeline()`
    gained a raw `verdict` column alongside the existing translated
    `verdict_label`, needed for `episode_ui_verdict_colour()` lookups
    (which take the raw key, not the translated string). This was scoped
    to the trajectory band specifically; the same "Nieuw" label is
    unchanged everywhere else a cluster's state is shown (rail chip,
    dossier header).

42. **Bulk assessment: select several clusters on the rail, classify all
    of them in one submit with one shared verdict and rationale** — often
    several clusters are artefacts at once, so opening each dossier in
    turn for the same classification was pure friction. A checkbox per
    rail item (rendered only for a signed-in session, same gate as the
    single-cluster form) and a bulk action bar (verdict picker + mandatory
    rationale + Apply/Clear) that appears once at least one is checked.
    Selection state lives entirely client-side — read from the DOM's
    checkbox `checked` attributes at submit time via a small inline
    `episodeBulkUpdate()` script, not synced to the server on every
    toggle — because the rail's own render is deliberately decoupled from
    `selected_cluster_id()` already (so a single-cluster click does not
    replace the whole rail and lose scroll position); round-tripping every
    checkbox toggle through a reactive would reintroduce exactly that
    problem for a feature whose whole point is checking several boxes in
    quick succession. `episode_app_server_assessment_actions()` gained a
    `bulk_assess_submit` observer that loops
    `episode_app_submit_assessment()` once per selected cluster, each
    getting its own `episode_assessment_event` row exactly as a
    one-at-a-time classification would (nothing new at the write-model
    level, only at the UI/submission level) — re-checks `current_user()`
    server-side and requires a non-empty rationale before writing
    anything, same as the single-cluster path, since the DOM/onclick is
    not a trust boundary.

43. **Added an "Info" screen** (`R/app_info.R`, nav entry, no `con`
    dependency since its content is static) explaining the three
    detection algorithms in a table (what each is, statistical vs.
    rule-based, how it decides to fire), the six cluster states, and the
    anonymous-read / sign-in-to-classify access model — so an
    epidemiologist reading an unfamiliar detector name on a dossier has
    somewhere in the app itself to look it up.

## Palette & branding

44. **Palette translated to English and made operator-configurable.**
    Dutch hue names (`blauw`, `groen`, `roze`, `geel`, `lila`, `bruin`)
    removed from R; the fallback palette moved from hardcoded R literals
    into `inst/config/palette.yaml`, resolved the same defaults-then-override
    way as `episode_config_resolve()` but through its own file and
    `EPISODE_PALETTE_CONFIG` env var (deliberately never touching
    `config_hash` — colour is a display concern, not detection
    reproducibility). Semantic role colours (`ink`, `petrol`, `carmine`...)
    are now derived from the base hues in one place
    (`episode_palette_semantic()`) rather than duplicated as separate
    literals, fixing a latent bug where a `certestyle` override only ever
    touched the raw hue keys and left the semantic tokens — the ones the
    CSS actually reads — on the shipped defaults. `episode_palette_from_
    certestyle()` reads through a name-based getter rather than `$`,
    since `certestyle::certe.colours` is a named character vector, not a
    list (`$` is invalid on an atomic vector — this crashed
    `episode_run_app()` for anyone with `certestyle` actually installed,
    caught only once tested against the real package). The shipped
    default palette itself uses generic, organisation-neutral colours,
    not Certe's own house colours, since that file is what a stranger
    cloning the repository sees; a real Certe instance gets Certe's
    actual colours from `certestyle` when installed.

45. **The shipped default palette went through two redesigns, both from
    direct user feedback.** The first shipped default (muted teal/blue/
    rose/amber) turned out to be visually indistinguishable from Certe's
    real house colours even with `certestyle` genuinely installed — the
    fallback exists precisely so an uninstalled/misconfigured `certestyle`
    is obviously different from the real thing, and it wasn't. A bolder
    violet-led scheme fixed that, but read as "too heavy on the eyes":
    the neutral text/border/background colours had been deriving from the
    brand hue instead of standing independently, so body text was
    literally rendered in a tint of whatever colour happened to be
    primary. The current palette fixes both: primary is now a forest
    green rather than violet, and neutrals (`ink`/`muted`/`border`/`bg`)
    are defined independently of any brand hue. Raw hue-word keys
    (`blue`/`green`/`pink`/`yellow`/`lilac`/`brown`) were also renamed to
    Bootstrap-style semantic roles (`primary`/`secondary`/`tertiary`/
    `success`/`warning`/`danger`) on request, each still mapped to one of
    `certestyle`'s six real hues when installed. This surfaced a real bug
    along the way: `episode_app_palette_css()` generated CSS custom
    property names from R's list names verbatim (e.g.
    `--episode-primary_tint`, underscored), but the stylesheet's `var()`
    references use the CSS convention (hyphenated,
    `--episode-primary-tint`) — every multi-word role's colour was
    silently not applying in the browser despite all R-side logic and
    automated tests passing, since nothing checked the two naming
    conventions actually matched. No test currently guards this; flagging
    as a gap rather than adding one speculatively.

46. **`certestyle::certe.colours`'s real key names are prefixed
    "certe"** (`certeblauw`, `certegeel`, ... — verified against
    `certe-medical-epidemiology/certestyle`'s own `R/certe_colours.R`,
    since guessing at Certe-internal naming had already caused prior bugs
    in this file). `episode_palette_from_certestyle()` had been looking
    up the bare hue word, which never matched, so a real Certe instance
    with `certestyle` installed silently got the shipped fallback palette
    instead of Certe's actual colours the entire time — this is also the
    real explanation for the earlier "the fallback looks too similar to
    Certe's colours" report (item 45): there was never a second palette
    to compare against, only the fallback shown twice.

## Accounts & assessment workflow

47. **Account bookkeeping (`episode_app_user`'s `password_hash`,
    `must_change`, `last_login_at`) is insert-only, event-sourced via a
    new `episode_app_user_event` table**, not `UPDATE`. The app only ever
    inserts, with no locking added — verified by inspection and enforced
    by an automated test (`test-insert_only.R`) that the app's own write
    surface issues no `UPDATE` or `DELETE` statements at all (scoped to
    the files a live Shiny session can actually reach, not the cron,
    which legitimately `UPDATE`s the facts it owns per ARCHITECTURE.md
    section 5.0). The "current" password hash is the most recent
    `password_change` event's, falling back to the account row's own
    initial value; "current" `must_change` and `last_login_at` are derived
    the same way — mirroring the pattern `episode_cluster_state` already
    established for cluster state.

48. **Closure (ARCHITECTURE.md section 6.1, "closure is an act, not a
    classification") is represented as an `episode_cluster_state` row**
    (`trigger = "closure"` for a person, `"system"` for cron auto-close),
    not as a new `episode_assessment_event`: the classification being
    closed already carries its own rationale, and closure records that it
    is over, not what it was. Found and fixed a real bug while wiring
    this: `episode_derive_state()` checked `nrow(events) == 0` before ever
    looking at `explicitly_closed`, so a cluster the cron auto-closes
    without anyone ever assessing it (ARCHITECTURE.md section 6, step 5 —
    "no assessment exists" is itself eligible for auto-closure) would
    read as "new" forever and never leave the open rail, since such a
    cluster genuinely has zero assessment events. Reordered so
    `explicitly_closed` is checked even when `events` is empty; updated
    the existing test that had asserted the old (incorrect) behaviour,
    and added a regression test for the cron auto-close case specifically.

49. **Mute revocation (`episode_stream_mute.revoked_at`) is left
    unimplemented.** Muting with a reason and expiry is implemented;
    revoking early is not — wiring `revoked_at` would need either an
    `UPDATE` (against the insert-only rule, item 47) or its own
    event-sourced redesign of the mute table, neither of which has been
    asked for yet. The column stays in the schema, always `NULL`, until
    it is actually needed.

50. **`episode_provision_user()` did not exist until asked for
    directly.** Earlier work claimed "accounts, login only to classify"
    as delivered, but no code path actually created an account — only ad
    hoc test/verification setup did. Added as an exported function plus a
    README "Accounts" section; there is deliberately no in-app account
    management screen (ARCHITECTURE.md section 12 describes accounts as
    externally provisioned), so this is the intended permanent entry
    point, not a stopgap.

51. **`episode_provision_user()` and `episode_run_app()` take `db_path`
    defaulting to the `EPISODE_DB` environment variable**, rather than
    requiring the caller to construct a `con` (or pass a literal path
    every time) by hand. `episode_db_open()` is the shared utility —
    resolves `EPISODE_DB`, connects, and gives a clear error if neither is
    set — so provisioning an account is one call at the console.
    Documented alongside `EPISODE_CONFIG` and `EPISODE_PALETTE_CONFIG` in
    the README "Environment variables" section.

## Geography

52. **Geography (the choropleth panel) does not depend on `certegis`.**
    An early version of `episode_ui_geo_map_chart()` called
    `certegis::add_map()` directly — Certe's own package, useful only to
    a Dutch operator, which is inconsistent with every other domain
    concept in this codebase (`pathogen`, institution types, region
    codes) already being operator-defined values rather than something
    baked in. The contract in `R/geo_data.R` is generic and
    country-agnostic instead: any `sf` object with a `pc` column
    (matching whatever an operator's own `episode_case.pc4` values are —
    real Dutch postcodes, zip codes, municipality codes, anything; this
    package never validates or interprets that column beyond joining it)
    and a `geometry` column. EpiSODE ships a Netherlands PC4 default
    (`inst/extdata/geo_postcodes4_nl.rds`, geometry only — population and
    area columns were dropped, not needed for the choropleth and not ours
    to redistribute beyond it — copied from `certegis` under the same
    GPL-2 licence, provenance in `data-raw/geo_postcodes4_nl.R`),
    overridable per-instance via `EPISODE_GEO_DATA`, the same shape of
    solution `EPISODE_CONFIG`/`EPISODE_PALETTE_CONFIG` already establish.
    `sf`/GDAL/GEOS/PROJ are a real system-level dependency beyond what
    CRAN alone supplies, which is exactly why the whole feature is
    guarded end-to-end: no `sf` means the geography panel silently falls
    back to the existing PC bar breakdown.
    `tests/testthat/test-geo_data.R` is consequently guarded with
    `skip_if_not_installed("sf")`. Region *naming* (Gebied/Provincie
    labels in `R/lattice_enumerate.R`, `R/app_read.R`) is a related but
    distinct, still-open gap: it also referenced `certegis` before this
    change and has been reworded to point at the same
    operator-suppliable-data pattern, but no such region-reference
    contract has actually been built yet — only the choropleth's geometry
    contract was in scope.

53. **Geographic reference data's join column is `pc`, not `pc4`.** `pc4`
    still implied a 4-digit Dutch postcode even though the column was
    already documented as accepting any code an operator's own
    `episode_case.pc4` values use. Renamed the *contract's* column to
    `pc` in `R/geo_data.R`, the shipped `inst/extdata/geo_postcodes4_nl.rds`,
    and `data-raw/geo_postcodes4_nl.R`. `episode_case.pc4` itself (the
    case-level DB column everything ultimately reads from) is unchanged —
    it is an internal, longstanding field name, not part of the
    operator-facing geo-data contract. Also removed the choropleth's
    lat/lon graticule and axis ticks (`coord_sf(datum = NA)`) — a map
    that isn't meant to be read at coordinate precision doesn't need them.

54. **`EPISODE_GEO_DATA_OVERLAY`: a second, independent geographic layer**
    — region outlines (provinces, municipalities, whatever an operator
    wants for orientation) drawn with colour but no fill, a thicker line
    than the choropleth, on top of it. Deliberately a much thinner
    contract than `EPISODE_GEO_DATA`: just an `sf` object with a
    `geometry` column, no `pc` join at all, since an outline layer
    carries no case counts of its own to attach. No shipped default
    (unlike the PC4 choropleth) — region boundaries are far more
    jurisdiction-specific than postcode geometry, and shipping "a"
    default would just be an arbitrary choice of country dressed up as a
    sensible one. `episode_ui_geo_map_chart()`'s overlay `geom_sf()` call
    is wrapped in its own `tryCatch()` separate from the choropleth's — a
    bad or CRS-mismatched overlay file must not take down a choropleth
    that was already rendering fine without it.

## Reporting

55. **`episode_report_render()`, its schema, and the report itself.**
    Gathers the same read models the app itself uses
    (`episode_cluster_object()`, `episode_app_epi_curve()`,
    `episode_app_linelist()`, `episode_app_similar_clusters()`) into a
    Quarto template (`inst/report/cluster_report.qmd`), with version
    numbering (`max(existing) + 1`, not a running counter, so a version
    can never collide even if rows are inspected out of order), a
    SHA-256 of the rendered file, and a dossier panel with a
    render-on-demand button for signed-in users.
    `episode_quarto_available()` guards on both the `quarto` R package
    *and* the separate Quarto CLI binary — `quarto::quarto_path()`
    returns `NULL` rather than erroring when the CLI is missing, which
    the R package alone does not make obvious.

56. **Line-list inclusion is a render-time parameter
    (`include_linelist`)**, independent of the *rendering* user's later
    session, matching how the live app already gates the panel on the
    *viewing* session (ARCHITECTURE.md section 9) — the two are
    deliberately different mechanisms for different audiences (a
    signed-in assessor rendering a report is not the same question as an
    anonymous viewer of the live app).

57. **Small-count suppression (`episode_report_suppress_small_counts()`)
    replaces a count with `"<threshold"` rather than blanking it or
    rounding to a bucket** — the simplest disclosure-control convention,
    easy for the reader to reconstruct as "somewhere between 1 and
    threshold-1" without a lookup table. Applied only to the report's
    geography breakdown (the one table ARCHITECTURE.md section 9
    specifically calls out); the live app's own geography panel is
    unaffected, matching "configurable for reports leaving the
    department" — a live in-app view is not leaving the department.

58. **Report rendering: `quiet = TRUE` was swallowing the real Quarto
    error.** The `quarto` R package always captures the CLI's stderr into
    the condition it raises on failure, but only *includes* it in the
    error message when the CLI itself was not told to run quietly — with
    `quiet = TRUE` a failure surfaced only as "Error returned by quarto
    CLI... Rerun with `quiet = FALSE` to see the full error message",
    with the actual cause never reaching the caller (or the in-app error
    banner) at all. Changed to `quiet = FALSE` in
    `episode_report_render()`, and both there and in
    `episode_app_server_report()`'s error display, switched from
    `conditionMessage()` to `rlang::cnd_message(e, inherit = TRUE)` so the
    full parent-condition chain (the actual Quarto/pandoc/chunk error)
    reaches the user, not just the wrapper's own top-level message.

59. **Report render button had no "in progress" feedback.** The render
    itself is a long, synchronous server call that blocks the whole Shiny
    session, so nothing pushed from a server-side observer can reach the
    browser before it finishes — a `reactiveVal` flipped at the start of
    the observer would not actually flush to the client until the
    observer (including the blocking call) returns. Fixed client-side
    instead: the button's own `onclick` disables it and reveals a
    "Rapport genereren..." paragraph synchronously, before the
    `Shiny.setInputValue()` call that starts the render. On success the
    whole dossier pane re-renders (fresh DOM, both reset automatically);
    on failure `episode_app_server_report()`'s error-display `renderUI`
    also emits a small inline `<script>` that resets the button and hides
    the pending text, since nothing else would.

60. **Bug: `inst/report/cluster_report.qmd` was hardcoded Dutch
    throughout, and used "PC4" for the postcode column.** Found by
    actually reading a rendered report sent back by a user, not by
    inspection alone. The template took a `lang` parameter and defined a
    `tr()` helper, but only ever used it for the level label and the
    curve-shape fragment — every section heading, table header, and
    static phrase ("Kerncijfers", "Geen gegevens beschikbaar.", "n.v.t.",
    the "PC4"/"Aantal" table header, the closing note) was a literal
    Dutch string, so an `lang = "en"` report still rendered entirely in
    Dutch except for those two spots. Also inconsistent with the app's
    own choropleth work (item 53 above): the in-app line list and
    geography panels already say "PC", not "PC4", but the report template
    still said "PC4". Fixed by routing every static label through `tr()`,
    reusing existing app-facing i18n keys where the wording already
    matched (`panel.geo.title`, `panel.geo.aside` = "PC",
    `panel.linelist.col.*`, `panel.similar.*`, `verloop.title`,
    `dossier.stat.*`) and adding a small set of new `report.*` keys in
    both `nl.json`/`en.json` for the handful of labels with no existing
    app equivalent (`report.key_figures_title`, `report.na`,
    `report.rt_title`/`report.rt_note`, `report.footer`, etc.). Verified
    with `knitr::knit()` directly against real cluster data — confirmed
    the English render now reads "Key figures" / "Confirmed" /
    "Expected" / "n/a" throughout instead of a two-word English facade
    over a Dutch document.

61. **`EPISODE_QUARTO_REPORT` and `episode_run_cron()` data-frame
    inputs, both requested directly.** `episode_report_render()` gained a
    `qmd_path` argument (defaulting to `EPISODE_QUARTO_REPORT`, matching
    the `EPISODE_CONFIG`/`EPISODE_GEO_DATA` pattern) so an operator can
    supply their own report template — a department that wants its own
    letterhead or section order does not need to fork the package.
    Extracted the resolution logic into `episode_report_qmd_path()` so it
    is unit-testable independent of Quarto actually being installed.
    Separately, `episode_run_cron()`'s three `*_source_fn` parameters
    (`ingest_source_fn`, `denominator_source_fn`,
    `institution_activity_source_fn`) accept the data frame itself, not
    only a zero/one-argument function that produces one — an operator who
    already has the data sitting in a variable had no reason to wrap it
    in `function() my_df` just to satisfy the parameter. Shared helper
    `episode_resolve_source()` (exported, since it is a small useful
    contract on its own) handles all three call sites identically: `NULL`
    stays `NULL`, a data frame passes through unchanged, a function gets
    called (with `...`, so `institution_activity_source_fn`'s
    `institutions` argument still reaches a real function but is silently
    ignored for a data frame), anything else errors clearly rather than
    failing deep inside ingestion with a confusing type error.

62. **Report template: the pathogen name in the H1 title is italicised**
    via the same `episode_ui_italicise_taxon()` used everywhere else
    pathogen names render — it happens to be the only place in the
    template a pathogen name appears outside a context that's already
    scoped to one organism (linelist, similar-clusters table).

## Analytics: priority score, similar clusters, performance

63. **Historical analogue matching (`episode_app_similar_clusters()`,
    "Vergelijkbare clusters") uses an unweighted sum of four `[0, 1]`
    similarity scores** (level match, log-ratio size similarity, circular
    day-of-year season distance, duration similarity) — ARCHITECTURE.md
    section 10.1 specifies the matching *dimensions* (organism, level,
    size, season, duration) but not a weighting scheme between them, so
    this is a documented judgement call rather than a tuned model,
    flagged alongside the codebase's other provisional numeric choices
    for future calibration (item 68). Organism match is exact-string and
    mandatory (not a scored dimension, a filter), consistent with how
    `pathogen` is treated as unconstrained free text everywhere else in
    this codebase.

64. **The Prestatie (Performance) screen**
    (`episode_app_performance()`, `episode_ui_performance_screen()`,
    `nav.performance` tab), matching ARCHITECTURE.md section 9's
    description: positive predictive value per detector per organism, the
    classification distribution, and three timeliness figures (first case
    to detection, detection to first assessment, detection to
    classification). PPV counts a cluster's *latest* verdict only (an
    earlier `artefact` superseded by a later `confirmed_epidemic` after
    more evidence came in should not count as a false positive forever),
    attributed to every detector that ever flagged that cluster;
    `cluster_not_yet` and an unassessed cluster are excluded from PPV
    rather than counted as either outcome, since neither is a judgement
    yet. Timeliness reads `episode_cluster.opened_at`/`first_day` and
    `episode_assessment_event.created_at` directly, not the state
    trajectory (`episode_cluster_state`) — ARCHITECTURE.md section 6.4's
    "supplies the timeliness figures ... directly rather than by
    reconstruction from event timestamps" is about avoiding
    reconstruction from state *transitions*, and the columns this screen
    needs are already stored directly on the two tables it reads. Median,
    not mean, since a single very slow assessment should not be able to
    swamp the reported figure the way it would a mean. "PVW" (the literal
    Dutch translation of the abbreviation) was tried on this screen and
    reverted back to "PPV" — no Dutch epidemiologist actually says "PVW"
    for positive predictive value in practice. The i18n *key*
    (`performance.col.ppv`) stays, since another locale might genuinely
    want a translated abbreviation; only the Dutch *value* changed back.

65. **Not attempted: eligibility gate tuning, priority score weight
    recalibration, suppression threshold review.** All three are
    explicitly volume/history-dependent (tuned against real signal
    volume, towards roughly ten assessed clusters a month, system-wide) —
    there is no real signal volume in a development environment to tune
    against, only synthetic data whose properties were chosen to exercise
    the detectors, not to resemble a real department's true incidence.
    Once an instance has run against real data for a few months, the
    Prestatie screen (item 64) is exactly the tool for making these
    calibration decisions with evidence rather than guesswork.

66. **Not attempted: the annual overview for the medical board and
    accreditation file.** Also volume/history-dependent by nature (an
    "annual overview" of a system with no real annual history to
    summarise would be either empty or fabricated); the existing Quarto
    reporting infrastructure (`episode_report_render()`,
    `EPISODE_QUARTO_REPORT`) is the natural mechanism to extend for this
    once real data exists to report on — a second template rather than a
    second rendering pipeline.

67. **`episode_ingest_source_synthetic_calibration()`: a
    volume-realistic synthetic generator for one pathogen, for testing
    calibration.** Nothing stops generating *volume-realistic* synthetic
    data for testing calibration, even though it is still not *real* data
    (no amount of realism changes that — this is a testing aid, not a
    substitute for item 65). `episode_synthetic_outbreak_volume()`
    injects many independent `same_place`-shaped case bumps
    (Poisson-distributed per month) for one named pathogen — *
    Clostridioides difficile* by default, this file's own worked example
    of an endemic organism that produces frequent small clusters at a
    busy institution — across LTC institutions and hospitals over the
    whole window, at a volume that actually produces double-digit monthly
    cluster counts once run through detection and reconciliation
    (verified: `n_bumps_per_month = 3-4` over a Poisson process
    realistically produces roughly a dozen clusters for that one organism
    over six months in a run against the synthetic institution set — an
    operator can tune `n_bumps_per_month` up or down and watch the
    Prestatie screen and eligibility gate respond, which is the actual
    point). Every generated case carries a `PT-VOL-*` patient key so it is
    never mistaken for anything but synthetic test data.

68. **Weight and threshold calibration in general needs an instance that
    has actually run for a while against real signal.** This applies
    across the codebase — priority score weights (item 9), the
    eligibility gate (item 5), curve-shape's boundary (item 12), and
    similar-cluster matching weights (item 63) are all documented
    judgement calls shipped as the architecture's stated defaults,
    unmodified, pending real-world evidence. The Prestatie screen (item
    64) and the synthetic calibration generator (item 67) are the tools
    for making that evidence-based, once real signal volume exists.

## Deployment & packaging

69. **Which machine runs the app long term.** Not decided here; an
    operator's own deployment concern.

70. **`LICENSE` (an unmodified copy of the stock GPL-2 text) was removed
    from the package root, then a full `LICENSE.md` was added back at the
    repository root.** `DESCRIPTION`'s `License: GPL-2` (no `+ file
    LICENSE`) already fully specifies the license without needing a
    physical copy in the R package itself; a physical `LICENSE` file
    there produced R CMD check's "LICENSE not mentioned in DESCRIPTION"
    NOTE (`License: GPL-2 + file LICENSE` is for a *modified* license
    text, which would itself trigger a different NOTE for an unmodified
    stock license — confirmed by trying it and reverting). `LICENSE.md`
    at the repository root (outside what R CMD check inspects) serves
    GitHub's own license detection and any casual visitor instead.

71. **`certeplot2` was a dead dependency.** Declared in `DESCRIPTION`
    (`Suggests`/`Remotes`) but never actually referenced anywhere in
    `R/` — a leftover from early planning, not a real dependency.
    Removed; the one real optional Certe package remaining is
    `certestyle` (`episode_palette()`). Every optional dependency
    (`certestyle`, `AMR`, `EpiEstim`, `mem`, `quarto`, `sf`) is gated
    behind `requireNamespace()` with a documented fallback, spot-checked
    and listed in a table in the deployment vignette. README's "Certe
    packages" wording corrected to match.

72. **`episode_demo()`**: a stranger cloning the repository can run the
    whole system in under a minute with one call — a temp SQLite
    database, a completed detection run against the bundled synthetic
    generator, a demo assessor account (credentials printed via
    `message()`), and the app itself. Formalised as an exported,
    documented, tested entry point. `ingest_source_fn`/
    `denominator_source_fn` parameters (defaulting to the full generator,
    unchanged for real use) let tests inject a small window instead of
    the full multi-year synthetic default, keeping the test suite fast.

73. **Vignettes**: `architecture` (engine/instance separation, topology,
    the data model at a glance), `detection-reconciliation` (the five
    detectors, how reconciliation turns raised signals into persistent
    clusters, suppression, derived state), and `deployment` (the data
    contract, `EPISODE_*` environment variables, accounts, the
    SharePoint/OneDrive sync warning, the optional-dependency fallback
    table). Deliberately narrative summaries pointing back at
    `ARCHITECTURE.md` for the exhaustive version, not a restructured copy
    of it — a vignette a first-time reader would actually read end to
    end. `knitr`/`rmarkdown` added to `Suggests`, `VignetteBuilder: knitr`
    added to `DESCRIPTION`.

74. **English i18n verified complete, not rebuilt.** `nl.json` and
    `en.json` carry an identical key set (checked programmatically: zero
    keys missing on either side), and every value that is identical
    between the two languages on inspection is a legitimately shared word
    or loanword (`EpiSODE`, `cluster`, `PC`, `artefact`, `Monitoring`,
    template placeholders), not an untranslated copy-paste.

75. **Git history audit**: no `.sqlite`/`.db` file, `.env`/`.Renviron`,
    private key, or credential-shaped string has ever entered the
    repository's history (checked across every commit on every ref, not
    only the current tree). Every email address in the history is either
    a genuine package author's own contact address (`DESCRIPTION`, git
    author identity — expected and intentional) or an obviously synthetic
    test/demo placeholder (`example.org`, `example.com`, `x.nl`,
    `domain.com`). Nothing patient-identifiable or instance-specific has
    ever been committed.

76. **README screenshots: placeholders shipped, real images left to be
    added.** The README links two named files
    (`data-raw/screenshots/dossier.png`, `.../performance.png`, with
    `data-raw/screenshots/README.md` documenting what each should show)
    that do not exist yet. `data-raw/` is already `.Rbuildignore`'d, so
    these never reach the CRAN tarball once added, while still rendering
    normally on GitHub (which reads the git tree directly, not the
    package build). An automated capture pass (e.g. via `chromote`) was
    considered and set aside: each `episode_demo()` call runs a full
    synthetic cycle over its entire default multi-year window (multiple
    minutes), and a properly populated, signed-in dossier view needs at
    least two sequential app interactions on top of that — real
    screenshots, taken by hand against a live instance, were judged
    simpler and more representative.

77. **CI: `.github/workflows/R-CMD-check.yaml`**, the standard
    `r-lib/actions` template (`usethis::use_github_action("check-standard")`'s
    shape) — macOS release, Windows release, Ubuntu devel/release/oldrel-1,
    triggered on every push to `main` and every pull request. GitHub-hosted
    runners have Pandoc (via `r-lib/actions/setup-pandoc`) and working
    system libraries for `sf`, unlike some development environments, so
    CI exercises the vignette build and the `sf`-gated tests end to end.

## Open items

- **`episode_stream`'s `ward` column** (item 17 above) is a schema change
  relative to the architecture document's literal DDL — flagged for
  review rather than treated as settled.
- **Cluster volume for endemic organisms at a single place** (item 16
  above) — two directions were discussed and deliberately parked.
- **Calibration of weights and thresholds against real signal** (item 68
  above) — the tools exist (Prestatie screen, synthetic calibration
  generator); the calibration itself needs a real instance with real
  history.
- **Mute revocation** (item 49) and **region-reference naming beyond the
  choropleth's own geometry contract** (item 52) are documented, narrow
  gaps, not yet built.
