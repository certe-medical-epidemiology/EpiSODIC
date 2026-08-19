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

## New in M2

23. **Static `ggplot2` charts rather than an interactive htmlwidgets
    stack.** `episode-mockup.jsx` uses Recharts (an SVG/React charting
    library) for the epi curve, long trend and denominator charts.
    MILESTONES.md M2 asks for "Shiny with bslib" but does not mandate a
    specific plotting approach, and ARCHITECTURE.md section 3.3 is
    explicit that the app must only ever do cheap reads, never recompute
    anything expensive at render time. `ggplot2` is already a near-certain
    transitive dependency of the R ecosystem this runs in, needs no extra
    JS toolchain, and its output is deterministic and easy to test outside
    a browser (`expect_s3_class(..., "ggplot")`, as `test-app_ui.R` does).
    An htmlwidgets library (plotly, echarts4r) would get closer to the
    mockup's exact interactivity (hover tooltips, zoom) but adds a
    dependency and a browser-only testing story for comparatively little
    gain in a read-only M2. Revisit if a future milestone asks for
    interactive drill-down on the charts themselves.

24. **`bslib` is used only for the Bootstrap/CSS reset and font-loading
    helper (`bslib::page_fluid()` + `bslib::bs_theme()`), not for its
    component library.** All visual design (header, nav, panels, chips,
    bars, pyramid) is bespoke `shiny::tags` styled by
    `inst/app/www/episode.css`, matching `episode-mockup.jsx` pixel-for-
    pixel rather than bslib's own Bootstrap look. This is what
    MILESTONES.md M2's "Shiny with bslib" literally asks for (bslib as
    the framework the app is built on) without constraining the app to
    look like generic Bootstrap.

25. **`episode_cluster_object()`'s `concentration` field now carries a full
    per-PC4 breakdown** (`concentration$rows`, a `data.frame(label, n)`
    covering every PC4 the cluster's cases touch), not only the dominant
    PC4's label/count/share. The geography panel (`episode_ui_geo_panel()`)
    needs the whole distribution to draw a bar chart; only ever exposing
    the single dominant row would have made that panel structurally unable
    to show anything but one bar.

26. **`episode_triangle_completeness()` returns an empty completeness
    frame instead of erroring** when every reporting-triangle row falls
    outside the `max_lag_days` window - the situation on a database's very
    first-ever cron run, where every case's only observed `run_date` is
    "today" regardless of how old its `sample_date` is, so lag is huge for
    anything but the most recent cases. `stats::aggregate()` errors
    ("no rows to aggregate") on a zero-row input rather than returning a
    zero-row result; guarded the same way `episode_app_denominator_series()`
    already guards its own `stats::aggregate()` call.

27. **Removed the top-level `LICENSE` file.** It was an unmodified copy of
    the stock GPL-2 text, which `DESCRIPTION`'s `License: GPL-2` (no
    `+ file LICENSE`) already fully specifies without needing a physical
    copy in the package; its presence produced R CMD check's "LICENSE not
    mentioned in DESCRIPTION" NOTE. `License: GPL-2 + file LICENSE` is for
    a *modified* license text (e.g. with additional restrictions), which
    would itself trigger a different NOTE ("License components with
    restrictions") for an unmodified stock license - confirmed by trying
    it and reverting.

## New in M3

28. **Palette translated to English and made operator-configurable**
    (post-M2 follow-up, before M3 proper). Dutch hue names (`blauw`,
    `groen`, `roze`, `geel`, `lila`, `bruin`) removed from R; the
    fallback palette moved from hardcoded R literals into
    `inst/config/palette.yaml`, resolved the same defaults-then-override
    way as `episode_config_resolve()` but through its own file and
    `EPISODE_PALETTE_CONFIG` env var (deliberately never touching
    `config_hash` - colour is a display concern, not detection
    reproducibility). Semantic role colours (`ink`, `petrol`, `carmine`...)
    are now derived from the base hues in one place
    (`episode_palette_semantic()`) rather than duplicated as separate
    literals, fixing a latent bug where a `certestyle` override only ever
    touched the raw hue keys and left the semantic tokens - the ones the
    CSS actually reads - on the shipped defaults. `episode_palette_from_
    certestyle()` now reads through a name-based getter rather than `$`,
    since `certestyle::certe.colours` is a named character vector, not a
    list (`$` is invalid on an atomic vector - this crashed
    `episode_run_app()` for anyone with `certestyle` actually installed,
    caught only once tested against the real package). The shipped
    default palette itself uses generic, organisation-neutral colours,
    not Certe's own house colours, since that file is what a stranger
    cloning the repository sees; a real Certe instance gets Certe's
    actual colours from `certestyle` when installed.

29. **Account bookkeeping (`episode_app_user`'s `password_hash`,
    `must_change`, `last_login_at`) is insert-only, event-sourced via a
    new `episode_app_user_event` table**, not `UPDATE`. Standing brief
    hard rule 7 ("the app only ever inserts... do not add any [locking]")
    reads as unconditional, and MILESTONES.md M3's own "done when" line
    calls for verifying by inspection that the app issues no UPDATE or
    DELETE statements at all - a bar now enforced by an automated test
    (`test-insert_only.R`), scoped to the app's own write surface (not
    the cron, which legitimately UPDATEs the facts it owns per
    ARCHITECTURE.md section 5.0). The "current" password hash is the most
    recent `password_change` event's, falling back to the account row's
    own initial value; "current" `must_change` and `last_login_at` are
    derived the same way - mirroring the pattern `episode_cluster_state`
    already established for cluster state.

30. **Closure (ARCHITECTURE.md section 6.1, "closure is an act, not a
    classification") is represented as an `episode_cluster_state` row**
    (`trigger = "closure"` for a person, `"system"` for cron auto-close),
    not as a new `episode_assessment_event`: the classification being
    closed already carries its own rationale, and closure records that
    it is over, not what it was. Found and fixed a real bug while wiring
    this: `episode_derive_state()` checked `nrow(events) == 0` before
    ever looking at `explicitly_closed`, so a cluster the cron
    auto-closes without anyone ever assessing it (ARCHITECTURE.md
    section 6, step 5 - "no assessment exists" is itself eligible for
    auto-closure) would read as "new" forever and never leave the open
    rail, since such a cluster genuinely has zero assessment events.
    Reordered so `explicitly_closed` is checked even when `events` is
    empty; updated the existing test that had asserted the old
    (incorrect) behaviour, and added a regression test for the cron
    auto-close case specifically.

31. **Mute revocation (`episode_stream_mute.revoked_at`) is left
    unimplemented in M3.** MILESTONES.md's M3 scope says "mute with
    reason and expiry", not "revoke early"; wiring `revoked_at` would
    need either an `UPDATE` (against the insert-only rule) or its own
    event-sourced redesign of the mute table, neither of which is asked
    for yet. The column stays in the schema, always `NULL`, until a
    milestone actually needs it.

32. **The cool-down escape hatch (`cooldown_reopen_ratio` in
    `inst/config/default.yaml`, ARCHITECTURE.md section 6.5) is still
    unimplemented.** Spotted while wiring `explicitly_closed`: a cluster
    closed as artefact or normal variation is supposed to reopen as
    Herbeoordeling nodig if the excess later grows by a material margin,
    but nothing in `R/reconcile.R` currently checks for this - the
    config value is defined and unused. This is cron/reconciliation
    logic (M1 scope), not app/UI (M3 scope), so left as a flagged gap
    rather than fixed in passing.

33. **The classification form's "close cluster" confirmation passes the
    translated prompt through a `data-confirm` HTML attribute** (escaped
    by `shiny::tags` the same way any other attribute value is) rather
    than interpolating it into the `onclick` JS string directly - the
    earlier draft did that and would have broken on any future i18n
    string containing an apostrophe.

34. **The shipped default palette went through two redesigns after M3
    shipped, both from direct user feedback.** The first shipped default
    (muted teal/blue/rose/amber) turned out to be visually
    indistinguishable from Certe's real house colours even with
    `certestyle` genuinely installed - the fallback exists precisely so
    an uninstalled/misconfigured `certestyle` is obviously different
    from the real thing, and it wasn't. A bolder violet-led scheme fixed
    that, but read as "too heavy on the eyes": the neutral text/border/
    background colours had been deriving from the brand hue instead of
    standing independently, so body text was literally rendered in a
    tint of whatever colour happened to be primary. The current palette
    fixes both: primary is now a forest green rather than violet, and
    neutrals (`ink`/`muted`/`border`/`bg`) are defined independently of
    any brand hue. Raw hue-word keys (`blue`/`green`/`pink`/`yellow`/
    `lilac`/`brown`) were also renamed to Bootstrap-style semantic roles
    (`primary`/`secondary`/`tertiary`/`success`/`warning`/`danger`) on
    request, each still mapped to one of `certestyle`'s six real hues
    when installed. This surfaced a real bug along the way:
    `episode_app_palette_css()` generated CSS custom property names from
    R's list names verbatim (e.g. `--episode-primary_tint`, underscored),
    but the stylesheet's `var()` references use the CSS convention
    (hyphenated, `--episode-primary-tint`) - every multi-word role's
    colour was silently not applying in the browser despite all R-side
    logic and automated tests passing, since nothing checked the two
    naming conventions actually matched. No test currently guards this;
    flagging as a gap rather than adding one speculatively.

35. **`episode_provision_user()` did not exist until asked for
    directly.** M3's own completion summary claimed "four accounts,
    login only to classify" as delivered, but no code path actually
    created an account - only ad hoc test/verification setup did. Added
    as an exported function plus a README "Accounts" section; there is
    still deliberately no in-app account management screen (ARCHITECTURE.md
    section 12 describes accounts as externally provisioned), so this is
    the intended permanent entry point, not a stopgap.

36. **`certestyle::certe.colours`'s real key names are prefixed
    "certe"** (`certeblauw`, `certegeel`, ... - verified against
    `certe-medical-epidemiology/certestyle`'s own `R/certe_colours.R`,
    since guessing at Certe-internal naming had already caused two prior
    bugs in this file). `episode_palette_from_certestyle()` had been
    looking up the bare hue word, which never matched, so a real Certe
    instance with `certestyle` installed silently got the shipped
    fallback palette instead of Certe's actual colours the entire time -
    this is also the real explanation for the earlier "the fallback
    looks too similar to Certe's colours" report (item 34): there was
    never a second palette to compare against, only the fallback shown
    twice.

37. **Every detection-algorithm identifier surfaced to the user
    (`same_place`, `rare_trigger`, `farringtonFlexible`) now renders in
    `<code>`**, on the dossier's "detected by" line, the settings panel,
    the empty-timeline notice, and the trend panel's notes - these are
    names from the codebase, not prose, and reading as plain text (as
    they did before) made them look like typos or garbled Dutch rather
    than deliberate identifiers. Implemented as `episode_ui_code_join()`,
    escaping first so it is always safe to pass through `shiny::HTML()`;
    building the substituted string via `episode_tr()` and then wrapping
    the whole result in `shiny::HTML()` at the render call, since
    `episode_tr()` itself does plain string substitution with no HTML
    awareness. One caller (`episode_ui_settings_panel()`'s row list) had
    to switch from `c(label, value)` to `list(label, value)`, since `c()`
    silently strips an `shiny::HTML()` value's class when combined with a
    plain string, which would have made that one row's `<code>` tags
    render as visible literal text instead of applying.

38. **Added an "Info" screen** (`R/app_info.R`, nav entry, no `con`
    dependency since its content is static) explaining the three
    detection algorithms in a table (what each is, statistical vs.
    rule-based, how it decides to fire), the six cluster states, and the
    anonymous-read / sign-in-to-classify access model - so an
    epidemiologist reading an unfamiliar detector name on a dossier has
    somewhere in the app itself to look it up.

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
