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

39. **`episode_provision_user()` and `episode_run_app()` now take
    `db_path` defaulting to the `EPISODE_DB` environment variable**,
    rather than requiring the caller to construct a `con` (or pass a
    literal path every time) by hand. `episode_db_open()` is the new
    shared utility - resolves `EPISODE_DB`, connects, and gives a clear
    error if neither is set - so provisioning an account is one call at
    the console. Documented alongside `EPISODE_CONFIG` and
    `EPISODE_PALETTE_CONFIG` (previously used but never written down) in
    a new README "Environment variables" section.

40. **The classification and mute-reason pickers were rebuilt as
    colour-coded button lists**, matching `episode-mockup.jsx`'s design,
    replacing the `<select>` elements substituted in during M3 - a
    dropdown hides the mapping between an option and its meaning behind
    a click, where the mockup's own design puts option, colour and hint
    text in view at once. `episode_ui_picker()` is a small reusable
    building block (plain inline `onclick`, consistent with the rest of
    the app's forms - no new JS file). Added a one-line `<p>` above the
    mute picker explaining what muting actually does, since "Stream
    dempen" alone does not say why or when to use it.

41. **Two real bugs found via user testing of the M3 UI**: the rail
    list's ratio line rendered literally "ratio NA" for any cluster
    detected by a rule-based detector (`same_place`, `rare_trigger`),
    which carries no `expected`/`ratio` at all - fixed by omitting the
    ratio segment when it is `NA` rather than formatting it anyway. And
    `episode_ui_format_datetime()` parsed a stored UTC timestamp with
    `as.POSIXct(iso, tz = "UTC")` but then called `format()` without an
    explicit `tz`, which uses the *parsed object's own* `tzone`
    attribute (still `"UTC"`) rather than the system's - every displayed
    time (status strip, assessment timeline, and the Activity/Archive
    tabs, which had not been going through this formatter at all) was
    silently shown in UTC labelled as if local. Fixed by passing an
    explicit `tz` argument (defaulting to `Sys.timezone()`, exposed as a
    parameter rather than hardcoded so tests can pass a fixed zone -
    `Sys.timezone()` caches its result for the R session, so
    `Sys.setenv(TZ = ...)` after the first call has no effect on it).

42. **Several dossier/interpretation wordings tightened on user
    feedback**: the top metrics card's "Waargenomen"/"Observed" label was
    ambiguous next to "verwacht"/"expected" (found vs. lab-confirmed vs.
    statistically expected all read plausibly) - renamed to
    "Vastgesteld"/"Confirmed". The case-free stat showed two raw numbers
    separated by a slash with no indication which was which
    (`"1462 / 21 d"`); split into a value ("1462 d") and a sub-label
    ("drempel 21 d"/"threshold 21 d") using the existing stat-tile
    pattern. "PC4" was relabelled "PC" everywhere user-facing (the
    underlying `pc4` column and ingestion contract are unchanged - this
    is a label-only change, not a granularity change, so an operator
    supplying coarser or finer postcodes is not misled by a label that
    implies exactly four digits). The concentration interpretation
    fragments named a PC4 value bare (`"de grootste locatie, 7228,
    omvat..."`, ambiguous with an actual named place) - now prefixed
    "postcode" everywhere a PC value appears in prose. "Locatie" was
    also dropped from the diffuse-concentration fragment's own wording
    (Certe's own jargon uses "locatie" for a physical centre/institution,
    a different concept from a PC-based geographic area) in favour of
    "het hoogst epidemische gebied"/"the area with the most cases".

43. **Added `episode_format_date_range()`**, collapsing a cluster's
    `first_day`/`last_day` into `"7-15 jan. 2025"` rather than repeating
    the month and year per endpoint, falling back a step at a time as
    the range widens (same month, same year, different years) and
    collapsing to a single date for a one-day cluster. Used on the rail
    list, which previously showed no date range at all.

## New in M4 and M5

1. **`episode_report_render()`, the report itself, and its schema were
   partly pre-scaffolded from earlier work** - `episode_report_render`'s
   table and `episode_db_report_render_insert()` already existed in the
   schema/write layer, unused. M4 added the missing half: gathering the
   data (reusing the same read models the app itself uses - `episode_
   cluster_object()`, `episode_app_epi_curve()`, `episode_app_linelist()`,
   `episode_app_similar_clusters()`), the Quarto template
   (`inst/report/cluster_report.qmd`), version numbering (`max(existing)
   + 1`, not a running counter, so a version can never collide even if
   rows are inspected out of order), a SHA-256 of the rendered file, and
   a dossier panel with a render-on-demand button for signed-in users.
   `episode_quarto_available()` guards on both the `quarto` R package
   *and* the separate Quarto CLI binary - `quarto::quarto_path()` returns
   `NULL` rather than erroring when the CLI is missing, which the R
   package alone does not make obvious, and is exactly the situation in
   this development environment (verified directly: `quarto` R package
   installs cleanly, the CLI does not exist here at all).

2. **Line-list inclusion is a render-time parameter
   (`include_linelist`)**, independent of the *rendering* user's later
   session, matching how the live app already gates the panel on the
   *viewing* session (ARCHITECTURE.md section 9) - the two are
   deliberately different mechanisms for different audiences (a
   signed-in assessor rendering a report is not the same question as an
   anonymous viewer of the live app).

3. **Small-count suppression (`episode_report_suppress_small_counts()`)
   replaces a count with `"<threshold"` rather than blanking it or
   rounding to a bucket** - the simplest disclosure-control convention,
   easy for the reader to reconstruct as "somewhere between 1 and
   threshold-1" without a lookup table. Applied only to the report's
   geography breakdown (the one table ARCHITECTURE.md section 9
   specifically calls out); the live app's own geography panel is
   unaffected, matching "configurable for reports leaving the
   department" - a live in-app view is not leaving the department.

4. **The cool-down escape hatch (ARCHITECTURE.md section 6.5) surfaced a
   deeper pre-existing gap while implementing it: `cooldown_days` had
   never been read anywhere in reconciliation at all**, despite being a
   real schema/config column since M1 - there was no cool-down
   suppression to build an escape hatch *onto*. Implemented both
   together: `episode_reconcile_find_cooldown_match()` only runs for a
   candidate that found zero ordinary matches (i.e. already outside
   `case_free_days`), looks one step further to `cooldown_days`, and
   absorbs it into a terminal-verdict (`artefact`/`expected_variation`)
   closed cluster rather than opening a new one - flagging
   `changed_since_assessment` only when the candidate's case count
   clears `cooldown_reopen_ratio` ("half again") against the closed
   cluster's own count. This also surfaced a second, independent latent
   bug: `episode_derive_state()` treated a terminal verdict as
   unconditionally "closed" regardless of `changed_since_assessment`, so
   even the *pre-existing* case-free-days-driven update path (which
   already set the flag correctly) had no way to surface it in the UI -
   fixed with a one-line change reusing the exact mechanism already
   built for non-terminal verdicts, rather than inventing new
   `episode_cluster_state` choreography.

5. **Baseline feedback (`episode_baseline_excluded_windows()`)
   excludes a stream's own confirmed-epidemic periods from what gets fed
   to Farrington**, both for detection and for the persisted trend
   series, and is listed on the Streams screen per ARCHITECTURE.md
   section 7.6's own requirement. The exclusion removes the affected
   case rows from the series entirely, covering the whole window
   regardless of which historical weeks `farringtonFlexible()`'s own
   `b`/`w` reference-selection actually reads for a given evaluated week
   - simpler than trying to track exactly which weeks Farrington would
   have referenced, and the plain-language architecture text ("excluded
   from the baseline") does not specify anything more precise than that.

6. **Patient-day normalisation (ARCHITECTURE.md section 7.1) is
   implemented at L2 (institution) only, never L1 (ward)** - there is no
   ward-level activity table in the schema, and section 7 itself only
   promises ward patient-days "if obtainable"; they are not. L1 detection
   is unaffected (still raw counts, exactly as before this existed).
   `episode_synthetic_institution_activity_source()` is a new, entirely
   optional data source (mirroring `episode_denominator_source_synthetic()`'s
   own established pattern) - `episode_run_cron()`'s new
   `institution_activity_source_fn` parameter defaults to `NULL`, so no
   existing caller's behaviour changes unless it opts in. Confirmed by
   direct inspection that `surveillance::sts()` accepts a `population`
   argument and `farringtonFlexible()`'s control list accepts
   `populationOffset`, resolving the "implementation note requiring
   verification" ARCHITECTURE.md section 7.1 itself flagged as unverified
   when this codebase depended on `certestats`'s wrapper - moot now that
   `surveillance::farringtonFlexible()` is called directly (`QUESTIONS.md`'s
   own R-milestone item about dropping `certestats`).

7. **Curve-shape classification (`episode_classify_curve_shape()`) is a
   Duiding fragment, not a dossier panel** - re-reading ARCHITECTURE.md
   section 9 closely: it says curve shape is "stated in the Duiding, with
   an intermediate verdict where the evidence is ambiguous", not that it
   gets its own visual panel the way Rt or geography do. The `2 ×
   incub_max_days` boundary between "propagated" and "ambiguous" is not a
   value the architecture specifies numerically (only "the first question
   in any outbreak investigation" and "an intermediate verdict" are
   given) - a documented judgement call, flagged for calibration
   alongside the codebase's other provisional M1-era thresholds (M6).

8. **MEM (`episode_detect_mem()`, the `mem` CRAN package - added to
   `Suggests`) runs only on `pathogen_region` (L5) streams**, for
   organisms flagged `mem_applicable` (Influenza A/B, RSV in the shipped
   `pathogen_config.csv`) - it needs a stable, population-level weekly
   series across several historical seasons to fit at all, which no ward
   or single institution has the volume to support, and ARCHITECTURE.md
   itself frames seasonal surveillance as a population-level question.
   `same_place` and Farrington are unaffected at every other level.
   Verified against the real `mem` package (installed and exercised
   directly, not guessed at from documentation alone - the same lesson
   the `certestyle` naming mistake taught earlier in this project):
   - **Season convention**: `mem`'s own bundled `flucyl` example data
     confirms the northern-hemisphere week-40-to-week-20 convention
     ARCHITECTURE.md section 7.3 describes, with row labels `"40".."52",
     "1".."20"` (33 weeks) and column labels `"YYYY/YYYY"`. Week 53 (some
     years have one) is folded into week 52 - `mem`'s own guidance
     ("accommodate week 53") describes a distinct-column treatment this
     does not attempt; a documented simplification.
   - **Threshold extraction**: `memmodel()`'s help text states "pre.post.
     intervals... Threhold is the upper limit of the confidence
     interval" - confirmed this means column 3 (not column 2, the point
     estimate) of the returned 2x3 matrix, via direct inspection of a
     fitted model's structure against that documentation.
   - Requires at least 2 complete prior seasons before firing at all
     (`min_seasons`), mirroring Farrington's own `b`-parameter baseline
     requirement; returns `NULL`/an empty detection record for an
     off-season date, no case data, `mem` not installed, or insufficient
     seasons, at every call site (`episode_detect_mem()`,
     `episode_mem_status()`, and the closure criterion via
     `episode_closure_criterion_met(mem_applicable = TRUE, mem_status =
     NULL)`) - a `mem_applicable` stream simply never closes via this
     criterion when MEM cannot be computed, rather than silently falling
     back to a case-free-days threshold that is not epidemiologically
     meaningful for a seasonal pathogen (ARCHITECTURE.md section 6.3).
   - End-to-end verified against a real cron run with realistic synthetic
     seasonal data: fires during peak season (mid-January), does not fire
     off-season or with only one season of history.

9. **Rt (`episode_compute_rt()`, `EpiEstim::estimate_R()`) uses
   `method = "parametric_si"` unconditionally** - `si_dist`
   (`gamma`/`lognormal`) is recorded in `episode_pathogen_config` but not
   yet used to select a distribution family, since `EpiEstim`'s
   parametric method only ever fits a Gamma-shaped serial interval
   (mean/sd parameterised) regardless of the literature source's own
   distributional assumption. A known simplification, not a silent one.
   Estimates whose window ends within `incomplete_days` (from
   `episode_app_completeness()`, the same reporting-triangle-derived
   figure the epi curve's own shading already uses) of the run date are
   withheld from the returned series entirely - not shown-and-captioned -
   since the trailing days are under-ascertained by construction and an
   Rt estimate ending there would read a reporting artefact as a real
   transmission change.

10. **Historical analogue matching (`episode_app_similar_clusters()`,
    "Vergelijkbare clusters") uses an unweighted sum of four `[0, 1]`
    similarity scores** (level match, log-ratio size similarity,
    circular day-of-year season distance, duration similarity) -
    ARCHITECTURE.md section 10.1 specifies the matching *dimensions*
    (organism, level, size, season, duration) but not a weighting
    scheme between them, so this is a documented judgement call rather
    than a tuned model, flagged alongside the codebase's other
    provisional M1-era numeric choices for M6 calibration. Organism
    match is exact-string and mandatory (not a scored dimension, a
    filter), consistent with how `pathogen` is treated as unconstrained
    free text everywhere else in this codebase.

## Parked for a future milestone

1. **Cluster volume for endemic organisms at a single place.**
   `same_place` has no eligibility/baseline gate by design (ARCHITECTURE.md
   section 7.2: it exists precisely to catch rare-pathogen bumps a
   baseline model can't see, so it needs none to fire). For an endemic
   organism like *Clostridioides difficile* at a busy institution, the
   default `n_cases: 3` / `k_days: 14` threshold gets crossed repeatedly
   as routine background noise - each bump becomes its own cluster once
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
     (e.g. `n_cases: 5` for *C. difficile*) - a one-line config change,
     cheapest to try first.
   - **A "series" grouping layered on top of clusters** (not a
     reconciliation-level merge, and not a change to `case_free_days`'s
     own closure semantics): clusters sharing a `stream_id` within a new,
     longer `series_gap_days` window would show as one row on the rail
     (aggregate case count and date span), expandable to each
     sub-cluster's own full timeline; classifying the open one would
     flag - not silently apply to - a later cluster that joins the same
     series while its last classification was `artefact`/
     `expected_variation`/`cluster_not_yet`, needing one click to
     confirm rather than a full re-assessment from zero. Real schema and
     UI work; not started.

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
