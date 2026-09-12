# The epidemic reform

Working plan for splitting EpiSODIC's central object into **Outbreaks**
(fine-grained, L1 to L3) and **Epidemics** (coarse, L4 and L5), and for
making the MEM detector genuinely pathogen-agnostic and hemisphere-agnostic.

Tracking issue: **#50**. Related: **#46** (synthetic ground truth).

This file is not shipped: `data-raw` is in `.Rbuildignore`.

---

## 0. How to use this document

You are most likely a Claude session picking this up part-way through. Read
sections 1 to 5 in full before touching any code. They are short, and they
carry every decision that has already been settled. Then find your milestone
in section 7 and do only that milestone.

### The rules of this document

1. **Section 1 is the state of the world.** Read it first. It says which
   milestone is next and what the previous sessions actually did.
2. **Sections 3 and 4 are settled.** Do not relitigate them. If you believe a
   decision is wrong, say so in your reply to the user and stop; do not
   quietly implement something else.
3. **Do exactly one milestone per session** unless the user says otherwise.
   Milestones are sized to be finishable and reviewable.
4. **You MUST update this file before you finish.** Specifically:
   - update the status board in section 1 (milestone state, branch, PR);
   - append a short entry to section 2, the session log, in the format given
     there, stating what you actually did and anything the next session needs
     to know that is not obvious from the diff;
   - if you discovered something that changes a later milestone's
     instructions, edit that milestone in place;
   - if you deferred something, add it to section 8.
   The update is part of the milestone, not an optional extra. A session that
   writes code and does not update this file has failed the milestone.
5. **Keep this document free of history.** State decisions as they stand, in
   the present tense. Do not write "we originally thought", "this used to be",
   or "after discussion". The session log in section 2 is the only place with
   a chronology, and it stays terse. This mirrors the comment rule in
   `CLAUDE.md`, and it exists so that the next model reads a design rather
   than a transcript.
6. **`CLAUDE.md` outranks this file** on code style, testing, comments,
   logging, i18n and database conventions. This file only says *what* to
   build. Read `CLAUDE.md` at the start of every session.

---

## 1. Status board

**Keep this block accurate. It is the first thing the next session reads.**

| | |
|---|---|
| Next milestone to start | **M0** |
| Integration branch | `epidemic-reform` (not yet created) |
| Schema version on `main` | 5 (`episodic_schema_version` in `R/schema_migrate.R`) |
| Execution environment | Must have a working R. See the precondition in section 7. |

| Milestone | State | Branch | PR | Notes |
|---|---|---|---|---|
| M0 MEM: full-year seasons, derived anchor, derived eligibility | not started | | | goes straight to `main`, not to the integration branch |
| M1 Schema: scale, epidemic season satellite, link table | not started | | | |
| M2 Write path: routing, epidemic closure, continuity, links | not started | | | |
| M3 Read path: Epidemics screen MVP | not started | | | |
| M4 Vocabulary: O-/E- ids, Outbreak wording, i18n sweep | not started | | | |
| M5 Documentation | not started | | | |
| M6 Table rename (optional) | not started | | | recommend deferring |

---

## 2. Session log

One entry per session. Newest last. Keep each entry to a few lines: what was
done, what was not, and anything surprising. No narrative.

```
2026-09-12 (Opus 5, planning)
  Wrote this document. No code changed.
  Verified against the tree at 6778425 the facts recorded in section 5.
  M0 was attempted here and abandoned: Claude Code on the web has no R
  (no Rscript, no /usr/lib/R), so nothing could be run. Recorded that as a
  hard precondition in section 7. Every milestone runs where R runs.
```

---

## 3. The design, settled

These decisions are made. They are recorded with their reasons, because a
reason is what lets you make the hundred small decisions the plan does not
cover. They are not open for revision inside a milestone.

### D1. Two scales, one underlying table

The application has two user-facing objects:

- **Outbreak**, at `pathogen_ward`, `pathogen_institution`, `pathogen_area`
  (L1 to L3). Localised. Investigated case by case. Has a line list.
- **Epidemic**, at `pathogen_province`, `pathogen_region` (L4 and L5).
  Regional. Assessed on population curves, not on a line list. Carries the
  annual seasonal declaration where the pathogen is seasonal.

They live in **one table with a `scale` discriminator**, not in two tables.
An Epidemic has a verdict, notes, an audit trail, a report, reconciliation
and a place in the suppression lattice, exactly as an Outbreak does. It is a
cluster at a different scale, not a different kind of thing, and two tables
would duplicate reconciliation, the assessment event stream, notes, the
dossier and the read layer to express a distinction one column wide.

Seasonal detail does **not** go in that table. It goes in a satellite table
keyed to the epidemic row, so no Outbreak row carries a nullable seasonal
column and a non-seasonal Epidemic (a regional Legionella excess, say) has no
satellite row rather than a row full of nulls.

*If, at M3, the Epidemic turns out to need a genuinely different lifecycle
rather than a differently-shaped one, revisit this. Not before.*

### D2. The scale boundary is configuration, not a constant

Which levels are epidemics is an operator setting. L3 `pathogen_area` is
derived by a postcode rule, so its population is not fixed across
deployments: a sparse country's area may exceed a dense one's province.
Hardcoding the boundary is exactly the kind of assumption `CLAUDE.md`
forbids. Shipped default: `pathogen_province` and `pathogen_region` are
epidemics, everything else is an outbreak.

### D3. An Epidemic is not necessarily seasonal

"Epidemic" means regional and impactful, not seasonal. A regional Legionella
excess with no seasonality at all is an Epidemic. Therefore:

- Closure comes from the **post-epidemic threshold** where the pathogen has a
  usable season, and from the existing **case-free-days / stale** rules where
  it does not.
- An Epidemic with no seasonal satellite row shows **no** seasonality panel,
  not an empty one.
- Never treat "no post-epidemic threshold" as "the season has not ended". That
  is the absence-as-zero failure `CLAUDE.md` names, arriving at the level of
  the design.

### D4. Containment is contextual, never causal

An Outbreak is recorded as having occurred **during** an Epidemic, never as
**part of** it. Claiming a ward outbreak is part of the regional epidemic is
a transmission claim that cannot be supported without typing data.

The relation is defined on **pathogen, time overlap, and geographic
nesting**. It is deliberately *not* defined on case-set containment: an L5
epidemic's cases are every case in the catchment, so every outbreak would be
trivially contained and the relation would say nothing.

The link is a table, not a column: an outbreak can be during both a province
epidemic and a region epidemic at once.

### D5. Detection and declaration are two acts

MEM's threshold crossing is a **measurement**. Declaring the season started is
a **decision** with an author, a timestamp and policy consequences across
multiple hospitals. The system raises the signal; a person declares.

Cost asymmetry is why: a false positive on an outbreak dossier costs an
epidemiologist an hour, a false positive on a seasonal declaration costs
screening policy across a whole catchment. An evidentiary bar that high is
never crossed automatically by one week above a threshold.

The signal fires on the **first** crossing, with no confirmation delay, so
timeliness is not lost. Confirmation belongs to the human act.

### D6. A season is 52 or 53 weeks and has no off-season

The current implementation discards ISO weeks 21 to 39 entirely. That is not
a bias towards a time slot, it is nineteen weeks a year in which MEM is
structurally incapable of speech: a summer pathogen is permanently invisible,
and an out-of-season rise is invisible while it happens.

After M0 there is no `NA` season and no truncation. Every week of the year
belongs to a season. Week 53 continues to be folded into week 52, the
documented simplification already in place.

### D7. The season anchor is derived from the data, early in the trough

Something must say where one season ends and the next begins, because
`mem::memmodel()` is defined over seasons. That cut must fall in the trough.
It is derived per (pathogen, stream) from the pooled weekly climatology, never
configured per pathogen and never fixed to week 40.

**Cut at the first week of the trough run, not at the nadir.** The reason is
labelling and fit contamination, not detection latency: an influenza epidemic
beginning in September belongs to the season it opens, and filed as the tail
of the previous season it both misnames every retrospective view and
contaminates that season's column for ever, so every future fit reads epidemic
weeks as pre-epidemic ones and the threshold creeps upward. That bias is
silent and one-directional, towards declaring late.

Worked example: a trough spanning weeks 19 to 35 gives an anchor of week 19
or 20, not the nadir around week 31. Note that the conventional week 40 is
late in the influenza trough, so for influenza the derived anchor lands
roughly twenty weeks earlier than the shipped constant.

### D8. The matrix cut and the epidemic's identity are separate questions

The cut is calendrical and serves the historical fit. Whether an ongoing
epidemic continues or a new one begins is decided by the post-epidemic
crossing. Keep them separate: an epidemic that has not crossed its
post-epidemic threshold keeps its identity across the cut, regardless of which
season column its weeks are filed in. Conflating the two produces a spurious
second declaration every time an epidemic straddles the boundary.

### D9. Seasonality is measured, not curated

Seasonality is a property of a pathogen **in a population**, not of a pathogen
universally. RSV's season moved worldwide after 2020 and returned on different
schedules in different countries; a shipped CSV column cannot know that.

So eligibility for MEM is derived from the instance's own data, and the
pathogen configuration carries a three-state **override** (`auto`, `yes`,
`no`), not a gate. This also removes the hemisphere question entirely: a
southern-hemisphere instance derives a southern anchor from its own series,
with no hemisphere setting anywhere in the code.

### D10. The per-pathogen trough replaces the global off-season

Removing weeks 21 to 39 does not abolish the off-season, it makes it derived
and per-pathogen. The same climatology that yields the anchor yields the
trough **run**, and "this pathogen is in its own trough" is the generalisation
of the old `in_season = FALSE`.

Use it only as a **backstop**: the post-epidemic crossing is the primary
closure, because in an anomalous year the epidemic may still be running when
the historical trough arrives. The backstop earns its place where counts fall
but never cleanly cross the threshold on a noisy series. When it fires, the
audit trail must record that closure was on seasonal grounds, not that a
post-epidemic threshold was crossed. Different evidence, and a reader in three
years must be able to tell which happened.

### D11. MEM and Farrington are complementary, and neither is a pandemic detector

- Farrington compares against the same calendar weeks in previous years, so it
  is **calendar-conditional** and fires at a low absolute count out of season.
  It is the early warning for anything early, large or out of season.
- MEM's pre-epidemic threshold is a **level**, not calendar-conditional. Once
  truncation is gone it fires in any week, but only when the count reaches
  genuinely epidemic level.

For an anomalous early rise the pair is what you want: Farrington says "this
is extraordinary for the time of year", MEM says "and it has reached epidemic
level". Do not try to make MEM a pandemic detector; a seasonal-baseline method
is built to be blind to exactly that, and Farrington already covers it at L4
and L5.

### D12. Suppression stays continuous across the scale boundary

Lattice suppression runs L1 to L5 today. Splitting the object must not sever
it at the scale boundary, or signals suppressed today stop being suppressed,
which is a regression introduced by the refactor. One configured pair crosses
the boundary: `pathogen_area` (outbreak) to `pathogen_province` (epidemic).

Formal suppression cannot help a MEM declaration, because MEM sees only the
population curve and cannot know a rise is concentrated in one institution.
The concentration of the crossing therefore belongs on the declaration screen
as **displayed evidence**, alongside positivity and contributing institutions,
for a person to look at before signing.

### D13. Things that must never happen

Carried from `CLAUDE.md` and restated because this reform touches every one of
them:

- No metric without a denominator rendered as zero. An Epidemic with no
  detected outbreaks reads "none detected", never "0 outbreaks" as a finding.
- No unmeasurable quantity substituted with a default. If the anchor cannot be
  derived, return `NULL` and say so; never fall back to week 40.
- No `format(x, big.mark = ...)`. Every number goes through
  `episodic_format_number()`. Ids (`O-123`, `E-45`) are identifiers, rendered
  as-is, never grouped.
- No `margin-left` in `inst/app/www/episodic.css`. Logical properties only.
- No untested detection, transformation or reporting path.

---

## 4. Vocabulary

Use these words consistently, in code, comments, commits and the interface.

| Term | Meaning |
|---|---|
| Outbreak | A cluster at L1 to L3. Id rendered `O-123`. |
| Epidemic | A cluster at L4 or L5. Id rendered `E-45`. |
| scale | The discriminator: `outbreak` or `epidemic`. |
| season | A 52 or 53 week window starting at the derived anchor week. Label `"YYYY/YYYY"`. |
| anchor week | The ISO week a season starts at. Derived per (pathogen, stream). |
| climatology | The 52-week pooled weekly profile used to derive the anchor and the trough. |
| trough run | The contiguous circular run of weeks below the trough percentile. |
| onset | The week the pre-epidemic threshold was crossed. A measurement. |
| declaration | A person's recorded statement that the epidemic has started or ended. A decision. |
| during | The Outbreak-to-Epidemic relation. Never "part of". |

Ids carry no `#`. They are `O-123` and `E-45`, from two independent sequences,
so `O-12` and `E-12` are different objects and a reader can never confuse them.

---

## 5. Verified facts about the current code

Checked against the tree at commit `6778425`. If you find one of these is no
longer true, fix it here as part of your milestone.

### MEM

- `R/detect_mem.R`, `episodic_mem_season_week()` returns `season = NA_character_`
  for ISO weeks 21 to 39. Those cases are discarded.
- `episodic_mem_seasonal_matrix()` builds a 33-row matrix, weeks `40:52` then
  `1:20`. Week 53 is folded into 52.
- `episodic_mem_status()` returns an early `in_season = FALSE` result off-season,
  before the `mem` and data guards.
- `R/run_cron.R` around line 1069 gates MEM on
  `isTRUE(as.logical(pc_mem$mem_applicable[1])) && identical(stream$level, "pathogen_region")`.
- **There is no seasonal closure anywhere.** `episodic_mem_status()`'s roxygen
  claims it is shared with "the seasonal closure criterion"; grep finds no such
  caller. `episodic_mem_status()` is called only by `episodic_detect_mem()` and
  by `R/app_pathogen.R:562`. M2 therefore *builds* the closure, it does not
  adapt one. Fix that docstring when you get to it.
- `mem_applicable` is read at: `R/db_cron_write.R:65` and `:71`,
  `R/app_pathogen.R:303`, `R/app_pathogen_ui.R:811`, `R/app_read.R:369`,
  `R/run_cron.R:1076`. Also `tests/testthat/helper-db.R` and
  `tests/testthat/test-app_pathogen.R`, and the schema and the CSV.

### Levels and scale

- The five levels, in lattice order, are listed in `R/app_archive.R:28`
  (`episodic_archive_levels`) and again in `R/cluster_manual.R:369`.
- `R/app_widgets.R:658`: `episodic_verdict_outbreak_levels <- c("pathogen_ward", "pathogen_institution")`.
  **This does not match the intended boundary**: L3 `pathogen_area` currently
  reads as "epidemic" in verdict labels and should read as "outbreak". M1
  replaces this constant with a derivation from the configured scale boundary.
- `R/reconcile_suppress.R:197`, `episodic_suppression_pairs()` returns three
  pairs: ward/institution, area/province, province/region. **The
  institution/area pair is absent.** Exactly one pair, area/province, crosses
  the new scale boundary. Do not add the missing pair as part of this reform;
  it changes detection behaviour and needs its own evaluation. Logged in
  section 8.

### Schema and database

- `episodic_schema_version <- 5L` at `R/schema_migrate.R:360`.
  `episodic_db_migrations()` at `:421`. Each migration is
  `function(con, dialect)`, idempotent, transactional, never drops or rewrites
  data, and must guard with an existence check because MySQL commits
  implicitly on DDL.
- `episodic_cluster` is at `inst/sql/schema.sql:248`. It carries
  `origin TEXT CHECK (origin IN ('detected', 'manual'))` and
  `opened_in_backfill`. It deliberately carries no verdict, state, `closed_at`
  or `snooze_until`: those are derived from `episodic_assessment_event`.
- `episodic_assessment_event` at `:323` has
  `verdict TEXT CHECK (verdict IS NULL OR verdict IN ('artefact', 'expected_variation', 'cluster_not_yet', 'possible_epidemic', 'confirmed_epidemic'))`.
  Adding declaration verdicts means extending that CHECK, in the schema and in
  a migration.
- `episodic_detection` at `:228` has a `params TEXT NOT NULL` column, already
  used for detector-specific JSON. The derived anchor and seasonality statistic
  go there; no schema change needed for that.
- `episodic_pathogen_config` at `:166` has
  `mem_applicable INTEGER NOT NULL DEFAULT 0 CHECK (mem_applicable IN (0, 1))`.

### App

- `R/app_ui.R:35`, `episodic_app_views()` lists nine views:
  `clusters`, `pathogen`, `archive`, `instance`, `streams`, `activity`,
  `performance`, `info`, `settings`. The order is the tab order.
- `R/app_ui.R:65`, `episodic_app_nav_group()`: `clusters`, `pathogen` and
  `archive` are their own nav groups; everything else maps to `instance`.
- Every screen is written into the page once and shown by `data-view` on
  `.episodic-shell`. Navigation does not rebuild screens.

### i18n

- Eight full language files of **705 keys** each: `ar de en es fr hi nl zh`.
- Two variant files carrying only differences: `en-US.json` (7 keys),
  `es-419.json` (3 keys). `episodic_language_variants` at `R/i18n.R:118`.
- `tests/testthat/test-i18n.R` enforces: identical key sets across all full
  files; a variant carries only keys its base has **and only keys whose value
  differs**; identical `{placeholder}` tokens per key across files. Any new key
  must be added to all eight full files, and to no variant unless it genuinely
  differs there.

---

## 6. Branch and PR mechanics

- **M0 is independent of the reform.** It goes on its own branch off `main`
  and is merged to `main` by its own PR. It fixes a defect on its own terms
  and de-risks everything after, because it is where the derived anchor and
  the derived eligibility test first meet real data.
- **M1 onwards live on a long-lived integration branch**, `epidemic-reform`,
  cut from `main` *after* M0 has merged. Each milestone is a PR from
  `epidemic-reform/mN-shortname` into `epidemic-reform`. When the whole reform
  is tested, one PR from `epidemic-reform` into `main`.
- **Merge `main` into `epidemic-reform` at the start of every milestone.** Not
  at the end of the reform, or the final merge is where all the pain arrives at
  once.
- Never push to `main` directly. Never force-push a shared branch.
- Commit messages carry the history this document does not. Reference #50.

---

## 7. Milestones

Each milestone states its goal, the files it touches, what to do, what not to
do, and how you know you are finished. Follow `CLAUDE.md` for style, header
banners, hanging-indent signatures, `@keywords internal`/`@noRd`, roxygen,
`episodic_trace()` severity, and parameterised SQL.

### Where milestones are executed

**Every milestone runs in an environment with a working R and the package's
dependencies installed.** Claude Code on the web has no R, and a milestone
written there is untested code whose arithmetic nobody has seen. M0 in
particular derives an anchor and a seasonality statistic from a weekly
climatology: whether those numbers behave is the whole point of the milestone,
and it is not knowable by reading.

So, at the start of every milestone, check for R:

```bash
command -v Rscript && Rscript -e 'cat(R.version.string)'
```

If it is absent, **stop and tell the user**. Do not write the milestone anyway
and leave the testing to them. Propose moving the session to a machine with R.
This is not a style preference; a detection change merged untested breaches the
no-untested-code rule in `CLAUDE.md`.

Where R is present, run before opening any PR:

```bash
Rscript -e 'devtools::test()'
Rscript -e 'devtools::document()'
Rscript -e 'styler::style_pkg()'
```

`devtools::test()` must be clean, not "clean apart from". Report the actual
output in the PR body, including any skips and why they skipped.

---

### M0. MEM: full-year seasons, derived anchor, derived eligibility

**Branch:** `claude/mem-agnostic-seasons` off `main`. PR into `main`.
**Schema version:** bump `episodic_schema_version` by one (expected: 6) and add
the matching migration.

#### Goal

MEM can fire in any ISO week of the year, for any pathogen whose own data show
a season, in either hemisphere, with no per-pathogen calendar setting.

#### Why this matters today

As shipped, `episodic_mem_season_week()` returns `NA` for ISO weeks 21 to 39,
so the detector is silent for those nineteen weeks. An influenza rise in
September falls in that gap. The detector is not late or underpowered in that
window, it is mute by construction.

#### Files

`R/detect_mem.R` (most of the work), `R/run_cron.R` (~1069),
`R/app_pathogen.R` (303, 543-580, 799-810), `R/app_pathogen_ui.R` (811),
`R/app_read.R` (369), `R/db_cron_write.R` (65, 71),
`inst/sql/schema.sql` (166), `R/schema_migrate.R`,
`inst/config/episodic_default_config.yaml`,
`inst/config/episodic_default_pathogen_config.csv`,
`inst/i18n/*.json` (8 full files), `tests/testthat/test-detect_mem.R`,
`tests/testthat/helper-db.R`, `tests/testthat/test-app_pathogen.R`,
`NEWS.md`.

#### Work

**1. Full-year seasons.**
`episodic_mem_season_week()` must never return `NA` for `season`. Given an
anchor week `a`, a date belongs to the season starting at the most recent
occurrence of week `a` on or before it. `episodic_mem_seasonal_matrix()`
becomes 52 rows, ordered from the anchor week round to the week before it.
Keep folding week 53 into 52 and keep documenting that as a simplification.
Both functions now take the anchor as an argument. Delete every literal `40`
and `20` from the season logic.

**2. Derive the anchor.** New internal function, suggested name
`episodic_mem_season_anchor(cases, config)`:

- Build a 52-week climatology: pool all available years and take the
  **median** count per ISO week (median, not sum, so one extreme season does
  not set the shape).
- Smooth it **circularly** with a centred moving average of
  `config$mem$climatology_smooth_weeks` weeks. Circular means week 1's window
  wraps to week 52.
- The trough is the set of weeks whose smoothed value is at or below the
  `config$mem$trough_percentile` quantile of the 52 smoothed values.
- Find the **contiguous circular runs** of trough weeks. Take the longest. On
  a tie, take the one whose minimum is lowest.
- The anchor is that run's **first** week.
- Return a list carrying the anchor week, the trough run, and the smoothed
  climatology. Return **`NULL`** when the case history spans fewer than
  `config$mem$min_climatology_years` whole years. Never fall back to week 40.

**3. Derive eligibility.** New internal function, suggested name
`episodic_mem_seasonality(cases, config)`:

- Detrend first, so a strong secular trend does not read as a season. Divide
  each week's count by a centred 52-week moving average of the series, and work
  on the resulting ratios.
- Statistic: the share of a year's cases falling in the peak
  `config$mem$seasonality_peak_weeks` consecutive weeks (circular window),
  averaged across whole years. A flat series gives roughly
  `peak_weeks / 52`.
- Seasonal when that share is at or above
  `config$mem$seasonality_min_peak_share`.
- Return the statistic **and** the verdict. Return `NULL` on insufficient
  history; do not return `FALSE`, which would mean "measured, and not
  seasonal".

**4. `mem_applicable` becomes `mem_mode`.**
Remove the `mem_applicable` column from `inst/sql/schema.sql`,
`inst/config/episodic_default_pathogen_config.csv` and every reader. Add
`mem_mode TEXT NOT NULL DEFAULT 'auto' CHECK (mem_mode IN ('auto', 'yes', 'no'))`.
Ship every pathogen as `auto`.

Rename rather than repurpose deliberately: `as.logical("auto")` is `NA`, so
keeping the old name would let a stale reader fail silently. Removing the
column forces every one of the six call sites to be updated.

Resolution order in the detector: `no` means never fit; `yes` means fit if the
data physically allow it (enough seasons), regardless of the seasonality
statistic; `auto` means fit only when `episodic_mem_seasonality()` says
seasonal.

**5. MEM runs at L4 as well as L5.**
Add `mem.levels` to the default config, shipped as
`[pathogen_province, pathogen_region]`. Replace the
`identical(stream$level, "pathogen_region")` test in `R/run_cron.R` with
membership of that list. Do not add a volume rule keyed on level: insufficient
volume already expresses itself as too few observed seasons or as a failed
seasonality test, and those refuse per stream, which is the correct
granularity.

**6. Status shape.**
`episodic_mem_status()` loses `in_season` and gains `in_trough` (is the
evaluated week inside this pathogen's derived trough run). It keeps
`epidemic_started`, the counts, both thresholds, the intensity bands and the
season label, and gains the anchor week and the seasonality statistic. It no
longer has an off-season early return. It still returns `NULL` when it cannot
compute, and `NULL` must stay distinguishable from a computed answer.

**7. Refusals are spoken, once.**
When MEM declines a stream, log one `episodic_trace()` line saying which
stream and which of the reasons: too little history for a climatology, too few
fully observed prior seasons, not seasonal by the statistic, or disabled by
`mem_mode = 'no'`. Severity `warn`, per the `CLAUDE.md` rule that a component
which structurally cannot produce output says so. One line per refusing
stream, not one per week.

**8. Record what was used.**
Put the anchor week, the seasonality statistic and the seasons used into the
detection's `params` JSON, so a past firing can be reproduced. No schema
change needed.

**9. Interface and i18n.**
`info.algorithms.mem` currently says MEM "runs at region level only" and names
influenza and RSV. Rewrite it: MEM fits where an instance's own data show a
season, at the configured levels, with the anchor derived from the data. The
Pathogen screen must show the derived anchor and the seasonality statistic,
and must state the refusal reason where MEM declines rather than showing an
empty panel. New keys go in all eight full language files, and in a variant
file only if the value genuinely differs there.

#### Do not

- Do not change any detection threshold to make a number look better. No
  tuning of `pre.post.intervals` handling, no new fudge factor.
- Do not implement seasonal closure. That is M2, and it needs the Epidemic
  object to attach to.
- Do not touch the validation study in `data-raw/validation/`.
- Do not add a hemisphere setting.

#### Done when

- No literal `40` or `20` remains in the season logic, and no code path returns
  an `NA` season.
- A synthetic summer-peaking series produces a winter anchor and fires MEM in
  summer; a synthetic winter-peaking series produces a summer anchor and fires
  MEM in winter. Both are tests.
- A synthetic flat series with a strong upward trend is refused as not
  seasonal, and the refusal is logged.
- `mem_mode = 'yes'` overrides a failing seasonality test; `'no'` overrides a
  passing one. Both are tests.
- An instance with under `min_climatology_years` of history gets `NULL` and a
  logged refusal, not a default anchor.
- Schema version bumped, migration added, `tests/testthat/test-schema_mariadb.R`
  still passes.
- `devtools::test()` clean, `devtools::document()` run, `NEWS.md` updated with
  one short line per change under the right heading.

---

### M1. Schema: scale, the seasonal satellite, the link table

**Branch:** `epidemic-reform/m1-schema` off `epidemic-reform` (create
`epidemic-reform` from `main` first, after M0 has merged).
**Schema version:** bump by one (expected: 7).

#### Goal

Every structure the reform needs exists in the database and nothing reads it
yet. This milestone is purely additive and changes no behaviour.

#### Files

`inst/sql/schema.sql`, `R/schema_migrate.R`,
`inst/config/episodic_default_config.yaml`, `R/config.R` if the new section
needs registering, `R/app_widgets.R` (658), `tests/testthat/test-schema*.R`,
`NEWS.md`.

#### Work

**1. `scale` on `episodic_cluster`.**
`scale TEXT NOT NULL DEFAULT 'outbreak' CHECK (scale IN ('outbreak', 'epidemic'))`.
The migration backfills it from each cluster's stream level using the
configured boundary. Comment it as what it is: the object's scale, decided by
the stream's level at the moment the cluster was opened.

**2. The scale boundary in config.**
New section:

```yaml
scale:
  epidemic_levels:
    - pathogen_province
    - pathogen_region
```

Document it in the shipped defaults as `CLAUDE.md` requires, since an unknown
key stops a run. Add a resolver, suggested name
`episodic_scale_for_level(level, config)`, returning `"outbreak"` or
`"epidemic"`.

Then **replace** `episodic_verdict_outbreak_levels` in `R/app_widgets.R:658`
with a derivation: the outbreak-worded levels are every level not in
`scale$epidemic_levels`. This corrects a live discrepancy, since L3
`pathogen_area` currently reads as "epidemic" and should read as "outbreak".

`scale` belongs in the config hash: it changes what a run computes.

**3. `episodic_epidemic_season`, the satellite.**
One row per epidemic cluster that has a season. Suggested columns:

- `cluster_id` primary key, references `episodic_cluster`
- `season_label`, `anchor_week`
- `onset_week_start`, the week the pre-epidemic threshold was crossed
- `pre_epidemic_threshold`, `post_epidemic_threshold`
- `intensity_medium`, `intensity_high`, `intensity_very_high` (nullable: the
  `mem` build may not report them)
- `seasons_used` (the fitted season labels, as text)
- `ended_week_start` (nullable while running)
- `ended_reason` nullable, `CHECK (ended_reason IS NULL OR ended_reason IN ('post_epidemic_threshold', 'trough'))`

A non-seasonal Epidemic has **no row here**. Absence is the representation of
"this epidemic is not seasonal", and nothing may read a missing row as a zero
or as "season not ended".

**4. `episodic_cluster_link`, the "during" relation.**
`(outbreak_cluster_id, epidemic_cluster_id)` composite primary key, both
referencing `episodic_cluster`, plus `created_at` and the `run_id` that
established it. An outbreak may link to more than one epidemic, so this is a
table and not a column.

**5. Declaration verdicts.**
Extend the `verdict` CHECK on `episodic_assessment_event` with the epidemic
declaration values. Suggested set: `season_started`, `season_not_yet`,
`season_ended`. Keep the five existing values. The CHECK is extended in
`schema.sql` **and** in the migration, and under MariaDB the adapter must
produce a working constraint.

Declarations reuse the existing append-only assessment event stream. Do not
build a parallel event table.

**6. Migration discipline.**
`function(con, dialect)`, idempotent, transactional, guarded by existence
checks because MySQL commits implicitly on DDL, never drops or rewrites data.
Follow the migration at `R/schema_migrate.R:435` as the model.

#### Do not

- Do not write to any new structure. No read path, no write path, no UI.
- Do not rename `episodic_cluster`. That is M6 and it is optional.

#### Done when

- A fresh database and a migrated version-6 database have identical structure.
  Add that as a test if one does not already exist.
- `test-schema_mariadb.R` passes: derived foreign keys still match, counts
  still agree.
- The whole suite passes with no behaviour change anywhere.

---

### M2. Write path: routing, epidemic closure, continuity, links

**Branch:** `epidemic-reform/m2-write`.

#### Goal

The cron pipeline opens Outbreaks and Epidemics correctly, closes Epidemics
correctly whether or not they are seasonal, keeps an epidemic's identity across
the season cut, and records the "during" links. Nothing in the interface has
changed yet.

#### Files

`R/run_cron.R`, `R/reconcile.R`, `R/reconcile_suppress.R`, `R/db_cron_write.R`,
`R/db_read.R`, `R/detect_mem.R` (the closure helper), `R/state_derive.R`,
tests, `NEWS.md`.

#### Work

**1. Route on open.** When `episodic_reconcile_stream()` opens a cluster, set
`scale` from the stream's level via `episodic_scale_for_level()`. Nothing else
about opening changes.

**2. Seasonal satellite on open.** When MEM is what raised an epidemic, write
the `episodic_epidemic_season` row from the `episodic_mem_status()` result.
When MEM did not raise it and MEM is not eligible for that stream, write **no**
row.

**3. Epidemic closure.** Build the criterion that does not currently exist
(see section 5: the docstring claiming it does is wrong; fix it).

- Seasonal epidemic: closed when the evaluated week's count falls at or below
  `post_epidemic_threshold`. Record `ended_reason = 'post_epidemic_threshold'`
  and the week.
- Seasonal epidemic whose counts fall but never cross cleanly: closed when the
  evaluated week is inside the derived trough run. Record
  `ended_reason = 'trough'`. This is a backstop, checked only after the primary
  test has failed.
- Non-seasonal epidemic: the existing `reconciliation.case_free_days` and
  `stale_open_days` rules, unchanged.
- Never treat a missing threshold as "not yet ended".

**4. Continuity across the cut.** An epidemic that has not been closed keeps
its identity when the season anchor rolls over. Do not open a second epidemic
for a rise that is the continuation of one already open on the same stream.
Reuse the existing reconciliation matching; the season label is metadata on
the satellite row, not part of cluster identity.

**5. The "during" links.** After reconciliation and before suppression, for
each open epidemic, link every open outbreak that satisfies all three of:
same pathogen; date ranges overlap; the outbreak's geography nests inside the
epidemic's. Links are recomputed each run, additively; do not delete links from
earlier runs.

Geographic nesting uses the same resolved geography the run already holds. Do
not resolve geography a second time: `CLAUDE.md` explains what happens when two
sites resolve it independently.

**6. Suppression stays continuous.** Verify that
`episodic_suppression_pairs()` still works across the scale boundary, in
particular the `pathogen_area` to `pathogen_province` pair, which is the only
one that crosses it. Add a test that a cross-scale suppression still fires.

#### Do not

- Do not add the missing `pathogen_institution`/`pathogen_area` suppression
  pair. Section 8.
- Do not modulate the priority score by epidemic status. Section 8.
- Do not touch the interface.

#### Done when

- A replay through `episodic_run_cron()` opens correctly scaled clusters.
- A seasonal epidemic closes on the threshold; a noisy one closes on the
  trough and says which; a non-seasonal one closes on case-free days. Three
  tests.
- An epidemic straddling the season cut keeps one id. Test.
- Links appear, an outbreak can be during two epidemics, and links survive the
  next run. Tests.
- Cross-scale suppression still fires. Test.
- Run transactionality is intact: the full run commits or nothing does.

---

### M3. Read path and the Epidemics screen (MVP)

**Branch:** `epidemic-reform/m3-screen`.

#### Goal

An epidemiologist can see epidemics, read the evidence, and record a
declaration. This is an MVP: it must be correct and complete in what it does,
and it need not do everything.

#### Files

`R/app_ui.R`, `R/app_server.R`, `R/app_read.R`, `R/app_dossier.R`,
`R/app_cluster_table.R`, `R/app_archive.R`, `R/app_performance.R`,
`R/app_charts.R`, `R/db_read.R`, a new `R/app_epidemic.R` and
`R/app_epidemic_ui.R`, `inst/app/www/episodic.css`, `inst/i18n/*.json`,
`_pkgdown.yml` if anything new is exported, tests, `NEWS.md`.

#### Work

**1. The screen.** Add `epidemics` to `episodic_app_views()`, placed after
`clusters`, and give it its own nav group in `episodic_app_nav_group()`. Follow
the existing pattern exactly: the screen is written into the page once and
shown by `data-view`.

**2. Filter the existing screens.** The Clusters screen shows
`scale = 'outbreak'` only. The Archive shows both, filterable. The Performance
screen's PPV and time-to-detection count outbreaks only; state that on the
screen, because a reader must know what the denominator is.

**3. The epidemic dossier.** Reuse the dossier code path; swap the evidence
panel. It carries:

- the seasonal weekly curve against the fitted thresholds, drawn from
  `episodic_mem_thresholds_for_season()`, which already excludes the season
  from its own fit;
- tests performed and positivity, from
  `episodic_app_pathogen_denominator()` (`R/app_pathogen.R:883`), because a
  rise in counts with flat positivity is a rise in testing;
- the number of contributing institutions and areas, and the concentration of
  the week's cases, per D12, so a reader can see whether the rise is diffuse
  or is one hospital;
- the list of outbreaks recorded as during this epidemic, each shown as its
  `O-` id. **An empty list reads "none detected"**, never "0".

**4. The declaration.** An epidemiologist records `season_started`,
`season_not_yet` or `season_ended` through the existing assessment path, with a
rationale, appended to `episodic_assessment_event`. Viewers may read and not
write, as everywhere else. The declaration is a person's act: nothing in the
system may write one automatically.

**5. Access gating.** Every new output and every data-bearing observer goes
behind `episodic_app_access_granted()`. This is not optional and is not
client-side.

**6. Numbers, direction and theme.** Every number through
`episodic_format_number()`. Ids rendered as-is. Logical CSS properties only.
New keys in all eight full language files.

#### Do not

- Do not build a patient-level line list for an epidemic. At L5 that is the
  whole catchment. Geographic and institutional breakdown instead.
- Do not build an epidemic report renderer. Section 8.
- Do not add automatic declaration on a second consecutive week. Section 8.

#### Done when

- The screen lists, opens, and records a declaration, against a test database.
- An epidemic with no seasonal satellite row shows no seasonality panel, not an
  empty one. Test.
- An epidemic with no linked outbreaks reads "none detected". Test.
- A viewer cannot declare. Test.
- Nothing renders for a signed-out visitor when `access.require_login` is true.
  Test.

---

### M4. Vocabulary: ids, wording, i18n

**Branch:** `epidemic-reform/m4-wording`.

#### Goal

The interface says Outbreak and Epidemic, in eight languages, and ids read
`O-123` and `E-45`.

#### Work

1. **Ids.** Render `O-` for `scale = 'outbreak'` and `E-` for
   `scale = 'epidemic'`. Start from `episodic_db_cluster_label()`
   (`R/db_read.R:1092`) and the per-language cluster-reference key that
   `tests/testthat/test-i18n.R:91` exercises. No `#`. Never grouped as a
   number.
2. **Wording.** Sweep user-facing "cluster" to "outbreak" or "epidemic" as the
   scale dictates, across all 705 keys in all eight full files. Add to a
   variant file only where the value genuinely differs; the test enforces this.
3. **Verdict labels.** `episodic_verdict_label()` keeps keying off the stored
   verdict value and switches wording on scale rather than on a hardcoded level
   list. The stored values never change.
4. **Notifications.** `R/notify.R` message building must name the right object.
   A seasonal onset notifies the epidemiology team that a declaration is due.
   It does **not** broadcast outward: the outward communication is a
   consequence of the human declaration, and is out of scope here.
5. **Reports.** `R/report_render.R` and `inst/report/` name the right object.

#### Done when

`test-i18n.R` passes: equal key sets, equal placeholder tokens, variants
carrying only genuine differences. No user-facing string says "cluster" where
it means one of the two objects. A screenshot in Arabic still mirrors
correctly.

---

### M5. Documentation

**Branch:** `epidemic-reform/m5-docs`.

Update `CLAUDE.md` (the pipeline diagram, the detectors table, the streams and
lattice section, the database table list, the config sections, the file
layout), the vignettes, `_pkgdown.yml` (every exported topic must be in a
section or the site build fails), and `NEWS.md`.

`CLAUDE.md` must end up describing the system as it then is, with no trace of
what it was. State the MEM season model, the scale split, the declaration as a
human act, and the absence-is-not-zero rules that this reform added instances
of.

---

### M6. Table rename (optional, recommended deferred)

Renaming `episodic_cluster` to `episodic_outbreak` and its dependants would
remove the last place where the schema says something the interface never says.
It is pure churn with no behavioural content, and it multiplies the merge
surface of an already large branch.

Recommendation: do not do it inside this reform. Open a separate issue once the
reform has merged to `main`.

---

## 8. Deferred, with reasons

Each of these is a real idea that is deliberately not in scope. Open an issue
for any the user wants pursued; do not smuggle them into a milestone.

| Item | Why deferred |
|---|---|
| The missing `pathogen_institution`/`pathogen_area` suppression pair | Adding a suppression pair changes detection behaviour and needs its own evaluation against the validation study. |
| Priority score modulated by epidemic status | An influenza ward outbreak in August is extraordinary where one in February is expected, so out-of-epidemic outbreaks arguably deserve a lift. Real, and a scoring change that needs its own tests and its own evaluation. |
| Automatic declaration after two consecutive weeks above threshold | Decide after watching a real season render on the M3 screen. The manual act is the safe default and loses no timeliness. |
| An outward seasonal statement or epidemic report | The analogue of the outbreak report, for the communication that follows a declaration. Wait until the declaration exists. |
| Seasonal onset as labelled ground truth in `episodic_synthetic_cases()` | Overlaps #46. Until it exists, MEM's positive predictive value is unmeasurable by construction, because the truth table contains no seasonal onset for a crossing to match. Do not read the 0/13 figure in #50 as evidence about MEM until this is done. |
| Validation study updates for the two-scale model | Per the user: a small separate issue suffices. |
| SARS-CoV-2 enabled for MEM | Becomes an operator decision once M0 ships, since eligibility is then measured from the instance's own data rather than curated. Ships as `auto`, so an instance whose data show the season gets it automatically. |

---

## 9. Invariants to check at the end of every milestone

Run through this list before opening any PR. It is short and it is the part
that gets skipped.

- [ ] Detection runs are transactional: the full run commits or nothing does.
- [ ] Notifications fire after commit, never inside the transaction, and their
      errors never propagate.
- [ ] `config_hash` is deterministic, and every new config section that affects
      what a run computes is inside it.
- [ ] The app inserts only. It never updates or deletes, and never writes a
      cron-owned table.
- [ ] No unmeasurable quantity is rendered as zero, anywhere, including in an
      empty list or a missing satellite row.
- [ ] Every new number goes through `episodic_format_number()`; every id is
      rendered as-is.
- [ ] Every new output and observer is behind `episodic_app_access_granted()`.
- [ ] `inst/app/www/episodic.css` uses logical properties only.
- [ ] New i18n keys exist in all eight full language files, and in a variant
      only where the value differs.
- [ ] Every new exported function is in a `_pkgdown.yml` section.
- [ ] Comments describe the code as it is. No defect history, no "used to".
- [ ] `NEWS.md` has one short line per change under `## New`/`## Changed`/`## Fixed`.
- [ ] `devtools::test()` clean; `devtools::document()` run.
- [ ] **This file updated**: status board, session log, and any milestone whose
      instructions your findings changed.
