# Brief: detection validation harness, and MariaDB in CI

For a session running on a machine that has **R** and a **MariaDB server**
available. Both tasks below need one or the other to be done honestly;
they were deliberately left out of PR #45 rather than written blind.

Branch: `claude/app-review-shipping-mz3b7t` (PR #45, open against `main`).
Read `CLAUDE.md` first: it is the project's own standard and this brief
does not restate it.

---

## 0. Where things stand

PR #45 was a shipping-readiness review of the whole package. In short, it
fixed:

- Both rule-based detectors rescanned the entire case history every run
  and re-emitted every hit ever found, which reset `runs_since_detected`
  so `reconciliation.close_after_runs` could never fire. Now bounded by
  `same_place.lookback_days` / `rare_trigger.lookback_days`.
- Farrington aggregated to the week *containing* `run_date`, testing a
  partial week against full-week baselines. Now the last **complete**
  week (`episodic_last_complete_week_start()`).
- `run_date` was ignored by reconciliation, which judged staleness
  against the wall clock.
- A closed cluster was re-closed every run, and a closed cluster that
  re-detected absorbed the new cases silently instead of returning to
  the board.
- The geography (L3 area rule, L5 catchment code, L4 province lookup,
  the map) was hardcoded to the northern Netherlands and silently fell
  back to it. Now `config$geography` plus operator-supplied files, with
  no defaults.
- `access.require_login` now ships `true`; schema versioning and
  `episodic_db_migrate()` exist; instance config is validated; mail is
  RFC 2047/2045 clean; Arabic renders RTL; refused sign-ins are audited.

The full reasoning is in the PR description. **Do not redo or reverse any
of it.** If something there looks wrong, say so rather than quietly
changing it.

It also added a guard on `episodic_demo()`: it now refuses a `db_path`
that already exists, and refuses a MariaDB DSN outright, so a demo can
never file synthetic cases into a real instance. `overwrite = TRUE`
rebuilds a demo in place. The harness must not use `episodic_demo()`
anyway; see "where the heavy runs must not go" below.

State at handoff: `devtools::test()` green (7118 pass, 0 fail, 2 skips:
the Quarto CLI absent, and one `sf`-not-installed branch), `document()`
and `style_pkg()` both no-ops.

Three issues are open and are **not** part of this brief. Do not pick
them up here:

- **#46**, the review pass still owed on the presentation and analytics
  half of the package. `R/cases_synthetic.R` is on that list and is also
  the source of this harness's ground truth, so if the harness work
  turns up something wrong in it, note it on #46 rather than widening
  this PR.
- **#47**, report and config-export output directories derived from
  `dirname(db_path)`, which is meaningless for a MariaDB DSN. Task B may
  well trip over this against a real server. Confirm it there if it
  does, and fix it on #47.
- **#48**, the `version_no` race in `episodic_report_render()`.

---

## Task A: a detection validation harness

### Why

`episodic_app_performance()` (the Performance screen) computes PPV and
timeliness **from the epidemiologists' own verdicts** on the instance's
own clusters. That is a good operational metric and a circular one for a
paper: it reports what the board thought of what the system showed them.
Sensitivity is structurally unmeasurable that way, because an outbreak
the detectors miss never becomes a cluster for anyone to judge.

Nothing currently measures detection performance against known truth.
For a paper claiming an outbreak detection system, that is the gap that
matters most.

### The ground truth already exists

`episodic_synthetic_cases()` injects six outbreaks
(`episodic_synthetic_outbreaks()` in `R/cases_synthetic.R`), each
deliberately shaped to exercise a different channel:

| Generator | Pathogen | Shape | Channel it is meant to exercise |
|---|---|---|---|
| `..._outbreak_rare_case()` | *Neisseria meningitidis* | 1 case | `rare_trigger` |
| `..._outbreak_ward_cluster()` | *C. difficile* | 3 cases, 1 ward, 11 days | `same_place` (ward) |
| `..._outbreak_ltc()` | Norovirus | 8 cases, 1 LTC institution | `same_place` (institution) |
| `..._outbreak_point_source()` | Norovirus | 14 cases, 1 ward, tight | `same_place` |
| `..._outbreak_propagated()` | *B. pertussis* | 4 generations, 20-day SI | Farrington / Rt |
| `..._outbreak_regional_wave()` | Influenza A | 6 rising weeks, deliberately spread so no place reaches `same_place`'s threshold | Farrington at L5 only |

Every injected case carries a `patient_key` matching `^PT-OUTBREAK-`,
with the outbreak's identity in the next segment (`RARE`, `WARD`, `LTC`,
`PS`, `PROP`, `WAVE`). Baseline cases are
`PT-<make.names(pathogen)>-<6 digits>`, so there is no collision.

So ground truth is recoverable **today**, by string match. Do not build
the harness on a string match. **Make it explicit**: have the generator
return, or attach, a ground-truth table with one row per injected
outbreak (`outbreak_id`, `pathogen`, `institution_key`, `ward`,
`first_day`, `last_day`, `n_cases`, `expected_channel`), and the
case-level membership that goes with it. A regex over `patient_key` is
exactly the kind of implicit coupling `CLAUDE.md` forbids, and it breaks
silently the first time someone renames a prefix.

### Design the harness this way

**Run it prospectively, not retrospectively.** Detection delay is
meaningless from a single run over the whole history. Loop `run_date`
weekly across an evaluation window, and on each iteration hand
`episodic_run_cron()` **only the cases with `sample_date <= run_date`**,
that is what a laboratory extract as of that date actually contains.
`episodic_farrington_weeks_owed()` will then correctly owe about one week
per run.

**Match on case sets, not on intervals.** A cluster's cases are in
`episodic_cluster_case`; an outbreak's are known from the ground-truth
table. Two directions, and they answer different questions:

- an **outbreak is detected** when some cluster contains at least
  `min_recall` of its cases (propose 0.5, make it an argument, report
  sensitivity to the choice);
- a **cluster is a true positive** when at least `min_precision` of *its
  own* cases belong to one seeded outbreak. Otherwise it is a false
  alarm.

Interval overlap alone will call an endemic winter cluster a hit because
it happens to span the same fortnight.

### What to measure

PPV alone is not a characterisation of a detection system, and neither is
sensitivity. The list below is what a methods reviewer will expect to
see; not all of it is equally hard, and the first three groups are the
minimum for a defensible claim.

**1. Detection performance.**

- **Sensitivity** (detected outbreaks / injected outbreaks), overall and
  per outbreak shape. The per-shape breakdown is the interesting result,
  because it says which channel found what.
- **PPV** (true-positive clusters / all clusters raised), overall and per
  detector.
- **False alarms per stream-week**, a rate rather than a count: a count
  is uninterpretable without its denominator, and the denominator here
  (streams x runs) is large and varies with the configuration. Report the
  denominator explicitly alongside it.
- **Case-level recall and precision as distributions**, not only as the
  binary hit that `min_recall` / `min_precision` produce. "The cluster
  contained 4 of the outbreak's 14 cases" and "it contained 13 of 14" are
  very different results that a single threshold flattens.
- **Fragmentation**: how many distinct clusters one seeded outbreak was
  split across. One outbreak surfacing as five clusters is a real
  operational cost that sensitivity hides, and it is exactly what lattice
  suppression is meant to reduce.

**2. Timeliness.**

- **Detection delay** in days, from the outbreak's first case to the
  `run_date` of the run that first raised the matching cluster, and also
  from its *third* case (onset is not observable in practice; three cases
  is roughly when a human would start wondering).
- **Treat undetected outbreaks as right-censored, not as missing.** A
  median delay computed only over the outbreaks that were detected is
  biased downward, and the bias is worst exactly where the method is
  weakest. Report time-to-detection as a Kaplan-Meier curve censored at
  the outbreak's last case, or report the median delay together with the
  proportion never detected and never quote the delay alone.
- **Proportion detected before the outbreak's peak**, and the
  **proportion of the outbreak's cases still to come at the moment of
  detection**. The second is the closest honest proxy for what earlier
  detection could have prevented, and it is more defensible than any
  "cases averted" claim, which would require an intervention model this
  work does not have.

**3. Alarm burden and specificity.**

- **A negative control run.** Generate histories with no injected
  outbreaks at all and measure the alarm rate on them. This is the
  cleanest specificity measurement available, because on seeded data
  every "false" alarm might be a real cluster the generator produced by
  chance. It needs a generator argument for suppressing the injected
  outbreaks; add one.
- **Alarm burden**: clusters surfaced per week that an epidemiologist
  would have to assess, before and after lattice suppression. This is the
  number a laboratory considering adoption actually cares about, and it
  is a headline result in its own right.

**4. Attribution and marginal value.**

- **Which detector fired first** for each detected outbreak, via
  `episodic_detection.detector` joined through `episodic_cluster`.
- **Drop-one analysis**: re-run with each detector disabled in turn and
  report the sensitivity lost and the false alarms saved. This answers
  "what does each channel actually add", which is the question a reviewer
  will ask about a four-detector design, and it cannot be answered by the
  first-to-fire attribution alone (two detectors may both find an
  outbreak, so the loser looks worthless until you remove the winner).

**5. Triage quality.**

- **Does the priority score rank true outbreaks above false alarms?**
  Report the score's discrimination between true-positive and
  false-alarm clusters (an AUC, or the median score in each group), and a
  calibration curve if there are enough clusters to support one. The
  score exists to decide what an epidemiologist looks at first, so its
  ranking is a claim the paper makes whether or not it measures it.

**6. Context and robustness.**

- **A naive comparator.** Sensitivity and false-alarm figures mean little
  in isolation. Run at least one trivial baseline over the same data, for
  instance "any 3 cases of one pathogen at one location within 7 days" or
  a plain 2-SD Shewhart limit on weekly counts, and report it in the same
  table. Reviewers ask for this, and a system that beats a trivial rule
  by a clear margin is a much stronger claim than one reported alone.
- **Sensitivity to the matching thresholds.** Sweep `min_recall` and
  `min_precision` and show the headline numbers do not depend on the
  particular values chosen.
- **A detection-threshold sweep**, if the budget allows: vary
  `farrington.alpha` and `same_place`'s N and K, and plot sensitivity
  against false alarms per stream-week. That curve, an AMOC-style
  delay-versus-false-alarm plot, is the standard way this literature
  presents an aberration detector, and it says far more than a single
  operating point. Note that this is a sweep for the paper, presented as
  a curve, and not a licence to move the shipped defaults; see "the
  result may be unflattering" below.

**How to present it.** Seeds are the replicates, so summarise across
seeds rather than pooling all outbreaks from all seeds into one
denominator: report medians with interquartile ranges, and binomial
confidence intervals (Wilson) for the proportions. State the number of
seeds, streams, runs and stream-weeks behind every figure. Record the
resolved `config_hash` and the package version with every result set, so
a number in the paper can be traced to the configuration that produced
it.

**Repeat over seeds.** One realisation is an anecdote. Loop
`episodic_synthetic_cases(seed = ...)` over, say, 20 seeds and report
medians with ranges. This is the difference between a figure and a
claim.

**Two known confounds to handle explicitly, not paper over:**

1. All six seeded outbreaks are anchored to `end_date`, so they sit in
   the last ~5 months of whatever window is generated. A prospective
   evaluation needs outbreaks dispersed across the window. Either add a
   generator argument for that or document the restriction plainly in
   the results.
2. Farrington needs `(b + 1) × 52` weeks and MEM needs two seasons, so
   the first three years of any generated history have only
   `same_place` and `rare_trigger` running. The evaluation window must
   start after the baseline requirement is met, or the sensitivity
   figure is measuring the warm-up rather than the method.

**Cost.** Every run refits Farrington per eligible stream. Bound the
evaluation window, and check the wall-clock cost before scaling the seed
count.

### Where it should live, and where the heavy runs must not

**Every large generation run belongs in `data-raw/`.** That directory is
listed in `.Rbuildignore`, so nothing in it ships in the built package,
nothing in it is run by `R CMD check`, and nothing in it is reachable
from an installed EpiSODIC. A multi-seed study takes minutes to hours,
and that cost must not land in the test suite, in CI, in an example, in a
vignette, or anywhere the Shiny app can reach.

Concretely:

- the multi-seed study, the threshold sweeps, the negative-control runs
  and the comparator runs are `data-raw/` scripts, and they write their
  results there (`data-raw/validation/`, say) as CSV, together with the
  package version and resolved `config_hash` that produced them;
- the exported `episodic_validate_detection()` defaults to one seed and a
  short evaluation window, so its example and any test of it finish in
  seconds. Guard anything slower with `skip_on_cran()` and `\donttest{}`;
- the harness must never run against a live instance. Each replicate
  creates its own throwaway SQLite database with `tempfile()`, writes its
  own instance configuration into a temporary directory (the geography
  keys are needed, since neither the map nor the province level has a
  built-in default), and removes both afterwards;
- do not build replicates with `episodic_demo()`. It creates a user
  account, sets four environment variables, writes configuration files
  beside the database, and now refuses a `db_path` that already exists.
  Call `episodic_run_cron()` directly against a database the harness
  creates itself;
- if the raw per-run output is large, commit the summary and leave the
  raw output out of git. The script must regenerate it.

A defensible split of the code itself:

- an exported `episodic_validate_detection()` returning a structured
  result (per-outbreak rows plus a summary), documented and indexed in
  `_pkgdown.yml`;
- tests for the matching and metric functions specifically, since they
  are pure functions over data frames and should be tested without
  running a single detection;
- a `data-raw/` script that runs the full study and writes the numbers
  the paper will quote;
- optionally a short vignette running a deliberately small version, so a
  reader can see how the claim was produced. A vignette is built on every
  check and every pkgdown build, so it must stay small enough that a
  build never depends on a long run.

### The result may be unflattering

If sensitivity for a given shape is poor, **report it and leave it**. Do
not tune thresholds until the number looks good and then present the
number, which is fitting the evaluation to the answer. If the harness
shows the shipped defaults are badly calibrated, that is a finding, and
the right response is a separate discussion with the maintainer, not a
quiet change to `inst/config/episodic_default_config.yaml` in the same
PR.

---

## Task B: MariaDB in CI, and a live-connection test

### Why

`episodic_db_schema_statements("mariadb")` rewrites the SQLite schema
into MySQL-safe DDL using a **hand-maintained per-table find-and-replace
table** (`R/schema_migrate.R`). It fails loudly when a pattern goes
stale, which is good, but nothing anywhere proves the SQL it emits is
accepted by a real server. `tests/testthat/test-schema_mariadb.R` tests
string rewriting and DSN parsing only. The suite never opens a MariaDB
connection, and neither does CI.

PR #45 added a table (`episodic_app_login_failure`) that needed a new
rewrite rule for its indexed `TEXT` column, verified by reading alone.
That is not a sustainable way to maintain a second dialect.

### What to do

1. Add a MariaDB service container to `.github/workflows/R-CMD-check.yaml`
   (or a separate workflow, since a service container on all five matrix
   configurations is wasteful; one Linux job is enough). Expose the DSN
   as an environment variable.
2. Add tests that `skip()` unless that variable is set, so the suite
   stays green on a laptop with no MariaDB. Cover at least:
   - `episodic_db_create()` against a real MariaDB, then
     `episodic_db_connect()`, then `episodic_db_migrate()` on a database
     with the version table dropped;
   - a full `episodic_run_cron()` over a small synthetic extract, and the
     dashboard read path (`episodic_app_open_clusters()`,
     `episodic_cluster_object()`, `episodic_app_activity_log()`) against
     the result;
   - the re-entrancy invariant `tests/testthat/test-db_write_reentrancy.R`
     documents. RMariaDB allows one active result per connection, and the
     comments in `R/reconcile.R` around `priority_score_fn` record a
     native crash that came from violating it. That test currently
     enforces the rule by reading source; against a live server it can be
     enforced by running.
3. If a real server rejects any of the generated DDL, fix the rewrite
   rules, and consider whether the per-table find-and-replace approach
   should be replaced by something that cannot silently drift.

---

## Conventions that are not negotiable

From `CLAUDE.md`, restated because they are the ones most easily missed:

- Every function touching detection logic, data transformation or
  reporting needs tests **and** a PR. Work on a branch, open a PR against
  `main`.
- Multi-line function signatures use **hanging indent** (first argument
  on the `function(` line, `) {` sharing the last argument's line).
  `styler::style_pkg()` preserves whichever shape it finds, so write it
  correctly the first time.
- Internal functions get `@keywords internal` and `@noRd`, and reference
  other functions as `` `f()` ``, never `[f()]`.
- Every new exported function must be added to `_pkgdown.yml` or the site
  build fails.
- `NEWS.md`: one short line per change under `## New` / `## Changed` /
  `## Fixed`. No sub-bullets, no multi-sentence entries.
- No silent failures, no hidden country- or laboratory-specific
  assumptions, no placeholder logic.
- Run `devtools::document()`, `devtools::test()` and
  `styler::style_pkg()` before pushing, and say in the PR what the suite
  reported.

## Out of scope

Do not, in this work:

- change detection defaults in `inst/config/episodic_default_config.yaml`
  (see "the result may be unflattering" above);
- revisit anything PR #45 settled;
- take on issues #46, #47 or #48. Each has its own branch and PR when
  its turn comes; none of them belongs in this work.

## Acceptance

**Task A** is done when a maintainer can run one `data-raw/` script and
get the measurements in "what to measure" above, per outbreak shape and
per detector, across several seeds, with undetected outbreaks handled as
censored rather than dropped, a negative-control alarm rate, at least one
naive comparator, and the matching rules and their sensitivity to the
chosen thresholds documented well enough to defend in review. The
exported entry point must still run in seconds at its defaults, and
nothing slow may reach the suite, CI or a vignette.

**Task B** is done when CI runs the suite against a real MariaDB and a
schema change that breaks the dialect adapter fails the build rather
than reaching an operator.
