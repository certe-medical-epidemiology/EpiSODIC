# Tuning detection for your catchment

This vignette is for someone with a running EpiSODIC instance whose
detection behaviour does not quite match their catchment: too many false
alarms, missing outbreaks, clusters closing too early, the wrong things
at the top of the board. It maps each of those complaints to the
parameters that control it, the trade-off each adjustment creates, and
how to verify the change helped. It does not re-explain how the
detectors work
([`vignette("detection-reconciliation")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/detection-reconciliation.md))
or how the configuration overlay system works
([`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)).

## Before you change anything

Every detection run records the exact configuration it used: a SHA-1
hash of the resolved settings (`config_hash`) and a full JSON snapshot
(`config_snapshot`), both on the run’s own row. A change you make today
affects only future runs, never the meaning of a past one. You can tune
with confidence, since nothing becomes retroactively wrong.

Point the `EPISODIC_CONFIG` environment variable at a YAML file with
only the keys you want to change; everything else keeps its shipped
default. See
[`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)
for the mechanics.

``` yaml
# my_instance.yaml - only what differs from the defaults.
farrington:
  alpha: 0.01
```

Three guidelines before you start:

1.  Change one thing at a time. Two changes at once, and the effect
    could be either or both.
2.  State what you expect the change to do before you make it.
3.  Decide how you will measure whether it worked: the Performance
    screen for a live instance, or
    [`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)
    for a controlled experiment. See “Verifying that a change helped”
    below.

## Too many false alarms

### The Farrington bound is too narrow

If Farrington fires on modest rises that are within normal variation for
your catchment, widen the statistical bound or raise the effect-size
floor that gates new clusters.

`farrington.alpha` is the significance level for the upper bound.
Lowering it from 0.05 to 0.01 widens the bound, so a run needs a larger
exceedance before it reports one. This is a sensitivity-specificity
trade-off: fewer false alarms, but genuine small outbreaks take longer
to appear or are missed entirely.

`effect_size_floor.min_excess_over_upperbound` (default: 3) requires at
least this many cases above the bound before a *new* cluster is opened.
Raising it filters out borderline exceedances. A match against an
already-open cluster is always merged, regardless of this floor.

`effect_size_floor.min_ratio_observed_expected` (default: 1.5) requires
the observed/expected ratio to reach this level. Together with the
excess floor, it means a signal must be both statistically and
practically notable. Either can be set to `~` (null) to disable it.

``` yaml
farrington:
  alpha: 0.01
effect_size_floor:
  min_excess_over_upperbound: 5
  min_ratio_observed_expected: 2.0
```

To measure the effect before deploying it:

``` r

result_before <- episodic_validate_detection(history_years = 4)
result_after  <- episodic_validate_detection(
  history_years = 4,
  config = list(
    farrington = list(alpha = 0.01),
    effect_size_floor = list(min_excess_over_upperbound = 5)
  )
)
result_before$summary[result_before$summary$metric == "sensitivity", ]
result_after$summary[result_after$summary$metric == "sensitivity", ]
```

### same_place fires on endemic patterns

The rule-based detector opens clusters whenever N cases of the same
pathogen appear at the same institution within K days. For a common
organism at a large hospital, three cases in 14 days may be entirely
unremarkable.

The global defaults (`same_place.default_n_cases` and
`same_place.default_k_days`) affect every pathogen that does not have
its own override. Raising `default_n_cases` globally also raises it for
rare organisms where it should stay low, so per-pathogen overrides under
`same_place.overrides` are usually the better lever:

``` yaml
same_place:
  overrides:
    Escherichia coli: {n_cases: 5, k_days: 14}
    Staphylococcus aureus: {n_cases: 4, k_days: 10}
```

Lowering `default_k_days` requires cases to be more tightly clustered in
time, which reduces noise from background endemic rates.

### Suppression lets redundant signals through

If the same real outbreak appears as separate clusters at multiple
lattice levels (a ward cluster and its parent institution cluster),
suppression is not aggressive enough.

`suppression.child_dominance_threshold` (default: 0.70) is the share of
a parent’s cases that a single child must account for to suppress the
parent. Lowering it makes it easier for a child to absorb its parent.
`suppression.parent_diffuse_threshold` (default: 0.50) and
`suppression.parent_min_flagged_children` (default: 2) control the
reverse: a parent absorbs its children when no single child dominates
and enough children overlap. A cluster someone has already assessed is
never suppressed, regardless of these thresholds.

``` yaml
suppression:
  child_dominance_threshold: 0.60
  parent_min_flagged_children: 3
```

Too aggressive suppression hides a genuinely independent signal behind a
coincidentally overlapping one. If you find suppressed clusters that
should not have been, raise the thresholds.

## Missing outbreaks

### Farrington never fits

If Farrington reports nothing and the run log says a stream does not
have enough baseline history, the model cannot fit because the stream’s
history is shorter than the baseline requirement.

`farrington.b` (default: 2) is the number of reference years. The model
needs `(b + 1) * 52` weeks before it fits at all, so at `b: 2` a stream
needs three years of history. Lowering it to 1 reduces the requirement
to two years, at the cost of a less stable model. Raise it as history
accrues: 5 is the conventional choice for a mature instance.

The three eligibility gates (`eligibility.min_baseline_weeks`,
`eligibility.min_median_weekly_count`,
`eligibility.min_nonzero_week_share`) determine whether a stream reaches
the statistical detectors at all. Streams that fail still run through
`same_place` and `rare_trigger`. Loosening these lets the model fit on
sparser streams, where it may not be meaningful.

``` yaml
farrington:
  b: 1
eligibility:
  min_baseline_weeks: 26
```

### same_place thresholds are too high

A ward-level cluster of 2 cases of a serious organism may go unreported
because the global `default_n_cases` is 3. A per-pathogen override is
the targeted fix:

``` yaml
same_place:
  overrides:
    Legionella: {n_cases: 2, k_days: 14}
```

### The eligibility gate excludes a stream

A stream you expect to be watched may fail the eligibility check.
`eligibility.min_nonzero_week_share` (default: 0.2) requires at least
20% of baseline weeks to have at least one case.
`eligibility.min_median_weekly_count` (default: 1) requires a median
weekly count of at least 1. For genuinely sparse organisms, lowering
these lets the stream through, though the statistical model is not
meaningful on a stream with very few cases.

``` yaml
eligibility:
  min_nonzero_week_share: 0.10
  min_median_weekly_count: 0
```

## Clusters close too early or too late

### Clusters close before the outbreak is really over

If a cluster auto-closes and a new one opens for the same ongoing
outbreak, the case-free interval or the run-count limit is too short.

`reconciliation.case_free_days_default` (default: 14) is the fallback
interval: if no case arrives within this many days, the cluster’s
episode ends. Per-pathogen `case_free_days` in the pathogen
configuration CSV overrides this for pathogens with longer incubation
periods. `reconciliation.close_after_runs` (default: 14) closes an
unconfirmed cluster after this many consecutive runs with no new case.

``` yaml
reconciliation:
  case_free_days_default: 21
  close_after_runs: 21
```

A cluster that has been confirmed by an epidemiologist is never
auto-closed by `close_after_runs`, only by case-free days.

### Clusters stay open long after the last case

If the active queue has stale clusters nobody has looked at,
`reconciliation.stale_open_days` (default: 60) force-closes any
still-unassessed cluster this many days after its last case.
`reconciliation.autoclose_unassessed` must be `true` (the default) for
either `close_after_runs` or `stale_open_days` to act on an unassessed
cluster.

``` yaml
reconciliation:
  stale_open_days: 30
  close_after_runs: 7
```

Aggressive auto-closure can close a genuine slow-burn outbreak before
anyone has assessed it.

### Cooldown absorption swallows a real recurrence

After a cluster closes, a cooldown window (per-pathogen `cooldown_days`
in the pathogen configuration CSV) absorbs nearby cases into the
recently-closed cluster rather than opening a new one.
`reconciliation.cooldown_reopen_ratio` (default: 1.5) is an escape
hatch: if the absorbed cases are at least 1.5 times the cluster’s own
size, the absorption is flagged rather than silent. Set to `~` to
disable this escape hatch entirely, or shorten `cooldown_days` per
pathogen to narrow the absorption window.

``` yaml
reconciliation:
  cooldown_reopen_ratio: ~
```

## Priority ranking does not match your triage

The priority score is a 0-100 weighted mean of seven components. A
component that cannot be computed for a given cluster (for example,
`density_component` requires a patient-day denominator) drops out and
the remaining weights renormalise.

`priority_score.weights` controls how much each component contributes:

``` yaml
priority_score:
  weights:
    excess_component: 0.25
    ratio_component: 0.15
    severity_component: 0.20
    growth_component: 0.15
    agreement_component: 0.10
    density_component: 0.10
    spatial_component: 0.05
```

Common adjustments:

- **Low-severity organisms rank too high**: raise `severity_component`.
  It reads from `severity_weight` in the pathogen configuration CSV.
- **Large-count clusters of common organisms outrank small-count
  clusters of serious ones**: raise `severity_component`, lower
  `excess_component`.
- **Spatially concentrated clusters should rank higher**: raise
  `spatial_component`.
- **Clusters where only one detector fired rank too high**: raise
  `agreement_component`, so signals corroborated by multiple detectors
  are ranked above those seen by one alone.

All seven weights must sum to 1.0.

## Seasonal detection is wrong

### MEM never runs

If no epidemics appear even for a pathogen you expect to be seasonal,
MEM may lack enough history to fit.

`mem.min_seasons` (default: 2) requires two fully-observed prior seasons
before fitting. `mem.min_climatology_years` (default: 3) requires three
complete ISO years of case history to derive a season anchor. Both are
“need real history first” dials with the same consequence as
`farrington.b`: set beyond what an instance carries, MEM says nothing.
Lower them for a newer instance, and raise them as history accrues.

Per-pathogen `mem_mode` in the pathogen configuration CSV overrides the
automatic seasonality test. Set to `yes` to force MEM for a pathogen
that the automatic test (`auto`) does not flag as seasonal, or to `no`
to suppress it for one that it does.

``` yaml
mem:
  min_seasons: 1
  min_climatology_years: 2
```

### MEM fires too early or too late

If the pre-epidemic threshold crossing does not align with clinical
reality, adjust the climatology or the seasonality test.

`mem.climatology_smooth_weeks` (default: 5) is the smoothing window for
the 52-week climatology used to find the season trough.
`mem.trough_percentile` (default: 0.25) is the quantile at or below
which a week counts as part of the trough. `mem.seasonality_peak_weeks`
(default: 8) and `mem.seasonality_min_peak_share` (default: 0.40)
control the automatic seasonality test: a pathogen is seasonal under
`mem_mode = 'auto'` if at least 40% of its detrended annual cases fall
within an 8-week peak window.

``` yaml
mem:
  climatology_smooth_weeks: 7
  trough_percentile: 0.20
```

Declaring the season started or ended is always a human act, never
automatic. MEM raises the signal; a person declares. See
[`vignette("detection-reconciliation")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/detection-reconciliation.md).

## Per-pathogen parameters

Per-pathogen biology lives in a CSV, separate from the YAML
configuration. The default file is
[`inst/config/episodic_default_pathogen_config.csv`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/config/episodic_default_pathogen_config.csv).

To override it, point `EPISODIC_PATHOGEN_CONFIG` at your own CSV
containing only the pathogens and columns you want to change. For each
pathogen you list, non-empty values override the default; empty cells
keep it. Pathogens not in your file keep their row entirely. New
pathogens are added.

``` r

Sys.setenv(EPISODIC_PATHOGEN_CONFIG = "/path/to/my_pathogens.csv")
```

A minimal overlay that changes only the severity weight for MRSA and
adds a local pathogen:

    pathogen,severity_weight,case_free_days
    MRSA,0.95,
    My Local Pathogen,0.60,21

The columns and what they control:

| Column | Effect |
|----|----|
| `pathogen` | Match key, raw lab-provided string |
| `episode_days` | Deduplication window for repeated isolates from the same patient (via [`AMR::get_episode()`](https://amr-for-r.org/reference/get_episode.html)) |
| `incub_min_days`, `incub_max_days` | Incubation period bounds, shown on the dossier timeline |
| `case_free_days` | Days without a new case before a cluster’s episode boundary, overrides `reconciliation.case_free_days_default` |
| `cooldown_days` | Cool-down window after closure during which new cases are absorbed rather than opening a new cluster |
| `rt_applicable` | Whether Rt estimation applies (0 or 1) |
| `si_mean_days`, `si_sd_days` | Serial interval parameters for Rt estimation |
| `si_dist` | Serial interval distribution: `gamma`, `lognormal`, or `weibull` |
| `mem_mode` | MEM seasonal detection: `auto` (derive from data), `yes` (force on), `no` (force off) |
| `severity_weight` | Clinical severity weight (0 to 1) feeding the priority score’s severity component |

A common confusion point: the per-pathogen *detection* thresholds for
`same_place` are in the YAML (under `same_place.overrides`), not in this
CSV. The CSV holds per-pathogen *biology*, the YAML holds per-pathogen
*detection rules*.

The validation framework accepts per-pathogen overrides through the
`pathogen_config` argument:

``` r

result <- episodic_validate_detection(
  history_years = 4,
  pathogen_config = data.frame(
    pathogen = "MRSA",
    severity_weight = 0.95,
    stringsAsFactors = FALSE
  )
)
```

## Verifying that a change helped

### Against synthetic ground truth

[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)
replays generated history through the full detection pipeline week by
week and matches what it found against seeded outbreaks on case sets.
Pass a `config` list (for YAML keys) or a `pathogen_config` data frame
(for CSV columns) with the settings you want to test:

``` r

baseline <- episodic_validate_detection(
  seeds = 1:10,
  history_years = 4,
  evaluation_weeks = 52
)

adjusted <- episodic_validate_detection(
  seeds = 1:10,
  history_years = 4,
  evaluation_weeks = 52,
  config = list(farrington = list(alpha = 0.01))
)

# Compare
baseline$summary[baseline$summary$metric %in% c("sensitivity", "ppv"), ]
adjusted$summary[adjusted$summary$metric %in% c("sensitivity", "ppv"), ]
```

This measures the machinery against known truth: does the change improve
sensitivity, reduce false alarms, or affect time-to-detection? The
defaults are deliberately small (one seed, four weeks) so it returns in
seconds; for a proper comparison use more seeds and a longer window. See
[`vignette("detection-validation")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/detection-validation.md)
for the full framework.

### Against your own board’s verdicts

The Performance screen measures the instance against its own
epidemiologists’ verdicts: what share of clusters the board classified
as noise (positive predictive value), and how quickly real signals were
detected (time-to-detection). This is not circular for the question “did
this change reduce false alarms”, which is exactly what PPV answers. It
cannot measure sensitivity, because an outbreak the detectors miss never
becomes a cluster for anyone to judge.

`config_hash` lets you compare runs before and after a change: filter
the Performance screen or the `episodic_detection_run` table by hash to
see the metrics under the old settings beside the new ones.

## Reference

Every tunable parameter across both the YAML configuration and the
per-pathogen CSV, with its default and one-line effect.

### Reconciliation

| Key | Default | Effect |
|----|---:|----|
| `reconciliation.case_free_days_default` | 14 | Fallback days without a case before a cluster episode boundary |
| `reconciliation.close_after_runs` | 14 | Consecutive runs with no new case before an unconfirmed cluster auto-closes |
| `reconciliation.cooldown_reopen_ratio` | 1.5 | Absorption escape-hatch ratio; `~` disables |
| `reconciliation.stale_open_days` | 60 | Force-closes unassessed clusters this many days after last case; `~` disables |
| `reconciliation.autoclose_unassessed` | `TRUE` | Whether auto-closure rules apply to clusters nobody assessed |

### Eligibility

| Key | Default | Effect |
|----|---:|----|
| `eligibility.min_baseline_weeks` | 52 | Weeks of history required before statistical detection runs |
| `eligibility.min_median_weekly_count` | 1 | Median weekly count over baseline; streams below are excluded from Farrington/MEM |
| `eligibility.min_nonzero_week_share` | 0.2 | Share of baseline weeks with at least one case |

### Effect-size floor

| Key | Default | Effect |
|----|---:|----|
| `effect_size_floor.min_excess_over_upperbound` | 3 | Cases above Farrington’s upper bound required to open a new cluster; `~` disables |
| `effect_size_floor.min_ratio_observed_expected` | 1.5 | Observed/expected ratio required to open a new cluster; `~` disables |

### same_place detector

| Key | Default | Effect |
|----|---:|----|
| `same_place.enabled` | `TRUE` | Master switch |
| `same_place.default_n_cases` | 3 | Cases at one institution within K days to fire |
| `same_place.default_k_days` | 14 | Time window in days |
| `same_place.lookback_days` | 90 | How far back a run reports hits; `~` unbounds (not recommended) |
| `same_place.overrides.<pathogen>` | (per pathogen) | `{n_cases, k_days}` per raw pathogen string |

### Farrington detector

| Key | Default | Effect |
|----|---:|----|
| `farrington.enabled` | `TRUE` | Master switch; switching off stops the detector, not the Pathogen screen’s baseline |
| `farrington.aggregation` | week | Aggregation unit |
| `farrington.b` | 2 | Reference years; model needs `(b+1)*52` weeks |
| `farrington.w` | 3 | Half-window (weeks) around the same calendar week in each reference year |
| `farrington.reweight` | `TRUE` | Down-weight past outbreak weeks by Anscombe residual |
| `farrington.weightsThreshold` | 2.58 | Residual cutoff for reweighting |
| `farrington.trend` | `TRUE` | Allow a linear trend term |
| `farrington.pastWeeksNotIncluded` | 26 | Trailing weeks excluded from the noise estimate |
| `farrington.limit54` | \[5, 4\] | Low-count safeguard |
| `farrington.alpha` | 0.05 | Significance level; lower = wider bound = fewer alarms |
| `farrington.max_weeks_tested` | 8 | Cap on trailing weeks tested per run |

### MEM detector

| Key | Default | Effect |
|----|---:|----|
| `mem.enabled` | `TRUE` | Master switch |
| `mem.min_seasons` | 2 | Fully-observed prior seasons required before fitting |
| `mem.levels` | \[pathogen_province, pathogen_region\] | Lattice levels MEM runs at |
| `mem.min_climatology_years` | 3 | Complete ISO years required to derive a season anchor |
| `mem.climatology_smooth_weeks` | 5 | Smoothing window for the 52-week climatology |
| `mem.trough_percentile` | 0.25 | Quantile at or below which a week is part of the trough |
| `mem.seasonality_peak_weeks` | 8 | Peak-concentration window width for the seasonality test |
| `mem.seasonality_min_peak_share` | 0.40 | Minimum share of detrended cases in the peak window for `mem_mode = 'auto'` |

### rare_trigger detector

| Key | Default | Effect |
|----|---:|----|
| `rare_trigger.enabled` | `TRUE` | Master switch |
| `rare_trigger.pathogens` | (4 shipped) | Curated list of raw pathogen strings; a single case fires |
| `rare_trigger.min_cases` | 1 | Cases in the window to fire |
| `rare_trigger.lookback_days` | 90 | How far back a run reports hits; `~` unbounds (not recommended) |

### Priority score

| Key | Default | Effect |
|----|---:|----|
| `priority_score.weights.excess_component` | 0.25 | Weight for cases above the model’s upper bound |
| `priority_score.weights.ratio_component` | 0.15 | Weight for observed/expected ratio |
| `priority_score.weights.severity_component` | 0.20 | Weight for the pathogen’s severity_weight |
| `priority_score.weights.growth_component` | 0.15 | Weight for cluster acceleration |
| `priority_score.weights.agreement_component` | 0.10 | Weight for share of detectors that fired |
| `priority_score.weights.density_component` | 0.10 | Weight for incidence per 1,000 patient-days versus baseline |
| `priority_score.weights.spatial_component` | 0.05 | Weight for case concentration in one location |

### Scale

| Key | Default | Effect |
|----|----|----|
| `scale.epidemic_levels` | \[pathogen_province, pathogen_region\] | Lattice levels that produce epidemics rather than outbreaks |

### Geography

| Key | Default | Effect |
|----|:---|----|
| `geography.region_code` | REGION | L5 whole-catchment code; name your service area |
| `geography.area_code_prefix` | AREA- | L3 prefix before the leading postcode characters |
| `geography.area_pc_characters` | 2 | Number of leading postcode characters for L3 grouping |

### Suppression

| Key | Default | Effect |
|----|---:|----|
| `suppression.child_dominance_threshold` | 0.70 | Case-overlap share for a child to suppress its parent |
| `suppression.parent_diffuse_threshold` | 0.50 | Below this per child, the rise counts as diffuse |
| `suppression.parent_min_flagged_children` | 2 | Minimum overlapping children before the diffuse rule fires |

### Report

| Key | Default | Effect |
|----|---:|----|
| `report.small_count_threshold` | 5 | Counts below this in geography/institution tables are suppressed to prevent re-identification |
| `report.output_dir` | ~ | Base directory for rendered reports; mandatory when `EPISODIC_DB` is a DSN |

### Access

| Key | Default | Effect |
|----|----|----|
| `access.require_login` | `FALSE` | Whether the dashboard requires a sign-in to read anything |

### Per-pathogen CSV columns

| Column | Typical default | Effect |
|----|---:|----|
| `episode_days` | 30 | Deduplication window for repeated isolates |
| `incub_min_days`, `incub_max_days` | (varies) | Incubation period bounds for the dossier timeline |
| `case_free_days` | 14 | Overrides `reconciliation.case_free_days_default` |
| `cooldown_days` | (varies) | Cool-down absorption window after closure |
| `rt_applicable` | 0 | Whether Rt estimation applies (0 or 1) |
| `si_mean_days`, `si_sd_days` | (varies) | Serial interval for Rt |
| `si_dist` | (varies) | Serial interval distribution |
| `mem_mode` | auto | MEM: `auto`, `yes`, or `no` |
| `severity_weight` | 1.00 | Clinical severity for priority scoring (0 to 1) |

## See also

- [**Detection and
  reconciliation**](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/vignettes/detection-reconciliation.Rmd) -
  how the detectors, reconciliation, suppression, and MEM work
- [**Deployment**](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/vignettes/deployment.Rmd) -
  the configuration overlay system and instance setup
- [**Measuring detection against known
  truth**](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/vignettes/detection-validation.Rmd) -
  measuring detection against known truth
- [**Environment
  variables**](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/vignettes/environment-variables.Rmd) -
  all `EPISODIC_*` variables including `EPISODIC_PATHOGEN_CONFIG`
