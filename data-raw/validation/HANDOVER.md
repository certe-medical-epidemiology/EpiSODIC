# Detection validation: what was measured, how, and what came out

A handover of the detection validation performed on EpiSODIC, written to
be read by someone who was not there and who intends to disagree with it.
It records the design, the parameters, the results, and the things that
are wrong with it.

Everything here is reproducible from this repository. Nothing in it was
tuned after seeing a result.

---

## 1. The question, and why the obvious answer is circular

EpiSODIC's dashboard has a Performance screen. It computes positive
predictive value and timeliness from the epidemiologists' own verdicts on
the clusters the system showed them. As an operational metric that is
sound: it says whether the board found the dossiers worth opening.

As evidence for a claim about a detection system it is circular, and in
one respect it is not merely weak but structurally incapable. **Sensitivity
cannot be measured that way at all.** An outbreak the detectors miss never
becomes a cluster, so it never reaches an epidemiologist, so it never
enters the denominator. A system that detected one outbreak in ten and was
right about that one would report a perfect record.

So the question this work answers is a different one: *how does EpiSODIC
perform against outbreaks that are known to be present because they were
deliberately put there?*

---

## 2. Design

### 2.1 The data, and its ground truth

`episodic_synthetic_cases()` generates laboratory surveillance data for a
fictional region: 8 hospitals (6 to 24 wards each), 20 long-term care
institutions and 36 municipalities, 64 reporting units in all. On top of a
seasonal Poisson baseline for 8 endemic pathogens it injects six
outbreaks, each shaped for a different detection channel:

| Id | Pathogen | Shape |
|---|---|---|
| `RARE` | *Neisseria meningitidis* | 1 case |
| `WARD` | *Clostridioides difficile* | 3 cases, one ward, 11 days |
| `LTC` | Norovirus | 8 cases, one nursing home |
| `PS` | Norovirus | 14 cases, one ward, tightly bunched |
| `PROP` | *Bordetella pertussis* | 15 cases, 4 generations, 20-day serial interval |
| `WAVE` | Influenza A | ~144 cases, 6 rising weeks, spread one case to a place |

The generator **returns what it injected**: one row per outbreak
(identity, pathogen, place, first/peak/last day, size) and the case-level
membership, keyed on `source_key`, read back with
`episodic_synthetic_ground_truth()`.

This matters more than it looks. Injected cases also carry a recognisable
`PT-OUTBREAK-*` patient key, and recovering membership by matching on that
string would have been easier. It would also have coupled the measurement
to a naming convention nobody has undertaken to keep, and it would have
failed *silently* - as a smaller set of outbreak cases, not as an error -
the first time the convention changed. Nothing in the harness matches on
patient keys.

### 2.2 A prospective replay, not a retrospective pass

A single detection run over the whole history sees an outbreak's last case
at the same instant as its first, so it can say nothing about timeliness.

The harness therefore steps `run_date` forward one week at a time and
hands each run **only the cases with `sample_date <= run_date`** - what a
laboratory extract taken that day would actually have contained. Each run
is a full `episodic_run_cron()`: detection, reconciliation, priority
scoring, lattice suppression. Clusters persist between runs exactly as
they do in production.

Each replicate builds its own throwaway SQLite database with `tempfile()`,
writes its own instance configuration into a temporary directory, and
removes both afterwards. No replicate touches a live instance, and none
goes through `episodic_demo()`.

### 2.3 How a cluster is matched to an outbreak

On the cases themselves, in both directions. Never on overlapping dates:
an endemic winter cluster spans the same fortnight as a seeded outbreak
and shares not one case with it.

- An **outbreak is detected** when some cluster's final membership holds at
  least `min_recall` of its cases.
- A **cluster is a true positive** when at least `min_precision` of *its
  own* cases belong to one seeded outbreak. Anything else is a false
  alarm - including a cluster holding every case of an outbreak and forty
  endemic ones besides, because a dossier that is one part outbreak to
  five parts background is not a dossier about the outbreak.

Both thresholds default to 0.5 and both are swept (§4.6). A cluster merged
into another is not counted at all: it was never a dossier anyone was
shown.

### 2.4 Three delays, because there are three claims

Reporting one number as "the detection delay" would be choosing rather
than measuring. All three are reported:

- **`delay_from_open`** - to the run that first raised the cluster that
  turned out to be this outbreak, whatever it held then. The most
  generous.
- **`delay_from_first`** - to the first run at which that cluster held
  `min_recall` of the outbreak *as it stood that day*. The operational
  answer: the day a dossier existed that was mostly this outbreak.
- **`captured_run`** - to the first run at which it held `min_recall` of
  the whole outbreak, cases still to come included. Partly a measure of
  how fast the outbreak grew.

Delay from the outbreak's **third** case is reported as well, since onset
is not observable in practice and three cases is roughly when a human
starts wondering.

### 2.5 Four conventions that do the heavy lifting

1. **A metric with no denominator is `NA`, never 0.** A history with
   nothing injected has no sensitivity; that is not a sensitivity of zero,
   and averaging it as one would be a straightforward error.
2. **An outbreak nothing found is right-censored, not dropped.** A median
   delay computed over the outbreaks that *were* detected is biased
   downward, and the bias is worst exactly where the method is weakest,
   because the shapes it misses are the slow ones. A Kaplan-Meier estimate
   over all outbreaks that began inside the window, censoring the misses at
   the time they were watched for, is reported alongside the plain median
   and the share never detected. Never quote the plain median alone.
3. **Seeds are the replicates.** One realisation of a stochastic generator
   is an anecdote. Every figure is summarised as a median and
   interquartile range across seeds, *and* pooled with an interval:
   Wilson for proportions, exact Poisson for rates.
4. **Rates carry their denominator.** False alarms are reported per
   stream-week, with the stream-week count stated on the same row. A count
   of false alarms without its denominator is uninterpretable, and the
   denominator here is large and moves with the configuration.

---

## 3. What was run

| | |
|---|---|
| Package version | 0.17.0 |
| Evaluation window | 26 weekly runs, 2026-01-04 to 2026-06-28 |
| History before the window | 4 years |
| Seeds | 20 (5 for the operating-point sweep) |
| Matching thresholds | `min_recall` 0.5, `min_precision` 0.5 |
| Detector settings | the shipped defaults, unmodified |

Four years of history is not arbitrary. Farrington needs
`(farrington$b + 1) x 52` weeks before it will fit at all (`b = 2`, so 156
weeks), and MEM needs two full seasons. An evaluation window opening
before that measures the warm-up rather than the method.

Eleven scenarios:

| Scenario | What it is for |
|---|---|
| `main` | The shipped configuration, all four detectors |
| `negative_control` | The same history with nothing injected |
| `drop_farrington`, `drop_mem`, `drop_same_place`, `drop_rare_trigger` | What each detector adds |
| `comparator_same_place_7`, `comparator_same_place_14` | A naive N-cases-at-one-place rule |
| `comparator_shewhart` | A 2-SD control limit on weekly regional counts |
| `comparator_same_place_14_control`, `comparator_shewhart_control` | Those rules on a clean history |
| `sweep_*` (6 points) | Sensitivity against false alarms as thresholds move |

The naive same-place rule is run at **both** three-in-seven days (the
obvious trivial rule) and three-in-fourteen (what EpiSODIC's own
`same_place` is configured for). Reporting only the tighter setting would
have made the comparator look weak for a reason that has nothing to do
with it being naive.

Detectors are switched off through `config$<detector>$enabled`, which is
part of `config_hash`, so two runs with different detectors active cannot
be confused for one another in the record.

### Where the outbreaks sit

All six are anchored to the end of the generated window by default, which
would place every one of them in the last eight weeks of a twenty-six week
evaluation. The study disperses them with `outbreak_offsets`, computed
from each outbreak's own span so that all six *begin* inside the window,
and the script reports any that did not.

---

## 4. Results

Numbers are in `results/`. Every row carries the package version, the
resolved `config_hash`, and the seeds, runs and stream-weeks behind it.

### 4.1 Detection performance

20 seeds x 26 weekly runs, 468,710 stream-weeks, config `9b97cc3f4a58`,
EpiSODIC 0.17.0. Proportions carry Wilson intervals, rates exact Poisson
intervals.

| | value | 95% CI |
|---|---|---|
| **Sensitivity** | 119/120 = **0.992** | 0.954-0.999 |
| **PPV** | 156/199 = **0.784** | 0.722-0.835 |
| **PPV after lattice suppression** | 156/199 = 0.784 | 0.722-0.835 |
| **False alarms** | 43/468,710 = **9.2 x 10<sup>-5</sup> per stream-week** | 6.6-12.4 x 10<sup>-5</sup> |
| **All alarms** | 199/468,710 = 4.2 x 10<sup>-4</sup> per stream-week | 3.7-4.9 x 10<sup>-4</sup> |
| Never detected | 1/120 = 0.008 | 0.001-0.046 |

Per outbreak shape, over 20 seeds each:

| Shape | Detected | | Design channel | Detected |
|---|---|---|---|---|
| `RARE` | 20/20 | | `rare_trigger` | 20/20 |
| `WARD` | 20/20 | | `farrington` | 20/20 |
| `LTC` | 20/20 | | `same_place` | 79/80 |
| `PS` | 20/20 | | | |
| `PROP` | **19/20** | | | |
| `WAVE` | 20/20 | | | |

PPV by the detector that first raised the cluster:

| First detector | PPV | |
|---|---|---|
| `rare_trigger` | 20/20 = 1.000 | |
| `farrington` | 50/53 = 0.943 | 0.846-0.981 |
| `same_place` | 84/110 = 0.764 | 0.676-0.833 |
| `farrington+mem` | 2/3 = 0.667 | 0.208-0.939 |
| **`mem` alone** | **0/13 = 0.000** | **0-0.228** |

Case-level agreement is not marginal. The median cluster was **0.983**
outbreak cases by its own composition, and the median outbreak was
captured at **recall 1.00** by its best-matching cluster.
**Fragmentation was 1** for every detected outbreak: none was split
across dossiers.

### 4.2 Timeliness

Medians with interquartile range across seeds, over the 119 detected
outbreaks that began inside the evaluation window.

| | median | IQR |
|---|---|---|
| Delay from the outbreak's first case | **6 days** | 5.5-6 |
| Delay from the run that first raised the cluster | 6 days | 5.5-6 |
| Delay from the outbreak's third case | **5 days** | 3-6 |
| Kaplan-Meier median, misses censored | **5 days** | 119 events of 120 |
| Cases still to come at the moment of detection | **0.130** | 0.071-0.223 |
| Detected before the outbreak's own peak | 30/119 = **0.252** | 0.183-0.337 |

The Kaplan-Meier median and the plain median agree here because almost
everything was detected. They would not agree on a method that missed
more, which is why both are reported.

### 4.3 Alarm burden and specificity

| | |
|---|---|
| Streams watched per run | median **903** (IQR 893-907) |
| New clusters per run | median **0** (IQR 0-0), 199 over 520 run-weeks |
| Negative control alarm rate | 32/466,037 = **6.9 x 10<sup>-5</sup> per stream-week** |

The negative control is the same endemic history with nothing seeded in
it, so every alarm on it is false by construction. It produced **32 alarms
across 20 seeds and 26 weeks** - about 1.6 dossiers per six-month replay
across nine hundred streams. On the seeded data the false-alarm rate is
9.2 x 10<sup>-5</sup>, statistically indistinguishable from the control's
6.9 x 10<sup>-5</sup>, which is what you would expect if the seeded
outbreaks are not themselves generating spurious neighbours.

Lattice suppression removed nothing (§5.3).

### 4.4 What each detector contributes

| Removed | Sensitivity | Lost | False alarms | Shapes it alone found |
|---|---|---|---|---|
| `same_place` | 0.592 | **0.400** | 43 -> 17 | WARD 20/20, PROP 19/20, LTC 9/20 |
| `rare_trigger` | 0.825 | 0.167 | 43 -> 43 | RARE 20/20 |
| `farrington` | 0.942 | 0.050 | 43 -> 42 | WAVE 6/20 |
| `mem` | 0.992 | **0.000** | 43 -> 29 | none |

Which detector got there first, per shape, over 20 seeds:

| Shape | First detector |
|---|---|
| `RARE` | `rare_trigger` 20/20 |
| `WARD` | `same_place` 20/20 |
| `PROP` | `same_place` 19/19 |
| `LTC` | `same_place` 15/20, `farrington` 5/20 |
| `PS` | `same_place` 18/20, `farrington` 2/20 |
| `WAVE` | `farrington` 18/20, `farrington+mem` 2/20 |

Two things are worth stating plainly.

**Farrington's contribution looks small and is not.** Removing it costs
only 0.05 sensitivity, because `same_place` picks up most shapes anyway.
But it is the only detector that finds the regional wave - the diffuse
signal deliberately spread one case to a place so that no local rule can
see it - and it does so with the highest PPV of any channel (0.943).
A drop-one figure understates a detector that covers a shape nothing else
covers.

**MEM contributes nothing here, and it is not because it is idle.** MEM
fired on 29 of the 199 clusters. Thirteen were raised by MEM alone, and
**all thirteen are false alarms** - every one region-level, every one RSV
or Influenza A, across 8 of the 20 seeds. The other 16 are
`farrington+mem`, all Influenza A, 14 of them the seeded wave that
Farrington found too.

MEM is doing exactly what MEM is for: detecting the onset of the epidemic
season in seasonal respiratory viruses, on four years of history that
contains precisely that.

**That 0/13 is partly an artefact of the ground truth, and must not be
read as damning.** The generator injects six outbreaks and none of them is
"the influenza season started", so a seasonal onset can never be scored a
true positive - there is nothing in the truth table for it to match, and
it is counted a false alarm by construction. The measurement says *MEM's
output does not correspond to any seeded outbreak*, which is true and is
not the same statement as *MEM's output is worthless*. Measuring MEM
fairly would require the seasonal onset to be a labelled ground-truth
event, which it is not (§5.3, and issue #46).

Nor is MEM redundant with Farrington, which the drop-one figure might be
taken to imply. Farrington compares the current week against the same
calendar weeks of previous years, so it is *designed to be blind to
expected seasonality*: a normal winter rise is what its baseline predicts,
and it does not fire. Farrington answers "is this more than the season
would predict?"; MEM answers "has the season started?". The study shows
the difference empirically - on all 13 MEM-only clusters, Farrington was
silent. Nothing else in the system produces that signal.

What survives the caveat is narrower and still real: under an assessment
queue whose unit is "a possible outbreak to investigate", MEM contributes
about one dossier per seasonal pathogen per year that is not one, and an
epidemiologist opening it would classify it `expected_variation`. Whether
a seasonal onset should be a cluster at all, or a distinct class of signal
with its own place in the app, is issue #50. It is deliberately left open
rather than settled by moving a threshold.

### 4.5 Does the priority score rank the real ones first?

| | median | IQR |
|---|---|---|
| **AUC, true positives against false alarms** | **0.866** | 0.792-0.961 |
| Priority score, true positives (n = 156) | 58.0 | 57.3-59.3 |
| Priority score, false alarms (n = 43) | 47.3 | 43.0-50.3 |

The score does rank real outbreaks above false alarms, with an AUC
around 0.87 and a median separation of roughly eleven points. The
calibration by score band is in `priority_score_calibration.csv`; with 199
clusters in five bands it is thin, and should be read as indicative.

### 4.6 Robustness to the matching thresholds

The headline does not depend on the pair chosen. `min_precision` cannot
affect sensitivity and `min_recall` cannot affect PPV, and the table shows
exactly that, which is also a check that the two rules are independent as
intended.

Sensitivity, by `min_recall` (rows):

| | 0.2 | 0.35 | 0.5 | 0.65 | 0.8 |
|---|---|---|---|---|---|
| **0.2** | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| **0.35** | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| **0.5** | 0.992 | 0.992 | 0.992 | 0.992 | 0.992 |
| **0.65** | 0.992 | 0.992 | 0.992 | 0.992 | 0.992 |
| **0.8** | 0.958 | 0.958 | 0.958 | 0.958 | 0.958 |

PPV, by `min_precision` (columns): 0.844, 0.784, 0.784, 0.754, 0.688.

Over the whole grid sensitivity moves between 0.958 and 1.000, and PPV
between 0.688 and 0.844. Nothing here rests on the 0.5/0.5 default.

### 4.7 Against a rule anyone could write in an afternoon

Same generated data, same replay, same metrics.

| | sensitivity | PPV | alarms per unit-week | denominator |
|---|---|---|---|---|
| **EpiSODIC** | **0.992** | **0.784** | 4.2 x 10<sup>-4</sup> | 468,710 stream-weeks |
| Naive same-place, 3 in 14 days | 0.667 | 0.433 | 3.9 x 10<sup>-4</sup> | 501,009 place-weeks |
| Naive same-place, 3 in 7 days | 0.500 | 0.604 | 2.0 x 10<sup>-4</sup> | 501,009 place-weeks |
| 2-SD Shewhart on weekly regional counts | 0.625 | 0.451 | 6.3 x 10<sup>-2</sup> | 4,697 pathogen-weeks |

**Read the denominators before comparing the rates.** They are not the
same unit: EpiSODIC watches the lattice, the same-place rule watches
places, and Shewhart watches one series per pathogen. Only the sensitivity
and PPV columns are directly comparable.

The naive same-place rule is run at three-in-fourteen days, which is what
EpiSODIC's own `same_place` is configured for, as well as at the tighter
three-in-seven. At the matched setting it reaches 0.667 sensitivity at
0.433 PPV; EpiSODIC reaches 0.992 at 0.784 on the same data. The margin is
therefore not an artefact of giving the comparator a harsher parameter.

On a clean history both comparators alarm as well: the same-place rule at
2.0 x 10<sup>-4</sup> per place-week, Shewhart at 4.1 x 10<sup>-2</sup> per
pathogen-week.

### 4.8 The operating-point curve

Five seeds per point, otherwise the main configuration. **This is a curve
for the paper, not a proposal to move the shipped defaults.**

| Setting | Sensitivity | PPV | False alarms per stream-week | Median delay |
|---|---|---|---|---|
| **shipped** (alpha 0.05, 3 in 14) | 1.000 | 0.760 | 1.0 x 10<sup>-4</sup> | 6 days |
| farrington alpha 0.01 | 1.000 | 0.714 | 1.2 x 10<sup>-4</sup> | 8.5 days |
| farrington alpha 0.10 | 1.000 | 0.717 | 1.3 x 10<sup>-4</sup> | 6 days |
| same_place 2 in 14 | 1.000 | 0.534 | 6.9 x 10<sup>-4</sup> | 6 days |
| same_place 4 in 14 | 0.967 | **0.867** | **5.1 x 10<sup>-5</sup>** | 5.5 days |
| same_place 3 in 7 | 0.900 | 0.848 | 6.0 x 10<sup>-5</sup> | 5 days |

Loosening `same_place` to two cases costs a great deal of PPV and gains no
sensitivity. Tightening it to four raises PPV to 0.867 and halves the
false-alarm rate for 0.033 of sensitivity. Whether that is a trade worth
making is a decision for the maintainer, taken deliberately and
separately; it is recorded here because the sweep found it, not because
this work is recommending it.

### 4.9 Reproducibility

The study was run twice from a cleared cache with the same seeds, and the
main scenario came out identical to the case: 119/120, 156/199, 43 false
alarms of 468,710 stream-weeks, median delay 6 days, same `config_hash`.
The seeds fully determine the result.

---

## 5. What is wrong with this

The section a critical reviewer should read first.

### 5.1 It is synthetic data, and one generator

Every result is conditional on `episodic_synthetic_cases()` being a fair
model of laboratory surveillance data, which it is not claimed to be. The
baseline is a seasonal Poisson process with no reporting-delay structure
beyond a short receipt lag, no outbreaks other than the six injected, no
data-quality faults, no changes in testing policy, and no correlation
between places. Real surveillance data has all of these, and each one
would be expected to *raise* the false-alarm rate.

The figures here are therefore an upper bound on performance and a lower
bound on alarm burden. They are useful for comparing detectors against
each other on identical data, and for the drop-one and comparator
contrasts, which are internally controlled. They are not a prediction of
what a laboratory will see.

### 5.2 Six outbreaks, one shape each

Sensitivity per shape rests on twenty Bernoulli trials, one per seed, and
its interval is correspondingly wide. The six shapes were chosen to
exercise different channels rather than to sample the space of real
outbreaks, and there is exactly one instance of each per replicate.

### 5.3 The generator cannot exercise lattice suppression

Fragmentation was 1 for every detected outbreak, so no outbreak ever
surfaced as a redundant parent and child at once, so lattice suppression
never fired and PPV before and after it is identical. That mechanism is
therefore **untested by this study**, and its absence from the results
should not be read as evidence that it works. It is a gap in the
generator, and it is recorded on issue #46.

### 5.4 The ground truth contains only outbreaks

The six injected events are all outbreaks in the ordinary sense: a rise
above what is expected, at a place or across a region. The truth table
contains no *seasonal epidemic onset*, even though the generator produces
seasonal baselines for eight pathogens across four years and an
epidemiologist would certainly call the start of the influenza season a
reportable moment.

Any detector whose job is to identify that moment is therefore scored
against a ground truth that does not contain it, and every signal it
raises is a false alarm by definition. This is exactly what happened to
MEM (§4.4), and it is why that result is presented with a caveat rather
than as a verdict. The same caution applies to any future detector aimed
at something the generator does not label.

### 5.5 The matching rule is a choice

`min_recall` and `min_precision` at 0.5 are defensible and arbitrary. The
threshold sweep (§4.6) exists so that a reader can see how much the
headline moves; it moves, and the direction is what you would expect.
Anyone quoting a single number should quote the threshold with it.

### 5.6 The version measured is the version this harness fixed

Building the harness uncovered defects that were fixed before the study
ran. The most consequential: the geography was resolved from two places
inside one run, so every geographic stream matched no case, so **Farrington
and MEM produced nothing at all** at the area, province and region levels.
Before that fix the study found zero Farrington detections across
twenty-six weeks and nine hundred streams.

These results therefore describe EpiSODIC 0.17.0 and **do not describe any
earlier version**. Anyone comparing against an older deployment is
comparing against different software. The defects and their fixes are in
`NEWS.md` and in pull request #49.

### 5.7 Detection is not assessment

The harness measures which clusters were raised, not what an
epidemiologist did with them. The priority-score discrimination in §4.5 is
the closest it comes to the triage question, and it measures the score's
ranking against ground truth rather than against a human's judgement.

---

## 6. Reproducing it

```bash
Rscript data-raw/validation/run_study.R
```

About three hours. Each scenario's full result is cached under
`results/raw/` and reused if present, so an interrupted run resumes; the
cache knows nothing about the code that produced it, so
`EPISODIC_VALIDATION_FRESH=true` ignores it, and a fresh run is what
settles any disagreement. `EPISODIC_VALIDATION_QUICK=true` exercises every
scenario at a fraction of the size in about five minutes, and says out
loud that the window is too short to disperse the outbreaks and so
measures no timeliness at all.

`data-raw/` is in `.Rbuildignore`: none of this ships in the built
package, none of it runs under `R CMD check`, and none of it is reachable
from an installed EpiSODIC. The exported entry point,
`episodic_validate_detection()`, returns in seconds at its own defaults,
which is what the examples and the test suite use.

The matching and metric functions are pure functions over data frames and
are tested without running a single detection, in
`tests/testthat/test-validate_match.R` and `test-validate_metrics.R`.

---

## 7. What was deliberately not done

**No threshold was tuned.** No value in
`inst/config/episodic_default_config.yaml` was changed in the course of
this work. The operating-point sweep is a curve for the paper, not a
proposal to move the shipped defaults. If the sweep suggests a better
operating point, that is a separate decision made deliberately and
afterwards; making it in the same breath as the evaluation would be
fitting the evaluation to the answer.

**No unflattering result was dropped.** The ones in §4 that reflect badly
on the system are there because they are what came out.
