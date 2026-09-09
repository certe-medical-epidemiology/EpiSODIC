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

<!-- RESULTS -->

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

### 5.4 The matching rule is a choice

`min_recall` and `min_precision` at 0.5 are defensible and arbitrary. The
threshold sweep (§4.6) exists so that a reader can see how much the
headline moves; it moves, and the direction is what you would expect.
Anyone quoting a single number should quote the threshold with it.

### 5.5 The version measured is the version this harness fixed

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

### 5.6 Detection is not assessment

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
