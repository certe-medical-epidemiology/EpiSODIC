# EpiSODE

**Epidemiological Signal Observation and Detection Engine**

EpiSODE is an outbreak cluster detection and assessment system for a
department of medical epidemiology. It ingests laboratory-confirmed
infections, detects statistical and rule-based aberrations, reconciles them
into persistent clusters, and gives a small board of epidemiologists a
dossier to assess each one, with a full audit trail and outbreak reports for
clinical colleagues.

The engine and the instance it runs against are kept separate: this
repository is open-source software with no data and no site-specific
configuration. See `ARCHITECTURE.md` for the full design and `MILESTONES.md`
for the build plan.

## Status

Under active development. Milestone 1 (core detection engine, no interface)
is in progress.

## Installation

```r
# install.packages("remotes")
remotes::install_github("certe-medical-epidemiology/episode")
```

EpiSODE's detection engine has **no dependency on any laboratory system,
data warehouse, or Certe-internal package**. Every detector runs on
open-source, CRAN-hosted packages (`surveillance` for Farrington). Certe
packages (`certegis`, `certeplot2`, `certestyle`) are optional and only
used by the interface (M2+) for geography and house-style rendering; without
them EpiSODE still runs completely on its bundled synthetic demo data.

## Data format

EpiSODE never queries a laboratory information system, data warehouse, or
any other data source itself. That is deliberately the operator's own step,
run before EpiSODE: extract from wherever your data lives, transform into
the shape below, then call `episode_run_cron()` with a function that returns
it. This keeps the engine reusable by any laboratory, not only Certe's.

```r
episode_run_cron(
  db_path = "/path/to/episode.sqlite",
  ingest_source_fn = function() my_extract_and_transform_function(),
  denominator_source_fn = NULL  # optional, see "Positivity metadata" below
)
```

### Cases (mandatory)

One row per confirmed-positive laboratory result. `episode_ingest_columns`
lists the exact allow-listed columns; the ones that need explanation:

| Column | Meaning |
|---|---|
| `pathogen` | The organism as your lab reports it, as free text. **Not** resolved against any taxonomy (this is deliberate: `AMR::as.mo()` only covers non-viral organisms, and EpiSODE has to detect clusters of anything a lab reports, viruses included). The same underlying isolate can appear more than once under different `pathogen` values when that is epidemiologically useful - an ETEC isolate reported as both `"Escherichia coli"` and `"ETEC"`, so each is watched on its own. This is your transform step's decision, not EpiSODE's. |
| `sample_date` | The anchor date. If your system falls back to a receipt date when sample date is unfilled, that fallback should already have happened before this row reaches EpiSODE. |
| `institution_key` | A stable identifier for the institution, hashed internally so a later rename does not fracture history. |
| `institution_type` | One of `hospital`, `ltc_institution`, `gp_municipality`, `ooh_service`, `other`. |
| `ward` | Only meaningful (and only used) for hospitals. |

**Deduplication is EpiSODE's job, not yours.** Send every positive result;
EpiSODE collapses isolates for the same patient and pathogen into one case
per episode, using the episode length configured per pathogen in
`inst/config/pathogen_config.csv`.

**Do not send negative results here.** This feed drives every detector; it
is deliberately positives-only so an operator never has to ship a
multi-year, per-test linelist just to run detection.

### Positivity metadata (optional)

If, and only if, you can produce it: a small, pre-aggregated table of test
counts, used purely as an interpretive aid on the dossier (a rising case
count with flat test volume is a much weaker signal than the same rise with
stable positivity) and never as a detection input.

| Column | Meaning |
|---|---|
| `pathogen` | Matches the cases feed. |
| `sample_date` | A period start (e.g. week start); this is aggregate data, not per-test. |
| `care_line` | `first`, `second`, `other`, or `unknown`. |
| `area_code` | Optional geographic stratum. |
| `n_tests` | Total tests run for this pathogen/period/stratum, positive and negative. |

This is realistic to produce for a multiplex PCR panel testing a fixed list
of targets (your LIS can report "we ran 40 GI panels this week" trivially).
It is not meaningful for open-ended culture results, where there is no
closed list of things a negative result could have been - if that is your
situation, simply never call this, and positivity panels stay blank for
your streams. See `episode_denominator_source_synthetic()` for a worked
example.

## Licence

GPL-2, see `LICENSE`.
