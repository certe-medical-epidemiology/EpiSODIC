# Read Back What Was Injected Into Synthetic Data

[`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md)
injects outbreaks into an otherwise endemic history. This reads back
exactly what it injected: one row per outbreak saying where and when it
ran and how large it was, and the case-level membership that goes with
it. It is what makes a detection result measurable against known truth
rather than against an epidemiologist's verdict on it - see
[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md).

## Usage

``` r
episodic_synthetic_ground_truth(cases)
```

## Arguments

- cases:

  A data frame from
  [`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md).

## Value

A list of two data frames:

- `outbreaks`:

  One row per injected outbreak: `outbreak_id`, `label`, `pathogen`,
  `institution_key` and `ward` (both `NA` where the outbreak was not
  confined to one), `first_day`, `peak_day`, `last_day`, `n_cases`, and
  the `expected_channel` and `expected_level` the shape was designed to
  exercise.

- `cases`:

  One row per injected case: `outbreak_id` and the `source_key` it
  carries in the case data, which is what joins it to `episodic_case`
  once a run has loaded it.

Both are empty (zero rows, same columns) when nothing was injected.

## Details

Injected cases also carry a recognisable `PT-OUTBREAK-*` patient key,
and it is tempting to recover membership by matching on it. Do not: that
couples whatever does the matching to a naming convention nobody has
undertaken to keep, and it fails silently, as a smaller set of outbreak
cases rather than an error, the first time the convention changes.

## See also

[`episodic_synthetic_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_cases.md),
[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)

## Examples

``` r
cases <- episodic_synthetic_cases(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
truth <- episodic_synthetic_ground_truth(cases)
truth$outbreaks[, c("outbreak_id", "pathogen", "n_cases")]
#>   outbreak_id                 pathogen n_cases
#> 1        RARE   Neisseria meningitidis       1
#> 2        WARD Clostridioides difficile       3
#> 3         LTC                Norovirus       8
#> 4          PS                Norovirus      14
#> 5        PROP     Bordetella pertussis      14
#> 6        WAVE              Influenza A     144
```
