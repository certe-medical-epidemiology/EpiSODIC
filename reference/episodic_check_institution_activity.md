# Check Your Hospital Activity Data Before You Hand It to EpiSODIC

The same purpose as
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md),
for the optional `institution_activity` feed: every check
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
would refuse a run over, reported instead of thrown, plus a couple of
things worth a look. Nothing is written, no database is needed.

## Usage

``` r
episodic_check_institution_activity(institution_activity)
```

## Arguments

- institution_activity:

  Your hospital activity data: a data frame or `tibble` with
  `institution_key`, `period_start`, `period_end`, `patient_days`
  (nullable `admissions`, `n_beds`, `source`) - see
  [`episodic_synthetic_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_synthetic_institution_activity.md)
  for the shape - or a zero-argument function returning one.

## Value

A data frame of findings with class `episodic_case_check`, the same
shape
[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
returns. Zero rows means the data set passed every check.

## Details

Unlike the case and denominator feeds, an `institution_key` that matches
no institution in your case data is not a problem here - it is
deliberately allowed, and only ever counted and warned about at run time
(see `episodic_institution_activity_load()`), since the two feeds need
not be perfectly synchronised and this function has no case data to
compare against in the first place.

## See also

[`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)
for the case data feed,
[`episodic_check_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_denominators.md)
for the positivity feed.

## Examples

``` r
institutions <- data.frame(
  institution_key = "HOSP-1", institution_type = "hospital", n_beds = 320
)
activity <- episodic_synthetic_institution_activity(
  institutions,
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
episodic_check_institution_activity(activity)
#> -- EpiSODIC institution activity data check --------------------------------
#>    13 rows, 6 columns
#> 
#> v Institution activity data satisfies its contract, and is ready for episodic_run_cron().
```
