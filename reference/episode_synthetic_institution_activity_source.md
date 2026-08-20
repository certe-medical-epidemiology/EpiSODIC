# Synthetic institution activity source

A worked example of the optional activity contract
(`episode_institution_activity_ingest_run()`): weekly patient-days per
hospital, modelled as `n_beds * occupancy` with light seasonal variation
(winter admissions run higher) and noise. Not called by
[`episode_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_run_cron.md)
unless an `institution_activity_source_fn` is supplied; demonstrates the
shape only, so the bundled demo can show an incidence-density curve
without a real hospital feed.

## Usage

``` r
episode_synthetic_institution_activity_source(
  institutions,
  start_date = as.Date("2021-01-01"),
  end_date = as.Date("2025-12-31"),
  seed = 1
)
```

## Arguments

- institutions:

  A data frame from `episode_db_institutions()` (or
  `episode_synthetic_institutions()`'s own shape before insertion),
  filtered internally to `institution_type == "hospital"`.

- start_date, end_date:

  The period to generate weekly rows for.

- seed:

  RNG seed.

## Value

A data frame with `institution_key`, `period_start`, `period_end`,
`patient_days`, `n_beds`, `source`.

## Examples

``` r
institutions <- data.frame(
  institution_key = "HOSP-1", institution_type = "hospital", n_beds = 320
)
activity <- episode_synthetic_institution_activity_source(
  institutions, start_date = as.Date("2025-01-01"), end_date = as.Date("2025-03-31")
)
head(activity)
#>   institution_key period_start period_end patient_days n_beds    source
#> 1          HOSP-1   2025-01-01 2025-01-07         1964    320 synthetic
#> 2          HOSP-1   2025-01-08 2025-01-14         1989    320 synthetic
#> 3          HOSP-1   2025-01-15 2025-01-21         2031    320 synthetic
#> 4          HOSP-1   2025-01-22 2025-01-28         2097    320 synthetic
#> 5          HOSP-1   2025-01-29 2025-02-04         1951    320 synthetic
#> 6          HOSP-1   2025-02-05 2025-02-11         2084    320 synthetic
```
