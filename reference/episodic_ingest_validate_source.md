# Check that your ingestion source has the right shape

Call this on the data frame your own ingestion source function returns,
while developing it, to get a clear error if a column is missing,
misnamed, or duplicated - before handing it to
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md).
See
[episodic_ingest_columns](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_ingest_interface.md)
for the required columns.

## Usage

``` r
episodic_ingest_validate_source(raw)
```

## Arguments

- raw:

  A data frame, as returned by your ingestion source function.

## Value

`raw`, invisibly, if it is valid. Throws an informative error otherwise
(a missing required column, an unexpected extra column, or duplicate
`source_key` values).

## Examples

``` r
raw <- episodic_ingest_source_synthetic(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
)
episodic_ingest_validate_source(raw)
```
