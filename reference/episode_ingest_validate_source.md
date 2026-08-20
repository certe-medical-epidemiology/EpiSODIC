# Validate that a raw ingestion source data frame satisfies the interface

Validate that a raw ingestion source data frame satisfies the interface

## Usage

``` r
episode_ingest_validate_source(raw)
```

## Arguments

- raw:

  A data frame as returned by an ingestion source function.

## Value

`raw`, invisibly, if valid. Errors otherwise.

## Examples

``` r
raw <- episode_ingest_source_synthetic(
  start_date = as.Date("2025-01-01"), end_date = as.Date("2025-01-31")
)
episode_ingest_validate_source(raw)
```
