# Resolve the EpiSODIC configuration

Loads the shipped defaults from `inst/config/default.yaml`, then, if the
`EPISODIC_CONFIG` environment variable points at a readable file, loads
it and overlays it on top: any key it sets replaces the corresponding
key in the defaults. Detection settings are never read from anywhere
else and never from inside this package's own tree at runtime beyond the
shipped defaults.

## Usage

``` r
episodic_config_resolve(
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- episodic_config_path:

  Path to the instance configuration file. Defaults to the
  `EPISODIC_CONFIG` environment variable. If unset or the file does not
  exist, only the shipped defaults are used, which is the supported way
  to run the bundled demo.

## Value

A nested list, the resolved configuration.

## Details

The result is what gets hashed into `config_hash` and stored verbatim as
`config_snapshot` on every detection run, so a run's exact parameters
are always recoverable from the database alone.

## Examples

``` r
config <- episodic_config_resolve()
names(config)
#> [1] "reconciliation"    "eligibility"       "effect_size_floor"
#> [4] "same_place"        "farrington"        "rare_trigger"     
#> [7] "priority_score"    "suppression"      
config$eligibility$min_baseline_weeks
#> [1] 52
```
