# Read the surveillance configuration

EpiSODIC's detection behaviour - which detectors run, their thresholds,
how a dossier's priority score is weighted, and so on - is controlled by
a YAML configuration file, not by function arguments. This function
reads that configuration: it starts from the package's built-in defaults
and, if you have set the `EPISODIC_CONFIG` environment variable to point
at your own YAML file, overlays your settings on top. You only need to
set the keys you want to change; anything you leave out keeps its
default.

## Usage

``` r
episodic_config_resolve(
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- episodic_config_path:

  Path to your own configuration file. Defaults to the `EPISODIC_CONFIG`
  environment variable; if that is unset or the file does not exist,
  only the built-in defaults are used.

## Value

A nested list with the resolved configuration, e.g.
`config$eligibility$min_baseline_weeks` or
`config$priority_score$weights`.

## Details

Running the bundled demo needs no configuration file at all - the
shipped defaults are enough on their own.

Every detection run stores the exact configuration it used (see
[`episodic_config_hash()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_hash.md)),
so you can always trace a past result back to the settings that produced
it, even after you have since changed them.

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
