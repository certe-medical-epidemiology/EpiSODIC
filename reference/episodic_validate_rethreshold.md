# Re-Match a Validation Result at Different Thresholds

`min_recall` and `min_precision` are choices, and a result that only
holds at one particular pair of them is not a result. This applies a
different pair to a replay that has already been run, so a sweep costs
arithmetic rather than another few hours of detection.

## Usage

``` r
episodic_validate_rethreshold(
  result,
  min_recall = result$meta$min_recall,
  min_precision = result$meta$min_precision
)
```

## Arguments

- result:

  An `episodic_validation` object, from
  [`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)
  or
  [`episodic_validate_comparator()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_comparator.md).

- min_recall, min_precision:

  The thresholds to apply instead.

## Value

An `episodic_validation` object of the same shape, with
`meta$min_recall` and `meta$min_precision` updated and
`meta$rethresholded_from` recording what it was re-matched from.

## Details

It re-derives everything the thresholds touch and nothing else: which
outbreaks count as detected, when, how badly they fragmented, and which
clusters count as true positives. The clusters that were raised, the
runs that raised them and the cases they held are what they were - those
are measurements, and no threshold changes them.

## See also

[`episodic_validate_detection()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_validate_detection.md)

## Examples

``` r
# \donttest{
result <- episodic_validate_detection()
strict <- episodic_validate_rethreshold(result, min_recall = 0.8)
strict$summary[strict$summary$metric == "sensitivity", ]
#>         metric     group_type         group n_seeds    median       q25
#> 1  sensitivity        overall all outbreaks       1 0.8333333 0.8333333
#> 2  sensitivity outbreak_shape           LTC       1        NA        NA
#> 3  sensitivity outbreak_shape          PROP       1        NA        NA
#> 4  sensitivity outbreak_shape            PS       1        NA        NA
#> 5  sensitivity outbreak_shape          RARE       1        NA        NA
#> 6  sensitivity outbreak_shape          WARD       1        NA        NA
#> 7  sensitivity outbreak_shape          WAVE       1        NA        NA
#> 8  sensitivity design_channel    farrington       1        NA        NA
#> 9  sensitivity design_channel  rare_trigger       1        NA        NA
#> 10 sensitivity design_channel    same_place       1 1.0000000 1.0000000
#>          q75 numerator denominator  estimate    ci_low   ci_high interval
#> 1  0.8333333         5           6 0.8333333 0.4364972 0.9699466   wilson
#> 2         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 3         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 4         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 5         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 6         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 7         NA         0           1 0.0000000 0.0000000 0.7934507   wilson
#> 8         NA         0           1 0.0000000 0.0000000 0.7934507   wilson
#> 9         NA         1           1 1.0000000 0.2065493 1.0000000   wilson
#> 10 1.0000000         4           4 1.0000000 0.5101092 1.0000000   wilson
#>          unit
#> 1  proportion
#> 2  proportion
#> 3  proportion
#> 4  proportion
#> 5  proportion
#> 6  proportion
#> 7  proportion
#> 8  proportion
#> 9  proportion
#> 10 proportion
# }
```
