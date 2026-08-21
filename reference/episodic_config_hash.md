# Fingerprint a configuration for reproducibility

Every detection run is stamped with a hash of the exact configuration
that produced it, so that two runs can be compared to see whether they
actually used the same settings, and any run's full configuration can be
recovered later even if the live configuration file has since changed.
The hash does not depend on the order of keys in your YAML file: it is
computed over a canonical (sorted, JSON) representation, so equivalent
configurations always produce the same hash.

## Usage

``` r
episodic_config_hash(config)
```

## Arguments

- config:

  A resolved configuration, as returned by
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md).

## Value

A list with `hash` (a 40-character hex digest) and `snapshot` (the
canonical configuration, as a JSON string).

## Details

You will not normally call this directly - EpiSODIC's detection pipeline
calls it automatically - but it is useful for confirming that two
configuration files are equivalent, or for recovering a full historic
configuration from a stored hash and snapshot.

## Examples

``` r
hashed <- episodic_config_hash(episodic_config_resolve())
hashed$hash
#> [1] "24d58726acf8c255f13a008025e4080afc3c23b3"
```
