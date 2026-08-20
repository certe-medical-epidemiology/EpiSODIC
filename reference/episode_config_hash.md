# Compute the SHA-1 hash and canonical JSON snapshot of a resolved config

The hash is taken over a canonicalised representation (keys sorted
recursively, then serialised as JSON) so that key ordering in the source
YAML never changes the hash. SHA-1 was chosen to match the `CHAR(40)`
width used for other `_key`/`_hash` columns in the schema.

## Usage

``` r
episode_config_hash(config)
```

## Arguments

- config:

  A resolved configuration, as returned by
  [`episode_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episode_config_resolve.md).

## Value

A list with elements `hash` (a 40-character SHA-1 hex digest) and
`snapshot` (the canonical JSON string).

## Examples

``` r
hashed <- episode_config_hash(episode_config_resolve())
hashed$hash
#> [1] "24d58726acf8c255f13a008025e4080afc3c23b3"
```
