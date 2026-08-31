# Send a test notification through all configured channels

Validates the notification configuration and sends a test message
through every enabled channel. Run this interactively after setting up
your instance configuration to verify that notifications work end to
end.

## Usage

``` r
episodic_notify_test(
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- episodic_config_path:

  Passed to
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md).

## Value

Invisibly, a named logical vector: `TRUE` for each channel that
succeeded, `FALSE` for each that failed.

## Examples

``` r
if (FALSE) { # \dontrun{
# After configuring notifications in your EPISODIC_CONFIG YAML:
episodic_notify_test()
} # }
```
