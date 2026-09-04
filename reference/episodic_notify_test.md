# Send a Test Notification Through All Configured Channels

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

  The config path.

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
