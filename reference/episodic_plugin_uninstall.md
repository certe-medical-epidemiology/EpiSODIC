# Uninstall a Plugin

Disables the plugin, if it was enabled, and removes its file from the
plugin folder. The record of its past runs stays.

## Usage

``` r
episodic_plugin_uninstall(
  id,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- id:

  The plugin's id, as
  [`episodic_plugin_list()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_list.md)
  shows it.

- episodic_config_path:

  The configuration file. Defaults to the `EPISODIC_CONFIG` environment
  variable.

## Value

Invisibly, `TRUE`.

## See also

[`episodic_plugin_install()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_install.md)

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_plugin_uninstall("share-regional")
} # }
```
