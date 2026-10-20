# Enable or Disable a Plugin

Enabling grants a plugin exactly what its file declares: its permissions
(which parts of the data it receives) and its secrets (which environment
variables it may read), and writes that grant into the configuration
file under `plugins.enabled`. A plugin that later declares more than it
was granted does not run until enabled again. Disabling removes it from
`plugins.enabled`; the file stays.

## Usage

``` r
episodic_plugin_enable(
  id,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)

episodic_plugin_disable(
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

Invisibly, what was granted (`episodic_plugin_enable()`), or whether the
plugin had been enabled (`episodic_plugin_disable()`).

## Details

The configuration file is the only record of which plugins are enabled,
and these functions write it the way the Plugins screen does, leaving
everything else in it as it is.

## See also

[`episodic_plugin_list()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_list.md),
[`episodic_plugin_install()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_install.md)

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_plugin_enable("share-regional")
episodic_plugin_disable("share-regional")
} # }
```
