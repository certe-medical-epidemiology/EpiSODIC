# List the Installed Plugins

Lists every plugin in the plugin folder and every plugin the
configuration enables, with its status and, when it cannot run, why. See
[`vignette("plugins")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/plugins.md).

## Usage

``` r
episodic_plugin_list(
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- episodic_config_path:

  The configuration file. Defaults to the `EPISODIC_CONFIG` environment
  variable.

## Value

A data frame with one row per plugin: `id`, `name`, `version`,
`trigger`, `placement`, `status` (`"enabled"`, `"disabled"`,
`"needs_approval"`, `"unavailable"` or `"missing"`), `permissions`,
`secrets`, `destination` and `problems` (the reasons, as text in the
`EPISODIC_LANGUAGE` language, separated by `"; "`).

## Details

The plugin folder is `EPISODIC_PLUGIN_DIR`, or a `plugins` folder beside
the configuration file (`EPISODIC_CONFIG`) when that is unset.

## See also

[`episodic_plugin_install()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_install.md),
[`episodic_plugin_enable()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_enable.md),
[`episodic_plugin_run()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_run.md),
[`vignette("plugins")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/plugins.md)

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_plugin_list()
} # }
```
