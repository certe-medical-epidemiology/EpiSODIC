# Install a Plugin

Copies a plugin file into the plugin folder: from an `https://` address
(a file on GitHub may be given by its page address and is fetched from
its raw address), or from a local file. The file is checked first; one
that is not a valid plugin is refused and nothing is written. A plugin
whose requirements this server does not meet yet is installed and listed
as unavailable, with the reasons, until they are met.

## Usage

``` r
episodic_plugin_install(
  source,
  overwrite = FALSE,
  token_env = NULL,
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA)
)
```

## Arguments

- source:

  An `https://` address or a local file path. The file name (without
  `.yaml`) is the plugin's id.

- overwrite:

  Whether to replace an installed plugin of the same id.

- token_env:

  For a private source: the name of the environment variable holding a
  bearer token. The token itself is never accepted as an argument.

- episodic_config_path:

  The configuration file. Defaults to the `EPISODIC_CONFIG` environment
  variable.

## Value

Invisibly, the plugin's id.

## Details

Installing never enables: see
[`episodic_plugin_enable()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_enable.md).
Installing a newer version over an enabled plugin keeps it enabled,
unless the new version asks for permissions or secrets it was not
granted, in which case it does not run until enabled again.

A plugin runs code on this server with the account EpiSODIC runs as.
Install only plugins from a source you trust, and read a plugin's steps
before enabling it. See
[`vignette("plugins")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/plugins.md).

## See also

[`episodic_plugin_list()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_list.md),
[`episodic_plugin_enable()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_enable.md),
[`episodic_plugin_uninstall()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_uninstall.md)

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_plugin_install("https://github.com/org/plugins/blob/v1.0.0/share-regional.yaml")
} # }
```
