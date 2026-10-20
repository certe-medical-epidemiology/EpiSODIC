# Run a Plugin

Runs an enabled plugin from a script, exactly as its button or hook
would, and records the run. A plugin triggered by a button needs the
cluster it acts on; one triggered after a detection run is given the
most recent committed run, or `run_id`.

## Usage

``` r
episodic_plugin_run(
  id,
  cluster_id = NULL,
  run_id = NULL,
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  episodic_config_path = Sys.getenv("EPISODIC_CONFIG", unset = NA),
  lang = Sys.getenv("EPISODIC_LANGUAGE")
)
```

## Arguments

- id:

  The plugin's id.

- cluster_id:

  For a button plugin: the cluster, by its number (the `12` of `O-12`).

- run_id:

  For a run plugin: the run; `NULL` for the most recent run that
  committed.

- db_path:

  The database. Defaults to the `EPISODIC_DB` environment variable.

- episodic_config_path:

  The configuration file. Defaults to the `EPISODIC_CONFIG` environment
  variable.

- lang:

  The language the plugin should write its message in.

## Value

Invisibly, a list with `status` (`"success"`, `"error"`, `"failed"` or
`"timeout"`), `message`, `link`, `step` and `detail`. A status other
than `"success"` is also raised as a warning.

## See also

[`episodic_plugin_list()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_list.md),
[`vignette("plugins")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/plugins.md)

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_plugin_run("share-regional", cluster_id = 12)
} # }
```
