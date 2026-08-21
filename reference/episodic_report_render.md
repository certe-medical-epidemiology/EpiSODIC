# Render an outbreak report for clinical colleagues

Produces a self-contained HTML outbreak report for one cluster - the
document you send to a treating physician, an infection prevention team,
or another clinical colleague who has neither an EpiSODIC account nor R
installed. The report includes the epidemic curve, trend chart, the
narrative summary shown in the dashboard, and (optionally) the case line
list.

## Usage

``` r
episodic_report_render(
  con,
  cluster_id,
  output_dir,
  user_id = NA,
  include_linelist = TRUE,
  small_count_threshold = NULL,
  config = episodic_config_resolve(),
  lang = Sys.getenv("EPISODIC_LANGUAGE"),
  qmd_path = Sys.getenv("EPISODIC_QUARTO_REPORT", unset = NA)
)
```

## Arguments

- con:

  A
  [DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

- cluster_id:

  The cluster to report on.

- output_dir:

  Directory the rendered HTML is written to. Created if it does not
  exist.

- user_id:

  The id of the user requesting the report, or `NA` for an automated
  (cron) render.

- include_linelist:

  If `TRUE` (default), the case line list is included in the report. Set
  to `FALSE` for a summary-only report, e.g. when sending outside your
  own organisation.

- small_count_threshold:

  Small counts in the geography/institution breakdown tables are
  suppressed (shown as `"<threshold"`) below this value, to avoid
  identifying individuals in a small population. Defaults to
  `config$report$small_count_threshold`.

- config:

  The resolved configuration (see
  [`episodic_config_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_config_resolve.md));
  only `config$report` is used.

- lang:

  Report language: `"nl"`, `"en"`, `"es"`, `"fr"`, `"de"`, `"zh"`,
  `"hi"`, or `"ar"`. Defaults to the `EPISODIC_LANGUAGE` environment
  variable, falling back to `"en"` if that is unset.

- qmd_path:

  Path to the Quarto template to render. Defaults to the
  `EPISODIC_QUARTO_REPORT` environment variable, falling back to the
  shipped template if that is unset.

## Value

Invisibly, a list with `file_path` (where the HTML was written),
`file_sha256`, `version_no`, and `report_id`.

## Details

Every render is kept, versioned, and logged to the database, including
exactly which cases it contained - so what was sent out on a given date
stays a fully recoverable record, even if the underlying data changes
later.

By default the report uses EpiSODIC's own report template. If your
organisation needs its own layout or branding, set the
`EPISODIC_QUARTO_REPORT` environment variable to your own `.qmd` file;
`inst/report/cluster_report.qmd` in the package source is a good
starting point to copy and adapt.

Rendering requires [Quarto](https://quarto.org) to be installed
separately (both the `quarto` R package and the Quarto command-line
tool) - this function raises an informative error if it is not found.

## Examples

``` r
if (FALSE) { # \dontrun{
# needs both the quarto R package and the Quarto CLI installed, plus a
# database with at least one detected cluster - see episodic_demo() for
# a populated one, and the app's rail for a cluster_id to render
db_path <- episodic_demo(launch = FALSE)
con <- episodic_db_connect(db_path)
episodic_report_render(con, cluster_id = 1, output_dir = tempdir())
DBI::dbDisconnect(con)
} # }
```
