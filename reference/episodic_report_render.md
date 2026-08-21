# Render an outbreak report for a cluster

A parameterised Quarto template rendered to self-contained HTML. Sent as
a file, not a link into the app: the medical staff receiving it have
neither R nor an account, and a static artefact is also the defensible
record of what was communicated on that date. Every render is versioned
and registered in `episodic_report_render`
(`episodic_db_report_render_insert()`) - never overwritten, so what was
handed to a microbiologist on a given morning stays exactly recoverable,
including which cases it contained (`case_ids`).

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
  lang = "nl",
  qmd_path = Sys.getenv("EPISODIC_QUARTO_REPORT", unset = NA)
)
```

## Arguments

- con:

  A
  [DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

- cluster_id:

  A cluster id.

- output_dir:

  Directory the rendered HTML is written to. Created if it does not
  exist.

- user_id:

  The rendering user's id, or `NA` for a cron pre-render (matches
  `episodic_report_render.user_id`'s own documented meaning).

- include_linelist:

  If `TRUE` (default), the line list is embedded in the report.

- small_count_threshold:

  Cells below this count are suppressed in the geography/institution
  breakdown tables, configurable for reports leaving the department.
  Defaults to `config$report$small_count_threshold`.

- config:

  The resolved configuration; only `config$report` is used.

- lang:

  Report language, `"nl"` (default) or `"en"`.

- qmd_path:

  Path to the Quarto template to render. Defaults to the
  `EPISODIC_QUARTO_REPORT` environment variable; if that is unset (or
  names a file that does not exist), falls back to the shipped
  `inst/report/cluster_report.qmd`. An operator's own template only
  needs to read `params$data_path` (an `.rds` path,
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html)'d to the same list
  this function assembles: `obj`, `epi_curve`, `trend`, `linelist`,
  `timeline`, `similar`, `small_count_threshold`, `rendered_at`, `lang`,
  `package_version`) - see the shipped template for the exact shape and
  for `episodic_tr(..., lang = d$lang)` usage.

## Value

Invisibly, a list with `file_path`, `file_sha256`, `version_no`,
`report_id`.

## Details

Data for the template is assembled here (from the same read models the
app itself uses: `episodic_cluster_object()`,
`episodic_app_epi_curve()`, `episodic_app_linelist()`) and handed to
Quarto as a single RDS side channel rather than as `execute_params`
values directly - a line list data frame has no clean YAML/JSON
representation, and passing one path string keeps the template itself
simple (`readRDS(params$data_path)`).

Line-list inclusion is decided here, at render time, via
`include_linelist` - independent of whoever later opens the file, unlike
the live app's own line-list panel, which is hidden entirely for
anonymous viewers and gated on the *viewer's* current session. A
rendered report has no viewer session at all.

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
