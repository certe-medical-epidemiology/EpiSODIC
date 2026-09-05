# Add One or More Manual (External) Clusters

Adds clusters that were never detected by EpiSODIC's own pipeline -
output from another algorithm or system, fed in as already-parsed R
values (an operator's own code is responsible for reading whatever
JSON/CSV that other system produces). A manual cluster is never
connected to this instance's own case data: it gets a real
`episodic_stream` identity (so it appears in the dashboard's normal
filtering, sorting and geography panels, exactly like a detected
cluster), but its case-level detail, if you supply any, is stored
separately in `episodic_cluster_manual_case` and never touches
`episodic_case`. It is therefore also never picked up by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)'s
reconciliation or automatic closure - a manual cluster only closes when
an epidemiologist records that verdict.

## Usage

``` r
episodic_add_manual_cluster(
  db_path = Sys.getenv("EPISODIC_DB", unset = NA),
  user_id,
  pathogen,
  level,
  first_day,
  last_day,
  n_cases = NA,
  care_line = NA,
  region_code = NA,
  institution_id = NA,
  ward = NA,
  expected = NA,
  excess = NA,
  ratio = NA,
  detector_agreement = 1L,
  priority_score = NA,
  case_dates = NULL,
  pc = NULL,
  sex = NULL,
  age = NULL,
  note = NA
)
```

## Arguments

- db_path:

  Path to an existing SQLite database, or a `mysql://` DSN (see
  [`episodic_db_dsn_mariadb()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_db_dsn_mariadb.md)).
  Defaults to the `EPISODIC_DB` environment variable.

- user_id:

  The `episodic_app_user` id this batch is attributed to

  - required, since provenance ("who added this, and when") is exactly
    what a hand-added cluster most needs. Recorded as the author of the
    seed note when `note` is supplied.

- pathogen:

  Raw pathogen string, exactly as it would appear in
  `episodic_case$pathogen` - used verbatim, not matched against any
  fixed list.

- level:

  One of the five lattice levels: `"pathogen_ward"`,
  `"pathogen_institution"`, `"pathogen_area"`, `"pathogen_province"`,
  `"pathogen_region"` (see `vignette("architecture")`).

- first_day, last_day:

  The cluster's episode window (`Date` or `"YYYY-MM-DD"` text).

- n_cases:

  The case count. Optional if `case_dates` (or any other per-cluster
  case-level detail) is supplied for that cluster, in which case it
  defaults to that detail's own length; required otherwise, since there
  is then nothing else to derive it from.

- care_line, region_code, institution_id, ward:

  The remaining `episodic_stream` identity fields, exactly as
  `episodic_stream_key()` expects them. `institution_id` must already
  exist in `episodic_institution` (e.g. resolved earlier via
  `episodic_institutions_resolve()`) - this function does not create
  institutions.

- expected, excess, ratio:

  Optional pass-through metrics, for external algorithms that do produce
  a baseline comparison. `NA` (the default) when the source has no such
  concept - exactly as for a detected cluster whose detector cannot
  produce one.

- detector_agreement:

  How many independent sources agree on this cluster. Defaults to `1L`
  (a single external source).

- priority_score:

  Optional override. When `NA` (the default), it is computed with the
  same `episodic_priority_score()` used for detected clusters, so manual
  and detected clusters sort comparably - from
  `expected`/`excess`/`ratio`, `detector_agreement`, and, when
  per-cluster case-level detail is supplied, growth slope and spatial
  concentration computed from it exactly as the cron pipeline would.

- case_dates, pc, sex, age:

  Optional per-cluster case-level detail, each a
  [`list()`](https://rdrr.io/r/base/list.html) of length `n` (one vector
  per cluster, all of equal length within a cluster) - `case_dates` a
  vector of sample dates, `pc` postcodes, `sex` `"M"`/`"F"`/`"U"`/`NA`,
  `age` in years. Feeds the dossier's epi curve, demography and
  geography panels for this cluster. Leave `NULL` (the default) for a
  source that only supplies an aggregate count - the cluster is still
  created, its panels just have nothing to draw.

- note:

  Optional markdown text to seed the cluster's note with (see the
  dossier's notes panel), attributed to `user_id`.

## Value

An integer vector of the newly created `cluster_id` values, one per
cluster, in the same order as the arguments.

## Details

Every argument is vectorised: pass a single value to add one cluster, or
vectors/lists of length `n` to add `n` clusters in one call, each
getting its own new cluster id. Arguments of length 1 are recycled
against the longest argument you supply; per-cluster case-level detail
(`case_dates`/`pc`/`sex`/`age`) is never recycled - if you supply it, it
must be a [`list()`](https://rdrr.io/r/base/list.html) of exactly the
target length, one element (a vector) per cluster.

The whole call is one transaction: either every requested cluster is
created, or (on any validation or database error) none are.

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_add_manual_cluster(
  user_id = 1L,
  pathogen = "Measles virus",
  level = "pathogen_area",
  first_day = "2025-01-10",
  last_day = "2025-01-20",
  region_code = "GR",
  case_dates = list(as.Date(c("2025-01-10", "2025-01-14", "2025-01-20"))),
  pc = list(c("9711AA", "9711AB", "9712CD")),
  note = "Reported by municipal health service, outside our own lab data."
)
} # }
```
