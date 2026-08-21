# EpiSODIC 0.3.1

## New: the Pathogen screen

- Added a Pathogen screen to the dashboard, alongside Clusters. Pick an
  pathogen and a period - a surveillance season, the last twelve months,
  the last five years, or an exact date range - and read that pathogen at
  the epidemiological level rather than the operational one: weekly
  incidence for the whole catchment, this period laid over earlier ones
  on a shared week-of-season axis, region-wide Rt, testing volume and
  positivity, age and sex, geography, and the signals that were raised
  while the period ran.
- The Moving Epidemic Method's thresholds are now visible. They have been
  fitted on every run since `mem` was added, and used only to decide
  whether a detector fired; the Pathogen screen draws them against the
  season's own curve, together with the medium/high/very high intensity
  bands, so "has the epidemic started" and "how hard is this season" can
  be read directly. Thresholds for a season are fitted only on the
  seasons before it, never on the season being judged.
- Rt is now also estimated for a pathogen as a whole, not only per
  cluster. Rt is a property of a transmission process, and for something
  like influenza A that process is the region's influenza A rather than
  the cases that happened to reconcile into one ward cluster; estimating
  it on a cluster feeds `EpiEstim` an incidence series that is
  systematically incomplete. The per-cluster panel remains - "is this
  ward outbreak still growing" is a real question - but it is no longer
  the only one on offer.
- The `mem` detector is now documented on the Info screen, alongside the
  three that were already there.

## Corrections to the epidemiological logic

- **Observed versus expected is now recorded.** `episodic_cluster` has
  always carried `expected`, `excess` and `ratio` columns, and both the
  dossier's stat grid and the interpretation engine's magnitude fragments
  read them - but nothing ever wrote them. Every cluster was persisted
  with all three left at `NA`, so the O/E ratio never appeared on a
  dossier or in the rail, and the "well above expectation" fragments
  could not fire. Reconciliation now carries the detections' own
  `expected`/`upperbound` through to the cluster.
- **The priority score now uses its evidence.** Five of its seven
  components were being left at their defaults, most damagingly
  `ratio = n_cases / max(n_cases, 1)`, which is identically 1 for every
  candidate; the ranking that orders the entire assessment queue was
  effectively severity weight times detector agreement. Excess, ratio,
  growth, incidence density and spatial concentration are now all
  computed and passed.
- **A component that cannot be computed no longer drags the score down.**
  Weight renormalisation applied only to the density component; a
  `same_place` or `rare_trigger` cluster, which by construction has no
  baseline and therefore no excess or ratio, was scored zero on both
  while keeping their weight. Ward-level same-place clusters - the ones
  an infection prevention nurse has to act on the same day - were
  systematically outranked by Farrington signals for a purely structural
  reason. The ratio and density components are also now anchored so that
  "exactly as expected" contributes nothing rather than half a score.
- **Doubling time no longer reports growth that is not there.** It was a
  regression of log(*cumulative*) cases on day, which has a positive
  slope for any series at all - a constant incidence still makes the
  cumulative count climb - so every cluster with three cases reported a
  finite doubling time. It is now a Poisson fit to the daily counts, and
  `NA` unless growth is real and fast enough to be measurable within the
  fitted window.
- **Positivity is a rate again.** The dossier's positivity line was the
  *cluster's* case count over the *region's* test count: numerator and
  denominator from different populations, so it tracked cluster size
  rather than test yield and could not answer the one question the panel
  exists for. Both are now counted region-wide over the same week, and
  the series is windowed to the cluster instead of spanning every week a
  denominator was ever supplied - which had left the interpretation
  engine comparing a week years before a cluster began against one long
  after it ended.
- **Reporting lag is measured from the run date, not from the cluster.**
  The incompleteness window was anchored on a cluster's own last case
  day, so a cluster that stopped generating cases weeks ago still had its
  epi-curve tail faded and its final Rt estimates withheld, permanently.
  The number of under-ascertained days was also one short, and a single
  noisy dip in a completion curve could stretch the window to a
  fortnight.
- **Rt no longer starts estimating with no infection history.**
  `EpiEstim` conditions each estimate on the infections before it, and at
  the start of a series there are none recorded - not because none
  occurred, but because the series starts there. The renewal denominator
  is too small and Rt reads high, on the youngest and least-evidenced
  clusters. Windows falling inside one mean serial interval of the series
  start are now dropped.
- **Seasonal clusters can close in summer.** `episodic_mem_status()`
  returned `NULL` off-season, and the closure criterion could not tell
  that apart from "MEM unavailable", so a `mem_applicable` cluster had no
  closure route at all between May and September: every confirmed
  influenza or RSV epidemic stayed open on the rail right through the
  summer. Out of season the calendar answers the question, subject to the
  ordinary case-free interval.
- **MEM evaluates the last complete week**, not the partial one in
  progress, and fits its thresholds only on seasons the case data spans
  end to end - a site whose data begins in January no longer contributes
  a season that is three-quarters structural zeros, which used to drag
  the pre-epidemic threshold down and fire the next winter's epidemic
  start early.
- **Concentration is a share of the cases with a known postcode.**
  Dividing by every case in the cluster diluted the measure by however
  many had no postcode recorded, pushing genuinely localised clusters
  down the queue; a missing postcode is absence of evidence about
  localisation, not evidence of dispersal.
- **The demographic baseline excludes the cluster being compared to it**,
  and every other cluster in its stream. For a rare pathogen whose
  recorded history is largely one cluster, that cluster dominated its own
  baseline and could never be found to have shifted away from it -
  exactly where a demographic shift is most worth surfacing.

## Geography

- The choropleth is cropped to the areas that actually have cases, plus a
  margin of context around them, and each of those areas is labelled with
  its postcode and case count. It used to be framed on the entire
  reference dataset - some four thousand PC4 polygons for the shipped
  Netherlands default - so a five-postcode cluster rendered as a handful
  of tinted specks inside a whole country, at which scale the question
  the panel exists to answer cannot be answered at all.
- The geography panel now takes the full width of the dossier pane rather
  than sharing a row with the demography pyramid, and shows the per-area
  bar breakdown underneath the map rather than instead of it.

## Terminology

- One word per language for the thing being watched, everywhere. English
  now says **pathogen** throughout, never "organism": influenza is not an
  organism, and neither is any of the other viruses this system spends
  most of its winter watching. Six shipped strings said "organism", and
  their translations followed suit - Spanish *organismo*, French
  *organisme*, Hindi *जीव*, Arabic *كائن* - so all of those moved with it,
  to *patógeno*, *pathogène*, *रोगजनक* and *العامل الممرض*. Dutch is
  **verwekker** and German **Erreger**, without exception. `AMR::
  microorganisms` keeps its name: it is the AMR package's own data
  object, and a hard dependency of this one.
- Asserted rather than left to review: `tests/testthat/test-i18n.R` now
  fails if any shipped language calls the concept an organism again, if
  Dutch renders it "pathogeen" in any casing, or if a language's
  navigation entry and screen title stop agreeing with each other.

## Interface

- The Pathogen screen's cluster table links through. Selecting a row -
  by click or by keyboard, since a table row carries no focus of its own
  - opens that cluster's dossier on the Clusters screen. The table is
  also named for what it holds: **Clusters in this period**, not
  "signals". A signal is a detection; these are the reconciled clusters
  those detections became, each carrying a verdict and a state.

  The rail's auto-selection had to be corrected for this to be honest.
  It reset the selection whenever the selected cluster was not currently
  *open*, and most of the clusters this table lists are closed - so every
  such link would have quietly landed on the top of the rail instead. It
  now only re-selects when nothing is selected or when the selected
  cluster has been merged into another, which is the case where the
  dossier really is stale. A cluster that closes while you are reading it
  no longer disappears out from under you either.
- Cluster ids are readable now. The id sits beside the pathogen name in
  the dossier title - *Salmonella* #300, muted and upright against the
  italicised taxon - rather than a line further down among the metadata,
  and it leads every row of the Pathogen screen's signals table, which
  previously listed clusters without ever naming one. It is what an
  assessor quotes in an email and searches the archive by, so it belongs
  where the eye lands. Each language marks the reference its own way
  (`n.º`, `n°`, `Nr.`, `رقم`), which is why it stays a translation key.
- Weekly charts label weeks. `ggplot2`'s default date scale put two ticks
  - "Oct" and "Jan" - across a whole quarter of weekly bars, which is not
  enough to name the week a rise started in. The epidemic curve, the
  multi-year trend and the positivity chart now carry the ISO week number
  over the month it falls in, with the year stated once and again only
  when it turns, thinning to month-and-year once the span passes about
  eighteen months. Week and month are both read off the week's Thursday,
  the day that decides which ISO year a week belongs to, so the week
  beginning 30 December 2024 reads "w01 / Jan 2025" rather than
  contradicting itself.
- The number beside each pathogen in the picker now says what it counts.
  "Norovirus (2197)" reads as an identifier; it is an all-time case
  count, and it is the number that decides whether a pathogen has enough
  history for the screen to say anything at all.
- The top navigation now shows which screen you are on. The stylesheet
  has always had a rule for it; nothing ever applied the class.

# EpiSODIC 0.3.0

- Expanded dashboard translations from Dutch and English to also cover
  Spanish, French, German, Mandarin Chinese, Hindi, and (Modern Standard)
  Arabic (`inst/i18n/{es,fr,de,zh,hi,ar}.json`), covering the top spoken
  languages in the world alongside Dutch and English. Pass any of these
  as `lang` to [episodic_run_app()], [episodic_demo()], or
  [episodic_report_render()]. `episodic_format_date_range()` now also
  formats month names for these languages, sourced from the same
  `inst/i18n/*.json` files as every other piece of dashboard text.
- Added the `EPISODIC_LANGUAGE` environment variable, read by
  [episodic_run_app()], [episodic_demo()], [episodic_report_render()],
  and [episodic_tr()] as the default `lang` when it is not passed
  explicitly. Falls back to `"en"` if unset.

# EpiSODIC 0.2.0

- Rewrote all documentation for an epidemiologist audience: every man page
  is now written for someone running or configuring EpiSODIC, not for a
  contributor reading the source. Several related functions were combined
  onto a single help page (dashboard chart builders, HTML formatting
  helpers, geographic reference data), and examples now show their output
  instead of assigning it silently.
- `EPISODIC_DB` also accepts a `mysql://` DSN (`episodic_db_dsn_mariadb()`
  / `episodic_db_dsn_mysql()`) to run against MariaDB/MySQL instead of
  SQLite, from the same schema file.
- Renamed the internal `episode_`/`episode-` prefix to `episodic_`/
  `episodic-` throughout (functions, database tables, CSS classes) to
  match the package name.
- Package version shown in the Shiny app header.
- README screenshots now ship in `man/figures/` so they render on CRAN
  too; removed placeholder pkgdown reference pages and trimmed the
  internal dev notes (`data-raw/`).

# EpiSODIC 0.1.0

First development version.

- Detection: automatic lattice enumeration, five detectors
  (`farrington`, `ears`, `same_place`, `rare_trigger`, `mem`), cluster
  reconciliation across runs, cross-lattice suppression, baseline
  feedback.
- Interface: bilingual (NL/EN) Shiny app, read-only for anonymous
  visitors; dossier view (epi curve, trend, geography, Rt, line list);
  audit-trailed classification workflow; Performance screen.
- Reporting: versioned Quarto outbreak reports with small-count
  suppression.
- SQLite backend; optional `EpiEstim`/`mem`/`quarto`/`sf` integrations,
  each with a documented fallback when absent.
- `episodic_demo()` runs the whole system against bundled synthetic
  data - no credentials or configuration required.
