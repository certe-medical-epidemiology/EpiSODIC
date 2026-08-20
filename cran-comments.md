# CRAN submission comments

## Package name

`R CMD check --as-cran` flags this as a new submission with a name conflict:
CRAN's incoming checks compare package names case-insensitively, and
`episode` (note the different capitalisation) was a CRAN package from
2017 to 2019, archived on 2019-12-01 for unaddressed installation warnings.
It has had no CRAN presence for several years and is, as far as we can
tell, unrelated in scope to this package (an outbreak cluster detection
and assessment system for medical epidemiology). We believe `EpiSODE` is
sufficiently distinct in capitalisation, and its long absence from CRAN
sufficiently reduces the risk of confusion, that the name should not be
mistaken for a revival of the older package.

The name also is not an arbitrary label we could swap without cost:
`EpiSODE` is a backronym (Epidemiological Signal Observation and
Detection Engine) that the package's methodology is being written up
under for a scientific publication, so it is tied to how this tool is
intended to be cited and referred to in that literature. We would ask
CRAN to weigh that before requiring a rename.

## Test environments

- Local: Ubuntu 24.04, R 4.3.3 (sandboxed development container; no
  Pandoc, no Quarto CLI, no `sf`/GDAL/GEOS/PROJ system libraries - every
  code path touching these is Suggests-gated and degrades gracefully, so
  this environment only exercises the fallback paths).
- GitHub Actions (`.github/workflows/R-CMD-check.yaml`, the standard
  `r-lib/actions` check-standard matrix): macOS release, Windows release,
  Ubuntu devel/release/oldrel-1. These runners have Pandoc and working
  `sf` system libraries, so they exercise the full code path, including
  vignette building and the `sf`-gated geography tests.

## R CMD check results

0 errors | 0 warnings | 0 notes on the GitHub Actions matrix.

Locally, `R CMD check --as-cran` additionally reports (all environment
artefacts of the sandboxed development container, not real issues):

- `checking package dependencies ... NOTE` - `AMR`, `RMariaDB`, `sf` are
  not installed in this container (all Suggests, all optional, gated
  behind `requireNamespace()` with a documented fallback).
- `checking for future file timestamps ... NOTE` - the container's clock
  cannot be verified against a time server.
- `checking top-level files ... NOTE` - `README.md`/`NEWS.md` cannot be
  checked without Pandoc, which is not installed in this container.
- Vignette-building warnings - Pandoc is not installed in this container;
  vignettes build cleanly on the GitHub Actions matrix, which has Pandoc.

## Additional notes

- This is a new submission.
- Every dependency, required or optional, is a CRAN package - no
  organisation-internal or GitHub-only package is depended on anywhere.
- The package ships a synthetic-data demo (`episode_demo()`) so its
  functionality can be evaluated without access to any real laboratory or
  hospital data.
