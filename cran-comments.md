# CRAN submission comments

## Package name

The package was originally developed under the name `EpiSODE`, which
`R CMD check --as-cran`'s incoming checks correctly flagged as a
case-insensitive conflict with `episode`, a CRAN package archived in
2019 (unrelated in scope). Per CRAN's Repository Policy ("Packages
should be named in a way that does not conflict (irrespective of case)
with any current or past CRAN package"), that conflict is not something
we asked CRAN to make an exception for - we renamed instead, to
`EpiSODIC` (Epidemiological Signal Observation, Detection,
Identification, and Classification), which has never existed as either a
current or archived CRAN package name (both `https://cran.r-project.org
/package=episodic` and the corresponding Archive URL 404). The new name
keeps the same backronym structure and pronunciation the package's
methodology is being written up under for a scientific publication, so
the identity the tool is meant to be cited under is preserved.

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
