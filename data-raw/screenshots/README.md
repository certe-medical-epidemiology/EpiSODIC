# README screenshots

Notes for regenerating the screenshots referenced from the top-level
`README.md`. The images themselves live in `man/figures/`, not in this
directory - `man/figures/` ships with the package (so they also render
on the CRAN page), whereas this whole directory (`data-raw/`) is
`.Rbuildignore`'d and never reaches CRAN.

Expected files (paths the main README already links to):

- `man/figures/main_screen.png` - a cluster dossier, ideally one with
  enough history to show the epi curve and interpretation panel
  populated (not a freshly-detected, still-empty one).
- `man/figures/performance_screen.png` - the Performance screen. Run
  `episodic_demo()` against `episodic_ingest_source_synthetic_calibration()`
  (see `R/ingest_synthetic.R`) rather than the plain demo generator if a
  fresh instance's Performance screen looks too sparse, then classify a
  handful of the resulting clusters so the PPV table and classification
  distribution have verdicts to show instead of dashes.

Regenerate at whatever resolution looks good on GitHub's and CRAN's
rendered README (widths are capped at 800px there); no fixed pixel
requirement beyond being legible at that width.
