# README screenshots

Referenced from the top-level `README.md`. Tracked in git so they render
on GitHub, but this whole directory (`data-raw/`) is `.Rbuildignore`'d,
so none of it ships in the package tarball or reaches CRAN.

Expected files (paths the main README already links to):

- `dossier.png` - a cluster dossier, ideally one with enough history to
  show the epi curve, trend and interpretation panels populated (not a
  freshly-detected, still-empty one).
- `performance.png` - the Prestatie (Performance) screen, with real
  entries in the PPV table and classification distribution - run
  `episodic_demo()` against `episodic_ingest_source_synthetic_calibration()`
  (see `R/ingest_synthetic.R`) rather than the plain demo generator if a
  fresh instance's Performance screen looks too sparse, then classify a
  handful of the resulting clusters so the screen has verdicts to show.

Regenerate at whatever resolution looks good on GitHub's rendered
README (widths above are capped at 800px there); no fixed pixel
requirement beyond being legible at that width.
