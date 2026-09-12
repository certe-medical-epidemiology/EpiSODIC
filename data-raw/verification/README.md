# Verification harness

Two checks the package cannot make of itself where it is written, for a
machine that has R, the package's dependencies, and a browser.

Nothing here ships: `data-raw` is in `.Rbuildignore`, and nothing in this
directory may be reached from the test suite, CI, an example or a vignette.
A full pass takes minutes, which is why it belongs nowhere near the critical
path of a commit.

- **`run_checks.R`** needs R. The suite, the package check, the formatter.
- **`run_visual.R`** needs a real layout engine. Whether a control is visible,
  what a press lands on, and whether a screen scrolls sideways are not
  questions a string comparison against a stylesheet can answer.

The structural half of the second is already in
`tests/testthat/test-app_navigation.R`: one place for each piece of state, no
class marking what is current, no inline handler, a stylesheet rule for every
screen and every pane. What a structural test cannot reach is whether the
result *renders*, and that is what `run_visual.R` measures.

## What to run

```bash
cd /path/to/EpiSODIC
Rscript data-raw/verification/run_checks.R      # suite, R CMD check, formatting
Rscript data-raw/verification/run_visual.R      # browser checks and screenshots
```

Both write to `data-raw/verification/output/` (git-ignored), print a one-line
summary per check, and exit non-zero if anything failed. That exit code is the
whole contract: a scheduler needs pass or fail without reading the log. A
weekly timer or cron entry is the right cadence, reporting on failure only.

## `run_checks.R`

- `devtools::test()`, failing on any failure, error or warning.
- `R CMD build` and `R CMD check --as-cran`, failing on any ERROR or WARNING.
  One NOTE is expected and allowed: the Author/Maintainer note that
  `Authors@R` always produces.
- `styler::style_pkg(dry = "on")`, failing if any file would change. Run it
  without `dry` to fix, then read the diff: styler cannot be configured to
  prefer this project's hanging-indent signatures, it detects the shape per
  function from the source, so a signature it reflows is one somebody wrote in
  the other shape.

## `run_visual.R`

Starts the app in a background R process against a freshly generated demo
database, drives it with chromote, and asserts. Per language and per viewport:

| Viewport | Width | What it is |
|---|---|---|
| `phone` | 360 x 780 | The tier with one pane at a time and the bottom segmented control |
| `phone-large` | 414 x 896 | Same tier, the widest phone |
| `tablet` | 820 x 1180 | The tier with the rail off-canvas |
| `laptop` | 1280 x 800 | The full three-pane row, just above the breakpoint |
| `desktop` | 1920 x 1080 | The full three-pane row with room to spare |

The invariants it holds, each a property of the design rather than a check on
one screen:

- **Every nav link is visible at every width.** Not merely present in the
  page: a non-zero bounding rectangle inside the viewport.
- **The page never scrolls sideways.** `documentElement.scrollWidth` no wider
  than the viewport, at every width, in Arabic as well as English.
- **Every navigation control clears 44px** on its short side at touch widths,
  measured from the rendered box rather than from the CSS.
- **Exactly one screen is visible at a time**, and a nav link changes which,
  with no server round trip needed for the change.
- **A pane survives a navigation.** The pane showing on a phone is still the
  one showing after leaving a screen and returning.
- **A screen returned to is not rebuilt.** Timed: the second visit is an order
  of magnitude faster than the first, because the first built it and the
  second only unhid it.
- **Arabic mirrors.** In `ar` the navigation starts at the right edge and the
  off-canvas rail comes in from the right.

Screenshots land in `output/screenshots/<lang>/<viewport>-<screen>.png`. Look
at them; they are the half no assertion covers. One file per combination,
overwritten each run, so `git status` is not where the results go and a diff
against a previous run means keeping a copy of the folder.

## Requirements

- R with the package's own dependencies, plus `devtools`, `styler`, `chromote`
  and `callr`.
- A Chrome or Chromium binary chromote can find. It reads `CHROMOTE_CHROME`
  first; set it if the binary is somewhere unusual.
- No display: Chrome runs headless.
- Roughly 2 GB free in `TMPDIR` for the demo database and the screenshots.

## Keeping it current

A screen absent from the view list in `run_visual.R` is a screen nothing
renders. Adding a view to `episodic_app_views()` means adding it there and to
`tests/testthat/test-app_navigation.R`.

When a check fails, the failure names the language, the viewport and the
assertion, and that combination's screenshot is already on disk. Read the
screenshot first: the assertion says a number was wrong, the picture says
which of the possible reasons it was.

If every check passes and something still looks wrong to a person, the person
is right. Add the assertion that would have caught it, here if it is about
rendering and in `test-app_navigation.R` if it is about structure.
