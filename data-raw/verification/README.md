# Verification harness

Instructions for a machine that has R, every dependency at its current
version, and **chromote**. Nothing in this directory ships: `data-raw` is
in `.Rbuildignore`, and nothing here may be reached from the test suite,
CI, an example or a vignette. It exists because two kinds of check cannot
be made where the package is written:

1. **The suite, the check and the formatter**, which need R.
2. **The browser**, which needs a real layout engine. Assertions about
   what is visible, what a press actually lands on, and whether a screen
   scrolls sideways are not assertions a string comparison against a
   stylesheet can make.

The second is the point of this harness. `tests/testthat/test-app_navigation.R`
already holds the structural half: one place for each piece of state, no
class marking what is current, no inline handler, a stylesheet rule for
every screen and every pane. What it cannot hold is whether the result
*renders* correctly, and that is what `run_visual.R` measures.

## What to run

```bash
cd /path/to/EpiSODIC
Rscript data-raw/verification/run_checks.R      # suite, R CMD check, formatting
Rscript data-raw/verification/run_visual.R      # browser checks and screenshots
```

Both write to `data-raw/verification/output/` (git-ignored), print a
one-line summary per check, and exit non-zero if anything failed. That
exit code is the whole contract: a scheduler needs to know pass or fail
without reading the log.

### `run_checks.R`

- `devtools::test()`, failing on any failure, error or warning.
- `R CMD build` and `R CMD check --as-cran`, failing on any ERROR or
  WARNING. One NOTE is expected and allowed: the Author/Maintainer note
  that `Authors@R` always produces.
- `styler::style_pkg(dry = "on")`, failing if any file would change. Run
  it without `dry` to fix, then re-read the diff: styler cannot be
  configured to prefer this project's hanging-indent signatures, it
  detects the shape per function from the source, so a signature it
  reflows is a signature somebody wrote in the other shape.

### `run_visual.R`

Starts the app in a background R process against a freshly generated
demo database, drives it with chromote, and asserts. Per language and
per viewport:

| Viewport | Width | What it is |
|---|---|---|
| `phone` | 360 x 780 | The tier with one pane at a time and the bottom segmented control |
| `phone-large` | 414 x 896 | Same tier, the widest phone |
| `tablet` | 820 x 1180 | The tier with the rail off-canvas |
| `laptop` | 1280 x 800 | The full three-pane row, just above the breakpoint |
| `desktop` | 1920 x 1080 | The full three-pane row with room to spare |

The checks it makes are listed in the script beside each assertion. The
ones that matter most, because each is a defect this redesign exists to
remove:

- **Every nav link is visible at every width.** Not merely present: a
  non-zero bounding rectangle inside the viewport.
- **The page never scrolls sideways.** `documentElement.scrollWidth` no
  wider than the viewport, at every width and in Arabic as well as
  English.
- **Every navigation control clears 44px** on its short side at touch
  widths, measured from the rendered box rather than from the CSS.
- **Exactly one screen is visible at a time**, and clicking a nav link
  changes which, with no server round trip needed for the change.
- **The pane survives a navigation.** Switch to the assessment pane on a
  phone, go to the Archive and back, and the assessment pane is still
  the one showing. This is the single most-reported symptom, and it is
  the one a structural test cannot catch.
- **A screen returned to is not rebuilt.** Timed: the second visit has
  to be an order of magnitude faster than the first, because the first
  built it and the second only unhid it.
- **Arabic mirrors.** In `ar`, the navigation starts at the right edge
  and the off-canvas rail comes in from the right.

Screenshots land in `output/screenshots/<lang>/<viewport>-<screen>.png`.
Look at them; they are the half of this no assertion covers. Deliberately
one file per combination and overwritten each run, so `git status` is not
where the results go and a diff against the previous run is a matter of
keeping a copy of the folder.

## Running it on the clock

A weekly run is the right cadence for the full pass: it is minutes, not
seconds, and nothing about it needs to be on the critical path of a
commit. Use whichever of these the machine already uses.

**systemd timer** (preferred on a server that already runs units):

```ini
# /etc/systemd/system/episodic-verify.service
[Unit]
Description=EpiSODIC verification pass
After=network-online.target

[Service]
Type=oneshot
User=<the account that owns the checkout>
WorkingDirectory=/path/to/EpiSODIC
ExecStart=/usr/bin/Rscript data-raw/verification/run_checks.R
ExecStart=/usr/bin/Rscript data-raw/verification/run_visual.R
```

```ini
# /etc/systemd/system/episodic-verify.timer
[Unit]
Description=EpiSODIC verification pass, weekly

[Timer]
OnCalendar=Mon 03:00
Persistent=true

[Install]
WantedBy=timers.target
```

`Persistent=true` matters: a machine that was asleep at 03:00 on Monday
runs the pass when it wakes, rather than skipping the week silently. Two
`ExecStart=` lines in a `Type=oneshot` service run in order and the unit
fails if either does, which is the behaviour wanted.

Then `systemctl enable --now episodic-verify.timer`, and
`systemctl list-timers episodic-verify` to confirm the next run is where
it should be.

**cron**, if that is what the machine uses:

```cron
0 3 * * 1 cd /path/to/EpiSODIC && /usr/bin/Rscript data-raw/verification/run_checks.R && /usr/bin/Rscript data-raw/verification/run_visual.R
```

cron mails output to the account on a non-zero exit and says nothing on
success, which is the correct default: a verification pass that reports
every time it passed is a verification pass nobody reads.

**Being told about it.** If the machine runs ntfy, append
`|| curl -d "EpiSODIC verification failed" ntfy.local/episodic` to the
cron line, or add
`OnFailure=` pointing at a one-shot notify unit for the systemd version.
Send on failure only.

## Requirements

- R, with the package's own dependencies, plus `devtools`, `styler`,
  `chromote` and `callr`.
- A Chrome or Chromium binary chromote can find. It looks at
  `CHROMOTE_CHROME` first; set it if the binary is somewhere unusual.
- No display: chromote runs Chrome headless, so the machine needs no X
  session.
- Roughly 2 GB free in `TMPDIR` for the demo database and the
  screenshots.

## When a visual check fails

The failure names the language, the viewport and the assertion, and the
screenshot for that combination is already on disk. Read the screenshot
first: an assertion about a bounding rectangle tells you a number was
wrong, and the picture tells you which of the several possible reasons
it was.

If the browser checks pass and something still looks wrong to a person,
that is worth more than the harness saying otherwise. Add the assertion
that would have caught it, in `run_visual.R` if it is about rendering and
in `tests/testthat/test-app_navigation.R` if it is about structure.
