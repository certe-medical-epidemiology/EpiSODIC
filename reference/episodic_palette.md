# The Dashboard's Colour Palette and Typography

Returns the colours and typography used throughout the EpiSODIC
dashboard and charts, as a named list. Useful if you want to match your
own plots or reports to the house style, or check what colour a given
status uses.

## Usage

``` r
episodic_palette()
```

## Value

A named list. The greyscale neutrals are `ink` (default text), `muted`
(secondary text), `faint` (tertiary text), `border`, `bg_subtle`, `bg`,
and `surface`. The semantic roles are `primary`, `secondary`,
`tertiary`, `success`, `warning`, and `danger`, each with
`_dark`/`_light`/`_tint` variants where used. The severity scale is
`severity_baseline`, `severity_low`, `severity_medium`, `severity_high`
and `severity_very_high`. `font` and `font_size_base` hold the app's
typography, not a colour.

## Details

The palette ships with an organisation-neutral default
([`inst/config/episodic_default_style.yaml`](https://github.com/certe-medical-epidemiology/EpiSODIC/blob/main/inst/config/episodic_default_style.yaml)).
To use your own institute's colours or fonts instead, point the
`EPISODIC_STYLE` environment variable at a YAML file that overrides only
the roles you want to change - anything you do not set keeps its shipped
default.

The same palette governs everything EpiSODIC draws: the dashboard, the
outbreak reports rendered by
[`episodic_report_render()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_report_render.md)
(their charts, text, headings, links and tables) and the emails it
sends. The process doing the render resolves it and hands it to the
template, so a report or an email always carries the colours and font of
the instance that produced it.

Each value in the `EPISODIC_STYLE` file is checked: a colour must be a
quoted hex colour (`"#RGB"`, `"#RRGGBB"` or `"#RRGGBBAA"`), `font` a CSS
font-family stack and `font_size_base` a CSS length (`px`, `pt`, `rem`
or `em`). A value that fails keeps the shipped value for that role only,
and a file that does not exist or cannot be read keeps the whole shipped
palette. Neither stops the dashboard, a report or an email: the problem
is shown on the Info screen's reference-data panel, written to every
detection run's log, and raised once as a warning.

This is independent of `episodic_config_resolve()` on purpose: colours
and typography never affect the `config_hash` recorded with a detection
run, since they have no bearing on reproducibility.

## Default font and colours

These are all the default values, and all can be changed using a custom
YAML file.

    font:                '"IBM Plex Sans", ui-sans-serif, system-ui, "Segoe UI", sans-serif'
    font_size_base:      "13px"
    ink:                 "#222222"
    muted:               "#495057"
    faint:               "#ADB5BD"
    border:              "#DEE2E6"
    bg_subtle:           "#EBEBEB"
    bg:                  "#F8F9FA"
    surface:             "#FFFFFF"
    primary:             "#008CBA"
    primary_dark:        "#005F7A"
    primary_light:       "#66BAD6"
    primary_tint:        "#D9EDF5"
    secondary:           "#333333"
    secondary_dark:      "#1A1A1A"
    tertiary:            "#20C997"
    tertiary_dark:       "#168D6A"
    success:             "#43AC6A"
    success_dark:        "#2F784A"
    warning:             "#F8AC59"
    warning_dark:        "#AE783E"
    danger:              "#F36A5A"
    danger_dark:         "#AA4A3F"
    severity_baseline:   "#ADB5BD"
    severity_low:        "#43AC6A"
    severity_medium:     "#E99002"
    severity_high:       "#F04124"
    severity_very_high:  "#222222"

Of note:

- `primary_dark` is the background colour of the navigation bar.

- `severity_baseline` to `severity_very_high` are one ordinal scale,
  shared by an epidemiologist's verdicts and the Moving Epidemic
  Method's intensity bands, so a step has one colour on every screen.
  Text on them is set to whichever of `ink`, white or black reads.

- `font` is a CSS font-family stack, and `font_size_base` is the app's
  base font size.

  - Every other font size in the dashboard is set in `rem` relative to
    it, so changing `font_size_base` scales the whole app's type
    proportionally (useful when swapping in a font that reads naturally
    smaller or larger than the default at the same pixel size).

  - Changing `font` only changes the CSS declaration; if it names a
    webfont rather than a system font, delivering that font (a
    self-hosted `@font-face` or a link to its provider) is the
    operator's own concern.

## Examples

``` r
pal <- episodic_palette()
pal$primary
#> [1] "#008CBA"
pal$danger
#> [1] "#F36A5A"
pal$font
#> [1] "\"IBM Plex Sans\", ui-sans-serif, system-ui, \"Segoe UI\", sans-serif"
pal$font_size_base
#> [1] "13px"
```
