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

A named list, one flat entry per key: `font` and `font_size_base`; the
neutrals, brand and status roles by their own names (`ink`, `primary`,
`danger`, ...); the severity steps as `severity_baseline` to
`severity_very_high`; and each component as `<group>_<key>`
(`state_new`, `care_line_first`, `chart_bars`, `header_brand_bar`, ...),
all resolved to hex colours. Derived from them, and not settable:
`<role>_text`, the role darkened to read as text on `surface` and `bg`,
and `<role>_on`, the text colour to write on a fill of the role.

## Details

The palette ships with an organisation-neutral default
([`inst/config/episodic_default_style.yaml`](https://github.com/msberends/EpiSODIC/blob/main/inst/config/episodic_default_style.yaml)).
To use your own institute's colours or fonts instead, point the
`EPISODIC_STYLE` environment variable at a YAML file that overrides only
what you want to change; anything you do not set keeps its shipped
default.
[`inst/config/episodic_style_template.yaml`](https://github.com/msberends/EpiSODIC/blob/main/inst/config/episodic_style_template.yaml)
is a complete file to start from.

The file is layered, from what a colour is to what it is used for:

- `typography`: `font`, a CSS font-family stack, and `font_size_base`,
  the size every other size in the app scales from (all of them are in
  `rem`), to compensate for a font that reads smaller or larger than the
  default at the same pixel size. Changing `font` only changes the CSS
  declaration; a webfont named there still needs delivering (a
  self-hosted `@font-face` or a link to its provider).

- `swatches`: your organisation's own colours, under names of your
  choosing.

- `neutral`: text, lines and backgrounds (`ink`, `muted`, `faint`,
  `border`, `bg_subtle`, `bg`, `surface`).

- `brand`: `primary`, `primary_strong` (the navigation bar, headings),
  `primary_soft`, `primary_tint`, `secondary` and `tertiary`.

- `status`: `success`, `warning` and `danger`.

- `severity`: one ordinal scale, `baseline` to `very_high`, shared by an
  epidemiologist's verdicts and the Moving Epidemic Method's intensity
  bands, so a step has one colour on every screen.

- `components`: every place where a colour carries a meaning (a
  cluster's state, a care line, the chips in a dossier header, a trend,
  each series of a chart, the header), each naming the colour it is
  drawn in.

Any colour may be a quoted hex value or the name of a colour defined
above it: a swatch, a neutral, brand or status role (`"primary"`), or a
severity step (`"severity.high"`). A house palette is so entered once,
as swatches, and every role and component follows it.

Text is always readable, whatever the colours: nothing in the file sets
a text colour. Where a colour is written as text, it is darkened just
enough to reach a contrast of 4.5:1 (WCAG AA) against its background,
keeping its hue; text on a filled colour is ink, white or black,
whichever reads; chart lines and marks are lifted to 3:1 against the
chart's background.

The same palette governs everything EpiSODIC draws: the dashboard, the
outbreak reports rendered by
[`episodic_report_render()`](https://msberends.github.io/EpiSODIC/reference/episodic_report_render.md)
(their charts, text, headings, links and tables) and the emails it
sends. The process doing the render resolves it and hands it to the
template, so a report or an email always carries the colours and font of
the instance that produced it.

Each value in the `EPISODIC_STYLE` file is checked. A value that cannot
be used (not a hex colour, a name that names nothing, a key that is not
part of the palette) keeps the shipped value for that key only, and a
file that does not exist or cannot be read keeps the whole shipped
palette. Neither stops the dashboard, a report or an email: the problem
is shown on the Info screen's reference-data panel, written to every
detection run's log, and raised once as a warning. So is an `ink` or
`muted` too light to read against `surface` or `bg`.

This is independent of `episodic_config_resolve()` on purpose: colours
and typography never affect the `config_hash` recorded with a detection
run, since they have no bearing on reproducibility.

## Default font and colours

The shipped file, in full:

    # EpiSODIC's shipped default palette and typography.
    #
    # Display only: the colours and font of the dashboard, the reports and
    # the emails. Deliberately a file of its own, apart from
    # episodic_default_config.yaml, so it never enters config_hash: how a
    # result looks has no bearing on how it was computed.
    #
    # An instance overrides it by pointing EPISODIC_STYLE at a YAML file
    # with only the keys it wants to change; everything else keeps the value
    # below. inst/config/episodic_style_template.yaml is a complete copy to
    # start from.
    #
    # The file is layered, from what a colour is to what it is used for:
    #
    #   swatches    an organisation's own colours, under names of its choosing
    #   neutral     text, lines and backgrounds
    #   brand       the identity colours
    #   status      success, warning, danger
    #   severity    the one ordinal scale for verdicts and MEM intensity
    #   components  every place on screen where a colour carries a meaning,
    #               each naming the colour it is drawn in
    #
    # Any colour may be written as a hex value in quotes ("#4A647D"; a bare
    # # starts a YAML comment) or as the name of a colour defined above it:
    # a swatch ("certe_blue"), a neutral, brand or status role ("primary",
    # "muted", "danger"), or a severity step ("severity.high"). So a house
    # palette is entered once, as swatches, and every role and component
    # follows it.
    #
    # Text is always readable, whatever the colours. Nothing here sets a
    # text colour: where a colour is written as text, EpiSODIC darkens it
    # just enough to reach a contrast of 4.5:1 (WCAG AA) against the
    # background it sits on, keeping its hue, and writes text on a filled
    # colour in ink, white or black, whichever reads. Chart lines and marks
    # are lifted the same way to 3:1 against the chart's background.


    # -- Typography -------------------------------------------------------
    #
    # font: a CSS font-family stack (quote any name containing a space).
    # font_size_base: the base size every other size in the app scales from
    # (all of them are in rem), so a font that reads smaller or larger than
    # IBM Plex Sans at the same pixel size is compensated for here.
    typography:
      font: '"IBM Plex Sans", ui-sans-serif, system-ui, "Segoe UI", sans-serif'
      font_size_base: "13px"


    # -- Swatches ---------------------------------------------------------
    #
    # Your own colours, under names of your choosing (lower case, digits and
    # underscores, starting with a letter, and not the name of a role
    # below). Swatches are hex values only; nothing refers to them until a
    # role or component below names them. The shipped palette needs none.
    swatches: {}


    # -- Neutrals ---------------------------------------------------------
    #
    # A true grey scale, independent of the brand hue, so body text is never
    # tinted by it. These are used as set: ink and muted are text, and a
    # value that does not reach 4.5:1 against surface and bg is reported on
    # the Info screen.
    neutral:
      ink: "#222222"         # body text
      muted: "#495057"       # secondary text, axis labels, anything not yet judged
      faint: "#ADB5BD"       # hairlines, placeholders and reference lines; never text to be read
      border: "#DEE2E6"      # panel and table borders
      bg_subtle: "#EBEBEB"   # table headers, empty map areas
      bg: "#F8F9FA"          # the page behind the panels
      surface: "#FFFFFF"     # panels, cards, charts


    # -- Brand ------------------------------------------------------------
    #
    # These defaults are Bootswatch "Yeti" (light), the theme the pkgdown
    # site is built with, so the app and its documentation look alike.
    brand:
      primary: "#008CBA"         # buttons, links, selection, focus, the bars of the curves
      primary_strong: "#005F7A"  # the navigation bar, headings, nowcast marks
      primary_soft: "#66BAD6"    # a lighter primary, for the 'new' state
      primary_tint: "#D9EDF5"    # the wash behind a selected row
      secondary: "#333333"       # a second identity colour, for the components that name it
      tertiary: "#20C997"        # a third, likewise


    # -- Status -----------------------------------------------------------
    #
    # Feedback: whether something worked, needs a look, or failed. Kept for
    # status alone, apart from the brand.
    status:
      success: "#43AC6A"
      warning: "#F8AC59"
      danger: "#F36A5A"


    # -- Severity ---------------------------------------------------------
    #
    # One ordinal scale, read wherever EpiSODIC grades how serious something
    # is: an epidemiologist's verdict on a cluster and MEM's intensity band
    # alike, so a colour is the same step on every screen, report and email.
    # Each step must read as more severe than the one before it at a glance.
    # The defaults are Yeti's $gray-500, $success, $warning, $danger and
    # $gray-900.
    severity:
      baseline: "#ADB5BD"
      low: "#43AC6A"
      medium: "#E99002"
      high: "#F04124"
      very_high: "#222222"


    # -- Components -------------------------------------------------------
    #
    # What each thing that carries a meaning is drawn in. Usually a name
    # from above, so it follows the palette; a hex value pins it.
    components:
      header:
        navbar: primary_strong            # the bar along the top
        brand_accent: secondary           # the accented part of the name in it
        brand_bar: [warning, success, danger, primary, secondary]  # the stripe under it, left to right
      # The state of an outbreak or epidemic, on its chip and its dot.
      state:
        new: primary_soft
        assessing: primary
        monitoring: danger
        closed: success
        reassess: tertiary
      # The care line a stream belongs to, on the rail's chip. Each line also
      # differs in form (filled, tinted, outlined, dashed), so two close
      # colours still tell apart.
      care_line:
        first: primary
        second: tertiary
        third: secondary
        other: muted
      # The outlined chips in the header of an outbreak or epidemic.
      chip:
        level: primary                    # its care line, level and place
        season: secondary                 # the season an epidemic belongs to
        during_epidemic: secondary        # an epidemic an outbreak ran during
        linked_cluster: danger            # another outbreak or epidemic sharing these cases
        flag: tertiary                    # added by hand, or changed since assessed
        mute_reason: secondary            # the reasons for muting a stream
      # The direction of a count compared with the period before.
      trend:
        rising: danger
        falling: success
        steady: muted
      chart:
        bars: primary                     # case counts
        observed: ink                     # an observed line against an expected one
        nowcast: primary_strong           # the expected final count of weeks still being reported
        outlook: secondary                # the forecast weeks ahead
        mem_start: secondary              # MEM's epidemic start threshold
        mem_end: tertiary                 # MEM's epidemic end threshold
        positivity: danger                # test positivity
        current_period: primary           # the period being read, among earlier ones
        earlier_periods: muted            # earlier periods, fading with age
        male: primary                     # the age pyramid
        female: secondary

## Examples

``` r
pal <- episodic_palette()
pal$primary
#> [1] "#008CBA"
pal$danger_text
#> [1] "#B95144"
pal$state_monitoring
#> [1] "#F36A5A"
pal$font
#> [1] "\"IBM Plex Sans\", ui-sans-serif, system-ui, \"Segoe UI\", sans-serif"
```
