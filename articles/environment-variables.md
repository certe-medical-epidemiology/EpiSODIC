# Environment variables

Every `EPISODIC_*` variable is optional; each entry point works fine
with the equivalent argument passed explicitly instead. Environment
variables exist so an operator can configure a running instance (a
`systemd` unit, a Docker container) without editing R code - set them
once where the instance runs, and every entry point that falls back to
them picks the setting up automatically.

[TABLE]

None of these need to be set to run the demo -
[`episodic_demo()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_demo.md)
uses a temporary SQLite file and every shipped default. See
[`vignette("deployment")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.md)
for `EPISODIC_DB` and `EPISODIC_CONFIG`/`EPISODIC_STYLE` in context
(database backend, custom report templates), and
[`vignette("data-format")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.md)’s
“Geographic reference data” section for the detail behind
`EPISODIC_GEO_DATA` and `EPISODIC_GEO_DATA_OVERLAY`.

## Checking what they actually delivered

Setting a variable and seeing nothing change on screen has, until now,
been indistinguishable from setting it wrongly. The dashboard’s **Info**
screen carries a “Reference data” panel that resolves each of these live
and reports, per variable, whether the instance’s own file is in use,
the shipped default is standing in, nothing is configured, the file was
configured and could not be used (with the reason), or an optional
package the feature needs is missing - alongside what it put into the
app: how many areas the map geometry holds, how many postcodes the
province mapping maps and to how many provinces, and how many of the
distinct postcodes in your own case data resolve to a province.

That last figure is the one worth reading first when a province is not
appearing where you expect: a mapping read successfully but matching
none of your postcodes looks, on every other screen, exactly like a
mapping that was never read.

The resolved file paths are shown only to a signed-in user. By default
the Info screen itself needs no sign-in, so the statuses and counts -
the part that answers “is my file being used” - are readable without
one, while the paths, which say more about the machine than a visitor
needs, are not. On an instance with `access.require_login` set the whole
screen is behind the sign-in like every other, so a signed-in reader
sees the paths there too.
