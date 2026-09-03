# EpiSODIC: an early warning system for infectious disease outbreaks

[![R-CMD-check](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml)

Statistical methods for spotting an unusual rise in infections are not new; the Farrington algorithm and its successors have run at national public health institutes since the early 1990s, and R has long had a solid reference implementation in the `surveillance` package. What has been missing is not a better method, but a system that actually runs one, day after day, in an ordinary laboratory.

An algorithm on its own only produces a single alarm at a single point in time. It does not remember that the alarm it raised yesterday and the one it raises today are the same ongoing event. It does not notice that a signal at ward level and a signal at hospital level are the same outbreak seen at two altitudes, so without reconciliation, one outbreak becomes several unrelated alerts competing for attention. It does not record why an epidemiologist decided a statistically significant blip was clinically irrelevant, or why a real cluster was eventually closed. And it certainly does not write a report an infection prevention nurse can act on.

**EpiSODIC is that system.** It takes a stream of laboratory-confirmed infections and carries it all the way from statistical detection through reconciliation into persistent clusters, structured epidemiologist assessment with a full audit trail, to an outbreak report ready for clinical colleagues, fully automated and running on a schedule of your choosing. It combines the Farrington method with rule-based detectors for pathogens too rare for a statistical baseline, and the Moving Epidemic Method for seasonal ones such as influenza and RSV, applied automatically across every ward, institution, and region in your data.

It is built to work at any laboratory, in any country, against any set of pathogens. Nothing about how it interprets your data is hardcoded. Set-up takes minutes and no data ever leaves your own infrastructure. New clusters can notify your team the moment they arise, by email, Microsoft Teams, Slack, or an ntfy server.

The dashboard and its outbreak reports are available in English, (Modern Standard) Arabic, Dutch, French, German, Hindi, Mandarin Chinese, and Spanish.

This software is 100% free and open-source, and only allows for local storage of data and configurations. This software is perfectly safe, and all code is open to anyone willing or required to assess.

<!--
Screenshots below live in man/figures/ (ships with the package, so
they also render on the CRAN page), regenerated against episodic_demo().
-->
<p align="center">
  <img src="man/figures/main_screen.png" alt="A cluster dossier: case stats, status trajectory, an automatically generated plain-language interpretation, the epidemic curve, and the classification panel" width="800">
  <br>
  <em>A cluster dossier - case stats, status trajectory, an automatically generated interpretation of the evidence, the epidemic curve, and the classification panel, alongside the rail of open clusters.</em>
</p>
<p align="center">
  <img src="man/figures/pathogen_screen.png" alt="The Pathogen screen: weekly numbers of cases, reproduction number, with geographic and demographic distribution" width="800">
  <br>
  <em>The Pathogen screen - weekly numbers of cases, reproduction number, with geographic and demographic distribution.</em>
</p>
<p align="center">
  <img src="man/figures/archive_screen.png" alt="The Archive screen: previously detected clusters" width="800">
  <br>
  <em>The Archive screen - previously detected clusters.</em>
</p>

## Installation

```r
# install.packages("remotes")
remotes::install_github("certe-medical-epidemiology/EpiSODIC")
```

## See it work, right now

No data, no credentials, no configuration - one call runs the whole system against bundled synthetic data:

```r
EpiSODIC::episodic_demo()
```

This creates a temporary database, runs one detection cycle, provisions a demo epidemiologist account (printed to the console), and opens the app. Pass `launch = FALSE` to skip opening the app and just get a populated database path back, e.g. for scripting.

## Documentation

Everything past "what is this and how do I try it" lives in the package's vignettes, so it stays organised by topic instead of one long page:

| Read this... | ...for |
|---|---|
| [Overview](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/overview.html) | What EpiSODIC does: the Clusters and Pathogen screens' two altitudes, and the Performance, Archive, Streams, Activity and Info screens around them. |
| [Getting your data in](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.html) | The case data contract - every column, what's optional, and `episodic_check_cases()`/`episodic_check_denominators()`/`episodic_check_institution_activity()` to check your extract before anything runs. |
| [Deployment](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.html) | Standing up a real instance: scheduling, accounts, the database backend, custom report templates. |
| [Environment variables](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.html) | The full `EPISODIC_*` reference table. |
| [Detection and reconciliation](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/detection-reconciliation.html) | How a laboratory result becomes a dossier: the four detectors, reconciliation, and suppression. |
| [FAQ](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/faq.html) | Answers to the questions that come up most often when getting started. |

Building the package locally (or browsing offline) works the same way: `vignette(package = "EpiSODIC")` lists all of them, and `vignette("data-format", package = "EpiSODIC")` (etc.) opens one.

## Licence

GPL-2.
