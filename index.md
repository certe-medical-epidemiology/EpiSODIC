# EpiSODIC: Epidemiological Signal Observation, Detection, Identification, and Classification

[![R-CMD-check](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/certe-medical-epidemiology/EpiSODIC/actions/workflows/R-CMD-check.yaml)

EpiSODIC is an automated outbreak cluster detection and assessment
system for infectious disease epidemiologists. It runs in R, reads
laboratory-confirmed infections, detects statistical and rule-based
aberrations, reconciles them into persistent clusters, and gives
epidemiologists a dossier to assess each one, with a full audit trail
and outbreak reports for clinical colleagues (e.g. clinical
microbiologists) and infection prevention nurses.

The dashboard and its outbreak reports are available in English, (Modern
Standard) Arabic, Dutch, French, German, Hindi, Mandarin Chinese, and
Spanish. This software is free; this repository is open-source software
with no data and no site-specific configuration.

![A cluster dossier: case stats, status trajectory, an automatically
generated plain-language interpretation, the epidemic curve, and the
classification panel](reference/figures/main_screen.png)  
*A cluster dossier - case stats, status trajectory, an automatically
generated interpretation of the evidence, the epidemic curve, and the
classification panel, alongside the rail of open clusters.*

![The Pathogen screen: weekly numbers of cases, reproduction number,
with geographic and demographic
distribution](reference/figures/pathogen_screen.png)  
*The Pathogen screen - weekly numbers of cases, reproduction number,
with geographic and demographic distribution.*

![The Performance screen: detection timeliness and positive predictive
value per detector and
pathogen](reference/figures/performance_screen.png)  
*The Performance screen - detection timeliness and positive predictive
value per detector and pathogen, computed from the stored verdicts (both
fill in as clusters get assessed).*

## Installation

``` r

# install.packages("remotes")
remotes::install_github("certe-medical-epidemiology/EpiSODIC")
```

## See it work, right now

No data, no credentials, no configuration - one call runs the whole
system against bundled synthetic data:

``` r

EpiSODIC::episodic_demo()
```

This creates a temporary database, runs one detection cycle, provisions
a demo epidemiologist account (printed to the console), and opens the
app. Pass `launch = FALSE` to skip opening the app and just get a
populated database path back, e.g. for scripting.

## Documentation

Everything past “what is this and how do I try it” lives in the
package’s vignettes, so it stays organised by topic instead of one long
page:

| Read this… | …for |
|----|----|
| [Overview](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/overview.html) | What EpiSODIC does: the Clusters and Pathogen screens’ two altitudes, and the Performance, Archive, Streams, Activity and Info screens around them. |
| [Getting your data in](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.html) | The case data contract - every column, what’s optional, and [`episodic_check_cases()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_cases.md)/[`episodic_check_denominators()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_denominators.md)/[`episodic_check_institution_activity()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_check_institution_activity.md) to check your extract before anything runs. |
| [Deployment](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.html) | Standing up a real instance: scheduling, accounts, the database backend, custom report templates. |
| [Environment variables](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.html) | The full `EPISODIC_*` reference table. |
| [Detection and reconciliation](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/detection-reconciliation.html) | How a laboratory result becomes a dossier: the four detectors, reconciliation, and suppression. |
| [FAQ](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/faq.html) | Answers to the questions that come up most often when getting started. |

Building the package locally (or browsing offline) works the same way:
`vignette(package = "EpiSODIC")` lists all of them, and
[`vignette("data-format", package = "EpiSODIC")`](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.md)
(etc.) opens one.

## Licence

GPL-2.
