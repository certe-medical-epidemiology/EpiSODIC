# The 0.17.0 validation study, kept

The detection validation study as it was run against EpiSODIC 0.17.0 on
9 September 2026, in 168.8 minutes. It is the study that
`data-raw/validation/HANDOVER.md` describes.

It is kept because it is not reproducible. `run_study.R` re-runs against
whatever the code is today, and the code moved: the scale boundary, the
lattice suppression change, the bounded re-runs and the MEM rework all
landed after this. Nothing can produce these numbers again.

The study that replaced it, run against 0.25.0 on 18 November 2026, is
in `data-raw/validation/results/`. What moved between them:

| | 0.17.0 | 0.25.0 |
|---|---|---|
| Sensitivity | 119/120 | 120/120 |
| Predictive value | 156/199 | 151/181 |
| False alarms over 520 runs | 43 | 30 |
| Stream-weeks | 468,710 | 460,534 |
| `config_hash` | `9b97cc3f` | `7fef366a` |

Two conclusions reversed rather than moved: removing MEM cost no
sensitivity here and costs thirteen seasonal waves there, and removing
Farrington cost five points of sensitivity here and costs none there.

`results/raw/` is committed, unlike the live study's, where it is
ignored as regenerable. Here it is the only copy.

Nothing reads this directory. It is a record.
