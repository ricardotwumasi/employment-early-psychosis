# External data

## frederick2019/

Supplementary files of Frederick DE, VanderWeele TJ (2019). Supported employment:
Meta-analysis and review of randomized controlled trials of individual placement and
support. PLoS ONE 14(2): e0212208. https://doi.org/10.1371/journal.pone.0212208
(CC BY 4.0).

Downloaded with curl on 2026-09-02 from

- `pone.0212208.s002.R`: S2 File, the authors' R code.
  https://journals.plos.org/plosone/article/file?type=supplementary&id=10.1371/journal.pone.0212208.s002
- `pone.0212208.s003.zip`: S3 File, the authors' R data files (nine `.rds` files).
  https://journals.plos.org/plosone/article/file?type=supplementary&id=10.1371/journal.pone.0212208.s003
- `competitive_employment_any.rds`: extracted from `pone.0212208.s003.zip` (25 rows,
  one per trial; two rows, Bejerholm 2015 and Tsang 2009, have NA counts).

Used by `R/07_external_prior_frederick2019.R`, which re-downloads the zip only if the
`.rds` file is missing.
