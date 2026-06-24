# 💼 Employment Outcomes in Early Psychosis: Meta-Analysis Code

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/ricardotwumasi/employment-early-psychosis/blob/main/LICENSE)
[![R](https://img.shields.io/badge/R-%E2%89%A54.1.0-blue.svg)](https://cran.r-project.org/)
[![PROSPERO](https://img.shields.io/badge/PROSPERO-CRD420261402841-brightgreen.svg)](https://www.crd.york.ac.uk/PROSPERO/view/CRD420261402841)

R code to reproduce our systematic review and meta-analysis of employment outcomes and
their moderators in early and first-episode psychosis. The review is registered on
PROSPERO ([CRD420261402841](https://www.crd.york.ac.uk/PROSPERO/view/CRD420261402841)),
and the whole synthesis runs in R using the [metafor](https://www.metafor-project.org/)
package. This repository holds the tidy data-extraction template and a scaffold script
for each planned analysis, so that the workflow can be run end to end once the studies
have been extracted.

The scripts deliberately reuse the approach of our two earlier reviews, the
[employer-discrimination review](https://github.com/ricardotwumasi/psychosis-employment-meta-analysis)
(pooled proportions, forest, Baujat, funnel, trim-and-fill, Egger's test and fail-safe N)
and the [core-beliefs review](https://github.com/ricardotwumasi/psychosis-core-beliefs)
(random-effects models with the Knapp-Hartung adjustment and prediction intervals).

## 🎯 Overview

In line with the registered protocol, there are four linked analyses, and each one
determines what is extracted into the template:

1. **Pooled employment rate** — the prevalence of competitive or paid employment across
   the first-episode samples. Proportions are pooled on the logit scale and
   back-transformed to a percentage. The time point closest to twelve months is the
   primary estimate; other time points are kept for sensitivity analyses.
2. **Effect of supported employment** — Individual Placement and Support, Supported
   Employment and Education, NAVIGATE and Horyzons compared with treatment as usual,
   from the randomised trials. The primary effect is a risk ratio, with an odds ratio as
   a sensitivity check.
3. **Employment and recovery** — the association between employment or vocational
   engagement and broader recovery (PANSS, SOFAS, GAF, quality of life), as a correlation
   or as a standardised mean difference between employed and unemployed groups. This
   evidence is expected to be sparser and partly narrative.
4. **Moderation** — study design, length of follow-up, region, treatment setting and
   diagnostic definition (first-episode psychosis, first-episode schizophrenia or early
   psychosis) tested through subgroup analysis and meta-regression.

Throughout, we report tau-squared, the I-squared statistic and a prediction interval.
Publication-bias diagnostics (funnel plot, Egger's test, trim-and-fill and fail-safe N)
only become meaningful with roughly ten or more studies, so they are applied to the
pooled employment rate and interpreted cautiously elsewhere.

## 💻 Requirements

- R (≥ 4.1.0)
- R packages: `metafor`, `meta`, `dplyr`, `tidyr`

## 🚀 Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/ricardotwumasi/employment-early-psychosis.git
   ```
2. Install the required R packages:
   ```r
   install.packages(c("metafor", "meta", "dplyr", "tidyr"))
   ```

## 📋 Data extraction template

`FEP_employment_data_extraction_template.csv` holds **one row per individual result**
rather than one row per study, which is the tidy format metafor prefers. A single study
that reports an employment rate, a trial outcome and a correlation therefore occupies
three rows.

The `effect_type` column tells the analysis which numbers to read for each row:

| `effect_type` | Used by | Columns read | metafor measure |
|---------------|---------|--------------|-----------------|
| `proportion`   | `01_pooled_employment_rate.R` | `events`, `n_assessed` | `PLO` (logit proportion) |
| `trial_binary` | `02_supported_employment.R`   | `events_intervention`, `n_intervention`, `events_control`, `n_control` | `RR` (and `OR` for sensitivity) |
| `correlation`  | `03_employment_recovery.R`    | `r`, `n_r` | `ZCOR` (Fisher's z) |
| `smd`          | `03_employment_recovery.R`    | `mean1`, `sd1`, `n1`, `mean2`, `sd2`, `n2` | `SMD` (Hedges' g) |

The study-level fields (`design`, `follow_up_months`, `region`, `setting`, `diagnosis`)
double as the moderators for analysis 4, so nothing extra is extracted for moderation.
The `quality_tool` and `quality_rating` columns carry across the EPHPP and Joanna Briggs
ratings for a later sensitivity analysis by study quality.

**Before extracting:**

- The four `EXAMPLE_*` rows are worked examples of each `effect_type`. **Delete them**
  before you begin.
- Record `timepoint_months` for every figure, and treat the point closest to twelve
  months as the primary estimate.
- Extract independently in pairs and reconcile any discrepancies, recording who extracted
  and who checked each row in the `extractor` and `checker` columns.

## 📜 Scripts

Each script is self-contained. It loads the packages, reads the template, drops the
`EXAMPLE_*` rows, filters to the rows it needs, calculates effect sizes with `escalc`,
fits a random-effects model with `rma` (REML with the Knapp-Hartung adjustment), and
prints the pooled estimate with its heterogeneity statistics and prediction interval.

| Script | Analysis |
|--------|----------|
| `01_pooled_employment_rate.R` | Pooled employment rate on the logit scale, with forest, Baujat and funnel plots, Egger's test, trim-and-fill and fail-safe N. |
| `02_supported_employment.R`   | Supported employment versus treatment as usual: pooled risk ratio with an odds ratio sensitivity check. |
| `03_employment_recovery.R`    | Association between employment and recovery: correlations (Fisher's z) and employed-versus-unemployed group comparisons (SMD), pooled separately. |
| `04_moderation.R`             | Subgroup analysis and meta-regression of the pooled employment rate on the study-level moderators. |

Run each script separately in R, for example:

```bash
Rscript 01_pooled_employment_rate.R
```

The scripts are scaffolds: they assume the template has been populated with real rows and
the example rows removed. Start with `01_pooled_employment_rate.R` once around ten studies
have been extracted, then add the remaining analyses.

## 📈 Interpreting the results

- **Forest plots** show the effect and confidence interval for each study and the pooled
  estimate, with a prediction interval added (`addpred = TRUE`).
- **Baujat plots** flag studies that contribute most to heterogeneity and to the pooled
  result.
- **Funnel plot, Egger's test and trim-and-fill** assess and adjust for potential
  publication bias; **fail-safe N** indicates how robust a result is.
- **tau-squared, I-squared and the prediction interval** describe the between-study
  heterogeneity that the moderator analyses then try to explain.

## 👥 Contributors

- Yanan Li
- Hazal Kaplankiran
- Ricardo Twumasi

## 🤖 AI statement

This code was scaffolded with the assistance of Claude Opus 4.8 (Anthropic, San Francisco: CA).

## 📚 References

1. Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. *Journal of Statistical Software*, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03
2. Harrer, M., Cuijpers, P., Furukawa, T. A., & Ebert, D. D. (2021). *Doing Meta-Analysis with R: A Hands-On Guide*. Chapman & Hall/CRC Press.
3. Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2021). *Introduction to Meta-Analysis* (2nd ed.). Wiley.
4. Wang, N. (2023). Conducting Meta-Analyses of Proportions in R. *Journal of Behavioral Data Science*, 3(2), 64-126. https://doi.org/10.35566/jbds/v3n2/wang
5. Chinn, S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. *Statistics in Medicine*, 19(22), 3127-3131.
6. Duval, S., & Tweedie, R. (2000). Trim and fill: a simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. *Biometrics*, 56(2), 455-463.
7. Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in meta-analysis detected by a simple, graphical test. *BMJ*, 315(7109), 629-634.
8. Higgins, J. P. T., & Thompson, S. G. (2002). Quantifying heterogeneity in a meta-analysis. *Statistics in Medicine*, 21(11), 1539-1558.

## License

MIT License. See [LICENSE](LICENSE) for details.
