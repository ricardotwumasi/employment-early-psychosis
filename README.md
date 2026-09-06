# Employment Outcomes in Early Psychosis: Bayesian Meta-Analysis Code

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/ricardotwumasi/employment-early-psychosis/blob/main/LICENSE)
[![R](https://img.shields.io/badge/R-4.4.2-blue.svg)](https://cran.r-project.org/)
[![PROSPERO](https://img.shields.io/badge/PROSPERO-CRD420261402841%20v2.0-brightgreen.svg)](https://www.crd.york.ac.uk/PROSPERO/view/CRD420261402841)

R code and extracted data to reproduce our systematic review and meta-analysis of employment
outcomes in first-episode psychosis. The review is registered on PROSPERO
([CRD420261402841](https://www.crd.york.ac.uk/PROSPERO/view/CRD420261402841); version 1.0
published 26 May 2026, revised by amendment as version 2.0 published 5 September 2026). Version 1.0
specified a narrative synthesis and no pooling model. The original frequentist analysis (metafor,
REML with the Knapp-Hartung adjustment) is reproduced in full, and the primary inference is a
Bayesian re-analysis in brms with exact binomial likelihoods and weakly informative half-normal
priors on the between-study standard deviation, reported following the guidance of Dienes (2021)
on Bayesian hypothesis tests. The Bayesian analysis was specified within the research team on
1 September 2026 before any Bayesian output was generated and was subsequently registered by
amendment; it is not prospectively registered.

Every estimand, prior, decision threshold, missing-data rule and sensitivity analysis was written
down before any Bayesian model was fitted; the specification is archived verbatim in
`docs/analysis_specification_2026-09-01.txt`, and every subsequent change is a dated entry in
`docs/amendment_2026-09-02.md` (A1 to A22) and `docs/amendment_2026-09-06.md` (A23 onwards).
`docs/document_control_register.md` records the gate status. **This is a development version:
the population-eligibility, outcome-window and review-record audits are unresolved (see the
amendment records), so the current estimates are historical baselines, not release results.**

## Analyses

1. **Pooled employment prevalence.** Binomial-normal hierarchical model on the logit scale.
   Primary estimand: point prevalence of competitive or paid employment at the follow-up closest
   to 12 months in first-episode cohorts with a competitive-employment outcome (k = 10). Period
   prevalence is a secondary estimand. Priors: pooled logit ~ Normal(0, 1.5); tau ~ Half-Normal(0, 1),
   with 0.5 and 2 as sensitivities.
2. **Effect of Individual Placement and Support (IPS).** IPS added to treatment as usual versus
   treatment as usual, risk ratio for competitive employment, three randomised trials. Normal-normal
   random-effects model on the log risk ratio; mu ~ Normal(0, 1); tau ~ Half-Normal(0, 0.5), with
   0.25 and 1 as sensitivities. An exact binomial logistic model on arm-level counts gives the
   pooled odds ratio.
3. **Dienes-style inference for the trials.** A null interval on the risk ratio, RR 0.80 to 1.25,
   with posterior probabilities below, inside and above it, the Kruschke decision rule and a
   robustness region; and Bayes factors with a half-normal model of the alternative, B_HN(0, s),
   computed exactly for the random-effects model by quadrature and checked by bridge sampling,
   with a robustness region over s.
4. **Exploratory moderators.** Separate regularised Bayesian meta-regressions (design, timepoint,
   vocational exposure, attrition), each adjusted for outcome basis. The original frequentist
   moderator models are reproduced alongside.
5. **Publication bias and recovery outcomes.** Original funnel plot, Egger test and trim-and-fill
   with the corrected interpretation; correlations and standardised mean differences with recovery
   measures are tabulated descriptively because they are too sparse to pool.

## Requirements

- R 4.4.2
- CmdStan 2.36.0 (install with `cmdstanr::install_cmdstan(version = "2.36.0")`)
- R packages, with the versions the reported results were generated with: brms 2.23.0,
  cmdstanr 0.9.0, posterior 1.6.0, metafor 4.6.0, bridgesampling, digest, dplyr, readr, stringr,
  tidyr, ggplot2, patchwork, knitr, bayesplot, scales

`run_all.R` checks the pinned versions of brms, cmdstanr, posterior, metafor and CmdStan and stops
if they differ, because posterior summaries can change between releases.

## Running the analysis

```bash
git clone https://github.com/ricardotwumasi/employment-early-psychosis.git
cd employment-early-psychosis
Rscript run_all.R                # uses cached fits in output/bayesian/fits if present, fits otherwise
Rscript run_all.R --cache-only   # replay: stops before any fitting call if a cache is missing
Rscript run_all.R --preflight    # lists which of the 42 registered fits have a readable cache
Rscript run_all.R --refit        # refits every Bayesian model
Rscript run_all.R --release      # refit with a clean tree and a synchronised renv.lock; fails on any cached fit
```

`Rscript run_all.R` without arguments is not a read-only check: it fits any model whose cache is
absent. Use `--cache-only` to inspect. Every run writes `output/run_manifest.json` (mode, git
commit, input and output hashes, package and CmdStan versions, and per-fit cache provenance).

A full run from scratch takes roughly 20 minutes on a laptop (42 Bayesian fits listed in
`data/registry/fit_registry.csv`, four
chains each, 2,000 warm-up and 4,000 sampling iterations per chain, seed 20260901). Results are
reproducible to the reported precision on the same CmdStan build; bit-identical reproduction across
platforms is not claimed.

## Verification, dependencies and continuous integration

`Rscript tests/testthat.R` runs deterministic checks: the raw file's hash, row count and schema;
the derived tables' schemas and the historical fixture values (ten primary prevalence studies,
2,818 baseline and 1,613 assessed participants; three primary trials, 296 randomised and 264
analysed); the correction ledger against the committed corrections table; the identifier
crosswalk; the closed-form and quadrature Bayes-factor helpers against stored values and the
Dienes worked example; the Frederick and VanderWeele (2019) reproduction; the fit registry against
the exported diagnostics; and the cache-only failure path with the fitting function shimmed. These
fixtures describe the historical analysis for regression purposes; they are not eligibility rules.

The GitHub Actions workflow `deterministic-checks` runs the cleaner, checks that the derived tables
are unchanged and runs the tests on Ubuntu without Stan. A green run is not a scientific refit and
does not certify any estimate.

`renv.lock` was written from the working library on 6 September 2026 and records the package
versions used. Restoration into a fresh library has not yet been demonstrated, `bayesplot` is a
development build from `https://stan-dev.r-universe.dev`, and CmdStan 2.36.0 must be installed
separately (`cmdstanr::install_cmdstan(version = "2.36.0")`). `DESCRIPTION` declares the
deterministic-check dependencies (Imports), the CRAN part of the analysis stack (Suggests) and
`cmdstanr` under `Config/Needs/analysis` (it is not on CRAN; install it from
`https://stan-dev.r-universe.dev`). The external
Frederick data are committed; `scripts/fetch_external_frederick2019.R` is the only step that
uses the network and is never run by the analysis.

## Repository layout

| Path | Contents |
|------|----------|
| `data/yanan_data_280826.csv` | Raw data extraction, one row per result. Never edited. |
| `data/derived/` | Analysis tables written by `R/00_clean_data.R`, including `corrections.csv` (every source-verified correction with its raw value, corrected value and source) and `reconciliation.csv`. |
| `data/external/frederick2019/` | Supplementary data and code of Frederick and VanderWeele (2019), used to anchor the Bayes-factor H1 scale. |
| `R/00_clean_data.R` | Reads the raw file, applies the corrections, derives the tables and stops if any pre-specified assertion fails. |
| `R/01_frequentist_registered.R` | Original frequentist metafor analysis, as submitted and on corrected data (the file name predates amendment A14 and is retained). |
| `R/02_bayes_prevalence.R` | Binomial-normal prevalence models and sensitivity analyses. |
| `R/03_bayes_trials.R` | Trial models, sensitivity analyses, exact binomial model, bridge sampling. |
| `R/04_bayes_metaregression.R` | Exploratory moderator models. |
| `R/05_dienes_inference.R` | Null-interval probabilities, decision rule, Bayes factors, robustness regions. |
| `R/06_figures_tables.R` | Figures and manuscript tables. |
| `R/07_external_prior_frederick2019.R` | Re-analysis of the external IPS meta-analysis data. |
| `R/utils.R` | Shared helpers: cached fitting, convergence gate, posterior summaries, Bayes-factor functions. |
| `run_all.R` | Runs everything in order and writes `output/session_info.txt`. |
| `output/frequentist/`, `output/bayesian/`, `output/tables/`, `output/figures/` | Summary tables, diagnostics and figures. Cached model objects are not committed. |
| `data/schema/`, `data/registry/`, `data/review/` | Extraction schemas, the fit registry, and the review-record files (correction ledger, dispositions, crosswalks, eligibility and candidate-result audits, student evidence log). |
| `docs/` | Analysis specification, amendment records, document-control register, registration archive, historical output fixtures and changelog. |
| `tests/`, `.github/workflows/` | Deterministic tests and the CI workflow (see below). |
| `FEP_employment_data_extraction_template.csv` | The blank extraction template used for the review. |

The four scaffold scripts that previously sat at the repository root have been replaced by the
numbered scripts in `R/`; they remain in the git history.

## Data extraction format

`data/yanan_data_280826.csv` holds one row per individual result. The `effect_type` column tells
the analysis which numbers to read:

| `effect_type` | Columns read | Used for |
|---------------|--------------|----------|
| `proportion`   | `events`, `n_assessed` | Pooled employment prevalence |
| `trial_binary` | `events_intervention`, `n_intervention`, `events_control`, `n_control` | IPS trials and other controlled comparisons |
| `correlation`  | `r`, `n_r` | Employment and recovery (descriptive) |
| `smd`          | `mean1`, `sd1`, `n1`, `mean2`, `sd2`, `n2` | Employment and recovery (descriptive) |

Three raw `study_id` values are shared by two different studies each; `R/00_clean_data.R` assigns
a unique `study_id_clean` and asserts that no further collisions appear.

## Contributors

- Hazal Kaplankiran
- Yanan Li
- Giulia Trotta
- Ricardo Twumasi
- Anna Georgiades

## AI statement

This code was scaffolded with the assistance of Claude Fable 5.1 (Anthropic, San Francisco: CA).

## References

1. Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28. https://doi.org/10.18637/jss.v080.i01
2. Dienes, Z. (2021). How to use and report Bayesian hypothesis tests. *Psychology of Consciousness: Theory, Research, and Practice*, 8(1), 9-26. https://doi.org/10.1037/cns0000258
3. Frederick, D. E., & VanderWeele, T. J. (2019). Supported employment: Meta-analysis and review of randomized controlled trials of individual placement and support. *PLoS ONE*, 14(2), e0212208. https://doi.org/10.1371/journal.pone.0212208
4. Guyatt, G. H., et al. (2011). GRADE guidelines 6. Rating the quality of evidence: imprecision. *Journal of Clinical Epidemiology*, 64(12), 1283-1293. https://doi.org/10.1016/j.jclinepi.2011.01.012
5. Röver, C., Bender, R., Dias, S., et al. (2021). On weakly informative prior distributions for the heterogeneity parameter in Bayesian random-effects meta-analysis. *Research Synthesis Methods*, 12(4), 448-474. https://doi.org/10.1002/jrsm.1475
6. Stijnen, T., Hamza, T. H., & Özdemir, P. (2010). Random effects meta-analysis of event outcome in the framework of the generalized linear mixed model with applications in sparse data. *Statistics in Medicine*, 29(29), 3046-3067. https://doi.org/10.1002/sim.4040
7. Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. *Journal of Statistical Software*, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03
8. IntHout, J., Ioannidis, J. P. A., & Borm, G. F. (2014). The Hartung-Knapp-Sidik-Jonkman method for random effects meta-analysis is straightforward and considerably outperforms the standard DerSimonian-Laird method. *BMC Medical Research Methodology*, 14, 25. https://doi.org/10.1186/1471-2288-14-25

## License

MIT License. See [LICENSE](LICENSE) for details.

## Citation

For citing this repository, please use:

<details>
<summary>BibTeX</summary>
<pre><code>@article{kaplankiran2026,
  title={Employment in First-Episode Psychosis: A Systematic Review and Bayesian Meta-Analysis},
  author={Kaplankiran, Hazal and Li, Yanan and Trotta, Giulia and Twumasi, Ricardo and Georgiades, Anna},
  journal={tbc},
  year={2026},
  publisher={tbc},
  doi={tbc}
}
</code></pre>
</details>
<details>
<summary>APA</summary>
<pre><code>Kaplankiran, H., Li, Y., Trotta, G., Twumasi, R., & Georgiades, A. (2026). Employment in First-Episode Psychosis: A Systematic Review and Bayesian Meta-Analysis. tbc.</code></pre>
</details>
<details>
<summary>Vancouver</summary>
<pre><code>Kaplankiran H, Li Y, Trotta G, Twumasi R, Georgiades A. Employment in First-Episode Psychosis: A Systematic Review and Bayesian Meta-Analysis. tbc. 2026</code></pre>
</details>

