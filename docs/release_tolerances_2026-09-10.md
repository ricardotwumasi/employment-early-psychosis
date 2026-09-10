# Predeclared tolerances for the independent rebuild of release candidate 1

Written on 10 September 2026 before `Rscript run_all.R --release` was started, as the plan (Section 10, Phase 4) requires. GT's independent rebuild in a clean environment restored from `renv.lock` (CmdStan 2.36.0, R 4.4.2) is compared with the release run on these criteria. Exact HMC draws need not match across machines; the same seed on the same platform is expected to reproduce them exactly.

| Quantity | Tolerance | Rationale |
|---|---|---|
| Pooled prevalence posterior median and 95 percent CrI limits (every prevalence model) | within 0.5 percentage points | Monte Carlo standard error of a 16,000-draw median on the proportion scale is below 0.2 points |
| Trial log-RR posterior median and CrI limits (every RR model) | within 2 percent on the RR scale | Monte Carlo error of the median with 16,000 draws and tau at most 1 |
| OR-model posterior median and CrI limits; absolute-risk scenarios | within 2 percent (OR) and 0.5 percentage points (risks) | As above |
| tau posterior median | within 0.02 | Half-normal priors; quantiles of a bounded posterior |
| Bridge-sampling Bayes factors | within 5 percent of the release value | The bridge estimator's own relative error is about 3 percent; the A18 two-significant-figure criterion against quadrature is reported separately and is not relaxed |
| Quadrature Bayes factors (deterministic) | identical to displayed precision (three significant figures) | Numerical integration, no randomness |
| Convergence gate | every fit passes the same gate (R-hat below 1.01, bulk and tail ESS above 1,000, no divergences, tree depth below 10, E-BFMI above 0.2) | Unchanged from the 1 September specification |
| Deterministic outputs (cleaner, release inputs, frequentist tables, quadrature tables) | byte-identical | No randomness |

A difference outside tolerance is a finding to be recorded in the register, not a reason to widen the tolerance.
