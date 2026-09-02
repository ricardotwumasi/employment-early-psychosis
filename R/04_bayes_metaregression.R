# ===============================================
# 04 - Exploratory Bayesian meta-regression of employment prevalence
# ===============================================
# Separate, regularised binomial-normal models on the legacy 23-study set, one
# moderator per model, each adjusted for outcome basis so that contrasts are
# within basis:
#   logit(p_i) = b0 + b_basis * period_i + b_mod * x_i + u_i
# Priors: reference-level intercept ~ Normal(0, 1.5); contrasts ~ Normal(0, 1)
# on the logit scale; tau ~ Half-Normal(0, 1).
# Moderators: design (RCT vs cohort), timepoint in years centred at 12 months,
# provisional vocational exposure (all / part / none), attrition.
# No regional model is fitted on the primary set (Asia k = 0, Oceania k = 1);
# the registered four-level region model lives in 01 and is not interpreted.
# All of these are exploratory: only the moderator contrast is reported, as an
# equal-tailed 95% credible interval and P(contrast > 0).
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(brms)
  library(posterior)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

out_dir <- root_path("output", "bayesian")
prevalence <- read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE) %>%
  mutate(
    basis_period  = as.integer(outcome_basis == "period"),
    design_rct    = as.integer(design == "RCT"),
    time_years_c  = (timepoint_months - 12) / 12,
    exposure      = factor(coalesce(vocational_exposure, "unknown"),
                           levels = c("none", "part", "all", "unknown")),
    attrition_c   = attrition - mean(attrition)
  )

mr_prior <- c(prior(normal(0, 1.5), class = "b", coef = "Intercept"),
              prior(normal(0, 1), class = "b"),
              prior(normal(0, 1), class = "sd"))

run_mr <- function(moderator, tag, contrast_terms) {
  mod_term <- if (nzchar(moderator)) paste0(" + ", moderator) else ""
  f <- bf(as.formula(paste0("events | trials(n_assessed) ~ 0 + Intercept + basis_period",
                            mod_term, " + (1 | study_id_clean)")))
  res <- fit_gated(f, as.data.frame(prevalence), mr_prior, binomial(), tag)
  d   <- as_draws_df(res$fit)
  rows <- bind_rows(lapply(contrast_terms, function(term) {
    col <- paste0("b_", term)
    check(col %in% names(d), paste("missing coefficient", col, "in", tag))
    q <- quantile(d[[col]], c(0.025, 0.5, 0.975), names = FALSE)
    data.frame(model = tag, term = term, k = nrow(prevalence),
               estimate_logit = q[2], ci_low = q[1], ci_high = q[3],
               odds_ratio = exp(q[2]), or_low = exp(q[1]), or_high = exp(q[3]),
               p_gt_0 = mean(d[[col]] > 0), stringsAsFactors = FALSE)
  }))
  list(rows = rows, diag = res$diag)
}

models <- list(
  run_mr("design_rct",   "mr_design",    "design_rct"),
  run_mr("time_years_c", "mr_timepoint", "time_years_c"),
  run_mr("exposure",     "mr_exposure",  c("exposurepart", "exposureall", "exposureunknown")),
  run_mr("attrition_c",  "mr_attrition", "attrition_c")
)
# The outcome-basis contrast itself, from a model without another moderator
basis_only <- run_mr("", "mr_outcome_basis", "basis_period")
models <- c(models, list(basis_only))

coefs <- bind_rows(lapply(models, `[[`, "rows"))
diag  <- bind_rows(lapply(models, `[[`, "diag"))
write_csv(coefs, file.path(out_dir, "metaregression_contrasts.csv"))
write_csv(diag,  file.path(out_dir, "metaregression_diagnostics.csv"))
check(all(diag$passes_gate), paste("meta-regression models failing the gate:",
                                   paste(diag$model[!diag$passes_gate], collapse = ", ")))

cat("\nExploratory meta-regression contrasts (logit scale, adjusted for outcome basis):\n")
print(coefs %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
