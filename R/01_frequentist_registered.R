# ===============================================
# 01 - Registered frequentist analysis (metafor)
# ===============================================
# Reproduces the analysis registered on PROSPERO (CRD420261402841) and
# reported in the MSc dissertation, then re-runs it on the source-corrected
# data and on the strict primary set, so that the Bayesian results in 02 to 05
# can be read against the registered method.
#
# Model: logit-transformed proportions (escalc measure = "PLO"), random effects
# by REML, Knapp-Hartung confidence intervals (test = "knha"), as in the
# original script. Conventional Wald intervals are reported alongside because
# the dissertation did not declare which one it used.
#
# Reproduction targets (dissertation, uncorrected data):
#   pooled prevalence 0.372, KH CI 0.298 to 0.454 (Wald on these data 0.301 to 0.450),
#   tau2 0.53, I2 95%; trials RR 1.52, KH 0.72 to 3.22, Wald 1.22 to 1.90;
#   trim-and-fill adjusted logit -0.4669 (38.5%); Weak excluded 34.7%.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(metafor)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

out_dir <- root_path("output", "frequentist")
fig_dir <- root_path("output", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

prevalence <- read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)
trials     <- read_csv(root_path("data", "derived", "trials.csv"), show_col_types = FALSE)

# -------------------------------
# Helpers
# -------------------------------
# Fit the registered logit random-effects model and return one summary row
# each for the KH and Wald intervals, with the prediction interval.
fit_prevalence <- function(d, label, events = "events", n = "n_assessed") {
  es <- escalc(measure = "PLO", xi = d[[events]], ni = d[[n]])
  fits <- list(knha = rma(yi, vi, data = es, method = "REML", test = "knha"),
               wald = rma(yi, vi, data = es, method = "REML", test = "z"))
  bind_rows(lapply(names(fits), function(nm) {
    m <- fits[[nm]]; p <- predict(m, transf = transf.ilogit)
    data.frame(model = label, interval = nm, k = m$k,
               estimate = p$pred, ci_low = p$ci.lb, ci_high = p$ci.ub,
               pi_low = p$pi.lb, pi_high = p$pi.ub,
               tau2 = m$tau2, tau = sqrt(m$tau2), I2 = m$I2, Q = m$QE, Q_p = m$QEp,
               stringsAsFactors = FALSE)
  })) %>% mutate(fit = list(fits$knha))
}

fit_trials <- function(d, label, measure = "RR",
                       ai = "events_intervention", n1i = "n_intervention",
                       ci = "events_control", n2i = "n_control") {
  es <- escalc(measure = measure, ai = d[[ai]], n1i = d[[n1i]], ci = d[[ci]], n2i = d[[n2i]])
  fits <- list(knha = rma(yi, vi, data = es, method = "REML", test = "knha"),
               wald = rma(yi, vi, data = es, method = "REML", test = "z"))
  bind_rows(lapply(names(fits), function(nm) {
    m <- fits[[nm]]
    data.frame(model = label, measure = measure, interval = nm, k = m$k,
               estimate = exp(m$b[1]), ci_low = exp(m$ci.lb), ci_high = exp(m$ci.ub),
               p_value = m$pval, tau2 = m$tau2, I2 = m$I2, Q = m$QE, Q_p = m$QEp,
               stringsAsFactors = FALSE)
  }))
}

# -------------------------------
# 1. Prevalence: as submitted, corrected, and by analysis set
# -------------------------------
prev_results <- bind_rows(
  fit_prevalence(prevalence, "legacy_k23_as_submitted", "events_raw", "n_assessed_raw"),
  fit_prevalence(prevalence, "legacy_k23_corrected"),
  fit_prevalence(filter(prevalence, in_point_relaxed_set), "point_relaxed_k14"),
  fit_prevalence(filter(prevalence, in_primary_set), "strict_point_primary"),
  fit_prevalence(filter(prevalence, in_period_set), "period_k9"),
  fit_prevalence(filter(prevalence, quality_rating != "Weak"), "legacy_excluding_weak"),
  fit_prevalence(filter(prevalence, in_primary_set, quality_rating != "Weak"),
                 "strict_point_excluding_weak")
)
legacy_fit    <- prev_results$fit[[which(prev_results$model == "legacy_k23_as_submitted")[1]]]
corrected_fit <- prev_results$fit[[which(prev_results$model == "legacy_k23_corrected")[1]]]
prev_table    <- prev_results %>% select(-fit)
write_csv(prev_table, file.path(out_dir, "prevalence_registered_models.csv"))

# Reproduction checks against the dissertation (uncorrected data)
rep <- prev_table %>% filter(model == "legacy_k23_as_submitted")
check(abs(rep$estimate[rep$interval == "knha"] - 0.372) < 0.0015,
      "legacy pooled prevalence does not reproduce 0.372")
check(abs(rep$ci_low[rep$interval == "knha"] - 0.298) < 0.0015 &&
        abs(rep$ci_high[rep$interval == "knha"] - 0.454) < 0.0015,
      "legacy KH interval does not reproduce 0.298 to 0.454")
# The marker's Wald interval (0.298 to 0.447) came from a dataset rebuilt from
# the dissertation's Table 1 and forest plot rather than from this file; on
# these data the Wald interval is 0.301 to 0.450 and is reported, not asserted.
check(abs(rep$tau2[1] - 0.53) < 0.01, "legacy tau2 does not reproduce 0.53")

# -------------------------------
# 2. Registered subgroup and moderator analyses (legacy set, corrected data)
# -------------------------------
es23 <- escalc(measure = "PLO", xi = events, ni = n_assessed, data = prevalence)
mod_fit <- function(formula, label) {
  m <- rma(yi, vi, mods = formula, data = es23, method = "REML", test = "knha")
  coefs <- data.frame(model = label, term = rownames(m$b), estimate = as.vector(m$b),
                      se = m$se, ci_low = m$ci.lb, ci_high = m$ci.ub, p_value = m$pval,
                      stringsAsFactors = FALSE)
  omnibus <- data.frame(model = label, QM = m$QM, QM_df1 = m$QMdf[1], QM_df2 = m$QMdf[2],
                        QM_p = m$QMp, tau2_residual = m$tau2, R2 = m$R2, stringsAsFactors = FALSE)
  list(coefs = coefs, omnibus = omnibus)
}
mods <- list(
  mod_fit(~ outcome_basis, "outcome_basis"),
  mod_fit(~ design, "design"),
  mod_fit(~ region, "region"),
  mod_fit(~ timepoint_months, "timepoint_months")
)
write_csv(bind_rows(lapply(mods, `[[`, "coefs")), file.path(out_dir, "moderators_registered_coefficients.csv"))
write_csv(bind_rows(lapply(mods, `[[`, "omnibus")), file.path(out_dir, "moderators_registered_omnibus.csv"))

# Subgroup estimates by outcome basis (separate models, as in the original script)
subgroups <- bind_rows(lapply(c("point", "period"), function(b) {
  fit_prevalence(filter(prevalence, outcome_basis == b), paste0("subgroup_", b)) %>% select(-fit)
}))
write_csv(subgroups, file.path(out_dir, "prevalence_subgroups_outcome_basis.csv"))

# -------------------------------
# 3. Publication-bias diagnostics on the registered model
# -------------------------------
# Reported for continuity with the registration. For proportions the funnel
# plot and Egger test are unreliable because the standard error is a function
# of the estimate (Hunter et al. 2014); the trim-and-fill p-value tests whether
# the adjusted logit differs from zero (a proportion of 50%), not bias.
bias_summary <- function(m, label) {
  eg <- regtest(m)
  tf <- trimfill(m)
  data.frame(model = label, k = m$k,
             egger_t = eg$zval, egger_df = eg$dfs, egger_p = eg$pval,
             trimfill_k0 = tf$k0, trimfill_side = tf$side,
             adjusted_logit = as.vector(tf$b), adjusted_se = tf$se,
             adjusted_prevalence = transf.ilogit(as.vector(tf$b)),
             adjusted_ci_low = transf.ilogit(tf$ci.lb),
             adjusted_ci_high = transf.ilogit(tf$ci.ub),
             adjusted_p_vs_50pct = tf$pval, stringsAsFactors = FALSE)
}
bias <- bind_rows(bias_summary(legacy_fit, "legacy_k23_as_submitted"),
                  bias_summary(corrected_fit, "legacy_k23_corrected"))
write_csv(bias, file.path(out_dir, "publication_bias_registered.csv"))
check(abs(bias$adjusted_logit[1] - (-0.4669)) < 0.002, "trim-and-fill adjusted logit does not reproduce -0.4669")

pdf(file.path(fig_dir, "funnel_registered_logit.pdf"), width = 6, height = 5)
funnel(corrected_fit, xlab = "Logit employment prevalence", main = NULL)
dev.off()

# -------------------------------
# 4. Trials: IPS added to TAU versus TAU (k = 3) and sensitivities
# -------------------------------
prim <- filter(trials, analysis_set == "primary")
k4   <- filter(trials, analysis_set %in% c("primary", "sensitivity_k4"))
trial_results <- bind_rows(
  fit_trials(prim, "primary_k3_available_case", "RR"),
  fit_trials(prim, "primary_k3_available_case", "OR"),
  fit_trials(prim, "primary_k3_missing_not_employed", "RR",
             n1i = "n_intervention_full", n2i = "n_control_full"),
  fit_trials(k4, "sensitivity_k4_available_case", "RR"),
  fit_trials(k4, "sensitivity_k4_missing_not_employed", "RR",
             n1i = "n_intervention_full", n2i = "n_control_full")
)
write_csv(trial_results, file.path(out_dir, "trials_registered_models.csv"))

rr <- trial_results %>% filter(model == "primary_k3_available_case", measure == "RR")
check(abs(rr$estimate[1] - 1.52) < 0.01, "primary RR does not reproduce 1.52")
check(abs(rr$ci_low[rr$interval == "knha"] - 0.72) < 0.01 &&
        abs(rr$ci_high[rr$interval == "knha"] - 3.22) < 0.01, "KH interval does not reproduce 0.72 to 3.22")
check(abs(rr$ci_low[rr$interval == "wald"] - 1.22) < 0.01 &&
        abs(rr$ci_high[rr$interval == "wald"] - 1.90) < 0.01, "Wald interval does not reproduce 1.22 to 1.90")

# Per-trial effects for the results table
per_trial <- escalc(measure = "RR", ai = events_intervention, n1i = n_intervention,
                    ci = events_control, n2i = n_control, data = k4) %>%
  as.data.frame() %>%
  transmute(study_id_clean, study_label, analysis_set,
            events_intervention, n_intervention, events_control, n_control,
            rr = exp(yi), rr_ci_low = exp(yi - 1.96 * sqrt(vi)), rr_ci_high = exp(yi + 1.96 * sqrt(vi)),
            log_rr = yi, se_log_rr = sqrt(vi))
write_csv(per_trial, file.path(out_dir, "trials_per_study_effects.csv"))

# Other controlled comparisons: descriptive RRs only, never pooled
others <- filter(trials, !analysis_set %in% c("primary", "sensitivity_k4"))
others_es <- escalc(measure = "RR", ai = events_intervention, n1i = n_intervention,
                    ci = events_control, n2i = n_control, data = others, add = 0.5, to = "only0") %>%
  as.data.frame() %>%
  transmute(study_id_clean, study_label, analysis_set, comparison_type,
            intervention_name, comparator, events_intervention, n_intervention,
            events_control, n_control, rr = exp(yi),
            rr_ci_low = exp(yi - 1.96 * sqrt(vi)), rr_ci_high = exp(yi + 1.96 * sqrt(vi)))
write_csv(others_es, file.path(out_dir, "trials_other_comparisons_descriptive.csv"))

cat("\nRegistered prevalence models:\n")
print(prev_table %>% select(model, interval, k, estimate, ci_low, ci_high, pi_low, pi_high, tau2, I2) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nTrial models:\n")
print(trial_results %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nPublication bias (registered model):\n")
print(bias %>% mutate(across(where(is.numeric), ~ round(.x, 4))) %>% as.data.frame())
