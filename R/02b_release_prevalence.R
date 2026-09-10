# ===============================================
# 02b - Release-candidate prevalence models (A33 scope, A25 measurement rule)
# ===============================================
# Same likelihood and priors as R/02_bayes_prevalence.R (A2): exact binomial
# likelihood with a study-level normal random effect on the logit scale,
# mu ~ Normal(0, 1.5), tau ~ Half-Normal(0, 1); sensitivities at 0.5 and 2.
# Inputs come from data/derived/release_prevalence.csv (R/00b). Sets:
#   rc_a33_point_primary_hn1        A33 point-prevalence primary set
#   rc_a33_point_prior_hn05 / hn2   tau prior sensitivities
#   rc_a33_point_duration_sens_hn1  adds the duration-sensitivity report(s)
#   rc_a33_period_hn1               A33 period set (secondary estimand)
#   rc_a33_point_excl_weak_hn1      EPHPP Weak removed
#   rc_a33_point_missing_bound_hn1  n_total denominator (missing = not employed)
#   rc_a33_point_normal_normal_hn1  logit normal-normal approximation
#   rc_loo_prev_<study>             leave-one-out on the primary set
# The strict registered set (A24) is empty by construction (A35) and is
# recorded as "not estimable" without a model (A26). The variable-window
# report (Turner 2019) is reported as a single-study binomial interval only.
# Outputs: output/release/bayesian/ and output/release/figures/.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(brms)
  library(posterior)
  library(ggplot2)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

out_dir <- root_path("output", "release", "bayesian")
fig_dir <- root_path("output", "release", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

rp <- read_csv(root_path("data", "derived", "release_prevalence.csv"), show_col_types = FALSE)

prev_formula <- bf(events | trials(n_assessed) ~ 1 + (1 | study_id_clean))
prev_prior   <- function(tau_scale) {
  c(prior(normal(0, 1.5), class = "Intercept"),
    set_prior(paste0("normal(0, ", tau_scale, ")"), class = "sd"))
}

run_prev <- function(d, tag, tau_scale = 1, events_col = "events", n_col = "n_assessed") {
  check(nrow(d) >= 3, paste0(tag, ": fewer than three studies; the sparse-pool rule (A26) forbids a hierarchical fit"))
  dd <- d %>% transmute(study_id_clean, events = .data[[events_col]], n_assessed = .data[[n_col]])
  res  <- fit_gated(prev_formula, dd, prev_prior(tau_scale), binomial(), tag)
  dr   <- pooled_draws(res$fit)
  pred <- prediction_draws(dr$mu, dr$tau)
  tau_q <- quantile(dr$tau, c(0.025, 0.5, 0.975), names = FALSE)
  # Model-implied mean across exchangeable settings, E[logistic(mu + tau Z)],
  # from the joint draws (plan Section 7): reported beside the median-setting
  # prevalence plogis(mu).
  mean_setting <- plogis(pred)
  summary <- summarise_effect(dr$mu, plogis, tag, k = nrow(dd)) %>%
    mutate(tau_scale = tau_scale,
           n_assessed_total = sum(dd$n_assessed), events_total = sum(dd$events),
           tau_median = tau_q[2], tau_low = tau_q[1], tau_high = tau_q[3],
           pi_low = quantile(mean_setting, 0.025, names = FALSE),
           pi_high = quantile(mean_setting, 0.975, names = FALSE),
           mean_setting_estimate = mean(mean_setting),
           p_below_50 = mean(plogis(dr$mu) < 0.5))
  g <- with_local_seed(mcmc_settings$seed, pp_check(res$fit, type = "intervals", ndraws = 200)) +
    labs(title = tag, x = "Study", y = "Employed (count)") + theme_minimal()
  ggsave(file.path(fig_dir, paste0("ppc_", tag, ".pdf")), g, width = 7, height = 4)
  list(summary = summary, diag = res$diag, fit = res$fit, draws = dr)
}

primary  <- filter(rp, in_a33_point_primary)
duration <- filter(rp, in_a33_point_duration_sens)
period   <- filter(rp, in_a33_period)
strict   <- filter(rp, in_strict_a24_point)
variable <- filter(rp, in_a33_variable_window)

results <- list(
  rc_a33_point_primary_hn1       = run_prev(primary, "rc_a33_point_primary_hn1", 1),
  rc_a33_point_prior_hn05        = run_prev(primary, "rc_a33_point_prior_hn05", 0.5),
  rc_a33_point_prior_hn2         = run_prev(primary, "rc_a33_point_prior_hn2", 2),
  rc_a33_point_duration_sens_hn1 = run_prev(duration, "rc_a33_point_duration_sens_hn1", 1),
  rc_a33_period_hn1              = run_prev(period, "rc_a33_period_hn1", 1),
  rc_a33_point_excl_weak_hn1     = run_prev(filter(primary, quality_rating != "Weak"), "rc_a33_point_excl_weak_hn1", 1),
  rc_a33_point_missing_bound_hn1 = run_prev(primary, "rc_a33_point_missing_bound_hn1", 1, n_col = "n_total")
)

# Normal-normal logit approximation on the primary set
es <- metafor::escalc(measure = "PLO", xi = primary$events, ni = primary$n_assessed)
nn_data <- data.frame(study_id_clean = primary$study_id_clean, yi = es$yi, sei = sqrt(es$vi))
nn_res  <- fit_gated(bf(yi | se(sei) ~ 1 + (1 | study_id_clean)), nn_data,
                     prev_prior(1), gaussian(), "rc_a33_point_normal_normal_hn1")
nn_dr   <- pooled_draws(nn_res$fit)
nn_pred <- prediction_draws(nn_dr$mu, nn_dr$tau)
nn_tau  <- quantile(nn_dr$tau, c(0.025, 0.5, 0.975), names = FALSE)
nn_summary <- summarise_effect(nn_dr$mu, plogis, "rc_a33_point_normal_normal_hn1", k = nrow(primary)) %>%
  mutate(tau_scale = 1, n_assessed_total = sum(primary$n_assessed), events_total = sum(primary$events),
         tau_median = nn_tau[2], tau_low = nn_tau[1], tau_high = nn_tau[3],
         pi_low = quantile(plogis(nn_pred), 0.025, names = FALSE),
         pi_high = quantile(plogis(nn_pred), 0.975, names = FALSE),
         mean_setting_estimate = mean(plogis(nn_pred)),
         p_below_50 = mean(plogis(nn_dr$mu) < 0.5))

# Leave-one-out on the primary set
loo_res <- lapply(primary$study_id_clean, function(s) {
  r <- run_prev(filter(primary, study_id_clean != s), paste0("rc_loo_prev_", s), 1)
  r$summary <- r$summary %>% mutate(left_out = s)
  r
})
write_csv(bind_rows(lapply(loo_res, `[[`, "summary")), file.path(out_dir, "rc_prevalence_leave_one_out.csv"))

# Study-level shrunken estimates from the primary model
primary_fit <- results$rc_a33_point_primary_hn1$fit
re <- as_draws_df(primary_fit)
study_cols <- grep("^r_study_id_clean\\[", names(re), value = TRUE)
study_effects <- bind_rows(lapply(study_cols, function(col) {
  theta <- re$b_Intercept + re[[col]]
  q <- quantile(plogis(theta), c(0.025, 0.5, 0.975), names = FALSE)
  data.frame(study_id_clean = sub("^r_study_id_clean\\[(.*),Intercept\\]$", "\\1", col),
             shrunken = q[2], shrunken_low = q[1], shrunken_high = q[3])
})) %>%
  left_join(primary %>% select(study_id_clean, study_label, region, events, n_assessed, quality_rating,
                               conflict_grade),
            by = "study_id_clean") %>%
  mutate(observed = events / n_assessed,
         observed_low = qbeta(0.025, events + 0.5, n_assessed - events + 0.5),
         observed_high = qbeta(0.975, events + 0.5, n_assessed - events + 0.5))
write_csv(study_effects, file.path(out_dir, "rc_prevalence_primary_study_effects.csv"))

# Strict registered comparison (A24, A26): no model. Recorded explicitly.
strict_record <- data.frame(
  set = "strict_a24_point", k = nrow(strict),
  status = if (nrow(strict) == 0) "not estimable: no prevalence result satisfies the registered population rule from the publication" else "see model",
  studies = paste(strict$study_id_clean, collapse = ", "), stringsAsFactors = FALSE)
# Variable-window report(s): single-study Jeffreys intervals, no pooling.
variable_record <- variable %>%
  transmute(study_id_clean, study_label, events, n_assessed, observed = events / n_assessed,
            observed_low = qbeta(0.025, events + 0.5, n_assessed - events + 0.5),
            observed_high = qbeta(0.975, events + 0.5, n_assessed - events + 0.5),
            note = "Variable follow-up (range 2 to 44 months); not pooled (A36)")
write_csv(strict_record, file.path(out_dir, "rc_prevalence_strict_record.csv"))
write_csv(variable_record, file.path(out_dir, "rc_prevalence_variable_window.csv"))

summary_table <- bind_rows(c(lapply(results, `[[`, "summary"), list(nn_summary)))
diag_table    <- bind_rows(c(lapply(results, `[[`, "diag"), list(nn_res$diag), lapply(loo_res, `[[`, "diag")))
write_csv(summary_table, file.path(out_dir, "rc_prevalence_summary.csv"))
write_csv(diag_table,    file.path(out_dir, "rc_prevalence_diagnostics.csv"))
pd <- results$rc_a33_point_primary_hn1$draws
write_csv(data.frame(model_id = "rc_a33_point_primary_hn1", .chain = pd$chain, .iteration = pd$iteration,
                     .draw = pd$draw, mu = pd$mu, tau = pd$tau),
          file.path(out_dir, "rc_prevalence_primary_draws.csv"))

check(all(diag_table$passes_gate), paste("release prevalence models failing the convergence gate:",
                                         paste(diag_table$model[!diag_table$passes_gate], collapse = ", ")))
cat("\nRelease-candidate prevalence models:\n")
print(summary_table %>% select(model, k, estimate, ci_low, ci_high, pi_low, pi_high, tau_median) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nStrict registered set:", strict_record$status, "\n")
