# ===============================================
# 03b - Release-candidate IPS trial models (A33 scope, A25 six-month interval)
# ===============================================
# Same models and priors as R/03_bayes_trials.R (A2): normal-normal log-RR
# random-effects model, mu ~ Normal(0, 1), tau ~ Half-Normal(0, 0.5)
# (sensitivities 0.25 and 1); arm-level binomial-logit OR model; bridge-
# sampling Bayes factors for the directional half-normal H1 at five scales,
# with the quadrature Bayes factor (utils bf_re) computed on the same inputs
# and compared against the A18 two-significant-figure criterion.
# Inputs: data/derived/release_trials.csv and release_trial_arms.csv (R/00b).
# The A33 pool is IPS versus usual care, employment attained during months 0
# to 6 (k = 3). The strict registered comparison (A24) is Killackey 2008
# alone and is reported as a single trial without a between-study model (A26).
# A27: absolute-risk scenarios come from the OR model, p1 = logistic(logit(p0)
# + beta), at control risks 0.20, 0.30, 0.43 and 0.50, per draw. The full
# joint posterior of the OR model is exported.
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

rt   <- read_csv(root_path("data", "derived", "release_trials.csv"), show_col_types = FALSE)
arms <- read_csv(root_path("data", "derived", "release_trial_arms.csv"), show_col_types = FALSE)
m_null <- log(1.25)

log_rr_data <- function(d, n1 = "n_intervention", n2 = "n_control") {
  es <- metafor::escalc(measure = "RR", ai = d$events_intervention, n1i = d[[n1]],
                        ci = d$events_control, n2i = d[[n2]])
  data.frame(study_id_clean = d$study_id_clean, study_label = d$study_label,
             yi = as.numeric(es$yi), sei = sqrt(as.numeric(es$vi)))
}
pool   <- filter(rt, in_trial_a33_k3_0to6)
strict <- filter(rt, in_trial_strict)
check(nrow(pool) >= 3, "A33 trial pool has fewer than three trials; the sparse-pool rule (A26) applies")

input_sets <- list(
  rc_a33_k3         = log_rr_data(pool),
  rc_a33_k3_missing = log_rr_data(pool, "n_intervention_full", "n_control_full")
)
write_csv(bind_rows(input_sets, .id = "input_set"), file.path(out_dir, "rc_trials_input_effects.csv"))

rr_formula <- bf(yi | se(sei) ~ 1 + (1 | study_id_clean))
rr_prior   <- function(tau_scale) {
  c(prior(normal(0, 1), class = "Intercept"),
    set_prior(paste0("normal(0, ", tau_scale, ")"), class = "sd"))
}
run_rr <- function(d, tag, tau_scale = 0.5) {
  res  <- fit_gated(rr_formula, d, rr_prior(tau_scale), gaussian(), tag)
  dr   <- pooled_draws(res$fit)
  pred <- prediction_draws(dr$mu, dr$tau)
  tau_q <- quantile(dr$tau, c(0.025, 0.5, 0.975), names = FALSE)
  ni_mu <- null_interval_probs(dr$mu, m_null)
  ni_new <- null_interval_probs(pred, m_null)
  summary <- summarise_effect(dr$mu, exp, tag, k = nrow(d)) %>%
    mutate(tau_scale = tau_scale,
           tau_median = tau_q[2], tau_low = tau_q[1], tau_high = tau_q[3],
           pi_low = quantile(exp(pred), 0.025, names = FALSE),
           pi_high = quantile(exp(pred), 0.975, names = FALSE),
           p_rr_gt_1 = mean(dr$mu > 0),
           p_rr_below_0.80 = ni_mu[["p_below"]],
           p_rr_in_null = ni_mu[["p_inside"]],
           p_rr_above_1.25 = ni_mu[["p_above"]],
           p_new_rr_gt_1 = mean(pred > 0),
           p_new_rr_below_0.80 = ni_new[["p_below"]],
           p_new_rr_in_null = ni_new[["p_inside"]],
           p_new_rr_above_1.25 = ni_new[["p_above"]])
  list(summary = summary, diag = res$diag, fit = res$fit, draws = dr)
}

results <- list(
  rc_trial_a33_k3_hn05    = run_rr(input_sets$rc_a33_k3, "rc_trial_a33_k3_hn05", 0.5),
  rc_trial_a33_k3_hn025   = run_rr(input_sets$rc_a33_k3, "rc_trial_a33_k3_hn025", 0.25),
  rc_trial_a33_k3_hn1     = run_rr(input_sets$rc_a33_k3, "rc_trial_a33_k3_hn1", 1),
  rc_trial_a33_k3_missing = run_rr(input_sets$rc_a33_k3_missing, "rc_trial_a33_k3_missing_hn05", 0.5)
)
loo_summaries <- list()
for (s in pool$study_id_clean) {
  tag <- paste0("rc_loo_trial_", s)
  r <- run_rr(filter(input_sets$rc_a33_k3, study_id_clean != s), tag, 0.5)
  r$summary <- r$summary %>% mutate(left_out = s)
  results[[tag]] <- r
  loo_summaries[[tag]] <- r$summary
}
write_csv(bind_rows(loo_summaries), file.path(out_dir, "rc_trials_leave_one_out.csv"))

# -------------------------------
# Arm-level binomial-logit model (OR) with the full joint posterior exported
# -------------------------------
exact_formula <- bf(events | trials(n_analysed) ~ 0 + study_id_clean + arm_ips + (0 + arm_ips | study_id_clean))
exact_prior <- c(prior(normal(0, 2), class = "b"),
                 prior(normal(0, 1), class = "b", coef = "arm_ips"),
                 prior(normal(0, 0.5), class = "sd"))
ex_res <- fit_gated(exact_formula, as.data.frame(arms), exact_prior, binomial(), "rc_exact_binomial_a33_k3")
ex_d   <- as_draws_df(ex_res$fit)
b      <- ex_d$b_arm_ips
ex_tau <- ex_d[[grep("^sd_", names(ex_d), value = TRUE)[1]]]
p0_pooled <- with(filter(arms, arm_ips == 0), sum(events) / sum(n_analysed))
rr_draws <- exp(b) / (1 - p0_pooled + p0_pooled * exp(b))
rr_q     <- quantile(rr_draws, c(0.025, 0.5, 0.975), names = FALSE)
g <- with_local_seed(mcmc_settings$seed, pp_check(ex_res$fit, type = "intervals", ndraws = 200)) +
  labs(title = "rc_exact_binomial_a33_k3", x = "Trial arm", y = "Employed (count)") + theme_minimal()
ggsave(file.path(fig_dir, "ppc_rc_exact_binomial_a33_k3.pdf"), g, width = 7, height = 4)
ex_summary <- summarise_effect(b, exp, "rc_exact_binomial_a33_k3", k = length(unique(arms$study_id_clean))) %>%
  rename(or = estimate, or_low = ci_low, or_high = ci_high) %>%
  mutate(p_or_gt_1 = mean(b > 0), tau_median = median(ex_tau),
         tau_low = quantile(ex_tau, 0.025, names = FALSE), tau_high = quantile(ex_tau, 0.975, names = FALSE),
         pooled_control_risk = p0_pooled, rr_at_p0 = rr_q[2], rr_at_p0_low = rr_q[1], rr_at_p0_high = rr_q[3])
write_csv(ex_summary, file.path(out_dir, "rc_trials_exact_binomial.csv"))
# Full joint posterior (all parameters, with chain and iteration identifiers)
joint <- ex_d %>% as.data.frame() %>% select(.chain, .iteration, .draw, everything()) %>%
  select(-any_of(c("lprior", "lp__")))
write_csv(joint, file.path(out_dir, "rc_exact_binomial_a33_k3_joint_draws.csv"))

# A27 absolute-risk scenarios from the OR model (conditional on p0 and on
# the OR-model assumptions; not the RR model in other units). Impossible
# draws cannot arise on the logit scale, so nothing is clipped or discarded.
p0_grid <- c(0.20, 0.30, 0.43, 0.50)
abs_rows <- bind_rows(lapply(p0_grid, function(p0) {
  p1 <- plogis(qlogis(p0) + b)
  rd <- p1 - p0
  # New-setting version: treatment effect for a new trial, beta + tau * z
  b_new <- prediction_draws(b, ex_tau)
  p1_new <- plogis(qlogis(p0) + b_new)
  data.frame(control_risk = p0,
             treated_risk = median(p1), treated_low = quantile(p1, 0.025, names = FALSE),
             treated_high = quantile(p1, 0.975, names = FALSE),
             risk_difference = median(rd), rd_low = quantile(rd, 0.025, names = FALSE),
             rd_high = quantile(rd, 0.975, names = FALSE), p_rd_gt_0 = mean(rd > 0),
             new_setting_treated_low = quantile(p1_new, 0.025, names = FALSE),
             new_setting_treated_high = quantile(p1_new, 0.975, names = FALSE))
}))
write_csv(abs_rows, file.path(out_dir, "rc_absolute_risk_scenarios.csv"))

# -------------------------------
# Bayes factors: bridge sampling and quadrature on the same inputs
# -------------------------------
s_values <- c(primary_log2 = log(2), frederick_published = log(1.63), bond2015 = log(1.69),
              bond2016 = log(1.96), modini2016 = log(2.40))
h0_res <- fit_gated(bf(yi | se(sei) ~ 0 + (1 | study_id_clean)), input_sets$rc_a33_k3,
                    prior(normal(0, 0.5), class = "sd"), gaussian(), "rc_bf_h0_hn05", save_all_pars = TRUE)
h0_bridge <- with_local_seed(mcmc_settings$seed, bridgesampling::bridge_sampler(h0_res$fit, silent = TRUE))
bridge_out <- lapply(names(s_values), function(nm) {
  s <- s_values[[nm]]
  h1_res <- fit_gated(bf(yi | se(sei) ~ 0 + Intercept + (1 | study_id_clean)), input_sets$rc_a33_k3,
                      c(set_prior(paste0("normal(0, ", s, ")"), class = "b", lb = 0),
                        prior(normal(0, 0.5), class = "sd")),
                      gaussian(), paste0("rc_bf_h1_", nm), save_all_pars = TRUE)
  h1_bridge <- with_local_seed(mcmc_settings$seed, bridgesampling::bridge_sampler(h1_res$fit, silent = TRUE))
  bfv <- bridgesampling::bf(h1_bridge, h0_bridge)
  quad <- bf_re(input_sets$rc_a33_k3$yi, input_sets$rc_a33_k3$sei, s, 0.5)
  list(row = data.frame(h1_scale_label = nm, s = s, rr_scale = exp(s), tau_scale = 0.5,
                        bf_bridge = as.numeric(bfv$bf), bf_quadrature = quad,
                        agree_2sf = signif(as.numeric(bfv$bf), 2) == signif(quad, 2),
                        discrepancy_pct = 100 * (as.numeric(bfv$bf) - quad) / quad,
                        h1_error_pct = bridgesampling::error_measures(h1_bridge)$percentage,
                        h0_error_pct = bridgesampling::error_measures(h0_bridge)$percentage,
                        category_quadrature = bf_category(quad),
                        stringsAsFactors = FALSE),
       diag = h1_res$diag)
})
bridge_rows <- bind_rows(lapply(bridge_out, `[[`, "row"))
write_csv(bridge_rows, file.path(out_dir, "rc_trials_bayes_factors.csv"))
# Robustness region over s for the quadrature BF (tau scale 0.5)
rr_s <- robustness_region_s(function(s) bf_re(input_sets$rc_a33_k3$yi, input_sets$rc_a33_k3$sei, s, 0.5), log(2))
write_csv(data.frame(tau_scale = 0.5, s_primary = log(2), category_at_primary = rr_s$category,
                     s_min = rr_s$s_min, s_max = rr_s$s_max, hits_grid_edge = rr_s$hits_grid_edge),
          file.path(out_dir, "rc_trials_bf_robustness_region.csv"))

# -------------------------------
# Strict registered comparison: single trial, no between-study model (A26)
# -------------------------------
strict_rows <- strict %>%
  transmute(study_id_clean, study_label, timepoint_months, events_intervention, n_intervention,
            events_control, n_control, rr = rr_computed) %>%
  mutate(log_rr = log(rr),
         se_log_rr = sqrt(1 / events_intervention - 1 / n_intervention + 1 / events_control - 1 / n_control),
         rr_low = exp(log_rr - 1.96 * se_log_rr), rr_high = exp(log_rr + 1.96 * se_log_rr),
         note = "Strict registered population rule (A24): single eligible trial; Wald interval on the log RR; no pooling (A26)")
write_csv(strict_rows, file.path(out_dir, "rc_trials_strict_record.csv"))

summary_table <- bind_rows(lapply(results, `[[`, "summary"))
diag_table    <- bind_rows(c(lapply(results, `[[`, "diag"), list(ex_res$diag, h0_res$diag),
                             lapply(bridge_out, `[[`, "diag")))
write_csv(summary_table, file.path(out_dir, "rc_trials_summary.csv"))
write_csv(diag_table,    file.path(out_dir, "rc_trials_diagnostics.csv"))
pd <- results$rc_trial_a33_k3_hn05$draws
write_csv(data.frame(model_id = "rc_trial_a33_k3_hn05", .chain = pd$chain, .iteration = pd$iteration,
                     .draw = pd$draw, mu = pd$mu, tau = pd$tau),
          file.path(out_dir, "rc_trials_primary_draws.csv"))

check(all(diag_table$passes_gate), paste("release trial models failing the convergence gate:",
                                         paste(diag_table$model[!diag_table$passes_gate], collapse = ", ")))
cat("\nRelease-candidate trial models (RR scale):\n")
print(summary_table %>% select(model, k, tau_scale, estimate, ci_low, ci_high, pi_low, pi_high, p_rr_gt_1) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nOR model and absolute-risk scenarios:\n")
print(ex_summary %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
print(abs_rows %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nBayes factors (bridge vs quadrature):\n")
print(bridge_rows %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nStrict comparison:\n")
print(strict_rows %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
