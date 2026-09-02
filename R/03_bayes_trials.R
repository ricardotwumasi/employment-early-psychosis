# ===============================================
# 03 - Bayesian synthesis of the IPS trials
# ===============================================
# Primary estimand: IPS added to treatment as usual versus treatment as usual,
# competitive employment, risk ratio, k = 3 (Erickson 2021, Killackey 2008,
# Killackey 2019), available-case 2x2 counts.
#
# Primary model: normal-normal random-effects model on the log risk ratio,
#   y_i ~ N(theta_i, s_i^2),  theta_i ~ N(mu, tau^2)
#   mu ~ Normal(0, 1)  (95% prior mass on RR 0.14 to 7.4)
#   tau ~ Half-Normal(0, 0.5)  (Rover et al. 2021 scale for log ratio measures)
# Sensitivities: tau scale 0.25 and 1; leave-one-out; k = 4 adding
# Nuechterlein 2020; missing-as-not-employed denominators; an exact binomial
# logistic model on the arm-level counts (pooled odds ratio).
# Bayes factors by bridge sampling for the half-normal H1 models used in 05.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(brms)
  library(posterior)
  library(ggplot2)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

out_dir <- root_path("output", "bayesian")
fig_dir <- root_path("output", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

trials     <- read_csv(root_path("data", "derived", "trials.csv"), show_col_types = FALSE)
trial_arms <- read_csv(root_path("data", "derived", "trial_arms.csv"), show_col_types = FALSE)

# Null interval on the log RR scale (frozen): |log RR| < log(1.25)
m_null <- log(1.25)

# -------------------------------
# Per-trial log risk ratios
# -------------------------------
log_rr_data <- function(d, n1 = "n_intervention", n2 = "n_control") {
  es <- metafor::escalc(measure = "RR", ai = d$events_intervention, n1i = d[[n1]],
                        ci = d$events_control, n2i = d[[n2]])
  data.frame(study_id_clean = d$study_id_clean, study_label = d$study_label,
             yi = as.numeric(es$yi), sei = sqrt(as.numeric(es$vi)))
}
prim <- filter(trials, analysis_set == "primary")
k4   <- filter(trials, analysis_set %in% c("primary", "sensitivity_k4"))

input_sets <- list(
  primary_k3            = log_rr_data(prim),
  primary_k3_missing    = log_rr_data(prim, "n_intervention_full", "n_control_full"),
  sensitivity_k4        = log_rr_data(k4),
  sensitivity_k4_missing = log_rr_data(k4, "n_intervention_full", "n_control_full")
)
write_csv(bind_rows(input_sets, .id = "input_set"), file.path(out_dir, "trials_input_effects.csv"))

# -------------------------------
# Normal-normal random-effects models
# -------------------------------
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
  primary_k3_hn05           = run_rr(input_sets$primary_k3, "primary_k3_hn05", 0.5),
  sens_prior_hn025          = run_rr(input_sets$primary_k3, "sens_prior_hn025", 0.25),
  sens_prior_hn1            = run_rr(input_sets$primary_k3, "sens_prior_hn1", 1),
  sens_k4_hn05              = run_rr(input_sets$sensitivity_k4, "sens_k4_hn05", 0.5),
  sens_k3_missing_not_employed = run_rr(input_sets$primary_k3_missing, "sens_k3_missing_not_employed", 0.5),
  sens_k4_missing_not_employed = run_rr(input_sets$sensitivity_k4_missing, "sens_k4_missing_not_employed", 0.5)
)
for (s in prim$study_id_clean) {
  tag <- paste0("loo_", s)
  results[[tag]] <- run_rr(filter(input_sets$primary_k3, study_id_clean != s), tag, 0.5)
}

# -------------------------------
# Exact binomial logistic model on arm-level counts (pooled odds ratio)
# -------------------------------
# Fixed trial intercepts, a common treatment effect and a random treatment
# effect by trial (Simmonds and Higgins form). Reported as an odds ratio, with
# an RR-scale summary at the pooled control risk for comparison only.
exact_formula <- bf(events | trials(n_analysed) ~ 0 + study_id_clean + arm_ips + (0 + arm_ips | study_id_clean))
exact_prior <- c(prior(normal(0, 2), class = "b"),
                 prior(normal(0, 1), class = "b", coef = "arm_ips"),
                 prior(normal(0, 0.5), class = "sd"))

run_exact <- function(arms, tag) {
  res <- fit_gated(exact_formula, as.data.frame(arms), exact_prior, binomial(), tag)
  d   <- as_draws_df(res$fit)
  b   <- d$b_arm_ips
  tau <- d[[grep("^sd_", names(d), value = TRUE)[1]]]
  p0  <- with(filter(arms, arm_ips == 0), sum(events) / sum(n_analysed))
  rr_draws <- exp(b) / (1 - p0 + p0 * exp(b))
  rr_q     <- quantile(rr_draws, c(0.025, 0.5, 0.975), names = FALSE)
  g <- pp_check(res$fit, type = "intervals", ndraws = 200) +
    labs(title = tag, x = "Trial arm", y = "Employed (count)") + theme_minimal()
  ggsave(file.path(fig_dir, paste0("ppc_trials_", tag, ".pdf")), g, width = 7, height = 4)
  summary <- summarise_effect(b, exp, tag, k = length(unique(arms$study_id_clean))) %>%
    rename(or = estimate, or_low = ci_low, or_high = ci_high) %>%
    mutate(p_or_gt_1 = mean(b > 0),
           tau_median = median(tau), tau_low = quantile(tau, 0.025, names = FALSE),
           tau_high = quantile(tau, 0.975, names = FALSE),
           pooled_control_risk = p0,
           rr_at_p0 = rr_q[2], rr_at_p0_low = rr_q[1], rr_at_p0_high = rr_q[3])
  list(summary = summary, diag = res$diag)
}
exact_results <- list(
  run_exact(filter(trial_arms, analysis_set == "primary"), "exact_binomial_k3"),
  run_exact(trial_arms, "exact_binomial_k4")
)
write_csv(bind_rows(lapply(exact_results, `[[`, "summary")), file.path(out_dir, "trials_exact_binomial.csv"))

# -------------------------------
# Bridge-sampling Bayes factors for the half-normal H1 models (checked in 05)
# -------------------------------
# H1: mu ~ Half-Normal(0, s) (directional, IPS increases employment);
# H0: mu = 0. Both with tau ~ Half-Normal(0, 0.5), matching the quadrature.
s_values <- c(primary_log2 = log(2), frederick_published = log(1.63), bond2015 = log(1.69),
              bond2016 = log(1.96), modini2016 = log(2.40))
# The H0 and H1 fits pass through the same convergence gate as every other model,
# because marginal likelihoods are sensitive to poorly explored posterior tails.
h0_res <- fit_gated(bf(yi | se(sei) ~ 0 + (1 | study_id_clean)), input_sets$primary_k3,
                    prior(normal(0, 0.5), class = "sd"), gaussian(), "bf_h0_hn05",
                    save_all_pars = TRUE)
h0_bridge <- bridgesampling::bridge_sampler(h0_res$fit, silent = TRUE)
bridge_out <- lapply(names(s_values), function(nm) {
  s <- s_values[[nm]]
  h1_res <- fit_gated(bf(yi | se(sei) ~ 0 + Intercept + (1 | study_id_clean)), input_sets$primary_k3,
                      c(set_prior(paste0("normal(0, ", s, ")"), class = "b", lb = 0),
                        prior(normal(0, 0.5), class = "sd")),
                      gaussian(), paste0("bf_h1_", nm), save_all_pars = TRUE)
  h1_bridge <- bridgesampling::bridge_sampler(h1_res$fit, silent = TRUE)
  bf <- bridgesampling::bf(h1_bridge, h0_bridge)
  list(row = data.frame(h1_scale_label = nm, s = s, rr_scale = exp(s), tau_scale = 0.5,
                        bf_bridge = as.numeric(bf$bf),
                        h1_error_pct = bridgesampling::error_measures(h1_bridge)$percentage,
                        h0_error_pct = bridgesampling::error_measures(h0_bridge)$percentage,
                        stringsAsFactors = FALSE),
       diag = h1_res$diag)
})
bridge_rows <- bind_rows(lapply(bridge_out, `[[`, "row"))
bridge_diag <- c(list(h0_res$diag), lapply(bridge_out, `[[`, "diag"))
write_csv(bridge_rows, file.path(out_dir, "trials_bridge_bf.csv"))

# -------------------------------
# Write summaries, diagnostics and primary draws
# -------------------------------
summary_table <- bind_rows(lapply(results, `[[`, "summary"))
diag_table    <- bind_rows(c(lapply(results, `[[`, "diag"), lapply(exact_results, `[[`, "diag"), bridge_diag))
write_csv(summary_table, file.path(out_dir, "trials_summary.csv"))
write_csv(diag_table,    file.path(out_dir, "trials_diagnostics.csv"))
write_csv(data.frame(mu = results$primary_k3_hn05$draws$mu, tau = results$primary_k3_hn05$draws$tau),
          file.path(out_dir, "trials_primary_draws.csv"))

check(all(diag_table$passes_gate), paste("trial models failing the convergence gate:",
                                         paste(diag_table$model[!diag_table$passes_gate], collapse = ", ")))

cat("\nBayesian trial models (RR scale):\n")
print(summary_table %>%
        select(model, k, tau_scale, estimate, ci_low, ci_high, pi_low, pi_high, tau_median,
               p_rr_gt_1, p_rr_below_0.80, p_rr_in_null, p_rr_above_1.25) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nExact binomial models (OR scale):\n")
print(bind_rows(lapply(exact_results, `[[`, "summary")) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nBridge-sampling Bayes factors:\n")
print(bridge_rows %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nDiagnostics:\n")
print(diag_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
