# ===============================================
# 02 - Bayesian pooled employment prevalence (binomial-normal model)
# ===============================================
# Primary model: exact binomial likelihood with a study-level normal random
# effect on the logit scale,
#   events_i ~ Binomial(n_i, p_i),  logit(p_i) = mu + u_i,  u_i ~ N(0, tau^2)
# Priors (frozen in the analysis plan):
#   mu  ~ Normal(0, 1.5)   near-uniform on the proportion scale
#   tau ~ Half-Normal(0, 1) weakly informative; sensitivities at 0.5 and 2
# Primary set: strict point-prevalence set (k = 10). Secondary: period
# prevalence (k = 9). Labelled sensitivities: construct-relaxed point set
# (k = 14), legacy dissertation set (k = 23), EPHPP Weak excluded, verified
# vocational-exposure samples dropped, missing-as-not-employed lower bound,
# normal-normal logit approximation, and leave-one-out.
# Reported: posterior median and equal-tailed 95% CrI of the pooled proportion,
# posterior mean and SD on the logit scale, tau, a 95% prediction interval for
# a new study, and P(pooled proportion < 0.50) as a descriptive quantity.
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
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

prevalence <- read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)

# -------------------------------
# Model definitions
# -------------------------------
prev_formula <- bf(events | trials(n_assessed) ~ 1 + (1 | study_id_clean))
prev_prior   <- function(tau_scale) {
  c(prior(normal(0, 1.5), class = "Intercept"),
    set_prior(paste0("normal(0, ", tau_scale, ")"), class = "sd"))
}

# Fit one binomial-normal model and return its summary rows and draws.
run_prev <- function(d, tag, tau_scale = 1, events_col = "events", n_col = "n_assessed") {
  dd <- d %>% transmute(study_id_clean, events = .data[[events_col]], n_assessed = .data[[n_col]])
  res  <- fit_gated(prev_formula, dd, prev_prior(tau_scale), binomial(), tag)
  dr   <- pooled_draws(res$fit)
  pred <- prediction_draws(dr$mu, dr$tau)
  tau_q <- quantile(dr$tau, c(0.025, 0.5, 0.975), names = FALSE)
  summary <- summarise_effect(dr$mu, plogis, tag, k = nrow(dd)) %>%
    mutate(tau_scale = tau_scale,
           tau_median = tau_q[2], tau_low = tau_q[1], tau_high = tau_q[3],
           pi_low = quantile(plogis(pred), 0.025, names = FALSE),
           pi_high = quantile(plogis(pred), 0.975, names = FALSE),
           p_below_50 = mean(plogis(dr$mu) < 0.5))
  # Posterior predictive check on the observed counts
  g <- pp_check(res$fit, type = "intervals", ndraws = 200) +
    labs(title = tag, x = "Study", y = "Employed (count)") + theme_minimal()
  ggsave(file.path(fig_dir, paste0("ppc_prevalence_", tag, ".pdf")), g, width = 7, height = 4)
  list(summary = summary, diag = res$diag, fit = res$fit, draws = dr)
}

# -------------------------------
# 1. Primary and secondary estimands
# -------------------------------
strict  <- filter(prevalence, in_primary_set)
period  <- filter(prevalence, in_period_set)
relaxed <- filter(prevalence, in_point_relaxed_set)
legacy  <- prevalence

results <- list(
  primary_strict_point_hn1   = run_prev(strict, "primary_strict_point_hn1", 1),
  secondary_period_hn1       = run_prev(period, "secondary_period_hn1", 1),
  sens_prior_hn05            = run_prev(strict, "sens_prior_hn05", 0.5),
  sens_prior_hn2             = run_prev(strict, "sens_prior_hn2", 2),
  sens_point_relaxed_k14     = run_prev(relaxed, "sens_point_relaxed_k14", 1),
  sens_legacy_k23            = run_prev(legacy, "sens_legacy_k23", 1),
  sens_strict_excl_weak      = run_prev(filter(strict, quality_rating != "Weak"), "sens_strict_excl_weak", 1),
  sens_strict_excl_exposed   = run_prev(filter(strict, !(vocational_exposure == "all" & exposure_verified == "yes")),
                                        "sens_strict_excl_exposed", 1),
  sens_strict_missing_bound  = run_prev(strict, "sens_strict_missing_not_employed_bound", 1, n_col = "n_total")
)

# -------------------------------
# 2. Normal-normal logit approximation (what the registered analysis assumes)
# -------------------------------
es <- metafor::escalc(measure = "PLO", xi = strict$events, ni = strict$n_assessed)
nn_data <- data.frame(study_id_clean = strict$study_id_clean, yi = es$yi, sei = sqrt(es$vi))
nn_res  <- fit_gated(bf(yi | se(sei) ~ 1 + (1 | study_id_clean)), nn_data,
                     prev_prior(1), gaussian(), "sens_strict_normal_normal_hn1")
nn_dr   <- pooled_draws(nn_res$fit)
nn_pred <- prediction_draws(nn_dr$mu, nn_dr$tau)
nn_tau  <- quantile(nn_dr$tau, c(0.025, 0.5, 0.975), names = FALSE)
nn_summary <- summarise_effect(nn_dr$mu, plogis, "sens_strict_normal_normal_hn1", k = nrow(strict)) %>%
  mutate(tau_scale = 1, tau_median = nn_tau[2], tau_low = nn_tau[1], tau_high = nn_tau[3],
         pi_low = quantile(plogis(nn_pred), 0.025, names = FALSE),
         pi_high = quantile(plogis(nn_pred), 0.975, names = FALSE),
         p_below_50 = mean(plogis(nn_dr$mu) < 0.5))

# -------------------------------
# 3. Leave-one-out on the primary set
# -------------------------------
loo_res <- lapply(strict$study_id_clean, function(s) {
  r <- run_prev(filter(strict, study_id_clean != s), paste0("loo_", s), 1)
  r$summary <- r$summary %>% mutate(left_out = s)
  r
})
loo_rows <- bind_rows(lapply(loo_res, `[[`, "summary"))
write_csv(loo_rows, file.path(out_dir, "prevalence_leave_one_out.csv"))

# -------------------------------
# 4. Study-level shrunken estimates from the primary model (for the forest plot)
# -------------------------------
primary_fit <- results$primary_strict_point_hn1$fit
re <- as_draws_df(primary_fit)
study_cols <- grep("^r_study_id_clean\\[", names(re), value = TRUE)
study_effects <- bind_rows(lapply(study_cols, function(col) {
  theta <- re$b_Intercept + re[[col]]
  q <- quantile(plogis(theta), c(0.025, 0.5, 0.975), names = FALSE)
  data.frame(study_id_clean = sub("^r_study_id_clean\\[(.*),Intercept\\]$", "\\1", col),
             shrunken = q[2], shrunken_low = q[1], shrunken_high = q[3])
})) %>%
  left_join(strict %>% select(study_id_clean, study_label, region, events, n_assessed, quality_rating),
            by = "study_id_clean") %>%
  mutate(observed = events / n_assessed,
         observed_low = qbeta(0.025, events + 0.5, n_assessed - events + 0.5),
         observed_high = qbeta(0.975, events + 0.5, n_assessed - events + 0.5))
write_csv(study_effects, file.path(out_dir, "prevalence_primary_study_effects.csv"))

# -------------------------------
# 5. Write summaries, diagnostics and primary draws
# -------------------------------
summary_table <- bind_rows(c(lapply(results, `[[`, "summary"), list(nn_summary)))
diag_table    <- bind_rows(c(lapply(results, `[[`, "diag"), list(nn_res$diag), lapply(loo_res, `[[`, "diag")))
write_csv(summary_table, file.path(out_dir, "prevalence_summary.csv"))
write_csv(diag_table,    file.path(out_dir, "prevalence_diagnostics.csv"))
write_csv(data.frame(mu = results$primary_strict_point_hn1$draws$mu,
                     tau = results$primary_strict_point_hn1$draws$tau),
          file.path(out_dir, "prevalence_primary_draws.csv"))

check(all(diag_table$passes_gate), paste("prevalence models failing the convergence gate:",
                                         paste(diag_table$model[!diag_table$passes_gate], collapse = ", ")))

cat("\nBayesian prevalence models:\n")
print(summary_table %>%
        select(model, k, tau_scale, estimate, ci_low, ci_high, pi_low, pi_high, tau_median, p_below_50) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nDiagnostics:\n")
print(diag_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
