# ===============================================
# 05 - Dienes-style hypothesis tests for the IPS trial synthesis
# ===============================================
# Two complementary tests, reported as Dienes (2020) recommends.
#
# 1. Null interval. Minimally interesting effect m = log(1.25): RR 1.25 is
#    GRADE's rough guide for appreciable benefit (Guyatt et al. 2011); the lower
#    limit 0.80 is our log-symmetry convention. RR 1.25 is a provisional generic
#    decision threshold, not a validated employment-specific minimal effect.
#    Reported: P(RR < 0.80), P(0.80 <= RR <= 1.25), P(RR > 1.25) for the pooled
#    effect and for a new trial; the Kruschke (2018) decision from the 95% CrI;
#    and the robustness region for m.
#
# 2. Bayes factors. H1 models the predicted scale of effect as a half-normal on
#    log RR with SD s, B_HN(0, s), against a point null. s is specified
#    independently of m and of the observed effect: s_primary = log(2.00),
#    anchored to a corrected re-analysis of Frederick and VanderWeele's (2019)
#    data (07_external_prior_frederick2019.R); sensitivity values log(1.63),
#    log(1.69), log(1.96), log(2.40). Reported: the exact random-effects BF
#    (quadrature over the H0 posterior of tau) for tau scales 0.25, 0.5, 1; the
#    fixed-effect BF from the Wald summary; the closed-form BF from the
#    posterior summary (normal approximation, shown for comparison); the
#    robustness region RR_B for s; and agreement with bridge sampling from 03.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

out_dir <- root_path("output", "bayesian")
tab_dir <- root_path("output", "tables")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

inputs  <- read_csv(file.path(out_dir, "trials_input_effects.csv"), show_col_types = FALSE)
draws   <- read_csv(file.path(out_dir, "trials_primary_draws.csv"), show_col_types = FALSE)
bridge  <- read_csv(file.path(out_dir, "trials_bridge_bf.csv"), show_col_types = FALSE)
freq    <- read_csv(root_path("output", "frequentist", "trials_registered_models.csv"), show_col_types = FALSE)

prim <- filter(inputs, input_set == "primary_k3")
y <- prim$yi; sei <- prim$sei

# -------------------------------
# Unit test of the closed-form half-normal Bayes factor (Dienes 2020, footnote 4)
# -------------------------------
check(abs(bf_hn(1, 2, 5) - 0.5615) < 0.001, "bf_hn does not reproduce the Dienes (2020) footnote-4 example")

# -------------------------------
# 1. Null-interval inference
# -------------------------------
m_null <- log(1.25)
mu   <- draws$mu
pred <- prediction_draws(mu, draws$tau)
ci   <- quantile(mu, c(0.025, 0.975), names = FALSE)
dec  <- null_interval_decision(ci[1], ci[2], m_null)
ni <- data.frame(
  quantity = c("pooled_effect", "new_trial"),
  p_rr_below_0.80  = c(null_interval_probs(mu, m_null)[["p_below"]],  null_interval_probs(pred, m_null)[["p_below"]]),
  p_rr_in_null     = c(null_interval_probs(mu, m_null)[["p_inside"]], null_interval_probs(pred, m_null)[["p_inside"]]),
  p_rr_above_1.25  = c(null_interval_probs(mu, m_null)[["p_above"]],  null_interval_probs(pred, m_null)[["p_above"]]),
  p_rr_gt_1        = c(mean(mu > 0), mean(pred > 0))
)
# Normal approximation to the same probabilities, for comparison only
mu_mean <- mean(mu); mu_sd <- sd(mu)
ni_normal <- data.frame(
  quantity = "pooled_effect_normal_approx",
  p_rr_below_0.80 = pnorm(-m_null, mu_mean, mu_sd),
  p_rr_in_null = pnorm(m_null, mu_mean, mu_sd) - pnorm(-m_null, mu_mean, mu_sd),
  p_rr_above_1.25 = 1 - pnorm(m_null, mu_mean, mu_sd),
  p_rr_gt_1 = 1 - pnorm(0, mu_mean, mu_sd)
)
null_interval_table <- bind_rows(ni, ni_normal)
write_csv(null_interval_table, file.path(tab_dir, "dienes_null_interval.csv"))

decision_table <- data.frame(
  m_log = m_null, m_rr = 1.25, null_interval_rr_low = 1 / 1.25, null_interval_rr_high = 1.25,
  cri_low_rr = exp(ci[1]), cri_high_rr = exp(ci[2]),
  decision = dec$decision,
  robustness_m_rr_min = exp(dec$m_min), robustness_m_rr_max = exp(dec$m_max),
  posterior_mean_log_rr = mu_mean, posterior_sd_log_rr = mu_sd
)
write_csv(decision_table, file.path(tab_dir, "dienes_null_interval_decision.csv"))

# -------------------------------
# 2. Bayes factors
# -------------------------------
s_values <- c(primary_log2 = log(2), frederick_published = log(1.63), bond2015 = log(1.69),
              bond2016 = log(1.96), modini2016 = log(2.40))
tau_scales <- c(0.25, 0.5, 1)

# Exact random-effects BF over the tau prior scales and H1 scales
bf_table <- bind_rows(lapply(names(s_values), function(nm) {
  bind_rows(lapply(tau_scales, function(ts) {
    data.frame(h1_scale_label = nm, s = s_values[[nm]], rr_scale = exp(s_values[[nm]]),
               tau_scale = ts, bf_random_effects = bf_re(y, sei, s_values[[nm]], ts),
               stringsAsFactors = FALSE)
  }))
})) %>%
  mutate(category = bf_category(bf_random_effects))

# Fixed-effect BF from the Wald (common-effect) summary and the closed-form BF
# from the posterior summary of the primary model
wald <- freq %>% filter(model == "primary_k3_available_case", measure == "RR", interval == "wald")
w    <- 1 / sei^2
d_fe <- sum(w * y) / sum(w); se_fe <- sqrt(1 / sum(w))
check(abs(exp(d_fe) - wald$estimate) < 0.01, "common-effect estimate disagrees with the registered Wald model")
bf_extra <- bind_rows(lapply(names(s_values), function(nm) {
  s <- s_values[[nm]]
  data.frame(h1_scale_label = nm, s = s, rr_scale = exp(s),
             bf_fixed_effect = bf_hn(d_fe, se_fe, s),
             bf_posterior_normal_approx = bf_hn(mu_mean, mu_sd, s),
             stringsAsFactors = FALSE)
}))
write_csv(bf_table, file.path(tab_dir, "dienes_bayes_factors_random_effects.csv"))
write_csv(bf_extra, file.path(tab_dir, "dienes_bayes_factors_fixed_and_approx.csv"))

# Agreement with bridge sampling (tau scale 0.5)
compare <- bf_table %>% filter(tau_scale == 0.5) %>%
  select(h1_scale_label, bf_random_effects) %>%
  inner_join(bridge %>% select(h1_scale_label, bf_bridge, h1_error_pct, h0_error_pct),
             by = "h1_scale_label") %>%
  mutate(ratio = bf_bridge / bf_random_effects,
         agrees_2sf = signif(bf_bridge, 2) == signif(bf_random_effects, 2) | abs(ratio - 1) < 0.05)
write_csv(compare, file.path(tab_dir, "dienes_bf_quadrature_vs_bridge.csv"))
check(all(abs(compare$ratio - 1) < 0.10),
      "quadrature and bridge-sampling Bayes factors differ by more than 10%")

# Robustness region for s (primary tau scale 0.5)
rr_s <- robustness_region_s(function(s) bf_re(y, sei, s, 0.5), log(2))
rr_s_table <- data.frame(
  tau_scale = 0.5, s_primary = log(2), category_at_primary = rr_s$category,
  s_min = rr_s$s_min, s_max = rr_s$s_max,
  rr_min = exp(rr_s$s_min), rr_max = exp(rr_s$s_max),
  hits_grid_edge = rr_s$hits_grid_edge, contiguous = rr_s$contiguous
)
write_csv(rr_s_table, file.path(tab_dir, "dienes_robustness_region_s.csv"))
write_csv(rr_s$grid, file.path(tab_dir, "dienes_bf_grid_over_s.csv"))

# Robustness region for s under the fixed-effect BF, for the same table
rr_s_fe <- robustness_region_s(function(s) bf_hn(d_fe, se_fe, s), log(2))
write_csv(data.frame(model = "fixed_effect", category = rr_s_fe$category,
                     s_min = rr_s_fe$s_min, s_max = rr_s_fe$s_max,
                     rr_min = exp(rr_s_fe$s_min), rr_max = exp(rr_s_fe$s_max),
                     hits_grid_edge = rr_s_fe$hits_grid_edge),
          file.path(tab_dir, "dienes_robustness_region_s_fixed_effect.csv"))

cat("\nNull-interval probabilities (RR in [0.80, 1.25]):\n")
print(null_interval_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nDecision:\n"); print(decision_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nRandom-effects Bayes factors B_HN(0, s):\n")
print(bf_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())
cat("\nFixed-effect and normal-approximation Bayes factors:\n")
print(bf_extra %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nQuadrature versus bridge sampling (tau scale 0.5):\n")
print(compare %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nRobustness region for s:\n"); print(rr_s_table %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
