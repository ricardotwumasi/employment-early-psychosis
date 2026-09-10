# ===============================================
# 06b - Release-candidate figures and tables (A33 scope)
# ===============================================
# Reads output/release/bayesian/ (02b, 03b) and data/derived/release_*.csv
# (00b) and writes output/release/tables/*.csv and *.tex and
# output/release/figures/rc_fig*.pdf. Also validates the release layer of the
# fit registry against the release diagnostics files (the historical layer is
# validated by R/06_figures_tables.R).
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(knitr)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

fig_dir <- root_path("output", "release", "figures")
tab_dir <- root_path("output", "release", "tables")
bay_dir <- root_path("output", "release", "bayesian")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

col_obs <- "#6b6b6b"; col_pos <- "#1b6ca8"; col_reg <- "#b5651d"
theme_pub <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0), legend.position = "bottom",
        legend.title = element_blank(), plot.title.position = "plot")
fmt  <- function(x, d = 2) formatC(x, format = "f", digits = d)
pct  <- function(x, d = 1) formatC(100 * x, format = "f", digits = d)
ci   <- function(e, l, h, d = 2) paste0(fmt(e, d), " (", fmt(l, d), " to ", fmt(h, d), ")")
cip  <- function(e, l, h, d = 1) paste0(pct(e, d), " (", pct(l, d), " to ", pct(h, d), ")")
write_tex <- function(df, file, align = NULL, col_spec = NULL) {
  k <- kable(df, format = "latex", booktabs = TRUE, linesep = "", escape = TRUE, align = align)
  if (!is.null(col_spec)) k <- sub("\\\\begin\\{tabular\\}\\{[^}]*\\}",
                                   paste0("\\\\begin{tabular}{", col_spec, "}"), k)
  writeLines(k, file.path(tab_dir, file))
}

rp        <- read_csv(root_path("data", "derived", "release_prevalence.csv"), show_col_types = FALSE)
rt        <- read_csv(root_path("data", "derived", "release_trials.csv"), show_col_types = FALSE)
prev_sum  <- read_csv(file.path(bay_dir, "rc_prevalence_summary.csv"), show_col_types = FALSE)
prev_stud <- read_csv(file.path(bay_dir, "rc_prevalence_primary_study_effects.csv"), show_col_types = FALSE)
prev_loo  <- read_csv(file.path(bay_dir, "rc_prevalence_leave_one_out.csv"), show_col_types = FALSE)
prev_strict <- read_csv(file.path(bay_dir, "rc_prevalence_strict_record.csv"), show_col_types = FALSE)
prev_var  <- read_csv(file.path(bay_dir, "rc_prevalence_variable_window.csv"), show_col_types = FALSE)
tr_sum    <- read_csv(file.path(bay_dir, "rc_trials_summary.csv"), show_col_types = FALSE)
tr_loo    <- read_csv(file.path(bay_dir, "rc_trials_leave_one_out.csv"), show_col_types = FALSE)
tr_exact  <- read_csv(file.path(bay_dir, "rc_trials_exact_binomial.csv"), show_col_types = FALSE)
tr_abs    <- read_csv(file.path(bay_dir, "rc_absolute_risk_scenarios.csv"), show_col_types = FALSE)
tr_bf     <- read_csv(file.path(bay_dir, "rc_trials_bayes_factors.csv"), show_col_types = FALSE)
tr_rr_s   <- read_csv(file.path(bay_dir, "rc_trials_bf_robustness_region.csv"), show_col_types = FALSE)
tr_strict <- read_csv(file.path(bay_dir, "rc_trials_strict_record.csv"), show_col_types = FALSE)
tr_inputs <- read_csv(file.path(bay_dir, "rc_trials_input_effects.csv"), show_col_types = FALSE)
diag_all  <- bind_rows(read_csv(file.path(bay_dir, "rc_prevalence_diagnostics.csv"), show_col_types = FALSE),
                       read_csv(file.path(bay_dir, "rc_trials_diagnostics.csv"), show_col_types = FALSE))

# Release-layer fit inventory against the registry
registry <- read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)
expected <- registry$fit_tag[registry$layer == "release"]
fitted   <- sub("_ad999$", "", diag_all$model)
check(length(expected) > 0 && !anyDuplicated(expected), "release layer of the fit registry is empty or duplicated")
check(setequal(fitted, expected) && !anyDuplicated(fitted),
      paste0("release fit inventory mismatch; missing: ", paste(setdiff(expected, fitted), collapse = ", "),
             "; unexpected: ", paste(setdiff(fitted, expected), collapse = ", ")))
check(all(diag_all$passes_gate), "a release fit failed the convergence gate")

# -------------------------------
# Figure: forest plot of the A33 point-prevalence primary set
# -------------------------------
prim <- prev_sum %>% filter(model == "rc_a33_point_primary_hn1")
f1 <- prev_stud %>%
  mutate(label = paste0(study_label, "  (", events, "/", n_assessed, ")")) %>%
  arrange(region, observed) %>%
  mutate(label = factor(label, levels = rev(label)))
f1_long <- bind_rows(
  f1 %>% transmute(label, region, kind = "Observed proportion (95% Jeffreys interval)",
                   est = observed, lo = observed_low, hi = observed_high, nudge = 0.18),
  f1 %>% transmute(label, region, kind = "Posterior study estimate (95% CrI)",
                   est = shrunken, lo = shrunken_low, hi = shrunken_high, nudge = -0.18))
fig1 <- ggplot(f1_long, aes(y = as.numeric(label) + nudge, x = est, colour = kind, shape = kind)) +
  annotate("rect", xmin = prim$pi_low, xmax = prim$pi_high, ymin = -Inf, ymax = Inf, fill = col_pos, alpha = 0.06) +
  geom_vline(xintercept = prim$estimate, colour = col_pos, linewidth = 0.4, linetype = "22") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5) +
  geom_point(size = 1.8) +
  scale_y_continuous(breaks = seq_along(levels(f1$label)), labels = levels(f1$label), expand = expansion(add = 0.6)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  scale_colour_manual(values = c(col_obs, col_pos)) + scale_shape_manual(values = c(16, 18)) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Proportion in competitive or paid employment", y = NULL,
       title = paste0("A33 early-psychosis set, k = ", prim$k, ": pooled point prevalence ", pct(prim$estimate),
                      "% (95% CrI ", pct(prim$ci_low), " to ", pct(prim$ci_high), "%); prediction interval ",
                      pct(prim$pi_low), " to ", pct(prim$pi_high), "% (shaded)")) +
  theme_pub + theme(strip.placement = "outside", plot.title = element_text(size = 8))
ggsave(file.path(fig_dir, "rc_fig1_forest_prevalence.pdf"), fig1, width = 7.2, height = 4.2)

# -------------------------------
# Figure: IPS trials, months 0 to 6
# -------------------------------
bay <- tr_sum %>% filter(model == "rc_trial_a33_k3_hn05")
study_rows <- tr_inputs %>% filter(input_set == "rc_a33_k3") %>%
  left_join(rt %>% select(study_id_clean, events_intervention, n_intervention, events_control, n_control),
            by = "study_id_clean") %>%
  transmute(label = paste0(study_label, "  (", events_intervention, "/", n_intervention, " vs ",
                           events_control, "/", n_control, ")"),
            est = exp(yi), lo = exp(yi - 1.96 * sei), hi = exp(yi + 1.96 * sei), kind = "Trial (95% CI)", group = "Trials")
rows2 <- bind_rows(
  study_rows,
  data.frame(label = paste0("Bayesian random effects, k = ", bay$k, " (posterior median, 95% CrI)"),
             est = bay$estimate, lo = bay$ci_low, hi = bay$ci_high, kind = "Posterior", group = "Pooled"),
  data.frame(label = "Prediction interval for a new trial", est = NA, lo = bay$pi_low, hi = bay$pi_high,
             kind = "Posterior", group = "Pooled"),
  tr_strict %>% transmute(label = paste0("Strict registered rule: ", study_label, " alone (95% CI)"),
                          est = rr, lo = rr_low, hi = rr_high, kind = "Strict comparison", group = "Pooled")
) %>% mutate(label = factor(label, levels = rev(label)), group = factor(group, levels = c("Trials", "Pooled")))
fig2 <- ggplot(rows2, aes(y = label, x = est, colour = kind, shape = kind)) +
  annotate("rect", xmin = 0.8, xmax = 1.25, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +
  geom_vline(xintercept = 1, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25, linewidth = 0.5) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_x_log10(breaks = c(0.25, 0.5, 0.8, 1.25, 2, 4, 8, 16, 32)) +
  scale_colour_manual(values = c(Posterior = col_pos, `Strict comparison` = col_reg, `Trial (95% CI)` = col_obs)) +
  scale_shape_manual(values = c(Posterior = 18, `Strict comparison` = 15, `Trial (95% CI)` = 16)) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Risk ratio for employment attained in months 0 to 6 (log scale); shaded band is the null interval", y = NULL) +
  theme_pub + theme(strip.text = element_blank())
ggsave(file.path(fig_dir, "rc_fig2_forest_trials.pdf"), fig2, width = 7.2, height = 3.4)

# -------------------------------
# Tables
# -------------------------------
model_labels <- c(
  rc_a33_point_primary_hn1 = "A33 point prevalence, primary (tau ~ HN(1))",
  rc_a33_point_prior_hn05 = "Prior sensitivity, tau ~ HN(0.5)",
  rc_a33_point_prior_hn2 = "Prior sensitivity, tau ~ HN(2)",
  rc_a33_point_duration_sens_hn1 = "Duration sensitivity (adds onset within eight years)",
  rc_a33_period_hn1 = "A33 period prevalence (secondary estimand)",
  rc_a33_point_excl_weak_hn1 = "EPHPP Weak excluded",
  rc_a33_point_missing_bound_hn1 = "Missing counted as not employed (lower bound)",
  rc_a33_point_normal_normal_hn1 = "Normal-normal logit approximation")
t_prev <- prev_sum %>%
  mutate(Model = model_labels[model], k = k,
         `Assessed` = n_assessed_total,
         `Pooled prevalence (95% CrI)` = cip(estimate, ci_low, ci_high),
         `Prediction interval` = paste0(pct(pi_low), " to ", pct(pi_high)),
         `tau (95% CrI)` = ci(tau_median, tau_low, tau_high)) %>%
  select(Model, k, Assessed, `Pooled prevalence (95% CrI)`, `Prediction interval`, `tau (95% CrI)`)
strict_row <- data.frame(Model = "Strict registered rule (A24): point prevalence", k = prev_strict$k,
                         Assessed = NA_real_, `Pooled prevalence (95% CrI)` = "Not estimable (no eligible study)",
                         `Prediction interval` = "", `tau (95% CrI)` = "", check.names = FALSE)
var_rows <- prev_var %>% transmute(Model = paste0("Variable follow-up, unpooled: ", study_label), k = 1L,
                                   Assessed = n_assessed,
                                   `Pooled prevalence (95% CrI)` = paste0(cip(observed, observed_low, observed_high), " (single study, Jeffreys interval)"),
                                   `Prediction interval` = "", `tau (95% CrI)` = "")
t_prev <- bind_rows(t_prev, strict_row, var_rows)
write_csv(t_prev, file.path(tab_dir, "rc_table_prevalence.csv"))
write_tex(t_prev, "rc_table_prevalence.tex", col_spec = "p{5.2cm}rrp{3.4cm}p{2.2cm}p{2.4cm}")

tr_labels <- c(rc_trial_a33_k3_hn05 = "A33 IPS pool, months 0 to 6 (tau ~ HN(0.5))",
               rc_trial_a33_k3_hn025 = "Prior sensitivity, tau ~ HN(0.25)",
               rc_trial_a33_k3_hn1 = "Prior sensitivity, tau ~ HN(1)",
               rc_trial_a33_k3_missing_hn05 = "Missing counted as not employed")
t_tr <- tr_sum %>% filter(model %in% names(tr_labels)) %>%
  mutate(Model = tr_labels[model],
         `RR (95% CrI)` = ci(estimate, ci_low, ci_high),
         `Prediction interval` = paste0(fmt(pi_low), " to ", fmt(pi_high)),
         `tau (95% CrI)` = ci(tau_median, tau_low, tau_high),
         `P(RR > 1)` = fmt(p_rr_gt_1, 3), `P(RR > 1.25)` = fmt(p_rr_above_1.25, 3),
         `P(0.80 < RR < 1.25)` = fmt(p_rr_in_null, 3)) %>%
  select(Model, k, `RR (95% CrI)`, `Prediction interval`, `tau (95% CrI)`, `P(RR > 1)`, `P(RR > 1.25)`, `P(0.80 < RR < 1.25)`)
t_tr <- bind_rows(t_tr,
  tr_strict %>% transmute(Model = paste0("Strict registered rule (A24): ", study_label, " alone, Wald 95% CI"), k = 1L,
                          `RR (95% CrI)` = ci(rr, rr_low, rr_high), `Prediction interval` = "", `tau (95% CrI)` = "",
                          `P(RR > 1)` = "", `P(RR > 1.25)` = "", `P(0.80 < RR < 1.25)` = ""),
  tr_exact %>% transmute(Model = "Arm-level binomial-logit model (OR scale)", k = k,
                         `RR (95% CrI)` = paste0("OR ", ci(or, or_low, or_high), "; RR at pooled control risk ",
                                                 pct(pooled_control_risk, 0), "%: ", ci(rr_at_p0, rr_at_p0_low, rr_at_p0_high)),
                         `Prediction interval` = "", `tau (95% CrI)` = ci(tau_median, tau_low, tau_high),
                         `P(RR > 1)` = fmt(p_or_gt_1, 3), `P(RR > 1.25)` = "", `P(0.80 < RR < 1.25)` = ""))
write_csv(t_tr, file.path(tab_dir, "rc_table_trials.csv"))
write_tex(t_tr, "rc_table_trials.tex", col_spec = "p{4.6cm}rp{3.2cm}p{1.8cm}p{2.0cm}rrr")

t_abs <- tr_abs %>% transmute(`Control risk` = pct(control_risk, 0),
                              `Treated risk (95% CrI)` = cip(treated_risk, treated_low, treated_high),
                              `Risk difference, percentage points (95% CrI)` = paste0(fmt(100 * risk_difference, 1), " (",
                                    fmt(100 * rd_low, 1), " to ", fmt(100 * rd_high, 1), ")"),
                              `P(difference > 0)` = fmt(p_rd_gt_0, 3),
                              `Treated risk, new setting (95% interval)` = paste0(pct(new_setting_treated_low), " to ", pct(new_setting_treated_high)))
write_csv(t_abs, file.path(tab_dir, "rc_table_absolute_risk.csv"))
write_tex(t_abs, "rc_table_absolute_risk.tex", align = "rllrl")

t_bf <- tr_bf %>% transmute(`H1 scale (s)` = paste0(h1_scale_label, " (RR ", fmt(rr_scale), ")"),
                            `BF quadrature` = fmt(bf_quadrature), `BF bridge` = fmt(bf_bridge),
                            `Discrepancy (%)` = fmt(discrepancy_pct, 1),
                            `Agree to 2 s.f.` = ifelse(agree_2sf, "yes", "no"),
                            `Category (quadrature)` = category_quadrature)
write_csv(t_bf, file.path(tab_dir, "rc_table_bayes_factors.csv"))
write_tex(t_bf, "rc_table_bayes_factors.tex", align = "lrrrll")

t_loo <- bind_rows(
  prev_loo %>% transmute(Analysis = "Prevalence (A33 point primary)", `Study left out` = left_out, k = k,
                         Estimate = cip(estimate, ci_low, ci_high)),
  tr_loo %>% transmute(Analysis = "IPS trials (A33, months 0 to 6)", `Study left out` = left_out, k = k,
                       Estimate = ci(estimate, ci_low, ci_high)))
write_csv(t_loo, file.path(tab_dir, "rc_table_leave_one_out.csv"))
write_tex(t_loo, "rc_table_leave_one_out.tex", align = "llrl")

t_diag <- diag_all %>% transmute(Model = model, `Max R-hat` = fmt(max_rhat, 3), `Min bulk ESS` = round(min_ess_bulk),
                                 `Min tail ESS` = round(min_ess_tail), Divergences = divergences,
                                 `Max tree depth` = max_treedepth, `Min E-BFMI` = fmt(min_ebfmi),
                                 Gate = ifelse(passes_gate, "pass", "fail"))
write_csv(t_diag, file.path(tab_dir, "rc_table_mcmc_diagnostics.csv"))
write_tex(t_diag, "rc_table_mcmc_diagnostics.tex")

t_mem <- rp %>% transmute(Study = study_label, Basis = outcome_basis, Months = timepoint_months,
                          `Events / n` = paste0(events, " / ", n_assessed),
                          `PROSPERO v2.0 conflict` = conflict_grade,
                          `A33 decision` = ifelse(a33_eligible, "eligible", ifelse(a33_duration_sens_only, "duration sensitivity", "not eligible")),
                          Membership = membership_reason)
write_csv(t_mem, file.path(tab_dir, "rc_table_membership.csv"))
write_tex(t_mem, "rc_table_membership.tex", col_spec = "lllrllp{5.5cm}")

t_trmem <- rt %>% transmute(Study = study_label, Months = timepoint_months, Basis = outcome_basis,
                            `IPS events / n` = paste0(events_intervention, " / ", n_intervention),
                            `Control events / n` = paste0(events_control, " / ", n_control),
                            RR = fmt(rr_computed), `PROSPERO v2.0 conflict` = conflict_grade, Role = release_role)
write_csv(t_trmem, file.path(tab_dir, "rc_table_trial_membership.csv"))
write_tex(t_trmem, "rc_table_trial_membership.tex", col_spec = "lrlllrlp{5cm}")


# -------------------------------
# LaTeX macros for the manuscript (numbers enter the text only through these)
# -------------------------------
mac <- function(name, value) paste0("\\newcommand{\\", name, "}{", value, "}")
pr  <- prev_sum %>% filter(model == "rc_a33_point_primary_hn1")
pd  <- prev_sum %>% filter(model == "rc_a33_point_duration_sens_hn1")
pp  <- prev_sum %>% filter(model == "rc_a33_period_hn1")
p05 <- prev_sum %>% filter(model == "rc_a33_point_prior_hn05")
p2  <- prev_sum %>% filter(model == "rc_a33_point_prior_hn2")
pmb <- prev_sum %>% filter(model == "rc_a33_point_missing_bound_hn1")
pew <- prev_sum %>% filter(model == "rc_a33_point_excl_weak_hn1")
tr  <- tr_sum %>% filter(model == "rc_trial_a33_k3_hn05")
t025 <- tr_sum %>% filter(model == "rc_trial_a33_k3_hn025")
t1  <- tr_sum %>% filter(model == "rc_trial_a33_k3_hn1")
tm  <- tr_sum %>% filter(model == "rc_trial_a33_k3_missing_hn05")
bfp <- tr_bf %>% filter(h1_scale_label == "primary_log2")
n_pool <- rt %>% filter(in_trial_a33_k3_0to6)
loo_p <- prev_loo; loo_t <- tr_loo
macros <- c(
  "% Generated by R/06b_release_tables.R; do not edit by hand.",
  mac("rcPrevK", pr$k), mac("rcPrevN", pr$n_assessed_total), mac("rcPrevEvents", pr$events_total),
  mac("rcPrevEst", pct(pr$estimate)), mac("rcPrevLo", pct(pr$ci_low)), mac("rcPrevHi", pct(pr$ci_high)),
  mac("rcPrevPiLo", pct(pr$pi_low)), mac("rcPrevPiHi", pct(pr$pi_high)),
  mac("rcPrevMeanSetting", pct(pr$mean_setting_estimate)),
  mac("rcPrevTau", fmt(pr$tau_median)), mac("rcPrevTauLo", fmt(pr$tau_low)), mac("rcPrevTauHi", fmt(pr$tau_high)),
  mac("rcPrevPbelowHalf", fmt(pr$p_below_50, 3)),
  mac("rcPrevHnHalfEst", pct(p05$estimate)), mac("rcPrevHnHalfLo", pct(p05$ci_low)), mac("rcPrevHnHalfHi", pct(p05$ci_high)),
  mac("rcPrevHnTwoEst", pct(p2$estimate)), mac("rcPrevHnTwoLo", pct(p2$ci_low)), mac("rcPrevHnTwoHi", pct(p2$ci_high)),
  mac("rcPrevDurK", pd$k), mac("rcPrevDurEst", pct(pd$estimate)), mac("rcPrevDurLo", pct(pd$ci_low)), mac("rcPrevDurHi", pct(pd$ci_high)),
  mac("rcPeriodK", pp$k), mac("rcPeriodN", pp$n_assessed_total), mac("rcPeriodEst", pct(pp$estimate)), mac("rcPeriodLo", pct(pp$ci_low)), mac("rcPeriodHi", pct(pp$ci_high)),
  mac("rcPeriodPiLo", pct(pp$pi_low)), mac("rcPeriodPiHi", pct(pp$pi_high)),
  mac("rcPrevMissEst", pct(pmb$estimate)), mac("rcPrevMissLo", pct(pmb$ci_low)), mac("rcPrevMissHi", pct(pmb$ci_high)),
  mac("rcPrevExclWeakK", pew$k), mac("rcPrevExclWeakEst", pct(pew$estimate)), mac("rcPrevExclWeakLo", pct(pew$ci_low)), mac("rcPrevExclWeakHi", pct(pew$ci_high)),
  mac("rcPrevLooMin", pct(min(loo_p$estimate))), mac("rcPrevLooMax", pct(max(loo_p$estimate))),
  mac("rcStrictPrevK", prev_strict$k),
  mac("rcTrialK", tr$k), mac("rcTrialNrand", sum(n_pool$n_intervention_full + n_pool$n_control_full)),
  mac("rcTrialNanalysed", sum(n_pool$n_intervention + n_pool$n_control)),
  mac("rcRR", fmt(tr$estimate)), mac("rcRRLo", fmt(tr$ci_low)), mac("rcRRHi", fmt(tr$ci_high)),
  mac("rcRRPiLo", fmt(tr$pi_low)), mac("rcRRPiHi", fmt(tr$pi_high)),
  mac("rcRRTau", fmt(tr$tau_median)), mac("rcRRTauLo", fmt(tr$tau_low)), mac("rcRRTauHi", fmt(tr$tau_high)),
  mac("rcPRRgtOne", fmt(tr$p_rr_gt_1, 2)), mac("rcPRRabove", fmt(tr$p_rr_above_1.25, 2)), mac("rcPRRnull", fmt(tr$p_rr_in_null, 2)),
  mac("rcPRRbelow", fmt(tr$p_rr_below_0.80, 2)), mac("rcPnewRRgtOne", fmt(tr$p_new_rr_gt_1, 2)), mac("rcPnewRRabove", fmt(tr$p_new_rr_above_1.25, 2)),
  mac("rcRRhnQuarter", ci(t025$estimate, t025$ci_low, t025$ci_high)), mac("rcRRhnOne", ci(t1$estimate, t1$ci_low, t1$ci_high)),
  mac("rcRRmissing", ci(tm$estimate, tm$ci_low, tm$ci_high)),
  mac("rcRRLooMin", fmt(min(loo_t$estimate))), mac("rcRRLooMax", fmt(max(loo_t$estimate))),
  mac("rcOR", fmt(tr_exact$or)), mac("rcORLo", fmt(tr_exact$or_low)), mac("rcORHi", fmt(tr_exact$or_high)),
  mac("rcORPgtOne", fmt(tr_exact$p_or_gt_1, 2)), mac("rcORpZero", pct(tr_exact$pooled_control_risk, 0)),
  mac("rcORrrAtPzero", ci(tr_exact$rr_at_p0, tr_exact$rr_at_p0_low, tr_exact$rr_at_p0_high)),
  mac("rcBFprimary", fmt(bfp$bf_quadrature)), mac("rcBFprimaryBridge", fmt(bfp$bf_bridge)),
  mac("rcBFagreeCount", sum(tr_bf$agree_2sf)), mac("rcBFmaxDiscrepancy", fmt(max(abs(tr_bf$discrepancy_pct)), 1)),
  mac("rcBFcategory", bfp$category_quadrature),
  mac("rcBFrrSmin", fmt(tr_rr_s$s_min)), mac("rcBFrrSmax", fmt(tr_rr_s$s_max)),
  mac("rcStrictTrialRR", ci(tr_strict$rr[1], tr_strict$rr_low[1], tr_strict$rr_high[1])),
  mac("rcStrictTrialLabel", tr_strict$study_label[1]),
  mac("rcVarWindowEst", cip(prev_var$observed[1], prev_var$observed_low[1], prev_var$observed_high[1])),
  mac("rcVarWindowLabel", prev_var$study_label[1])
)
for (i in seq_len(nrow(tr_abs))) {
  tag <- c("Twenty", "Thirty", "FortyThree", "Fifty")[i]
  macros <- c(macros,
    mac(paste0("rcAbsTreated", tag), cip(tr_abs$treated_risk[i], tr_abs$treated_low[i], tr_abs$treated_high[i])),
    mac(paste0("rcAbsRD", tag), paste0(fmt(100 * tr_abs$risk_difference[i], 1), " (", fmt(100 * tr_abs$rd_low[i], 1), " to ", fmt(100 * tr_abs$rd_high[i], 1), ")")),
    mac(paste0("rcAbsPgt", tag), fmt(tr_abs$p_rd_gt_0[i], 2)))
}
writeLines(macros, file.path(tab_dir, "rc_numbers.tex"))

cat("Release tables written to", tab_dir, "\n")
cat("Robustness region over s (quadrature, tau 0.5):", tr_rr_s$category_at_primary, tr_rr_s$s_min, "to", tr_rr_s$s_max, "\n")
