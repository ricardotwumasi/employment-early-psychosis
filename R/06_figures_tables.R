# ===============================================
# 06 - Figures and manuscript tables
# ===============================================
# Reads the summary files written by 01 to 05 and 07 and produces:
#   output/figures/fig1_forest_prevalence.pdf     strict primary set, observed vs shrunken
#   output/figures/fig2_forest_trials.pdf         IPS trials with Bayesian and registered pooled rows
#   output/figures/fig3_dienes.pdf                posterior of RR against the null interval; BF over s
#   output/figures/figS1_prior_sensitivity.pdf    tau prior sensitivity, prevalence and trials
#   output/figures/figS2_tau_posteriors.pdf       tau posterior against its prior
#   output/tables/*.csv and *.tex                 manuscript and supplementary tables
# Colour: a recessive grey for observed data and one accent for posterior
# quantities; identity is also carried by shape and direct labels.
# ===============================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(knitr)
})
source(if (file.exists("R/utils.R")) "R/utils.R" else "utils.R")

fig_dir <- root_path("output", "figures")
tab_dir <- root_path("output", "tables")
bay_dir <- root_path("output", "bayesian")
frq_dir <- root_path("output", "frequentist")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

col_obs <- "#6b6b6b"   # observed data (recessive)
col_pos <- "#1b6ca8"   # posterior quantities (accent)
col_reg <- "#b5651d"   # registered frequentist reference rows
theme_pub <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0), legend.position = "bottom",
        legend.title = element_blank(), plot.title.position = "plot")

prevalence <- read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)
trials     <- read_csv(root_path("data", "derived", "trials.csv"), show_col_types = FALSE)
prev_sum   <- read_csv(file.path(bay_dir, "prevalence_summary.csv"), show_col_types = FALSE)
prev_stud  <- read_csv(file.path(bay_dir, "prevalence_primary_study_effects.csv"), show_col_types = FALSE)
prev_draws <- read_csv(file.path(bay_dir, "prevalence_primary_draws.csv"), show_col_types = FALSE)
prev_loo   <- read_csv(file.path(bay_dir, "prevalence_leave_one_out.csv"), show_col_types = FALSE)
prev_freq  <- read_csv(file.path(frq_dir, "prevalence_registered_models.csv"), show_col_types = FALSE)
tr_sum     <- read_csv(file.path(bay_dir, "trials_summary.csv"), show_col_types = FALSE)
tr_draws   <- read_csv(file.path(bay_dir, "trials_primary_draws.csv"), show_col_types = FALSE)
tr_exact   <- read_csv(file.path(bay_dir, "trials_exact_binomial.csv"), show_col_types = FALSE)
tr_freq    <- read_csv(file.path(frq_dir, "trials_registered_models.csv"), show_col_types = FALSE)
tr_study   <- read_csv(file.path(frq_dir, "trials_per_study_effects.csv"), show_col_types = FALSE)
tr_other   <- read_csv(file.path(frq_dir, "trials_other_comparisons_descriptive.csv"), show_col_types = FALSE)
mr         <- read_csv(file.path(bay_dir, "metaregression_contrasts.csv"), show_col_types = FALSE)
bias       <- read_csv(file.path(frq_dir, "publication_bias_registered.csv"), show_col_types = FALSE)
di_null    <- read_csv(file.path(tab_dir, "dienes_null_interval.csv"), show_col_types = FALSE)
di_dec     <- read_csv(file.path(tab_dir, "dienes_null_interval_decision.csv"), show_col_types = FALSE)
di_bf      <- read_csv(file.path(tab_dir, "dienes_bayes_factors_random_effects.csv"), show_col_types = FALSE)
di_bf2     <- read_csv(file.path(tab_dir, "dienes_bayes_factors_fixed_and_approx.csv"), show_col_types = FALSE)
di_grid    <- read_csv(file.path(tab_dir, "dienes_bf_grid_over_s.csv"), show_col_types = FALSE)
di_rr      <- read_csv(file.path(tab_dir, "dienes_robustness_region_s.csv"), show_col_types = FALSE)
di_cmp     <- read_csv(file.path(tab_dir, "dienes_bf_quadrature_vs_bridge.csv"), show_col_types = FALSE)
diag_all   <- bind_rows(read_csv(file.path(bay_dir, "prevalence_diagnostics.csv"), show_col_types = FALSE),
                        read_csv(file.path(bay_dir, "trials_diagnostics.csv"), show_col_types = FALSE),
                        read_csv(file.path(bay_dir, "metaregression_diagnostics.csv"), show_col_types = FALSE))

# Fit inventory: every model the pipeline fits must have passed through the
# convergence gate in 02, 03 or 04 and appear here exactly once. A model that
# was re-run at adapt_delta = 0.999 carries the suffix _ad999 and still counts.
expected_fits <- read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)$fit_tag
check(length(expected_fits) == 42 && !anyDuplicated(expected_fits),
      "fit registry does not contain exactly 42 unique fit_tag values")
fitted_models <- sub("_ad999$", "", diag_all$model)
check(setequal(fitted_models, expected_fits) && !anyDuplicated(fitted_models),
      paste0("fit inventory mismatch; missing: ", paste(setdiff(expected_fits, fitted_models), collapse = ", "),
             "; unexpected: ", paste(setdiff(fitted_models, expected_fits), collapse = ", ")))
check(all(diag_all$passes_gate), "a fit in the inventory failed the convergence gate")
recovery   <- read_csv(root_path("data", "derived", "recovery.csv"), show_col_types = FALSE)
external   <- read_csv(file.path(tab_dir, "external_frederick2019_reanalysis.csv"), show_col_types = FALSE)

fmt  <- function(x, d = 2) formatC(x, format = "f", digits = d)
pct  <- function(x, d = 1) formatC(100 * x, format = "f", digits = d)
ci   <- function(e, l, h, d = 2) paste0(fmt(e, d), " (", fmt(l, d), " to ", fmt(h, d), ")")
cip  <- function(e, l, h, d = 1) paste0(pct(e, d), " (", pct(l, d), " to ", pct(h, d), ")")
write_tex <- function(df, file, caption = NULL, label = NULL, align = NULL, col_spec = NULL) {
  k <- kable(df, format = "latex", booktabs = TRUE, linesep = "", escape = TRUE,
             align = align, caption = caption, label = label)
  # Optional explicit tabular column specification (for wrapped text columns)
  if (!is.null(col_spec)) k <- sub("\\\\begin\\{tabular\\}\\{[^}]*\\}",
                                   paste0("\\\\begin{tabular}{", col_spec, "}"), k)
  writeLines(k, file.path(tab_dir, file))
}

# -------------------------------
# Figure 1: forest plot of the strict primary set
# -------------------------------
prim <- prev_sum %>% filter(model == "primary_strict_point_hn1")
f1 <- prev_stud %>%
  mutate(label = paste0(study_label, "  (", events, "/", n_assessed, ")"),
         region = factor(region, levels = c("Europe", "North America", "Oceania"))) %>%
  arrange(region, observed) %>%
  mutate(label = factor(label, levels = rev(label)))
f1_long <- bind_rows(
  f1 %>% transmute(label, region, kind = "Observed proportion (95% Jeffreys interval)",
                   est = observed, lo = observed_low, hi = observed_high, nudge = 0.18),
  f1 %>% transmute(label, region, kind = "Posterior study estimate (95% CrI)",
                   est = shrunken, lo = shrunken_low, hi = shrunken_high, nudge = -0.18)
)
fig1 <- ggplot(f1_long, aes(y = as.numeric(label) + nudge, x = est, colour = kind, shape = kind)) +
  annotate("rect", xmin = prim$pi_low, xmax = prim$pi_high, ymin = -Inf, ymax = Inf,
           fill = col_pos, alpha = 0.06) +
  geom_vline(xintercept = prim$estimate, colour = col_pos, linewidth = 0.4, linetype = "22") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5) +
  geom_point(size = 1.8) +
  scale_y_continuous(breaks = seq_along(levels(f1$label)), labels = levels(f1$label),
                     expand = expansion(add = 0.6)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.8),
                     breaks = seq(0, 0.8, 0.2)) +
  scale_colour_manual(values = c(col_obs, col_pos)) +
  scale_shape_manual(values = c(16, 18)) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Proportion in competitive or paid employment",
       y = NULL,
       title = paste0("Pooled point prevalence ", pct(prim$estimate), "% (95% CrI ",
                      pct(prim$ci_low), " to ", pct(prim$ci_high), "%); prediction interval ",
                      pct(prim$pi_low), " to ", pct(prim$pi_high), "% (shaded)")) +
  theme_pub + theme(strip.placement = "outside", plot.title = element_text(size = 8))
ggsave(file.path(fig_dir, "fig1_forest_prevalence.pdf"), fig1, width = 7.2, height = 4.6)

# -------------------------------
# Figure 2: IPS trials
# -------------------------------
bay_k3 <- tr_sum %>% filter(model == "primary_k3_hn05")
bay_k4 <- tr_sum %>% filter(model == "sens_k4_hn05")
reg_kh <- tr_freq %>% filter(model == "primary_k3_available_case", measure == "RR", interval == "knha")
reg_w  <- tr_freq %>% filter(model == "primary_k3_available_case", measure == "RR", interval == "wald")
rows2 <- bind_rows(
  tr_study %>% filter(analysis_set == "primary") %>%
    transmute(label = paste0(study_label, "  (", events_intervention, "/", n_intervention, " vs ",
                             events_control, "/", n_control, ")"),
              est = rr, lo = rr_ci_low, hi = rr_ci_high, kind = "Trial (95% CI)", group = "Trials"),
  tr_study %>% filter(analysis_set == "sensitivity_k4") %>%
    transmute(label = paste0(study_label, "  (", events_intervention, "/", n_intervention, " vs ",
                             events_control, "/", n_control, "); sensitivity only"),
              est = rr, lo = rr_ci_low, hi = rr_ci_high, kind = "Trial (95% CI)", group = "Trials"),
  data.frame(label = "Bayesian random effects, k = 3 (posterior median, 95% CrI)",
             est = bay_k3$estimate, lo = bay_k3$ci_low, hi = bay_k3$ci_high, kind = "Posterior", group = "Pooled"),
  data.frame(label = "Prediction interval for a new trial, k = 3",
             est = NA, lo = bay_k3$pi_low, hi = bay_k3$pi_high, kind = "Posterior", group = "Pooled"),
  data.frame(label = "Bayesian random effects, k = 4 sensitivity",
             est = bay_k4$estimate, lo = bay_k4$ci_low, hi = bay_k4$ci_high, kind = "Posterior", group = "Pooled"),
  data.frame(label = "Registered REML, Knapp-Hartung 95% CI, k = 3",
             est = reg_kh$estimate, lo = reg_kh$ci_low, hi = reg_kh$ci_high, kind = "Registered", group = "Pooled"),
  data.frame(label = "Registered REML, Wald 95% CI, k = 3",
             est = reg_w$estimate, lo = reg_w$ci_low, hi = reg_w$ci_high, kind = "Registered", group = "Pooled")
) %>% mutate(label = factor(label, levels = rev(label)),
             group = factor(group, levels = c("Trials", "Pooled")))
fig2 <- ggplot(rows2, aes(y = label, x = est, colour = kind, shape = kind)) +
  annotate("rect", xmin = 0.8, xmax = 1.25, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +
  geom_vline(xintercept = 1, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25, linewidth = 0.5) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_x_log10(breaks = c(0.25, 0.5, 0.8, 1.25, 2, 4, 8, 16, 32)) +
  scale_colour_manual(values = c(Posterior = col_pos, Registered = col_reg, `Trial (95% CI)` = col_obs)) +
  scale_shape_manual(values = c(Posterior = 18, Registered = 15, `Trial (95% CI)` = 16)) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Risk ratio for competitive employment (log scale); shaded band is the null interval",
       y = NULL) +
  theme_pub + theme(strip.text = element_blank())
ggsave(file.path(fig_dir, "fig2_forest_trials.pdf"), fig2, width = 7.2, height = 3.6)

# -------------------------------
# Figure 3: Dienes panels
# -------------------------------
dens <- data.frame(rr = exp(tr_draws$mu))
p_below <- di_null$p_rr_below_0.80[di_null$quantity == "pooled_effect"]
p_in    <- di_null$p_rr_in_null[di_null$quantity == "pooled_effect"]
p_above <- di_null$p_rr_above_1.25[di_null$quantity == "pooled_effect"]
fig3a <- ggplot(dens, aes(x = rr)) +
  annotate("rect", xmin = 0.8, xmax = 1.25, ymin = 0, ymax = Inf, fill = "grey85", alpha = 0.6) +
  geom_density(fill = col_pos, alpha = 0.25, colour = col_pos, linewidth = 0.5) +
  geom_vline(xintercept = 1, linewidth = 0.3) +
  scale_x_log10(breaks = c(0.5, 0.8, 1.25, 2, 4, 8), limits = c(0.4, 12)) +
  annotate("text", x = 3.2, y = Inf, vjust = 1.5, hjust = 0, size = 2.6,
           label = paste0("P(RR < 0.80) = ", fmt(p_below), "\nP(0.80 to 1.25) = ", fmt(p_in),
                          "\nP(RR > 1.25) = ", fmt(p_above))) +
  labs(x = "Pooled risk ratio (posterior, log scale)", y = "Density", title = "A  Posterior against the null interval") +
  theme_pub + theme(axis.text.y = element_blank())
grid_long <- bind_rows(lapply(c(0.25, 0.5, 1), function(ts) {
  data.frame(s = di_grid$s, tau_scale = ts,
             bf = if (ts == 0.5) di_grid$bf else NA_real_)
}))
# Recompute the curves for the two other tau scales (cheap) from the inputs
inputs <- read_csv(file.path(bay_dir, "trials_input_effects.csv"), show_col_types = FALSE) %>%
  filter(input_set == "primary_k3")
for (ts in c(0.25, 1)) {
  idx <- grid_long$tau_scale == ts
  grid_long$bf[idx] <- vapply(grid_long$s[idx], function(s) bf_re(inputs$yi, inputs$sei, s, ts), numeric(1))
}
fig3b <- ggplot(grid_long, aes(x = exp(s), y = bf, linetype = factor(tau_scale))) +
  annotate("rect", xmin = di_rr$rr_min, xmax = di_rr$rr_max, ymin = 0, ymax = Inf, fill = col_pos, alpha = 0.08) +
  geom_hline(yintercept = c(1 / 3, 1, 3), linewidth = 0.3, colour = "grey50") +
  geom_line(colour = col_pos, linewidth = 0.6) +
  geom_vline(xintercept = 2, linewidth = 0.4, linetype = "22") +
  scale_x_log10(breaks = c(1.05, 1.25, 1.5, 2, 3, 5, 10)) +
  scale_y_log10(breaks = c(0.33, 1, 3, 10, 30, 100), labels = c("1/3", "1", "3", "10", "30", "100")) +
  scale_linetype_manual(values = c("dotted", "solid", "dashed"),
                        labels = c("tau ~ HN(0.25)", "tau ~ HN(0.5)", "tau ~ HN(1)")) +
  labs(x = "H1 scale s expressed as a risk ratio, exp(s) (log scale)", y = "Bayes factor B_HN(0, s)",
       title = "B  Evidence as a function of the predicted scale of effect") +
  theme_pub
fig3 <- fig3a + fig3b + plot_layout(widths = c(1, 1.2))
ggsave(file.path(fig_dir, "fig3_dienes.pdf"), fig3, width = 7.2, height = 3.2)

# -------------------------------
# Supplementary figures
# -------------------------------
ps <- bind_rows(
  prev_sum %>% filter(model %in% c("sens_prior_hn05", "primary_strict_point_hn1", "sens_prior_hn2")) %>%
    transmute(analysis = "Point prevalence (strict set)", tau_scale, est = estimate, lo = ci_low, hi = ci_high),
  tr_sum %>% filter(model %in% c("sens_prior_hn025", "primary_k3_hn05", "sens_prior_hn1")) %>%
    transmute(analysis = "IPS trials, risk ratio", tau_scale, est = estimate, lo = ci_low, hi = ci_high)
)
figS1 <- ggplot(ps, aes(x = factor(tau_scale), y = est)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, colour = col_pos) +
  geom_point(colour = col_pos, size = 2) +
  facet_wrap(~ analysis, scales = "free") +
  labs(x = "Half-normal prior scale for tau", y = "Posterior median and 95% CrI") + theme_pub
ggsave(file.path(fig_dir, "figS1_prior_sensitivity.pdf"), figS1, width = 6, height = 3)

tau_df <- bind_rows(
  data.frame(analysis = "Point prevalence (strict set), prior HN(1)", tau = prev_draws$tau, scale = 1),
  data.frame(analysis = "IPS trials, prior HN(0.5)", tau = tr_draws$tau, scale = 0.5)
)
prior_df <- bind_rows(lapply(unique(tau_df$analysis), function(a) {
  sc <- unique(tau_df$scale[tau_df$analysis == a])
  x <- seq(0, 3, length.out = 300)
  data.frame(analysis = a, tau = x, dens = 2 * dnorm(x, 0, sc))
}))
figS2 <- ggplot(tau_df, aes(x = tau)) +
  geom_density(fill = col_pos, alpha = 0.25, colour = col_pos) +
  geom_line(data = prior_df, aes(y = dens), colour = col_obs, linetype = "22") +
  facet_wrap(~ analysis, scales = "free") + xlim(0, 3) +
  labs(x = "Between-study SD tau", y = "Density (posterior shaded; prior dashed)") + theme_pub
ggsave(file.path(fig_dir, "figS2_tau_posteriors.pdf"), figS2, width = 6, height = 3)

# -------------------------------
# Table 1: study characteristics
# -------------------------------
set_label <- function(p) {
  case_when(p$in_primary_set ~ "Primary",
            p$overlapping_cohort ~ "Overlapping cohort",
            !p$population_fep ~ "Outside primary estimand (A5)",
            p$definition_class == "composite" ~ "Composite outcome",
            p$definition_class == "retention" ~ "Retention outcome",
            p$outcome_basis == "period" ~ "Period prevalence",
            TRUE ~ "")
}
t1 <- prevalence %>%
  arrange(author, year) %>%
  transmute(Study = study_label, Country = country, Design = design,
            `N` = n_total, `n assessed` = n_assessed, `Age` = ifelse(is.na(mean_age), "", fmt(mean_age, 1)),
            `% male` = ifelse(is.na(pct_male), "", fmt(pct_male, 0)),
            `Months` = timepoint_months, Basis = outcome_basis,
            `Employed` = paste0(events, "/", n_assessed, " (", pct(events / n_assessed, 0), "%)"),
            EPHPP = quality_rating, Set = set_label(prevalence %>% arrange(author, year)))
write_csv(t1, file.path(tab_dir, "table1_study_characteristics.csv"))
write_tex(t1, "table1_study_characteristics.tex")

# -------------------------------
# Table 2: prevalence, Bayesian and registered
# -------------------------------
bay_row <- function(m, label) {
  r <- prev_sum %>% filter(model == m)
  data.frame(Analysis = label, k = r$k, `Estimate (95% CrI)` = cip(r$estimate, r$ci_low, r$ci_high),
             `Logit mean (SD)` = paste0(fmt(r$link_mean), " (", fmt(r$link_sd), ")"),
             `Prediction interval` = paste0(pct(r$pi_low), " to ", pct(r$pi_high)),
             `tau (95% CrI)` = ci(r$tau_median, r$tau_low, r$tau_high), check.names = FALSE)
}
frq_row <- function(m, label) {
  r <- prev_freq %>% filter(model == m, interval == "knha")
  data.frame(Analysis = label, k = r$k, `Estimate (95% CrI)` = paste0(cip(r$estimate, r$ci_low, r$ci_high), " (KH CI)"),
             `Logit mean (SD)` = "",
             `Prediction interval` = paste0(pct(r$pi_low), " to ", pct(r$pi_high)),
             `tau (95% CrI)` = paste0(fmt(r$tau), " (REML point estimate)"), check.names = FALSE)
}
t2 <- bind_rows(
  bay_row("primary_strict_point_hn1", "Primary: point prevalence, strict set, binomial-normal, tau ~ HN(1)"),
  frq_row("strict_point_primary", "Same set, registered REML logit model"),
  bay_row("secondary_period_hn1", "Secondary: period prevalence"),
  bay_row("sens_prior_hn05", "Prior sensitivity: tau ~ HN(0.5)"),
  bay_row("sens_prior_hn2", "Prior sensitivity: tau ~ HN(2)"),
  bay_row("sens_strict_excl_weak", "Excluding EPHPP Weak studies"),
  bay_row("sens_strict_excl_exposed", "Excluding samples all offered a vocational intervention"),
  bay_row("sens_strict_missing_not_employed_bound", "Lower bound: unassessed counted as not employed"),
  bay_row("sens_strict_normal_normal_hn1", "Normal-normal logit approximation"),
  bay_row("sens_point_relaxed_k14", "Construct-relaxed point prevalence (adds Chua, Williams, Lin, Rinaldi 2004)"),
  bay_row("sens_legacy_k23", "Legacy dissertation set, point and period combined"),
  frq_row("legacy_k23_corrected", "Legacy set, registered REML logit model, corrected data"),
  frq_row("legacy_k23_as_submitted", "Legacy set, registered REML logit model, as submitted")
)
write_csv(t2, file.path(tab_dir, "table2_prevalence.csv"))
write_tex(t2, "table2_prevalence.tex", align = "lrllll")

# -------------------------------
# Table 3: trials
# -------------------------------
trow <- function(m, label) {
  r <- tr_sum %>% filter(model == m)
  data.frame(Analysis = label, k = r$k,
             `RR (95% CrI)` = ci(r$estimate, r$ci_low, r$ci_high),
             `Log RR mean (SD)` = paste0(fmt(r$link_mean), " (", fmt(r$link_sd), ")"),
             `Prediction interval` = paste0(fmt(r$pi_low), " to ", fmt(r$pi_high)),
             `tau (95% CrI)` = ci(r$tau_median, r$tau_low, r$tau_high),
             `P(RR > 1)` = fmt(r$p_rr_gt_1, 3),
             `P(RR > 1.25)` = fmt(r$p_rr_above_1.25, 3), check.names = FALSE)
}
frow <- function(m, int, label) {
  r <- tr_freq %>% filter(model == m, measure == "RR", interval == int)
  data.frame(Analysis = label, k = r$k, `RR (95% CrI)` = paste0(ci(r$estimate, r$ci_low, r$ci_high), " (CI)"),
             `Log RR mean (SD)` = "",
             `Prediction interval` = "", `tau (95% CrI)` = paste0(fmt(sqrt(r$tau2)), " (REML)"),
             `P(RR > 1)` = ifelse(r$p_value < 0.001, "p < 0.001", paste0("p = ", fmt(r$p_value, 3))), `P(RR > 1.25)` = "", check.names = FALSE)
}
ex_k3 <- tr_exact %>% filter(model == "exact_binomial_k3")
t3 <- bind_rows(
  trow("primary_k3_hn05", "Primary: IPS + TAU vs TAU, log RR, tau ~ HN(0.5)"),
  frow("primary_k3_available_case", "knha", "Registered REML, Knapp-Hartung"),
  frow("primary_k3_available_case", "wald", "Registered REML, Wald"),
  trow("sens_prior_hn025", "Prior sensitivity: tau ~ HN(0.25)"),
  trow("sens_prior_hn1", "Prior sensitivity: tau ~ HN(1)"),
  trow("loo_ERICKSON_2021", "Leave out Erickson 2021"),
  trow("loo_KILLACKEY_2008", "Leave out Killackey 2008"),
  trow("loo_KILLACKEY_2019", "Leave out Killackey 2019"),
  trow("sens_k3_missing_not_employed", "Missing counted as not employed (randomised denominators)"),
  trow("sens_k4_hn05", "Sensitivity: adds Nuechterlein 2020 (active vocational comparator)"),
  trow("sens_k4_missing_not_employed", "k = 4, missing counted as not employed"),
  data.frame(Analysis = "Exact binomial arm-level model, k = 3 (odds-ratio scale)", k = 3,
             `RR (95% CrI)` = paste0("OR ", ci(ex_k3$or, ex_k3$or_low, ex_k3$or_high)),
             `Log RR mean (SD)` = paste0("log OR ", fmt(ex_k3$link_mean), " (", fmt(ex_k3$link_sd), ")"),
             `Prediction interval` = paste0("RR at fixed control risk ", pct(ex_k3$pooled_control_risk, 0),
                                            "% (illustrative): ",
                                            ci(ex_k3$rr_at_p0, ex_k3$rr_at_p0_low, ex_k3$rr_at_p0_high)),
             `tau (95% CrI)` = ci(ex_k3$tau_median, ex_k3$tau_low, ex_k3$tau_high),
             `P(RR > 1)` = paste0("P(OR > 1) = ", fmt(ex_k3$p_or_gt_1, 3)), `P(RR > 1.25)` = "", check.names = FALSE)
)
write_csv(t3, file.path(tab_dir, "table3_trials.csv"))
write_tex(t3, "table3_trials.tex", align = "lrllllll")

# -------------------------------
# Table 4: Dienes hypothesis tests
# -------------------------------
bf_wide <- di_bf %>%
  mutate(label = c(primary_log2 = "log(2.00), corrected Frederick and VanderWeele re-analysis (primary)",
                   frederick_published = "log(1.63), Frederick and VanderWeele as published",
                   bond2015 = "log(1.69), Bond et al. 2015",
                   bond2016 = "log(1.96), Bond et al. 2016",
                   modini2016 = "log(2.40), Modini et al. 2016")[h1_scale_label]) %>%
  select(label, tau_scale, bf_random_effects) %>%
  tidyr::pivot_wider(names_from = tau_scale, values_from = bf_random_effects, names_prefix = "tau HN(") %>%
  rename_with(~ paste0(.x, ")"), starts_with("tau HN(")) %>%
  left_join(di_bf2 %>% mutate(label = c(primary_log2 = "log(2.00), corrected Frederick and VanderWeele re-analysis (primary)",
                                         frederick_published = "log(1.63), Frederick and VanderWeele as published",
                                         bond2015 = "log(1.69), Bond et al. 2015",
                                         bond2016 = "log(1.96), Bond et al. 2016",
                                         modini2016 = "log(2.40), Modini et al. 2016")[h1_scale_label]) %>%
              select(label, `Fixed effect` = bf_fixed_effect), by = "label") %>%
  mutate(across(where(is.numeric), ~ fmt(.x, 2))) %>%
  rename(`H1 scale s` = label)
write_csv(bf_wide, file.path(tab_dir, "table4_bayes_factors.csv"))
write_tex(bf_wide, "table4_bayes_factors.tex", align = "lrrrr")

null_tab <- di_null %>%
  mutate(Quantity = c(pooled_effect = "Pooled effect", new_trial = "A new trial",
                      pooled_effect_normal_approx = "Pooled effect, normal approximation")[quantity]) %>%
  transmute(Quantity, `P(RR < 0.80)` = fmt(p_rr_below_0.80, 3), `P(0.80 to 1.25)` = fmt(p_rr_in_null, 3),
            `P(RR > 1.25)` = fmt(p_rr_above_1.25, 3), `P(RR > 1)` = fmt(p_rr_gt_1, 3))
write_csv(null_tab, file.path(tab_dir, "table4_null_interval.csv"))
write_tex(null_tab, "table4_null_interval.tex", align = "lrrrr")

# -------------------------------
# Supplementary tables
# -------------------------------
mr_tab <- mr %>%
  transmute(Model = model, Term = term, `Logit contrast (95% CrI)` = ci(estimate_logit, ci_low, ci_high),
            `Odds ratio (95% CrI)` = ci(odds_ratio, or_low, or_high), `P(> 0)` = fmt(p_gt_0, 3))
write_csv(mr_tab, file.path(tab_dir, "tableS_metaregression.csv"))
write_tex(mr_tab, "tableS_metaregression.tex")

other_tab <- tr_other %>%
  transmute(Study = study_label, Comparison = comparison_type, Intervention = intervention_name,
            Comparator = comparator,
            `Events/n intervention` = paste0(events_intervention, "/", n_intervention),
            `Events/n control` = paste0(events_control, "/", n_control),
            `RR (95% CI)` = ci(rr, rr_ci_low, rr_ci_high))
check(identical(names(other_tab), c("Study", "Comparison", "Intervention", "Comparator",
                                    "Events/n intervention", "Events/n control", "RR (95% CI)")),
      "tableS_other_comparisons: unexpected column names")
check(!any(grepl("minimal", unlist(other_tab), fixed = TRUE)),
      "tableS_other_comparisons: a name-repair token leaked into the table")
write_csv(other_tab, file.path(tab_dir, "tableS_other_comparisons.csv"))
write_tex(other_tab %>% select(-Intervention, -Comparator), "tableS_other_comparisons.tex", col_spec = "lp{5.5cm}llp{2.6cm}")

diag_tab <- diag_all %>%
  transmute(Model = model, `Max R-hat` = fmt(max_rhat, 3), `Min bulk ESS` = round(min_ess_bulk),
            `Min tail ESS` = round(min_ess_tail), Divergences = divergences,
            `Max tree depth` = max_treedepth, `Min E-BFMI` = fmt(min_ebfmi, 2), Gate = ifelse(passes_gate, "pass", "fail"))
write_csv(diag_tab, file.path(tab_dir, "tableS_mcmc_diagnostics.csv"))
write_tex(diag_tab, "tableS_mcmc_diagnostics.tex")

loo_tab <- prev_loo %>% transmute(`Study left out` = left_out, k, `Estimate (95% CrI)` = cip(estimate, ci_low, ci_high))
write_csv(loo_tab, file.path(tab_dir, "tableS_prevalence_loo.csv"))
write_tex(loo_tab, "tableS_prevalence_loo.tex")

bias_tab <- bias %>%
  transmute(Model = model, k, `Egger t (df)` = paste0(fmt(egger_t), " (", egger_df, ")"), `Egger p` = fmt(egger_p, 3),
            `Trim-and-fill k0` = trimfill_k0, `Adjusted prevalence (95% CI)` = cip(adjusted_prevalence, adjusted_ci_low, adjusted_ci_high),
            `p vs 50%` = fmt(adjusted_p_vs_50pct, 4))
write_csv(bias_tab, file.path(tab_dir, "tableS_publication_bias.csv"))
write_tex(bias_tab, "tableS_publication_bias.tex")

# Recovery outcomes: Hedges' g for Chua, correlations for Lin and Pothier
rec_smd <- recovery %>% filter(effect_type == "smd")
g <- metafor::escalc(measure = "SMD", m1i = mean1, sd1i = sd1, n1i = n1, m2i = mean2, sd2i = sd2, n2i = n2,
                     data = rec_smd)
rec_tab <- bind_rows(
  data.frame(Study = rec_smd$study_label, Outcome = rec_smd$outcome_measure,
             Statistic = paste0("Hedges g = ", fmt(g$yi), " (", fmt(g$yi - 1.96 * sqrt(g$vi)), " to ", fmt(g$yi + 1.96 * sqrt(g$vi)), ")"),
             Note = "Employed minus unemployed; higher PANSS is worse, higher GAF is better"),
  recovery %>% filter(effect_type == "correlation") %>%
    transmute(Study = study_label, Outcome = outcome_measure,
              Statistic = paste0("r = ", fmt(r), ", n = ", n_r), Note = "Correlation with employment")
)
write_csv(rec_tab, file.path(tab_dir, "tableS_recovery.csv"))
write_tex(rec_tab, "tableS_recovery.tex", col_spec = "llp{5cm}p{5.5cm}")

arms <- read_csv(root_path("data", "derived", "trial_arms.csv"), show_col_types = FALSE) %>%
  transmute(Trial = study_label, Set = ifelse(analysis_set == "primary", "Primary", "k = 4 sensitivity"),
            Arm = ifelse(arm_ips == 1, "IPS", "Control"), Employed = events,
            `n analysed` = n_analysed, `n randomised` = n_full)
write_csv(arms, file.path(tab_dir, "tableS_trial_counts.csv"))
write_tex(arms, "tableS_trial_counts.tex")

ext_tab <- external %>% transmute(Model = model, k, `RR (95% CI)` = ci(rr, ci_low, ci_high), `tau2` = fmt(tau2, 3), `I2 (%)` = fmt(i2, 1))
write_tex(ext_tab, "tableS_external_frederick.tex")

# Correction and reconciliation tables for the supplement
corr <- read_csv(root_path("data", "derived", "corrections.csv"), show_col_types = FALSE) %>%
  transmute(Study = study_id_clean, Row = effect_type, Field = field, Raw = raw, Corrected = corrected, Source = source)
write_tex(corr, "tableS_corrections.tex", col_spec = "lllp{3.2cm}p{5cm}p{6.5cm}")

cat("Figures and tables written to", fig_dir, "and", tab_dir, "\n")
cat("\nDienes decision:\n"); print(di_dec)
cat("\nBF comparison (quadrature vs bridge):\n"); print(di_cmp)
