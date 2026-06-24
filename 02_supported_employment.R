# ===============================================
# 02 - Supported employment vs treatment as usual
# ===============================================
# Description:
#   Analysis 2 of the early-psychosis employment meta-analysis (PROSPERO CRD420261402841).
#   Pools the randomised trials of supported employment (Individual Placement and Support,
#   Supported Employment and Education, NAVIGATE, Horyzons) against treatment as usual.
#   The primary effect is a risk ratio (escalc measure = "RR"); an odds ratio is reported
#   as a sensitivity check.
#
#   Uses rows where effect_type == "trial_binary" (columns: events_intervention,
#   n_intervention, events_control, n_control).
# ===============================================

# -------------------------------
# 1. Load packages
# -------------------------------
required_packages <- c("metafor", "dplyr")
lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# -------------------------------
# 2. Read and filter the data
# -------------------------------
dat <- read.csv("FEP_employment_data_extraction_template.csv", stringsAsFactors = FALSE)
dat <- dat[!grepl("^EXAMPLE", dat$study_id), ]
trials <- dat[dat$effect_type == "trial_binary", ]

if (nrow(trials) == 0) {
  stop("No rows with effect_type == 'trial_binary' found. Populate the template first.")
}

# -------------------------------
# 3. Primary effect: risk ratio
#    ai / n1i = intervention arm, ci / n2i = control arm
# -------------------------------
es_rr <- escalc(measure = "RR",
                ai = events_intervention, n1i = n_intervention,
                ci = events_control,      n2i = n_control,
                data = trials)

res_rr <- rma(yi, vi, data = es_rr, method = "REML", test = "knha")
summary(res_rr)

cat("\nPooled risk ratio:",
    sprintf("%.2f [%.2f, %.2f]\n",
            exp(res_rr$b), exp(res_rr$ci.lb), exp(res_rr$ci.ub)))
cat(sprintf("tau^2 = %.3f, I^2 = %.1f%%\n", res_rr$tau2, res_rr$I2))

forest(res_rr,
       transf = exp,
       refline = 1,
       slab = paste(es_rr$author, es_rr$year),
       header = "Trial",
       xlab = "Risk ratio (employment)",
       cex = 0.8)

# -------------------------------
# 4. Sensitivity check: odds ratio
# -------------------------------
es_or <- escalc(measure = "OR",
                ai = events_intervention, n1i = n_intervention,
                ci = events_control,      n2i = n_control,
                data = trials)
res_or <- rma(yi, vi, data = es_or, method = "REML", test = "knha")
cat("Sensitivity (odds ratio):",
    sprintf("%.2f [%.2f, %.2f]\n",
            exp(res_or$b), exp(res_or$ci.lb), exp(res_or$ci.ub)))

# -------------------------------
# 5. Optional: a trial reporting only an adjusted odds ratio with a confidence interval
#    can be folded in as a pre-computed effect. Record reported_es / reported_ci_low /
#    reported_ci_high, then combine with the risk-ratio set on the log scale, for example:
#
#      adj <- dat[dat$reported_es_type == "OR" & !is.na(dat$reported_es), ]
#      adj_es <- escalc(measure = "GEN",
#                       yi = log(adj$reported_es),
#                       vi = ((log(adj$reported_ci_high) - log(adj$reported_ci_low)) /
#                             (2 * qnorm(0.975)))^2)
#
#    and rbind the relevant yi/vi columns before fitting rma(). Mixing risk ratios and
#    odds ratios changes the interpretation, so do this only in the odds-ratio model.
# -------------------------------
