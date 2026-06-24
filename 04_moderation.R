# ===============================================
# 04 - Moderation (subgroup analysis and meta-regression)
# ===============================================
# Description:
#   Analysis 4 of the early-psychosis employment meta-analysis (PROSPERO CRD420261402841).
#   Explains heterogeneity in the pooled employment rate using the study-level fields that
#   are already recorded: design, follow_up_months, region, setting and diagnosis. Nothing
#   extra is extracted; the moderators are existing columns. Subgroup analysis and
#   meta-regression only become meaningful once enough studies are available, so add or
#   drop terms to avoid overfitting.
#
#   Builds on the proportion rows (effect_type == "proportion"), as in
#   01_pooled_employment_rate.R.
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
# 2. Read, filter and reduce to the primary endpoint (closest to 12 months)
# -------------------------------
dat <- read.csv("FEP_employment_data_extraction_template.csv", stringsAsFactors = FALSE)
dat <- dat[!grepl("^EXAMPLE", dat$study_id), ]
prop <- dat[dat$effect_type == "proportion", ]

if (nrow(prop) == 0) {
  stop("No rows with effect_type == 'proportion' found. Populate the template first.")
}

primary <- prop %>%
  group_by(study_id) %>%
  slice_min(abs(timepoint_months - 12), n = 1, with_ties = FALSE) %>%
  ungroup()

es <- escalc(measure = "PLO", xi = events, ni = n_assessed, data = primary)

# -------------------------------
# 3. Subgroup analysis (example: by diagnostic definition)
# -------------------------------
res_sub <- rma(yi, vi, mods = ~ factor(diagnosis), data = es, method = "REML", test = "knha")
summary(res_sub)
cat(sprintf("\nTest of moderators (diagnosis): QM(df = %d) = %.2f, p = %.3f\n",
            res_sub$QMdf[1], res_sub$QM, res_sub$QMp))

# -------------------------------
# 4. Meta-regression on the study-level moderators
#    (add or drop terms as the number of studies allows)
# -------------------------------
res_reg <- rma(yi, vi,
               mods = ~ factor(design) + follow_up_months + factor(region) + factor(diagnosis),
               data = es, method = "REML", test = "knha")
summary(res_reg)

cat(sprintf("\nResidual tau^2 = %.3f, residual I^2 = %.1f%%\n", res_reg$tau2, res_reg$I2))
cat(sprintf("R^2 (heterogeneity explained) = %.1f%%\n", res_reg$R2))
cat(sprintf("Test of moderators: QM(df = %d) = %.2f, p = %.3f\n",
            res_reg$QMdf[1], res_reg$QM, res_reg$QMp))
