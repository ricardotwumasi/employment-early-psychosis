# ===============================================
# 01 - Pooled employment rate (early psychosis)
# ===============================================
# Description:
#   Analysis 1 of the early-psychosis employment meta-analysis (PROSPERO CRD420261402841).
#   Pools the prevalence of competitive or paid employment across first-episode samples.
#   Proportions are pooled on the logit scale (escalc measure = "PLO") and back-transformed
#   to a percentage. The time point closest to twelve months is the primary estimate; other
#   time points are kept for sensitivity analyses.
#
#   Reads FEP_employment_data_extraction_template.csv and uses the rows where
#   effect_type == "proportion" (columns: events, n_assessed).
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

# Drop the worked-example rows and keep only proportion results
dat <- dat[!grepl("^EXAMPLE", dat$study_id), ]
prop <- dat[dat$effect_type == "proportion", ]

if (nrow(prop) == 0) {
  stop("No rows with effect_type == 'proportion' found. Populate the template first.")
}

# Primary endpoint: per study, keep the row whose time point is closest to 12 months.
# The remaining time points can be analysed separately as a sensitivity analysis.
primary <- prop %>%
  group_by(study_id) %>%
  slice_min(abs(timepoint_months - 12), n = 1, with_ties = FALSE) %>%
  ungroup()

# -------------------------------
# 3. Effect sizes on the logit scale
# -------------------------------
es <- escalc(measure = "PLO", xi = events, ni = n_assessed, data = primary)

# -------------------------------
# 4. Random-effects model (REML, Knapp-Hartung)
# -------------------------------
res <- rma(yi, vi, data = es, method = "REML", test = "knha")
summary(res)

# Back-transform the pooled estimate to a proportion
pred <- predict(res, transf = transf.ilogit)
cat("\nPooled employment rate:",
    sprintf("%.1f%% [%.1f%%, %.1f%%]\n",
            100 * pred$pred, 100 * pred$ci.lb, 100 * pred$ci.ub))

# Heterogeneity and prediction interval (back-transformed to the proportion scale)
cat(sprintf("tau^2 = %.3f, I^2 = %.1f%%, Q(%d) = %.2f, p = %.3f\n",
            res$tau2, res$I2, res$k - 1, res$QE, res$QEp))
cat(sprintf("95%% prediction interval: %.1f%% to %.1f%%\n",
            100 * pred$pi.lb, 100 * pred$pi.ub))

# -------------------------------
# 5. Forest plot (back-transformed to proportions)
# -------------------------------
forest(res,
       transf = transf.ilogit,
       slab = paste(es$author, es$year),
       header = "Study",
       xlab = "Employment rate",
       refline = NA,
       addpred = TRUE,
       cex = 0.8)

# -------------------------------
# 6. Influence and publication bias
#    (publication-bias diagnostics need roughly 10 or more studies; interpret cautiously)
# -------------------------------
baujat(res)             # influential studies
funnel(res)             # funnel plot
regtest(res)            # Egger's test
trimfill(res)           # trim-and-fill adjusted estimate
fsn(yi, vi, data = es)  # fail-safe N
