# ===============================================
# 03 - Employment and recovery
# ===============================================
# Description:
#   Analysis 3 of the early-psychosis employment meta-analysis (PROSPERO CRD420261402841).
#   Examines whether employment and vocational engagement are associated with broader
#   recovery (PANSS, SOFAS, GAF, quality of life). Two kinds of evidence are pooled
#   separately because they sit on different scales:
#     - correlations            (escalc measure = "ZCOR"), columns r, n_r
#     - employed vs unemployed   (escalc measure = "SMD"),  columns mean1, sd1, n1,
#       group comparisons                                    mean2, sd2, n2
#   This evidence is expected to be sparser, so some studies may be described narratively.
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
# 2. Read the data
# -------------------------------
dat <- read.csv("FEP_employment_data_extraction_template.csv", stringsAsFactors = FALSE)
dat <- dat[!grepl("^EXAMPLE", dat$study_id), ]

# -------------------------------
# 3. Correlations (Fisher's z)
# -------------------------------
corr <- dat[dat$effect_type == "correlation", ]
if (nrow(corr) > 0) {
  es_r <- escalc(measure = "ZCOR", ri = r, ni = n_r, data = corr)
  res_r <- rma(yi, vi, data = es_r, method = "REML", test = "knha")
  summary(res_r)

  pred_r <- predict(res_r, transf = transf.ztor)
  cat("\nPooled correlation (r):",
      sprintf("%.2f [%.2f, %.2f]\n", pred_r$pred, pred_r$ci.lb, pred_r$ci.ub))
  cat(sprintf("95%% prediction interval: %.2f to %.2f\n", pred_r$pi.lb, pred_r$pi.ub))

  forest(res_r,
         transf = transf.ztor,
         slab = paste(es_r$author, es_r$year),
         header = "Study",
         xlab = "Correlation (r)",
         refline = 0,
         addpred = TRUE,
         cex = 0.8)
} else {
  message("No correlation rows yet; describe this evidence narratively until available.")
}

# -------------------------------
# 4. Group comparisons (standardised mean difference, Hedges' g)
# -------------------------------
smd <- dat[dat$effect_type == "smd", ]
if (nrow(smd) > 0) {
  es_d <- escalc(measure = "SMD",
                 m1i = mean1, sd1i = sd1, n1i = n1,
                 m2i = mean2, sd2i = sd2, n2i = n2,
                 data = smd)
  res_d <- rma(yi, vi, data = es_d, method = "REML", test = "knha")
  summary(res_d)

  cat("\nPooled SMD (Hedges' g):",
      sprintf("%.2f [%.2f, %.2f]\n", res_d$b, res_d$ci.lb, res_d$ci.ub))
  pred_d <- predict(res_d)
  cat(sprintf("95%% prediction interval: %.2f to %.2f\n", pred_d$pi.lb, pred_d$pi.ub))

  forest(res_d,
         slab = paste(es_d$author, es_d$year, es_d$outcome_measure),
         header = "Study",
         xlab = "Standardised mean difference (g)",
         refline = 0,
         addpred = TRUE,
         cex = 0.8)
} else {
  message("No SMD rows yet; describe this evidence narratively until available.")
}
