# ===============================================
# 07 - External prior: Frederick and VanderWeele (2019) re-analysis
# ===============================================
# Description:
#   Reproduces the pooled risk ratio for competitive employment at any time
#   (IPS versus control) reported by Frederick DE, VanderWeele TJ (2019),
#   PLoS ONE 14(2): e0212208, doi 10.1371/journal.pone.0212208, and then
#   refits the model with the 2 x 2 cells assigned correctly.
#
#   The authors' S2 File (pone.0212208.s002.R, lines 26 and 34 to 38) runs
#     df <- subset(df, df$ips_any > 0)
#     res <- rma(ai = ips_any, bi = n_ips_randomized,
#                ci = tau_any, di = n_tau_randomized,
#                data = df, measure = "RR", slab = authors, method = "REML")
#   In metafor, bi and di are the NON-event cells, so passing the arm totals
#   makes every denominator (events + n) and shrinks each risk ratio towards
#   1. The correct call passes the arm totals as n1i and n2i.
#
#   Data: S3 File (zipped .rds files) and S2 File (R code) downloaded from
#   PLOS ONE on 2026-09-02 into data/external/frederick2019/, see
#   data/external/README.md. The download only runs when the committed copy
#   is missing, so the script works offline.
#
#   Output: output/tables/external_frederick2019_reanalysis.csv
# ===============================================

# -------------------------------
# 1. Packages and working directory
# -------------------------------
if (!requireNamespace("metafor", quietly = TRUE)) install.packages("metafor")
library(metafor)

# run_all.R runs this from the repository root; allow a direct run from R/
if (!dir.exists("data") && dir.exists("../data")) setwd("..")

# -------------------------------
# 2. Data (committed copy, downloaded only if absent)
# -------------------------------
ext_dir  <- "data/external/frederick2019"
rds_path <- file.path(ext_dir, "competitive_employment_any.rds")
zip_path <- file.path(ext_dir, "pone.0212208.s003.zip")
plos_url <- paste0("https://journals.plos.org/plosone/article/file?",
                   "type=supplementary&id=10.1371/journal.pone.0212208.")

if (!file.exists(rds_path)) {
  dir.create(ext_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(zip_path)) {
    download.file(paste0(plos_url, "s003"), zip_path, mode = "wb")
  }
  unzip(zip_path, files = "competitive_employment_any.rds", exdir = ext_dir)
}

df <- readRDS(rds_path)
df$authors <- paste(sapply(strsplit(df$article, ","), `[`, 1), df$year)

# The authors' subset() drops the two trials with NA counts (Bejerholm 2015,
# Tsang 2009); no trial has a zero cell, so this is the only exclusion.
dropped <- df$authors[is.na(df$ips_any) | !(df$ips_any > 0)]
df <- subset(df, df$ips_any > 0)
cat("Trials in the data:", nrow(df) + length(dropped),
    "| dropped for NA counts:", paste(dropped, collapse = "; "), "\n")

# -------------------------------
# 3. Models
# -------------------------------
# (a) Authors' model verbatim: arm totals passed as bi/di, REML tau-squared
res_auth <- rma(ai = ips_any, bi = n_ips_randomized,
                ci = tau_any, di = n_tau_randomized,
                data = df, measure = "RR", slab = authors, method = "REML")

# (b) Corrected cells, same estimator (REML)
res_corr <- rma(ai = ips_any, n1i = n_ips_randomized,
                ci = tau_any, n2i = n_tau_randomized,
                data = df, measure = "RR", slab = authors, method = "REML")

# (c) Corrected cells, REML with the Knapp-Hartung adjustment
res_kh <- rma(ai = ips_any, n1i = n_ips_randomized,
              ci = tau_any, n2i = n_tau_randomized,
              data = df, measure = "RR", slab = authors, method = "REML",
              test = "knha")

# -------------------------------
# 4. Check the published estimate reproduces, then tabulate
# -------------------------------
published   <- c(1.63, 1.46, 1.82)
reproduced  <- round(exp(c(res_auth$b, res_auth$ci.lb, res_auth$ci.ub)), 2)
if (!isTRUE(all.equal(as.numeric(reproduced), published))) {
  stop("Authors' model does not reproduce the published RR 1.63 (1.46 to 1.82); got ",
       paste(reproduced, collapse = ", "))
}

row_of <- function(model, res, note) {
  data.frame(model = model, k = res$k,
             rr = round(exp(res$b)[1], 3),
             ci_low = round(exp(res$ci.lb), 3), ci_high = round(exp(res$ci.ub), 3),
             tau2 = round(res$tau2, 4), i2 = round(res$I2, 1),
             note = note, stringsAsFactors = FALSE)
}

tab <- rbind(
  row_of("authors_published", res_auth,
         "Authors' S2 code: arm totals passed as bi/di (non-event cells); REML"),
  row_of("corrected_reml", res_corr,
         "Arm totals passed as n1i/n2i; REML (same estimator as authors)"),
  row_of("corrected_reml_knha", res_kh,
         "Arm totals passed as n1i/n2i; REML with Knapp-Hartung adjustment")
)

# -------------------------------
# 5. Output
# -------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
out_path <- "output/tables/external_frederick2019_reanalysis.csv"
write.csv(tab, out_path, row.names = FALSE)

print(tab, row.names = FALSE, right = FALSE)
cat("\nWritten:", out_path, "\n")
