# ===============================================
# run_all.R - reproduce the full analysis
# ===============================================
# Usage, from the repository root:
#   Rscript run_all.R            # uses cached Bayesian fits if present
#   Rscript run_all.R --refit    # ignores the cache and refits every model
#
# Runs the numbered scripts in order, each in a fresh environment, and writes
# output/session_info.txt. The pinned versions below are the ones the reported
# results were generated with; the run aborts if they differ, because posterior
# summaries can change across CmdStan and brms releases.
# ===============================================

options(warn = 1)
if ("--refit" %in% commandArgs(trailingOnly = TRUE)) options(fep.refit = TRUE)

if (!file.exists("run_all.R")) stop("Run this script from the repository root.")

pinned <- c(brms = "2.23.0", cmdstanr = "0.9.0", metafor = "4.6.0", posterior = "1.6.0")
for (pkg in names(pinned)) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Package not installed: ", pkg)
}
installed <- vapply(names(pinned), function(p) as.character(packageVersion(p)), character(1))
mismatch  <- installed != pinned
if (any(mismatch)) {
  stop("Package version mismatch (installed vs pinned): ",
       paste0(names(pinned)[mismatch], " ", installed[mismatch], " vs ", pinned[mismatch],
              collapse = "; "),
       "\nInstall the pinned versions or edit `pinned` deliberately and re-run.")
}
cmdstan_v <- as.character(cmdstanr::cmdstan_version())
if (cmdstan_v != "2.36.0") stop("CmdStan ", cmdstan_v, " installed; results were generated with 2.36.0.")

scripts <- c(
  "R/00_clean_data.R",
  "R/01_frequentist_registered.R",
  "R/02_bayes_prevalence.R",
  "R/03_bayes_trials.R",
  "R/04_bayes_metaregression.R",
  "R/05_dienes_inference.R",
  "R/07_external_prior_frederick2019.R",
  "R/06_figures_tables.R"
)

t0 <- Sys.time()
for (s in scripts) {
  cat("\n==================== ", s, " ====================\n", sep = "")
  source(s, local = new.env(), echo = FALSE)
}

dir.create("output", showWarnings = FALSE)
writeLines(c(
  paste("Run completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Elapsed:", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes"),
  paste("CmdStan:", cmdstan_v),
  "",
  capture.output(sessionInfo())
), "output/session_info.txt")
cat("\nDone. Session information written to output/session_info.txt\n")
