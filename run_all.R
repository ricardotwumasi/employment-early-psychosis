# ===============================================
# run_all.R - reproduce the full analysis
# ===============================================
# Usage, from the repository root:
#   Rscript run_all.R                # uses cached Bayesian fits if present
#   Rscript run_all.R --refit        # ignores the cache and refits every model
#   Rscript run_all.R --cache-only   # forbids fitting; stops if any cache entry
#                                     # is missing, unreadable or stale
#   Rscript run_all.R --release      # refit + requires a synchronised renv.lock,
#                                     # a clean git tree, and that every model
#                                     # was actually fitted (no cache hits)
#   Rscript run_all.R --preflight    # pre-fit check (no fitting, no caches
#                                     # needed): release inputs re-derive to the
#                                     # committed files, registry schema and
#                                     # membership agree; exits 1 on failure
#
# Runs the numbered scripts in order, each in a fresh environment, and writes
# output/session_info.txt and output/run_manifest.json. The pinned versions
# below are the ones the reported results were generated with; the run aborts
# if they differ, because posterior summaries can change across CmdStan and
# brms releases.
# ===============================================

options(warn = 1)
if (!file.exists("run_all.R")) stop("Run this script from the repository root.")
source("R/utils.R")

args        <- commandArgs(trailingOnly = TRUE)
opt_refit   <- "--refit" %in% args
opt_cache_only <- "--cache-only" %in% args
opt_release <- "--release" %in% args
opt_preflight <- "--preflight" %in% args

if (opt_cache_only && (opt_refit || opt_release)) {
  stop("CACHE_ONLY: --cache-only cannot be combined with --refit or --release.")
}
if (opt_release) opt_refit <- TRUE
if (opt_refit) options(fep.refit = TRUE)
if (opt_cache_only) options(fep.cache_only = TRUE)

mode <- if (opt_release) "release" else if (opt_cache_only) "cache-only" else if (opt_refit) "refit" else "cache"

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

# -------------------------------
# --preflight: checks before any fit (A42)
# -------------------------------
if (opt_preflight) {
  fails <- preflight_release()
  if (length(fails) > 0) {
    cat("preflight FAILED:\n", paste0("  - ", fails, "\n"), sep = "")
    quit(save = "no", status = 1)
  }
  cat("preflight passed: environment pins, release inputs re-derived from the decision tables,",
      "registry schema and registry membership agree\n")
  quit(save = "no", status = 0)
}

# -------------------------------
# --release preconditions
# -------------------------------
if (opt_release) {
  options(fep.release = TRUE)
  check_release_preconditions()
}

# -------------------------------
# Provenance manifest
# -------------------------------
# Registered before the scripts run, so a failure partway through still
# leaves output/run_manifest.json describing what happened.
manifest_inputs <- c(
  "data/yanan_data_280826.csv",
  "data/review/correction_ledger.csv",
  "data/review/population_eligibility.csv",
  "data/review/release_construct_review.csv",
  "data/review/release_new_results.csv",
  "data/review/amendment_dates.csv",
  "data/review/candidate_results.csv",
  "data/review/source_review_2026-09-23/results_draft.csv",
  "data/review/source_review_2026-09-23/candidate_inventory_addendum_2026-10-16.csv",
  "data/review/source_review_2026-09-23/cohort_draft.csv",
  "data/review/source_review_2026-09-23/signoff_2026-10-16.csv",
  "docs/amendment_2026-09-02.md",
  "docs/amendment_2026-09-06.md",
  "data/registry/fit_registry.csv",
  "data/schema/raw_extraction_schema_v1.csv",
  "docs/analysis_specification_2026-09-01.txt",
  "data/external/frederick2019/competitive_employment_any.rds"
)
if (file.exists("renv.lock")) manifest_inputs <- c(manifest_inputs, "renv.lock")

manifest_package_list <- c("brms", "cmdstanr", "metafor", "posterior", "bridgesampling",
                           "bayesplot", "digest", "readr", "dplyr", "ggplot2")

write_manifest <- function(m) {
  input_hashes <- setNames(
    lapply(manifest_inputs, function(f) if (file.exists(f)) file_sha256(f) else NA_character_),
    manifest_inputs
  )
  out_files <- list.files(root_path("output"), recursive = TRUE, full.names = TRUE)
  out_files <- out_files[!grepl("/bayesian/fits/", out_files, fixed = TRUE)]
  out_files <- out_files[!grepl("log", basename(out_files), ignore.case = TRUE)]
  output_hashes <- setNames(
    lapply(out_files, file_sha256),
    paste0("output/", sub(paste0(root_path("output"), "/"), "", out_files, fixed = TRUE))
  )
  packages <- setNames(
    lapply(manifest_package_list, function(p) {
      tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)
    }),
    manifest_package_list
  )
  fits <- fit_log()
  manifest <- list(
    status          = m$status,
    failure_message = m$failure_message,
    mode            = mode,
    started_at      = format(m$started_at, "%Y-%m-%d %H:%M:%S %Z"),
    finished_at     = format(m$finished_at, "%Y-%m-%d %H:%M:%S %Z"),
    elapsed_minutes = as.numeric(round(difftime(m$finished_at, m$started_at, units = "mins"), 2)),
    git_start = m$git_start,
    git = list(
      commit = tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE), error = function(e) NA_character_),
      dirty  = length(tryCatch(system2("git", c("status", "--porcelain"), stdout = TRUE),
                               error = function(e) character(0))) > 0
    ),
    input_hashes  = input_hashes,
    r_version     = R.version.string,
    packages      = packages,
    cmdstan       = list(version = cmdstan_v,
                        path = tryCatch(as.character(cmdstanr::cmdstan_path()), error = function(e) NA_character_)),
    fits            = if (nrow(fits) == 0) list() else fits,
    gate_attempts_n = length(getOption("fep.gate_attempts", list())),
    output_hashes   = output_hashes
  )
  dir.create(root_path("output"), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(manifest, root_path("output", "run_manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE, digits = NA)
  write_gate_attempts(root_path("output", "bayesian", "gate_attempts.csv"))
}

run_pipeline <- function() {
  m <- new.env()
  m$status <- "running"
  m$failure_message <- NULL
  m$started_at <- Sys.time()
  # Tree state before any script writes output (the post-run "git" entry is
  # dirty whenever generated outputs changed, which says nothing about the start)
  m$git_start <- list(
    commit = tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE), error = function(e) NA_character_),
    dirty  = length(tryCatch(system2("git", c("status", "--porcelain"), stdout = TRUE),
                             error = function(e) character(0))) > 0
  )
  on.exit({
    m$finished_at <- Sys.time()
    write_manifest(m)
  }, add = TRUE)

  tryCatch({
    scripts <- c(
      "R/00_clean_data.R",
      "R/00b_release_inputs.R",
      "R/01_frequentist_registered.R",
      "R/02_bayes_prevalence.R",
      "R/03_bayes_trials.R",
      "R/04_bayes_metaregression.R",
      "R/05_dienes_inference.R",
      "R/07_external_prior_frederick2019.R",
      "R/06_figures_tables.R",
      "R/02b_release_prevalence.R",
      "R/03b_release_trials.R",
      "R/06b_release_tables.R"
    )
    for (s in scripts) {
      cat("\n==================== ", s, " ====================\n", sep = "")
      source(s, local = new.env(), echo = FALSE)
    }
    if (opt_release) {
      assert_release_fits_fresh(fit_log())
      verify_run_artefacts(fit_log())
    }
    m$status <- "completed"
  }, error = function(e) {
    m$status <- "failed"
    m$failure_message <- conditionMessage(e)
    stop(e)
  })

  dir.create("output", showWarnings = FALSE)
  writeLines(c(
    paste("Run completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Mode:", mode),
    paste("Elapsed:", round(difftime(Sys.time(), m$started_at, units = "mins"), 1), "minutes"),
    paste("CmdStan:", cmdstan_v),
    "",
    capture.output(sessionInfo())
  ), "output/session_info.txt")
  cat("\nDone. Session information written to output/session_info.txt\n")
}

run_pipeline()
