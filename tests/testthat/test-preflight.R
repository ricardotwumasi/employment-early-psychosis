# preflight_cache() (D1) is the fast, no-fitting check that every registered
# model has exactly one readable cache file before anyone runs
# --cache-only or --release; skipped only in a checkout without the (large,
# gitignored-in-spirit but currently committed) fits directory.

test_that("preflight_cache() reports 42 of 42 historical-layer fits with exactly one readable cache file", {
  fits_dir <- root_path("output", "bayesian", "fits")
  skip_if_not(dir.exists(fits_dir), "no cached fits in this checkout")

  invisible(capture.output(result <- preflight_cache(layers = "historical")))
  expect_equal(nrow(result), 42)
  expect_true(all(result$n_files == 1 & result$readable))
})

# The pre-fit check (A42) must fail loudly: release candidate 1's preflight
# printed "0 of 1" for a missing fit and still exited with status 0.
test_that("preflight_release() passes on the committed inputs and registry", {
  expect_identical(preflight_release(), character(0))
})

test_that("preflight_release() reports a registry that disagrees with the derived membership", {
  reg <- readr::read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)
  broken <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(reg[reg$fit_tag != "rc_loo_prev_DRAKE_2013", ], broken, na = "NA")
  expect_match(preflight_release(registry_path = broken), "leave-one-out prevalence rows", all = FALSE)
  readr::write_csv(reg[reg$fit_tag != "rc_standalone_R18_18", ], broken, na = "NA")
  expect_match(preflight_release(registry_path = broken), "standalone-interval rows", all = FALSE)
})

test_that("run_all.R --preflight exits with status 1 on failure and 0 on success", {
  # --preflight checks the pinned Bayesian stack first; CI installs only the
  # package Imports, so this end-to-end check runs where brms and CmdStan exist.
  # preflight_release() itself is tested above in every environment.
  skip_if_not_installed("brms")
  skip_if_not_installed("cmdstanr")
  ok <- withr::with_dir(repo_root(), suppressWarnings(system2("Rscript", c("run_all.R", "--preflight"), stdout = TRUE, stderr = TRUE)))
  expect_null(attr(ok, "status"), info = paste(ok, collapse = "\n"))
  src <- readLines(root_path("run_all.R"))
  expect_true(any(grepl("quit(save = \"no\", status = 1)", src, fixed = TRUE)))
})

test_that("verify_run_artefacts() fails when a registered fit was not fitted in this run", {
  fits <- data.frame(model_id = "rc_a33_point_primary_hn1", cache_file = "missing.rds", key = "x",
                     cache_key_version = 1L, from_cache = FALSE, cache_mtime = Sys.time(), stringsAsFactors = FALSE)
  expect_error(verify_run_artefacts(fits), "post-fit artefact check failed")
})
