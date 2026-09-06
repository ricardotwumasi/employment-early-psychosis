# preflight_cache() (D1) is the fast, no-fitting check that every registered
# model has exactly one readable cache file before anyone runs
# --cache-only or --release; skipped only in a checkout without the (large,
# gitignored-in-spirit but currently committed) fits directory.

test_that("preflight_cache() reports 42 of 42 registered fits with exactly one readable cache file", {
  fits_dir <- root_path("output", "bayesian", "fits")
  skip_if_not(dir.exists(fits_dir), "no cached fits in this checkout")

  invisible(capture.output(result <- preflight_cache()))
  expect_equal(nrow(result), 42)
  expect_true(all(result$n_files == 1 & result$readable))
})
