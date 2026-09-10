# Release mode (run_all.R --release) promises that no fit is loaded from a
# cache and that the run starts from a synchronised lockfile and a clean
# tree. Before 10 September 2026 the post-run assertion could never fire
# because --release also forced fep.refit, so the failure path had not been
# demonstrated (Gate 1, document control register). These tests exercise
# each guard in isolation.

test_that("assert_release_fits_fresh stops when any logged fit came from a cache", {
  fits <- data.frame(model_id = c("a", "b"), from_cache = c(FALSE, TRUE), stringsAsFactors = FALSE)
  expect_error(assert_release_fits_fresh(fits), "loaded from cache: b")
  expect_true(assert_release_fits_fresh(data.frame(model_id = "a", from_cache = FALSE)))
})

test_that("assert_release_fits_fresh stops when nothing was fitted at all", {
  expect_error(assert_release_fits_fresh(fit_log()[0, ]), "nothing was fitted")
})

test_that("fit_cached refuses to consult the cache when fep.release is set without fep.refit", {
  withr::local_options(list(fep.release = TRUE, fep.refit = FALSE, fep.cache_only = FALSE))
  expect_error(
    # A plain data frame stands in for a brmsprior so that the guard is
    # exercised in the Stan-free CI environment as well.
    fit_cached(y ~ 1, data.frame(y = 1), data.frame(prior = "normal(0, 1)", class = "Intercept"),
               stats::gaussian(), "release_guard_test"),
    "RELEASE: fep.release is set but fep.refit is not")
})

test_that("check_release_preconditions stops on a missing lockfile, an unsynchronised library or a dirty tree", {
  tmp <- withr::local_tempdir()
  expect_error(check_release_preconditions(lockfile = file.path(tmp, "renv.lock")), "requires renv.lock")
  lf <- file.path(tmp, "renv.lock"); writeLines("{}", lf)
  expect_error(check_release_preconditions(lf, synced_fun = function(x) FALSE, porcelain_fun = function() character(0)),
               "synchronized")
  expect_error(check_release_preconditions(lf, synced_fun = function(x) TRUE, porcelain_fun = function() " M file"),
               "clean git tree")
  expect_true(check_release_preconditions(lf, synced_fun = function(x) TRUE, porcelain_fun = function() character(0)))
})
