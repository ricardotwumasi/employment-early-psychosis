# fep.cache_only (D1) exists so that a release run can prove it never
# silently fell back to fitting; these tests exercise every way that
# guarantee could fail (missing cache, corrupt cache, wrong class, stale
# key, and a failing gate) inside an isolated fake repository, with brm()
# itself made to error if it is ever reached, since ABSOLUTE RULE 1
# forbids fitting a model anywhere in this checkout.

# Points repo_root() (and so root_path()) at a throwaway directory for the
# duration of one test, restoring the pinned real-repo root on exit.
local_fake_repo <- function(env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  file.create(file.path(tmp, "run_all.R"))
  dir.create(file.path(tmp, "output", "bayesian", "fits"), recursive = TRUE)
  old_repo_root <- repo_root
  assign("repo_root", function() tmp, envir = globalenv())
  withr::defer(assign("repo_root", old_repo_root, envir = globalenv()), envir = env)
  tmp
}

# Mirrors fit_cached()'s own key recipe exactly, so a dummy cache file can be
# placed at the path fit_cached() will actually look for.
fake_cache_key <- function(formula, data, prior, family,
                          adapt_delta = mcmc_settings$adapt_delta, save_all_pars = FALSE) {
  digest::digest(list(
    formula = deparse(formula), data = data, prior = as.data.frame(prior),
    family = family$family, link = family$link,
    settings = c(mcmc_settings[c("chains", "warmup", "iter", "seed")],
                adapt_delta = adapt_delta, save_all_pars = save_all_pars)
  ))
}

f <- y ~ 1
d <- data.frame(y = 1)
p <- data.frame()
fam <- gaussian()

test_that("fit_cached() rejects fep.cache_only with fep.refit before any brm() call is reachable", {
  local_fake_repo()
  old_call <- fep_brm_call
  withr::defer(assign("fep_brm_call", old_call, envir = globalenv()))
  assign("fep_brm_call", function(...) stop("brm must not be called"), envir = globalenv())
  withr::local_options(fep.cache_only = TRUE, fep.refit = TRUE)

  expect_error(fit_cached(f, d, p, fam, "conflict_tag"), "CACHE_ONLY:")
})

test_that("fit_cached() in cache-only mode stops with a clear message when the cache file is missing", {
  local_fake_repo()
  old_call <- fep_brm_call
  withr::defer(assign("fep_brm_call", old_call, envir = globalenv()))
  assign("fep_brm_call", function(...) stop("brm must not be called"), envir = globalenv())
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE)

  expect_error(fit_cached(f, d, p, fam, "missing_tag"), "CACHE_ONLY: missing cache")
})

test_that("fit_cached() in cache-only mode stops when the cache file exists but is not readable as RDS", {
  tmp <- local_fake_repo()
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE)
  key <- fake_cache_key(f, d, p, fam)
  cache_file <- file.path(tmp, "output", "bayesian", "fits", paste0("corrupt_tag_", substr(key, 1, 12), ".rds"))
  writeBin(as.raw(0:19), cache_file)

  expect_error(fit_cached(f, d, p, fam, "corrupt_tag"), "CACHE_ONLY: unreadable cache")
})

test_that("fit_cached() in cache-only mode stops when the cached object is not a brmsfit", {
  tmp <- local_fake_repo()
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE)
  key <- fake_cache_key(f, d, p, fam)
  cache_file <- file.path(tmp, "output", "bayesian", "fits", paste0("wrongclass_tag_", substr(key, 1, 12), ".rds"))
  saveRDS(list(not_a_fit = TRUE), cache_file)

  expect_error(fit_cached(f, d, p, fam, "wrongclass_tag"), "not a brmsfit")
})

test_that("fit_cached() in cache-only mode stops when the cached fit's provenance key does not match", {
  tmp <- local_fake_repo()
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE)
  key <- fake_cache_key(f, d, p, fam)
  cache_file <- file.path(tmp, "output", "bayesian", "fits", paste0("stale_tag_", substr(key, 1, 12), ".rds"))
  fake_fit <- structure(list(), class = "brmsfit")
  attr(fake_fit, "fep_cache_meta") <- list(key = "not-the-right-key")
  saveRDS(fake_fit, cache_file)

  expect_error(fit_cached(f, d, p, fam, "stale_tag"), "CACHE_ONLY:.*(key mismatch|stale)")
})

test_that("fit_cached() in cache-only mode succeeds and logs from_cache = TRUE for a valid, matching cache entry", {
  tmp <- local_fake_repo()
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE)
  key <- fake_cache_key(f, d, p, fam)
  cache_file <- file.path(tmp, "output", "bayesian", "fits", paste0("good_tag_", substr(key, 1, 12), ".rds"))
  fake_fit <- structure(list(), class = "brmsfit")
  attr(fake_fit, "fep_cache_meta") <- list(key = key, cache_key_version = 1L, tag = "good_tag")
  saveRDS(fake_fit, cache_file)

  result <- fit_cached(f, d, p, fam, "good_tag")
  expect_s3_class(result, "brmsfit")
  log <- fit_log()
  row <- log[log$model_id == "good_tag", ]
  expect_equal(nrow(row), 1)
  expect_true(row$from_cache)
})

test_that("fit_gated() in cache-only mode stops with CACHE_ONLY when the gate fails, without attempting a retry", {
  tmp <- local_fake_repo()
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE)
  key <- fake_cache_key(f, d, p, fam)
  cache_file <- file.path(tmp, "output", "bayesian", "fits", paste0("failing_tag_", substr(key, 1, 12), ".rds"))
  fake_fit <- structure(list(), class = "brmsfit")
  attr(fake_fit, "fep_cache_meta") <- list(key = key, cache_key_version = 1L, tag = "failing_tag")
  saveRDS(fake_fit, cache_file)

  old_diag <- diagnose_fit
  withr::defer(assign("diagnose_fit", old_diag, envir = globalenv()))
  assign("diagnose_fit", function(fit, tag) {
    data.frame(model = tag, max_rhat = 2, min_ess_bulk = 1, min_ess_tail = 1,
              divergences = 5, max_treedepth = 11, min_ebfmi = 0.1, passes_gate = FALSE,
              stringsAsFactors = FALSE)
  }, envir = globalenv())

  old_call <- fep_brm_call
  withr::defer(assign("fep_brm_call", old_call, envir = globalenv()))
  assign("fep_brm_call", function(...) stop("brm must not be called"), envir = globalenv())

  expect_error(fit_gated(f, d, p, fam, "failing_tag"), "CACHE_ONLY: gate failed")
})

test_that("fit_cached() in cache-only mode accepts a legacy cache without provenance metadata because the 42 historical fits (cached before 6 September 2026) carry none and are identified by the key prefix in the file name", {
  tmp <- local_fake_repo()
  withr::local_options(fep.cache_only = TRUE, fep.refit = FALSE, fep.fit_log = list())
  key <- fake_cache_key(f, d, p, fam)
  cache_file <- file.path(tmp, "output", "bayesian", "fits", paste0("legacy_tag_", substr(key, 1, 12), ".rds"))
  saveRDS(structure(list(), class = "brmsfit"), cache_file)

  expect_message(result <- fit_cached(f, d, p, fam, "legacy_tag"), "legacy cache without metadata")
  expect_s3_class(result, "brmsfit")
  log <- fit_log()
  expect_true(log[log$model_id == "legacy_tag", "from_cache"])
})
