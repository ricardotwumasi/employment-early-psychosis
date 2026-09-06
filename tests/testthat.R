# ===============================================
# testthat.R - test runner
# ===============================================
# Run from the repository root: Rscript tests/testthat.R
# testthat::test_dir() changes the working directory to tests/testthat/ while
# tests run, so the repository root is located first (same "walk up to
# run_all.R" logic as R/utils.R's repo_root(), duplicated here because the
# test helpers need it before R/utils.R is sourced) and passed to the test
# helpers via an option, so root_path() resolves correctly regardless of the
# working directory during a test.
# ===============================================

library(testthat)

find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start)
  repeat {
    if (file.exists(file.path(d, "run_all.R"))) return(d)
    parent <- dirname(d)
    if (parent == d) stop("Cannot locate the repository root (run_all.R not found).", call. = FALSE)
    d <- parent
  }
}

root <- find_repo_root()
options(fep.repo_root = root)
test_dir(file.path(root, "tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
