# ===============================================
# helper-setup.R - loaded once before the test files run
# ===============================================
# Sources R/utils.R and pins repo_root() to the repository root for the
# duration of the test run, because testthat::test_dir() runs tests with the
# working directory set to tests/testthat/, where R/utils.R's own
# file.exists("run_all.R") / file.exists("../run_all.R") search would not
# find it.
# ===============================================

.fep_find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start)
  repeat {
    if (file.exists(file.path(d, "run_all.R"))) return(d)
    parent <- dirname(d)
    if (parent == d) stop("Cannot locate the repository root.", call. = FALSE)
    d <- parent
  }
}

.fep_repo_root <- getOption("fep.repo_root")
if (is.null(.fep_repo_root)) .fep_repo_root <- .fep_find_repo_root()

source(file.path(.fep_repo_root, "R", "utils.R"))

# Overrides R/utils.R's repo_root() (which only checks the working directory
# and its immediate parent) so that root_path() works from any test's
# working directory. Behaviour for the pipeline scripts is unchanged; this
# reassignment only exists in the test session.
repo_root <- function() .fep_repo_root
