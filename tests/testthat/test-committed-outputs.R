# Mandatory, never skipped: the committed Bayesian outputs are what the
# manuscript's numbers are drawn from, and this suite never fits a model
# (ABSOLUTE RULE: no model fitting in this checkout), so these files are the
# only check that the committed artefacts are actually present and shaped
# as every downstream script expects.

test_that("both primary draw CSVs exist, have 16000 rows, and contain mu and tau", {
  for (f in c("prevalence_primary_draws.csv", "trials_primary_draws.csv")) {
    path <- root_path("output", "bayesian", f)
    expect_true(file.exists(path), info = f)
    d <- readr::read_csv(path, show_col_types = FALSE)
    expect_equal(nrow(d), 16000, info = f)
    # D1 promotes the export to model_id, .chain, .iteration, .draw, mu, tau;
    # until run_all.R is actually re-run, the committed files only have mu
    # and tau, so this checks a superset (it will still pass, unstrengthened,
    # after promotion).
    expect_true(all(c("mu", "tau") %in% names(d)), info = f)
  }
})

test_that("the three Bayesian diagnostics CSVs exist", {
  for (f in c("prevalence_diagnostics.csv", "trials_diagnostics.csv", "metaregression_diagnostics.csv")) {
    expect_true(file.exists(root_path("output", "bayesian", f)), info = f)
  }
})
