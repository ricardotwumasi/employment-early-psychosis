# The fit registry (D5) is now the single source of truth for "which 42
# models does this pipeline fit" (R/06_figures_tables.R reads it instead of
# a hardcoded vector); if it drifted from the diagnostics CSVs the gate
# check in 06 would pass on a registry that no longer describes reality.

registry_all <- readr::read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)
# Historical layer only: the release layer (10 September 2026) is checked in
# test-release-inputs.R against output/release/.
registry <- registry_all[registry_all$layer == "historical", ]

test_that("the historical layer of the registry has exactly 42 unique fit_tag values", {
  expect_equal(nrow(registry), 42)
  expect_false(anyDuplicated(registry$fit_tag) > 0)
  expect_false(anyDuplicated(registry_all$fit_tag) > 0)
  expect_true(all(registry_all$layer %in% c("historical", "release")))
})

test_that("the registry equals the union of the three diagnostics CSVs' model names, with all passing the gate", {
  diag_all <- rbind(
    readr::read_csv(root_path("output", "bayesian", "prevalence_diagnostics.csv"), show_col_types = FALSE),
    readr::read_csv(root_path("output", "bayesian", "trials_diagnostics.csv"), show_col_types = FALSE),
    readr::read_csv(root_path("output", "bayesian", "metaregression_diagnostics.csv"), show_col_types = FALSE)
  )
  fitted_models <- sub("_ad999$", "", diag_all$model)
  expect_setequal(registry$fit_tag, fitted_models)
  expect_true(all(diag_all$passes_gate))
})

test_that("every registry tag has a non-empty formula and prior_spec", {
  expect_true(all(nzchar(registry$formula)))
  expect_true(all(nzchar(registry$prior_spec)))
})
