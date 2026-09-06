# The identity crosswalk (D4) is what a reader uses to trace a
# result_id back to a citable report and DOI; if the join to the source
# registry were not 1:1, or the result of the one deliberate Killackey 2019
# timepoint correction (A6) were miscounted, the traceability the sidecar
# exists for would be broken silently.

crosswalk <- readr::read_csv(root_path("data", "review", "result_crosswalk.csv"), show_col_types = FALSE)
registry  <- readr::read_csv(root_path("data", "review", "source_registry.csv"), show_col_types = FALSE)

test_that("exactly one row has id_changed, and it is the KILLACKEY_2019 proportion row", {
  changed <- crosswalk[crosswalk$id_changed, ]
  expect_equal(nrow(changed), 1)
  expect_equal(changed$study_id_clean, "KILLACKEY_2019")
  expect_equal(changed$effect_type, "proportion")
})

test_that("every crosswalk registry_key joins to exactly one source_registry row with a non-missing DOI", {
  keys <- unique(crosswalk$registry_key)
  matches <- vapply(keys, function(k) sum(registry$study_key == k), integer(1))
  expect_true(all(matches == 1))
  joined <- merge(data.frame(registry_key = keys), registry, by.x = "registry_key", by.y = "study_key")
  expect_false(any(is.na(joined$doi)))
})

test_that("result_uid is unique", {
  expect_false(anyDuplicated(crosswalk$result_uid) > 0)
})

test_that("the A5 compatibility sidecar agrees with prevalence.csv$population_fep for every study", {
  prevalence <- readr::read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)
  compat <- readr::read_csv(root_path("data", "review", "primary_estimand_compatibility.csv"), show_col_types = FALSE)
  joined <- merge(prevalence[, c("study_id_clean", "population_fep")], compat, by = "study_id_clean")
  expect_equal(nrow(joined), nrow(prevalence))
  expect_true(all(joined$population_fep == joined$compatible_with_primary_prevalence_estimand))
})
