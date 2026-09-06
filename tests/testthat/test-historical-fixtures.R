# Historical fixtures: these are the exact set memberships and totals the
# 2 September 2026 analysis (and every cached Bayesian fit) was built on
# (ABSOLUTE RULE 4: no change to set membership). If any of these drift, the
# cached fits under output/bayesian/fits/ are silently describing a
# different dataset than the one on disk.

test_that("the strict primary prevalence set is exactly the ten historical study IDs", {
  prevalence <- readr::read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)
  strict <- prevalence[prevalence$in_primary_set, ]
  expect_setequal(strict$study_id_clean,
                  c("ABDELBAKI_2013", "ANDERSEN_2024", "CRAIG_2014", "DUDLEY_2014", "EACK_2011",
                    "HEGELSTAD_2019", "POTHIER_2019", "RINALDI_2010", "ROSENHECK_2017", "VANDUIN_2021"))
  expect_equal(sum(strict$n_total), 2818)
  expect_equal(sum(strict$n_assessed), 1613)
})

test_that("the primary trial set is exactly Erickson 2021, Killackey 2008 and Killackey 2019 with their historical counts", {
  trials <- readr::read_csv(root_path("data", "derived", "trials.csv"), show_col_types = FALSE)
  prim <- trials[trials$analysis_set == "primary", ]
  expect_setequal(prim$study_id_clean, c("ERICKSON_2021", "KILLACKEY_2008", "KILLACKEY_2019"))

  by_study <- function(id) prim[prim$study_id_clean == id, ]
  erickson <- by_study("ERICKSON_2021")
  expect_equal(c(erickson$events_intervention, erickson$n_intervention,
                erickson$events_control, erickson$n_control), c(34, 47, 25, 50))
  expect_equal(c(erickson$n_intervention_full, erickson$n_control_full), c(56, 53))

  killackey08 <- by_study("KILLACKEY_2008")
  expect_equal(c(killackey08$events_intervention, killackey08$n_intervention,
                killackey08$events_control, killackey08$n_control), c(13, 20, 2, 21))
  expect_equal(c(killackey08$n_intervention_full, killackey08$n_control_full), c(20, 21))

  killackey19 <- by_study("KILLACKEY_2019")
  expect_equal(c(killackey19$events_intervention, killackey19$n_intervention,
                killackey19$events_control, killackey19$n_control), c(47, 66, 29, 60))
  expect_equal(c(killackey19$n_intervention_full, killackey19$n_control_full), c(73, 73))

  expect_equal(sum(prim$n_intervention_full, prim$n_control_full), 296)
  expect_equal(sum(prim$n_intervention, prim$n_control), 264)
})
