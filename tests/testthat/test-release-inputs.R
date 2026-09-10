# Release inputs (R/00b_release_inputs.R, 10 September 2026). These tests
# encode the approved rules A24 (strict comparison), A33 (scope), A25/A36
# (measurement), not the pool sizes: each expectation states why a study is
# where it is, so that a silent change to a decision table, the ledger or
# the construct review is caught here rather than in the manuscript.

rp <- readr::read_csv(root_path("data", "derived", "release_prevalence.csv"), show_col_types = FALSE)
rt <- readr::read_csv(root_path("data", "derived", "release_trials.csv"), show_col_types = FALSE)
arms <- readr::read_csv(root_path("data", "derived", "release_trial_arms.csv"), show_col_types = FALSE)
ledger <- readr::read_csv(root_path("data", "review", "correction_ledger.csv"),
                          col_types = readr::cols(.default = readr::col_character()), na = character(0))
registry <- readr::read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)
in_set <- function(set) sort(rp$study_id_clean[rp[[set]]])
row_of <- function(id) rp[rp$study_id_clean == id, ]

test_that("the strict registered rule (A24) pools nothing: recorded consequence A35, not a target", {
  expect_equal(sum(rp$in_strict_a24_point), 0)
  expect_equal(rt$study_id_clean[rt$in_trial_strict], "KILLACKEY_2008")
})

test_that("A33 point primary set is exactly the paid-employment point measures inside the window, one per cohort", {
  expect_equal(in_set("in_a33_point_primary"),
               c("ABDELBAKI_2013", "ANDERSEN_2024", "CRAIG_2014", "DUDLEY_2014", "HEGELSTAD_2019", "RINALDI_2010"))
  # Eack 2011 enters only the duration sensitivity: onset within eight years permitted
  expect_equal(setdiff(in_set("in_a33_point_duration_sens"), in_set("in_a33_point_primary")), "EACK_2011")
})

test_that("Abdel-Baki enters with the employment-only row (C17/C18), not the productive-occupation composite", {
  r <- row_of("ABDELBAKI_2013")
  expect_equal(c(r$events, r$n_assessed), c(40, 91))
  expect_match(r$outcome_definition, "education and homemaking excluded")
})

test_that("Craig 2014 enters with the day-of-interview point count (C15), not the cumulative 41/134", {
  expect_equal(row_of("CRAIG_2014")$events, 36)
})

test_that("Rinaldi 2004 is excluded for cohort overlap, Williams 2016 for baseline employment selection", {
  expect_true(row_of("RINALDI_2004")$overlapping_cohort)
  expect_false(row_of("RINALDI_2004")$in_a33_point_primary)
  expect_true(row_of("WILLIAMS_2016")$baseline_selected)
  expect_false(row_of("WILLIAMS_2016")$in_a33_point_primary)
})

test_that("composites and count-free reports contribute no prevalence row (A25)", {
  for (s in c("POTHIER_2019", "CHUA_2019", "ROSENHECK_2017", "HUMENSKY_2017", "LIN_2026")) {
    expect_false(row_of(s)$in_a33_point_primary, info = s)
    expect_false(row_of(s)$in_a33_period, info = s)
  }
})

test_that("Turner 2019 is outside the fixed window (range 2 to 44 months) and only in the variable-follow-up analysis", {
  expect_equal(in_set("in_a33_variable_window"), "TURNER_2019")
  expect_false(row_of("TURNER_2019")$in_a33_period)
})

test_that("Van Duin 2021 is a six-month period measure (C28) and sits in the period set only", {
  r <- row_of("VANDUIN_2021")
  expect_equal(r$outcome_basis, "period")
  expect_true(r$in_a33_period)
  expect_false(r$in_a33_point_primary)
})

test_that("the A33 IPS pool is the three IPS versus usual care trials on a common 0 to 6 month interval", {
  pool <- rt[rt$in_trial_a33_k3_0to6, ]
  expect_equal(sort(pool$study_id_clean), c("ERICKSON_2021", "KILLACKEY_2008", "KILLACKEY_2019"))
  expect_true(all(pool$timepoint_months == 6 & pool$outcome_basis == "period"))
  # Erickson uses the months 0 to 6 cells (C20 to C23), not the months 6 to 12 cells
  e <- pool[pool$study_id_clean == "ERICKSON_2021", ]
  expect_equal(c(e$events_intervention, e$n_intervention, e$events_control, e$n_control), c(30, 50, 30, 52))
  # Nuechterlein has no six-month interval and an active comparator
  expect_false(rt$in_trial_a33_k3_0to6[rt$study_id_clean == "NUECHTERLEIN_2020"])
  expect_equal(nrow(arms), 6)
})

test_that("release-layer ledger rows are approved, dated and applied only in the release layer", {
  rel <- ledger[ledger$layer == "release", ]
  expect_true(all(grepl("^RT", rel$approver)))
  expect_true(all(rel$decided_date == "2026-09-10"))
  expect_true(all(rel$amendment_ref == "A36"))
  # The historical derived file must still hold the before-values
  hist <- readr::read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)
  expect_equal(hist$events[hist$study_id_clean == "CRAIG_2014"], 41)
  expect_equal(hist$events[hist$study_id_clean == "ABDELBAKI_2013"], 55)
})

test_that("the release layer of the registry lists one leave-one-out fit per pooled study and the OR model with exported draws", {
  rel <- registry[registry$layer == "release", ]
  expect_equal(sort(sub("^rc_loo_prev_", "", rel$fit_tag[grepl("^rc_loo_prev_", rel$fit_tag)])),
               in_set("in_a33_point_primary"))
  expect_equal(sort(sub("^rc_loo_trial_", "", rel$fit_tag[grepl("^rc_loo_trial_", rel$fit_tag)])),
               sort(rt$study_id_clean[rt$in_trial_a33_k3_0to6]))
  expect_true(rel$draws_exported[rel$fit_tag == "rc_exact_binomial_a33_k3"])
})

test_that("R/00b reproduces the committed release inputs from the committed decision tables", {
  tmp <- withr::local_tempdir()
  before <- lapply(c("release_prevalence.csv", "release_trials.csv", "release_trial_arms.csv"),
                   function(f) file_sha256(root_path("data", "derived", f)))
  out <- withr::with_dir(repo_root(),
    system2("Rscript", c(shQuote(root_path("R", "00b_release_inputs.R"))), stdout = TRUE, stderr = TRUE))
  expect_null(attr(out, "status"), info = paste(tail(out, 5), collapse = "\n"))
  after <- lapply(c("release_prevalence.csv", "release_trials.csv", "release_trial_arms.csv"),
                  function(f) file_sha256(root_path("data", "derived", f)))
  expect_identical(before, after)
})
