# Release inputs (R/00b_release_inputs.R; 10 September 2026, extended for
# release candidate 2 on 16 October 2026). These tests encode the approved
# rules A24 (strict comparison), A33 (scope), A25/A36 (measurement) and A42
# (new results, period selection, dated approval, one contribution per
# cohort), not the pool sizes: no test asserts a k. Sets are recomputed here
# from the decision tables, selections are checked against the independent
# candidate inventory, and source-specific cells are pinned so that a silent
# change to a decision table, the ledger or the construct review is caught
# here rather than in the manuscript.

rp <- readr::read_csv(root_path("data", "derived", "release_prevalence.csv"), show_col_types = FALSE)
rt <- readr::read_csv(root_path("data", "derived", "release_trials.csv"), show_col_types = FALSE)
arms <- readr::read_csv(root_path("data", "derived", "release_trial_arms.csv"), show_col_types = FALSE)
ledger <- readr::read_csv(root_path("data", "review", "correction_ledger.csv"),
                          col_types = readr::cols(.default = readr::col_character()), na = character(0))
registry <- readr::read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)
in_set <- function(set) sort(rp$study_id_clean[rp[[set]]])
row_of <- function(id) rp[rp$study_id_clean == id, ]

read_chr <- function(...) readr::read_csv(root_path(...), col_types = readr::cols(.default = readr::col_character()),
                                          na = character(0))
elig   <- read_chr("data", "review", "population_eligibility.csv")
cons   <- read_chr("data", "review", "release_construct_review.csv")
newres <- read_chr("data", "review", "release_new_results.csv")
amend  <- read_chr("data", "review", "amendment_dates.csv")
inventory <- dplyr::bind_rows(
  read_chr("data", "review", "candidate_results.csv"),
  read_chr("data", "review", "source_review_2026-09-23", "results_draft.csv"),
  read_chr("data", "review", "source_review_2026-09-23", "candidate_inventory_addendum_2026-10-16.csv"))
uid_row <- function(uid) rp[!is.na(rp$result_uid) & rp$result_uid == uid, ]

test_that("every release set equals its rule recomputed from the decision tables, not a remembered list", {
  # Decisions keyed by result_uid: historical results take the population
  # decision of their study and the construct decision that names them; new
  # results carry both decisions on their own row.
  dec <- dplyr::bind_rows(
    dplyr::inner_join(
      dplyr::transmute(elig, study_id_clean, a24 = grepl("^eligible", decision_a24_strict),
                       a33 = grepl("^eligible", decision_a33), dur = grepl("duration sensitivity only", decision_a33)),
      dplyr::transmute(cons, study_id_clean, result_uid = candidate_result_uid,
                       point_ok = usable_point_prevalence == "yes", period_ok = usable_period_prevalence == "yes",
                       variable = grepl("^variable", window_status), selected = baseline_selection != "none"),
      by = "study_id_clean"),
    dplyr::transmute(newres, study_id_clean, a24 = grepl("^eligible", decision_a24_strict),
                     a33 = grepl("^eligible", decision_a33), dur = grepl("duration sensitivity only", decision_a33),
                     result_uid, point_ok = outcome_basis == "point", period_ok = outcome_basis == "period",
                     variable = grepl("^variable", window_status), selected = baseline_selected == "TRUE"))
  expect_false(anyDuplicated(dec$result_uid) > 0)
  x <- dplyr::inner_join(rp, dplyr::select(dec, -study_id_clean), by = "result_uid", suffix = c("", ".dec"))
  expect_equal(nrow(x), nrow(rp))
  in_win <- !is.na(x$timepoint_months) & x$timepoint_months >= 6 & x$timepoint_months <= 24 & !x$variable
  point  <- x$outcome_basis == "point" & x$point_ok & in_win & !x$selected & !x$overlapping_cohort
  period <- x$outcome_basis == "period" & x$period_ok & in_win & !x$selected & !x$overlapping_cohort
  expect_equal(x$in_a33_point_primary, point & x$a33)
  expect_equal(x$in_a33_point_duration_sens, point & (x$a33 | x$dur))
  expect_equal(x$in_a33_period, period & (x$a33 | x$dur))
  expect_equal(x$in_a33_variable_window, x$variable & x$outcome_basis == "period" & (x$a33 | x$dur) & !x$selected)
  # The strict comparison follows the A24 decision column alone (A35): the
  # strict set is whatever that column admits, empty or not.
  expect_equal(x$in_strict_a24_point, point & x$a24)
  trial_dec <- dplyr::transmute(elig, study_id_clean, a24 = grepl("^eligible", decision_a24_strict))
  y <- dplyr::left_join(rt, trial_dec, by = "study_id_clean", suffix = c("", ".dec"))
  expect_equal(y$in_trial_strict, y$timepoint_months == 6 & y$outcome_basis == "period" &
                 grepl("^IPS added to TAU", y$comparison_type) & y$a24)
})

test_that("no cohort contributes twice to any pooled set (A42), while standalone results may share a programme", {
  for (set in c("in_a33_point_primary", "in_a33_point_duration_sens", "in_a33_period", "in_strict_a24_point")) {
    ids <- rp$cohort_id[rp[[set]]]
    expect_false(anyNA(ids), info = set)
    expect_false(anyDuplicated(ids) > 0, info = set)
  }
  for (set in c("in_trial_a33_k3_0to6", "in_trial_strict")) {
    expect_false(anyDuplicated(rt$cohort_id[rt[[set]]]) > 0, info = set)
  }
  # Clarke 2022 reports the Killackey 2019 trial cohort: it may sit in the
  # point set because Killackey 2019 contributes only to the period set
  clarke <- uid_row("NX02_02")
  expect_equal(clarke$cohort_id, "KILLACKEY_2019")
  expect_false(rp$in_a33_point_primary[rp$study_id_clean == "KILLACKEY_2019"])
  porteous <- rp[rp$study_id_clean == "PORTEOUS_2007", ]
  expect_equal(nrow(porteous), 2)
  expect_true(all(porteous$in_a33_variable_window))
  expect_false(any(porteous$in_a33_period | porteous$in_a33_point_primary))
})

test_that("each pooled result is the one its selection rule picks from the full candidate inventory (A36, A42)", {
  # Point rule (A36): assessment nearest 12 months within 6 to 24, earlier on
  # ties. Period rule (A42): interval ending nearest 12 months within 6 to 24,
  # earlier on ties. Candidates are every compatible employment count of the
  # same cohort in the inventory, including companion reports, so the check
  # is not circular. The adopted basis of a selected cell is RT's (A42).
  emp <- "^(competitive employment|paid employment|any employment|any paid|current employment)"
  num <- function(v) suppressWarnings(as.numeric(v))
  pick <- function(t) { t <- sort(unique(t[!is.na(t) & t >= 6 & t <= 24])); t[which.min(abs(t - 12))] }
  for (set in c("in_a33_point_duration_sens", "in_a33_period")) {
    basis <- if (set == "in_a33_period") "period" else "point"
    time_col <- if (basis == "point") "assessment_time_months" else "reference_period_end_months"
    for (i in which(rp[[set]])) {
      sel <- inventory[inventory$result_uid == rp$result_uid[i], ]
      expect_equal(nrow(sel), 1, info = rp$result_uid[i])
      cand <- inventory[inventory$cohort_id == rp$cohort_id[i] & inventory$outcome_basis == basis &
                          grepl(emp, inventory$outcome_construct, ignore.case = TRUE) &
                          nzchar(inventory$numerator) & nzchar(inventory$denominator), ]
      expect_equal(num(sel[[time_col]]), pick(c(num(cand[[time_col]]), num(sel[[time_col]]))), info = rp$result_uid[i])
    }
  }
})

test_that("Abdel-Baki enters with the employment-only row (C17/C18), not the productive-occupation composite", {
  r <- row_of("ABDELBAKI_2013")
  expect_equal(c(r$events, r$n_assessed), c(40, 91))
  expect_match(r$outcome_definition, "education and homemaking excluded")
})

test_that("Craig 2014 enters with the day-of-interview point count (C15), not the cumulative 41/134", {
  expect_equal(row_of("CRAIG_2014")$events, 36)
})

test_that("verified new cells enter with the counts RT adopted (A42)", {
  # Drake: employment-classified denominator 102 + 193, not the 319 interviewed
  expect_equal(unlist(uid_row("NU23_01")[c("events", "n_assessed", "n_total")]), c(events = 102, n_assessed = 295, n_total = 385))
  # Clarke: complete-case sample of 100; the missing-as-unemployed bound uses the 146 randomised
  expect_equal(unlist(uid_row("NX02_02")[c("events", "n_assessed", "n_total")]), c(events = 37, n_assessed = 100, n_total = 146))
  # Nossel: six-month cell, period basis by RT's adjudication
  expect_equal(unlist(uid_row("NU16_01")[c("events", "n_assessed")]), c(events = 124, n_assessed = 260))
  expect_equal(uid_row("NU16_01")$outcome_basis, "period")
  # Fowler: whole sample 6/37 + 9/29 (C30, C31)
  expect_equal(c(row_of("FOWLER_2019")$events, row_of("FOWLER_2019")$n_assessed), c(15, 66))
  # Porteous: two contract windows, each its own result
  expect_equal(c(uid_row("NX18_01")$events, uid_row("NX18_01")$n_assessed, uid_row("NX18_02")$events, uid_row("NX18_02")$n_assessed),
               c(36, 100, 58, 125))
  # Lin: observed cases at month 12 (Table S3), reported but never pooled (A5, A43)
  lin <- row_of("LIN_2026")
  expect_equal(c(lin$events, lin$n_assessed), c(121, 303))
  expect_false(lin$in_a33_point_primary | lin$in_a33_point_duration_sens | lin$in_strict_a24_point)
})

test_that("Rinaldi 2004 is excluded for cohort overlap, Williams 2016 for baseline employment selection", {
  expect_true(row_of("RINALDI_2004")$overlapping_cohort)
  expect_false(row_of("RINALDI_2004")$in_a33_point_primary)
  expect_true(row_of("WILLIAMS_2016")$baseline_selected)
  expect_false(row_of("WILLIAMS_2016")$in_a33_point_primary)
})

test_that("composites, count-free reports and A5-excluded Lin contribute to no fixed-window prevalence set (A25, A43)", {
  for (s in c("POTHIER_2019", "CHUA_2019", "ROSENHECK_2017", "HUMENSKY_2017", "LIN_2026")) {
    expect_false(row_of(s)$in_a33_point_primary, info = s)
    expect_false(row_of(s)$in_a33_period, info = s)
  }
})

test_that("variable-window results (Turner, Humensky, Porteous) are reported one by one and pooled nowhere", {
  v <- rp[rp$in_a33_variable_window, ]
  expect_true(all(grepl("^variable", v$window_status)))
  expect_false(any(v$in_a33_period | v$in_a33_point_primary | v$in_a33_point_duration_sens))
  expect_true(row_of("TURNER_2019")$in_a33_variable_window)
  expect_true(row_of("HUMENSKY_2017")$in_a33_variable_window)
})

test_that("Van Duin 2021 is a period measure whose interval the A42 rule selects: months 0 to 6 (C40 to C43)", {
  r <- row_of("VANDUIN_2021")
  expect_equal(r$outcome_basis, "period")
  expect_equal(c(r$events, r$n_assessed, r$timepoint_months), c(15, 57, 6))
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

test_that("release-layer ledger rows are approved, dated on or after their amendment and applied only in the release layer", {
  rel <- ledger[ledger$layer == "release", ]
  expect_true(all(grepl("^RT", rel$approver)))
  adate <- as.Date(amend$decision_date[match(rel$amendment_ref, amend$amendment_ref)])
  expect_false(anyNA(adate))
  expect_true(all(as.Date(rel$decided_date) >= adate))
  # The historical derived file must still hold the before-values
  hist <- readr::read_csv(root_path("data", "derived", "prevalence.csv"), show_col_types = FALSE)
  before <- c(CRAIG_2014 = 41, ABDELBAKI_2013 = 55, FOWLER_2019 = 5, HUMENSKY_2017 = 12, VANDUIN_2021 = 25, LIN_2026 = 119)
  expect_equal(hist$events[match(names(before), hist$study_id_clean)], unname(before))
})

test_that("new results and decision rows are approved, dated and source-checked before adoption", {
  adate <- as.Date(amend$decision_date[match(newres$amendment_ref, amend$amendment_ref)])
  expect_true(all(nzchar(newres$approver) & as.Date(newres$adoption_date) >= adate))
  expect_true(all(as.Date(newres$source_check_date) <= as.Date(newres$adoption_date)))
  expect_true(all(as.Date(cons$review_date) >= as.Date(amend$decision_date[match(cons$amendment_ref, amend$amendment_ref)])))
})

# R/00b must refuse inputs that break the approval or validation rules. Each
# case runs R/00b against a copy of the inputs with one defect.
run_00b_with <- function(edit) {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  for (d in c("R", "data/derived", "data/review")) dir.create(file.path(tmp, d), recursive = TRUE)
  file.create(file.path(tmp, "run_all.R"))
  for (f in c("R/utils.R", "R/00b_release_inputs.R", "data/derived/prevalence.csv", "data/derived/trials.csv",
              "data/review/correction_ledger.csv", "data/review/population_eligibility.csv",
              "data/review/release_construct_review.csv", "data/review/release_new_results.csv",
              "data/review/amendment_dates.csv")) {
    file.copy(root_path(f), file.path(tmp, f))
  }
  edit(tmp)
  out <- withr::with_dir(tmp, suppressWarnings(system2("Rscript", "R/00b_release_inputs.R", stdout = TRUE, stderr = TRUE)))
  list(status = attr(out, "status"), text = paste(out, collapse = "\n"))
}
edit_new <- function(f) function(tmp) {
  p <- file.path(tmp, "data/review/release_new_results.csv")
  d <- readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()), na = character(0))
  readr::write_csv(f(d), p, na = "")
}

test_that("R/00b runs cleanly on an unmodified copy (control for the failure cases below)", {
  r <- run_00b_with(function(tmp) NULL)
  expect_null(r$status, info = r$text)
})

test_that("an adopted result without an approver cannot enter", {
  r <- run_00b_with(edit_new(function(d) { d$approver[1] <- ""; d }))
  expect_false(is.null(r$status)); expect_match(r$text, "lacks an approver")
})

test_that("an adoption dated before its amendment cannot enter", {
  r <- run_00b_with(edit_new(function(d) { d$adoption_date[1] <- "2026-10-15"; d }))
  expect_false(is.null(r$status)); expect_match(r$text, "on or after its amendment")
})

test_that("a new result with more events than assessed, or a non-integer count, is refused", {
  r <- run_00b_with(edit_new(function(d) { d$events[1] <- "400"; d }))
  expect_false(is.null(r$status)); expect_match(r$text, "more events than assessed")
  r <- run_00b_with(edit_new(function(d) { d$n_assessed[1] <- "29.5"; d }))
  expect_false(is.null(r$status)); expect_match(r$text, "non-negative integers")
})

test_that("a second result from a cohort already in a pooled set is refused", {
  r <- run_00b_with(edit_new(function(d) { d$cohort_id[d$result_uid == "NU23_01"] <- "ABDELBAKI_2013"; d }))
  expect_false(is.null(r$status)); expect_match(r$text, "cohort contributes more than once")
})

test_that("a duplicated result_uid is refused", {
  r <- run_00b_with(edit_new(function(d) { d$result_uid[2] <- d$result_uid[1]; d }))
  expect_false(is.null(r$status)); expect_match(r$text, "result_uid missing or duplicated")
})

test_that("the release layer of the registry lists one leave-one-out fit per pooled study, one standalone interval per variable-window result, and the OR model with exported draws", {
  rel <- registry[registry$layer == "release", ]
  expect_setequal(sub("^rc_standalone_", "", rel$fit_tag[rel$role == "standalone_interval"]),
                  rp$result_uid[rp$in_a33_variable_window])
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
