# Reporting route for standalone variable-window results (A42). Release
# candidate 1 printed only the first such result, so a second one would have
# vanished from the manuscript or swapped places with the first when the row
# order changed. These tests run the same functions R/02b and R/06b use, on
# the current release inputs, so they need no fitted model.

rp <- readr::read_csv(root_path("data", "derived", "release_prevalence.csv"), show_col_types = FALSE)
registry <- readr::read_csv(root_path("data", "registry", "fit_registry.csv"), show_col_types = FALSE)
rec <- standalone_record(rp[rp$in_a33_variable_window, ])
fmt <- function(e, l, h) sprintf("%.1f (%.1f to %.1f)", 100 * e, 100 * l, 100 * h)
mac <- standalone_macros(rec, fmt)

test_that("every variable-window result has its own record row, registry row and macro family", {
  expect_setequal(rec$result_uid, rp$result_uid[rp$in_a33_variable_window])
  expect_setequal(rec$result_uid, sub("^rc_standalone_", "", registry$fit_tag[registry$role == "standalone_interval"]))
  for (u in rec$result_uid) {
    k <- standalone_macro_key(u)
    for (suffix in c("Est", "Cell", "Label")) {
      expect_equal(sum(grepl(paste0("\\\\rcVar", k, suffix, "\\}"), mac, fixed = FALSE)), 1, info = paste(u, suffix))
    }
  }
})

test_that("macro names are letters only and distinct, whatever the row order", {
  keys <- standalone_macro_key(rec$result_uid)
  expect_true(all(grepl("^[A-Za-z]+$", keys)))
  expect_false(anyDuplicated(keys) > 0)
  shuffled <- standalone_macros(standalone_record(rp[rev(which(rp$in_a33_variable_window)), ]), fmt)
  expect_identical(shuffled, mac)
})

test_that("each interval is the Jeffreys interval of its own cell", {
  for (i in seq_len(nrow(rec))) {
    expect_equal(rec$observed_low[i], qbeta(0.025, rec$events[i] + 0.5, rec$n_assessed[i] - rec$events[i] + 0.5))
    expect_true(any(grepl(paste0("Cell\\}\\{", rec$events[i], "/", rec$n_assessed[i], "\\}"), mac)), info = rec$result_uid[i])
  }
})
