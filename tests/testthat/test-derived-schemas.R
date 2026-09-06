# The seven derived tables' column sets and row counts are the interface
# every downstream script (01 to 07) reads against; the fixture below is a
# frozen snapshot (2026-09-06) so an accidental column reorder, rename or
# row-count drift is caught even though it might not fail any single
# numeric assertion in 00_clean_data.R.

fixture <- readr::read_csv(root_path("tests", "testthat", "fixtures", "derived_schema_2026-09-06.csv"),
                           show_col_types = FALSE)
expected_rows <- c(all_rows_clean = 44, corrections = 14, prevalence = 23, reconciliation = 35,
                  recovery = 9, trial_arms = 8, trials = 12)

test_that("each derived table has exactly its frozen column names, in order", {
  for (f in names(expected_rows)) {
    expected_cols <- fixture$column[fixture$file == f]
    actual_header <- strsplit(readLines(root_path("data", "derived", paste0(f, ".csv")), n = 1, warn = FALSE),
                              ",", fixed = TRUE)[[1]]
    expect_identical(actual_header, expected_cols, info = f)
  }
})

test_that("each derived table has its frozen row count", {
  for (f in names(expected_rows)) {
    d <- readr::read_csv(root_path("data", "derived", paste0(f, ".csv")), show_col_types = FALSE)
    expect_equal(nrow(d), expected_rows[[f]], info = f)
  }
})
