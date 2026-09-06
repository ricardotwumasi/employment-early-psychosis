# The correction ledger replaced a hardcoded tribble in 00_clean_data.R
# (D3); if its projection onto the tribble's original columns ever drifted,
# every corrected count in data/derived/prevalence.csv and trials.csv would
# silently change without any code review catching it, because
# 00_clean_data.R just reads whatever is in the ledger.

test_that("the correction ledger's projection reproduces the archived 2026-09-02 tribble exactly", {
  source(root_path("tests", "testthat", "fixtures", "corrections_tribble_2026-09-02.R"), local = TRUE)
  ledger <- readr::read_csv(root_path("data", "review", "correction_ledger.csv"),
                           col_types = readr::cols(.default = readr::col_character()), na = character(0))
  projected <- ledger[, c("study_id_clean", "effect_type", "field", "raw", "corrected", "source", "reason")]
  expected <- as.data.frame(corrections_tribble_2026_09_02, stringsAsFactors = FALSE)
  expect_equal(as.data.frame(projected), expected, ignore_attr = TRUE)
})

test_that("corrections.csv reproduces the historical output hash recorded in docs/fixtures/", {
  fixture_lines <- readLines(root_path("docs", "fixtures", "historical_outputs_2026-09-02.sha256"))
  fixture_lines <- fixture_lines[!grepl("^#", fixture_lines)]
  target <- fixture_lines[grepl("data/derived/corrections\\.csv$", fixture_lines)]
  expect_length(target, 1)
  expected_sha <- strsplit(trimws(target), "\\s+")[[1]][1]
  expect_identical(file_sha256(root_path("data", "derived", "corrections.csv")), expected_sha)
})
