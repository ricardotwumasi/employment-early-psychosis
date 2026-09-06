# Every reconciliation flag (computed effect size disagrees with the
# reported one by more than tolerance) must have been reviewed and
# dispositioned (D3); an unreviewed flag would mean a possible extraction
# error is silently shipping in the primary or trial estimates.

test_that("every flagged reconciliation row has exactly one disposition, and no disposition is unmatched", {
  reconciliation <- readr::read_csv(root_path("data", "derived", "reconciliation.csv"), show_col_types = FALSE)
  dispositions <- readr::read_csv(root_path("data", "review", "reconciliation_dispositions.csv"), show_col_types = FALSE)
  flagged <- reconciliation[reconciliation$flag, c("study_id_clean", "effect_type")]

  expect_equal(nrow(flagged), 7)
  key <- function(d) paste(d$study_id_clean, d$effect_type)
  expect_setequal(key(flagged), key(dispositions))
  expect_false(anyDuplicated(key(flagged)) > 0)
  expect_false(anyDuplicated(key(dispositions)) > 0)
  expect_true(all(grepl("^accepted:", dispositions$disposition)))
})
