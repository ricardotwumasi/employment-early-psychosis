# The blank extraction template must match schema v2 exactly, or a future
# extractor's filled-in rows will silently misalign with 00_clean_data.R's
# column expectations (as happened historically with outcome_basis).

test_that("template header equals schema v2 column order", {
  schema_v2 <- readr::read_csv(root_path("data", "schema", "extraction_template_schema_v2.csv"),
                               show_col_types = FALSE)
  tpl <- readr::read_csv(root_path("FEP_employment_data_extraction_template.csv"),
                         col_types = readr::cols(.default = readr::col_character()),
                         na = character(0))
  expect_identical(names(tpl), schema_v2$column)
})

test_that("every example row has a non-empty page_source and numeric-looking events/n_assessed where present", {
  tpl <- readr::read_csv(root_path("FEP_employment_data_extraction_template.csv"),
                         col_types = readr::cols(.default = readr::col_character()),
                         na = character(0))
  example_rows <- tpl[grepl("^EXAMPLE_", tpl$study_id), ]
  expect_gt(nrow(example_rows), 0)
  expect_true(all(nzchar(example_rows$page_source)))
  for (col in c("events", "n_assessed")) {
    vals <- example_rows[[col]][nzchar(example_rows[[col]])]
    if (length(vals) > 0) expect_false(any(is.na(suppressWarnings(as.numeric(vals)))))
  }
})
