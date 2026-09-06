# Raw extraction file: the whole pipeline's identifiers, corrections and
# derived tables are keyed off this file's exact bytes and row order, so a
# silent edit anywhere in it (even whitespace) would invalidate the frozen
# analysis without necessarily breaking any single downstream number.

raw_path <- root_path("data", "yanan_data_280826.csv")

test_that("raw extraction file is byte-identical to the frozen hash", {
  expect_identical(file_sha256(raw_path),
                   "a167afc2fb70a2e1fbf96eddc3f540c266235cd3507b855adb5be786d757f70f")
})

test_that("raw extraction file has exactly 44 data rows", {
  bytes <- readBin(raw_path, what = "raw", n = file.info(raw_path)$size)
  txt <- rawToChar(bytes)
  Encoding(txt) <- "bytes"
  txt <- gsub(rawToChar(as.raw(c(0xa3, 0xa8))), " (", txt, fixed = TRUE, useBytes = TRUE)
  txt <- gsub(rawToChar(as.raw(c(0xa3, 0xa9))), ") ", txt, fixed = TRUE, useBytes = TRUE)
  txt <- gsub(rawToChar(as.raw(0xa0)), " ", txt, fixed = TRUE, useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  raw <- readr::read_csv(I(txt), col_types = readr::cols(.default = readr::col_character()),
                        na = character(0), show_col_types = FALSE, progress = FALSE)
  expect_equal(nrow(raw), 44)
})

test_that("raw header matches the frozen column order in schema v1 (00_clean_data.R checks this on every run)", {
  schema_v1 <- readr::read_csv(root_path("data", "schema", "raw_extraction_schema_v1.csv"), show_col_types = FALSE)
  header <- strsplit(readLines(raw_path, n = 1, warn = FALSE), ",", fixed = TRUE)[[1]]
  expect_identical(header, schema_v1$column)
})

test_that("raw file has no trailing newline (a tool that added one would silently change the SHA-256)", {
  size <- file.info(raw_path)$size
  last_byte <- readBin(raw_path, what = "raw", n = size)[size]
  expect_false(last_byte == as.raw(0x0a))
})
