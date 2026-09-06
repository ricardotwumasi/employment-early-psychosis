# R/07 (not sourced by this suite; see test-cache-only.R for why fitting
# scripts are never run in tests) claims that Frederick and VanderWeele's
# published S2 code passes arm totals into metafor's non-event-count
# arguments. This reproduces both calls directly from the committed .rds to
# verify that claim independently of R/07's own internal self-check.

test_that("metafor reproduces the authors' RR (arms as non-events) and the corrected RR (arms as totals)", {
  skip_if_not_installed("metafor")
  rds_path <- root_path("data", "external", "frederick2019", "competitive_employment_any.rds")
  skip_if_not(file.exists(rds_path), "external Frederick 2019 data not present")

  df <- readRDS(rds_path)
  df$authors <- paste(sapply(strsplit(df$article, ","), `[`, 1), df$year)
  df <- subset(df, df$ips_any > 0)

  res_auth <- metafor::rma(ai = ips_any, bi = n_ips_randomized,
                           ci = tau_any, di = n_tau_randomized,
                           data = df, measure = "RR", slab = authors, method = "REML")
  res_corr <- metafor::rma(ai = ips_any, n1i = n_ips_randomized,
                           ci = tau_any, n2i = n_tau_randomized,
                           data = df, measure = "RR", slab = authors, method = "REML")

  expect_equal(as.numeric(exp(res_auth$b)), 1.633329, tolerance = 1e-6)
  expect_equal(as.numeric(exp(res_corr$b)), 2.040312, tolerance = 1e-6)
})
