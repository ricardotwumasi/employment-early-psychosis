# These pin the Dienes-style Bayes-factor and null-interval machinery in
# R/utils.R against worked examples and the committed pipeline outputs, so a
# refactor that changes the maths (not just the code) is caught even though
# no model needs to be fitted to catch it.

test_that("bf_hn(1, 2, 5) reproduces the Dienes (2020) footnote-4 worked example (documented at R/utils.R bf_hn())", {
  expect_equal(bf_hn(1, 2, 5), 0.5615, tolerance = 1e-4)
})

test_that("bf_re on the primary trial inputs reproduces the committed quadrature Bayes factor to 1e-9", {
  inputs <- readr::read_csv(root_path("output", "bayesian", "trials_input_effects.csv"), show_col_types = FALSE)
  prim <- inputs[inputs$input_set == "primary_k3", ]
  # The authoritative stored quadrature value is in dienes_bayes_factors_random_effects.csv
  # (the bf_bridge column of trials_bridge_bf.csv is the independent bridge-sampling
  # check computed in R/03, agreeing only to about 10%, per R/05's own check).
  target <- readr::read_csv(root_path("output", "tables", "dienes_bayes_factors_random_effects.csv"),
                            show_col_types = FALSE)
  expected <- target$bf_random_effects[target$h1_scale_label == "primary_log2" & target$tau_scale == 0.5]
  expect_length(expected, 1)

  val <- bf_re(prim$yi, prim$sei, log(2), 0.5)
  expect_equal(val, expected, tolerance = 1e-9)
})

test_that("bf_re on the primary trial inputs rounds to the reported 6.41", {
  inputs <- readr::read_csv(root_path("output", "bayesian", "trials_input_effects.csv"), show_col_types = FALSE)
  prim <- inputs[inputs$input_set == "primary_k3", ]
  expect_equal(round(bf_re(prim$yi, prim$sei, log(2), 0.5), 2), 6.41)
})

test_that("bf_re's quadrature upper bound (5 vs 10) does not matter for tau_scale 0.25, 0.5 or 1", {
  inputs <- readr::read_csv(root_path("output", "bayesian", "trials_input_effects.csv"), show_col_types = FALSE)
  prim <- inputs[inputs$input_set == "primary_k3", ]
  for (ts in c(0.25, 0.5, 1)) {
    a <- bf_re(prim$yi, prim$sei, log(2), ts, upper = 5)
    b <- bf_re(prim$yi, prim$sei, log(2), ts, upper = 10)
    expect_lt(abs(a / b - 1), 1e-6)
  }
})

test_that("null_interval_probs partitions draws correctly around the null interval", {
  probs <- null_interval_probs(c(-1, 0, 0.1, 0.5), m = log(1.25))
  expect_equal(unname(probs["p_below"]), 0.25)
  expect_equal(unname(probs["p_inside"]), 0.5)
  expect_equal(unname(probs["p_above"]), 0.25)
})

test_that("bf_category's boundaries are strict: exactly 3 and exactly 1/3 are 'insensitive'", {
  expect_equal(bf_category(3), "insensitive")
  expect_equal(bf_category(3 + 1e-9), "H1")
  expect_equal(bf_category(1 / 3), "insensitive")
  expect_equal(bf_category(1 / 3 - 1e-9), "H0")
})
