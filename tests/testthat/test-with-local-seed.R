# with_local_seed() (D1) is what makes the historically unseeded pp_check()
# and bridge_sampler() calls reproducible without perturbing anything else
# that draws random numbers later in the same script; both properties are
# load-bearing, not just "it returns a value".

test_that("with_local_seed gives identical results on repeated calls", {
  x1 <- with_local_seed(42, runif(10))
  x2 <- with_local_seed(42, runif(10))
  expect_identical(x1, x2)
})

test_that("with_local_seed does not advance or otherwise disturb the caller's random stream", {
  set.seed(1)
  before <- runif(5)
  set.seed(1)
  with_local_seed(999, runif(3))
  after <- runif(5)
  expect_identical(before, after)
})

test_that("with_local_seed leaves no .Random.seed behind when none existed beforehand", {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(list = ".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  had_before <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  expect_false(had_before)
  with_local_seed(1, runif(1))
  had_after <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  expect_false(had_after)
})
