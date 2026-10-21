
test_that("Poisson PMF works correctly", {
  expect_equal(poisson_pmf(3, 2), stats::dpois(3, 2))
  expect_equal(poisson_pmf(0, 3), exp(-3))
  expect_equal(sum(poisson_pmf(0:20, 3)), 1, tolerance = 1e-6)
})

test_that("Poisson CDF works correctly", {
  expect_equal(poisson_cdf(3, 2), stats::ppois(3, 2))
  expect_equal(poisson_cdf(0, 3), exp(-3))
})

test_that("Poisson random generation works correctly", {
  set.seed(123)
  x <- poisson_rnd(100, 3)
  
  expect_length(x, 100)
  expect_true(all(x >= 0))
  expect_true(all(x == floor(x)))
})

test_that("Poisson moments are correct", {
  result <- poisson_moments(3)
  
  expect_equal(result$mean, 3)
  expect_equal(result$variance, 3)
  expect_error(poisson_moments(-1))
  expect_error(poisson_moments(NA_real_))
})
