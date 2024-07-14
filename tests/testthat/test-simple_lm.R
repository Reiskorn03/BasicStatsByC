test_that("simple_lm works correctly", {
  result <- simple_lm(mtcars, mpg ~ wt, plot = FALSE)
  expect_true("coefficients" %in% names(result))
  expect_true("r.squared" %in% names(result))
  expect_true("adj.r.squared" %in% names(result))
  expect_true("fstatistic" %in% names(result))
})
