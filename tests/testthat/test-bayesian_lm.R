test_that("bayesian_lm works correctly", {
  result <- bayesian_lm(mtcars, mpg ~ wt, plot = FALSE)
  expect_true("BFBayesFactor" %in% class(result))
})
