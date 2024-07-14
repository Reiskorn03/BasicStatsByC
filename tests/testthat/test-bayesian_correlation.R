test_that("bayesian_correlation works correctly", {
  result <- bayesian_correlation(mtcars, "mpg", "wt", plot = FALSE)
  expect_true("BFBayesFactor" %in% class(result))
})
