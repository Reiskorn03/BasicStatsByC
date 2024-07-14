test_that("bayesian_t_test works correctly", {
  result <- bayesian_t_test(mtcars, "am", "mpg", plot = FALSE)
  expect_true("BFBayesFactor" %in% class(result))
})
