test_that("summary_stats works correctly", {
  result <- summary_stats(mtcars)
  expect_true("Mean" %in% colnames(result))
  expect_true("Median" %in% colnames(result))
  expect_true("StdDev" %in% colnames(result))
  expect_true("Min" %in% colnames(result))
  expect_true("Max" %in% colnames(result))
  expect_equal(rownames(result), names(mtcars)[sapply(mtcars, is.numeric)])
})
