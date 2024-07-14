test_that("t_test works correctly", {
  result <- t_test(mtcars, "am", "mpg", plot = FALSE)
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("conf.int" %in% names(result))
})
