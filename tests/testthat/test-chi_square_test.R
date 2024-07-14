test_that("chi_square_test works correctly", {
  result <- chi_square_test(mtcars, "cyl", "gear", plot = FALSE)
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("parameter" %in% names(result))
})
