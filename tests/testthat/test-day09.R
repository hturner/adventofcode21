test_that("f09a works", {
  x <- example_data_09()
  low <- f09a(x)
  expect_equal(sum(1 + x[low]), 15)
})

test_that("f09b works", {
  x <- example_data_09()
  low <- f09a(x)
  expect_equal(f09b(x, low), 1134)
})
