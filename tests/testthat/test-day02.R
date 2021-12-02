test_that("f02a gives correct answer for example data", {
  x <- example_data_02()
  expect_equal(f02a(x$direction, x$amount), c(horizontal = 15, depth = 10))
})

test_that("f02b gives correct answer for example data", {
  x <- example_data_02()
  expect_equal(f02b(x$direction, x$amount), c(horizontal = 15, depth = 60))
})
