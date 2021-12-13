test_that("f09a works", {
  x <- example_data_09()
  expect_equal(f09a(x), 15)
})

test_that("f09b works", {
  x <- example_data_09()
  expect_equal(f09b(x), 1134)
})
