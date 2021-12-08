test_that("f07a works", {
  x <- example_data_07()
  expect_equal(f07a(x), 37)
})
test_that("f07b works", {
  x <- example_data_07()
  expect_equal(f07b(x), 168)
})
