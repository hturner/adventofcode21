test_that("f06a works on example data", {
  x <- example_data_06()
  expect_equal(f06a(x,18), 26)
  expect_equal(f06a(x,80), 5934)
})
