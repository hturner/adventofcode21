test_that("f15a works on example data", {
  x <- example_data_15()
  expect_equal(f15a(x), 40)
})

test_that("f15b works on example data", {
  expect_equal(f15a(f15b(example_data_15(1))), 315)
})
