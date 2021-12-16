test_that("f13a works on example data", {
  expect_equal(f13a(example_data_13(1), example_data_13(2)), 17)
})

test_that("f13b works on example data", {
  expect_equal(f12b(example_data_12(1)), 36)
})
