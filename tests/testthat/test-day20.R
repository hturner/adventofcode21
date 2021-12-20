test_that("f20a works on example data", {
  expect_equal(f20a(example_data_20(1), example_data_20(2), steps = 2), 35)
  expect_equal(f20a(example_data_20(1), example_data_20(2), steps = 50), 3351)
})
