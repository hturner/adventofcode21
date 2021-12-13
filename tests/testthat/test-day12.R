test_that("f06a works on example data", {
  expect_equal(f12a(example_data_12(1)), 10)
  expect_equal(f12a(example_data_12(2)), 19)
})

test_that("f06a works on example data", {
  expect_equal(f12b(example_data_12(1)), 36)
})
