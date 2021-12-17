test_that("f17a works on example data", {
  expect_equal(max(f17a(example_data_17(1))), 45)
})

test_that("f17b works on example data", {
  expect_equal(length(f17b(example_data_17(1))), 112)
})
