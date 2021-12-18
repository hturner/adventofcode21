test_that("f18a works on example data", {
  expect_equal(max(f18a(example_data_18(2))), 4140)
})
