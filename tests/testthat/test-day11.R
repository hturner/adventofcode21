test_that("f11a works", {
  expect_equal(f11a(example_data_11(1), 2), 9)
  expect_equal(f11a(example_data_11(2), 10), 204)
  expect_equal(f11a(example_data_11(2), 100), 1656)
})

test_that("f11b works", {
  expect_equal(f11b(example_data_11(2)), 195)
})
