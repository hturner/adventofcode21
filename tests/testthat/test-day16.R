test_that("f16a works on example data", {
  expect_equal(f16a(example_data_16(1))$literal, 2021)
  expect_equal(f16a(example_data_16(2))$literal, list(10, 20))
  expect_equal(f16a(example_data_16(3))$literal, as.list(1:3))
  expect_equal(f16a(example_data_16(4))$version_sum, 16)
  expect_equal(f16a(example_data_16(5))$version_sum, 12)
  expect_equal(f16a(example_data_16(6))$version_sum, 23)
  expect_equal(f16a(example_data_16(7))$version_sum, 31)
})

test_that("f16a works on example data for part 2", {
  expect_equal(f16a(example_data_16(8))$value, 3)
  expect_equal(f16a(example_data_16(9))$value, 54)
  expect_equal(f16a(example_data_16(10))$value, 7)
  expect_equal(f16a(example_data_16(11))$value, 9)
  expect_equal(f16a(example_data_16(12))$value, 1)
  expect_equal(f16a(example_data_16(13))$value, 0)
  expect_equal(f16a(example_data_16(14))$value, 0)
  expect_equal(f16a(example_data_16(15))$value, 1)
})
