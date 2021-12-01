test_that("f01a gives correct answer for example data", {
  expect_equal(f01a(example_data_01()), 7)
})

test_that("f01b gives correct answer for example data", {
  expect_equal(f01b(example_data_01()), 5)
})
