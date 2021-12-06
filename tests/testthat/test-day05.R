test_that("f05a gives correct answer for example data", {
  x <- example_data_05()
  expect_equal(do.call("f05a", x), 5)
})

test_that("f05b gives correct answer for example data", {
  x <- example_data_05()
  expect_equal(do.call("f05b", x), 12)
})
