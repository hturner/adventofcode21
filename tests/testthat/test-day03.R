test_that("f03a gives correct answer for example data", {
  x <- example_data_03()
  expect_equal(f03a(x), c(gamma = 22, epsilon = 9))
})

test_that("f0bb gives correct answer for example data", {
  x <- example_data_03()
  expect_equal(f03b(x), c(oxygen_generator = 23, CO2_scrubber = 10))
})
