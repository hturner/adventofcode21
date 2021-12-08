test_that("f08a works", {
  x <- example_data_08()
  expect_equal(f08a(x), 26)
})

test_that("f08b works", {
  x <- example_data_08()
  expect_equal(f08b(x), 61229)
})
