test_that("f04a gives correct answer for example data", {
  x <- example_data_04()
  expect_equal(f04a(x[[1]], x[[2]]), c(188, 24))
})

test_that("f04b gives correct answer for example data", {
  x <- example_data_04()
  expect_equal(f04b(x[[1]], x[[2]]), c(148, 13))
})
