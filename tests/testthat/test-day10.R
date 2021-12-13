test_that("f10a works", {
  x <- example_data_10()
  expect_equal(sum(f10a(x)), 26397)
})

test_that("f10b works", {
  x <- example_data_10()
  expect_equal(sum(f10b(x)), 288957)
})
