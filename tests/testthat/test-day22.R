test_that("f22a works on example data", {
  ex <- example_data_22(1)
  expect_equal(f22a(ex$x, ex$on), 39)
})
