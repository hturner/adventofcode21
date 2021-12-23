test_that("f21a works on example data", {
  expect_equal(f21a(4, 8), 739785)
})

test_that("f21b works on example data", {
  expect_equal(max(f21b(4, 8)), 444356092776315)
})
