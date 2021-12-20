test_that("f19a works on example data", {
  expect_equal(nrow(f19a(example_data_19())$beacons), 79)
  expect_equal(max(dist(f19a(example_data_19())$scanners, method = "manhattan")),
               3621)
})
