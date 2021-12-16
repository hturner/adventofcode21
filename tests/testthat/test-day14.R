test_that("f14a works on example data", {
  expect_equal(f14a(example_data_14(1), example_data_14(2)), 1588)
})

test_that("f14b works on example data", {
  template <- readLines(system.file("input14.txt", package = "adventofcode21"),
                        n = 1)
  code <- scan(system.file("input14.txt", package = "adventofcode21"),
               what = "character", skip = 2, sep = "\n")
  template <- strsplit(template, "")[[1]]
  code <- structure(gsub(".*[ ]([A-Z])", "\\1", code),
                    names = gsub("([A-Z]+).*", "\\1", code))
  expect_equal(f14a(template, code, 10), f14b(template, code, 10))
  expect_equal(f14b(example_data_14(1), example_data_14(2), 40), 2188189693529)
})
