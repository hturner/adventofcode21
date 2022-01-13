test_that("helper functions work", {
  x <- 2
  expect_equal({inp(x);mul(x, -1);x}, -2)
  prog1 <- c("inp z", "inp x", "mul z 3", "eql z x")
  prog2 <- c("inp w", "add z w", "mod z 2", "div w 2", "add y w", "mod y 2",
             "div w 2", "add x w", "mod x 2", "div w 2", "mod w 2")
  z <- 2
  x <- 0
  run_prog(prog1)
  expect_equal(z, 0)
  z <- 2
  x <- 6
  run_prog(prog1)
  expect_equal(z, 1)
  w <- 5
  x <- y <- z <- 0
  run_prog(prog2)
  expect_equal(paste0(w, x, y, z), "0101")
  w <- 9
  x <- y <- z <- 0
  run_prog(prog2)
  expect_equal(paste0(w, x, y, z), "1001")
})
