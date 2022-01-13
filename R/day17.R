#' Day 17: Trick Shot
#'
#' [Trick Shot](https://adventofcode.com/2021/day/17)
#'
#' @name day17
#' @rdname day17
#' @details
#'
#' **Part One**
#'
#' You finally decode the Elves\' message. `HI`, the message says. You
#' continue searching for the sleigh keys.
#'
#' Ahead of you is what appears to be a large [ocean
#' trench](https://en.wikipedia.org/wiki/Oceanic_trench). Could the keys
#' have fallen into it? You\'d better send a probe to investigate.
#'
#' The probe launcher on your submarine can fire the probe with any
#' [integer](https://en.wikipedia.org/wiki/Integer) velocity in the `x`
#' (forward) and `y` (upward, or downward if negative) directions. For
#' example, an initial `x,y` velocity like `0,10` would fire the probe
#' straight up, while an initial velocity like `10,-1` would fire the probe
#' forward at a slight downward angle.
#'
#' The probe\'s `x,y` position starts at `0,0`. Then, it will follow some
#' trajectory by moving in *steps*. On each step, these changes occur in
#' the following order:
#'
#' -   The probe\'s `x` position increases by its `x` velocity.
#' -   The probe\'s `y` position increases by its `y` velocity.
#' -   Due to drag, the probe\'s `x` velocity changes by `1` toward the
#'     value `0`; that is, it decreases by `1` if it is greater than `0`,
#'     increases by `1` if it is less than `0`, or does not change if it is
#'     already `0`.
#' -   Due to gravity, the probe\'s `y` velocity decreases by `1`.
#'
#' For the probe to successfully make it into the trench, the probe must be
#' on some trajectory that causes it to be within a *target area* after any
#' step. The submarine computer has already calculated this target area
#' (your puzzle input). For example:
#'
#'     target area: x=20..30, y=-10..-5
#'
#' This target area means that you need to find initial `x,y` velocity
#' values such that after any step, the probe\'s `x` position is at least
#' `20` and at most `30`, *and* the probe\'s `y` position is at least `-10`
#' and at most `-5`.
#'
#' Given this target area, one initial velocity that causes the probe to be
#' within the target area after any step is `7,2`:
#'
#'     .............#....#............
#'     .......#..............#........
#'     ...............................
#'     S........................#.....
#'     ...............................
#'     ...............................
#'     ...........................#...
#'     ...............................
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTT#TT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'
#' In this diagram, `S` is the probe\'s initial position, `0,0`. The `x`
#' coordinate increases to the right, and the `y` coordinate increases
#' upward. In the bottom right, positions that are within the target area
#' are shown as `T`. After each step (until the target area is reached),
#' the position of the probe is marked with `#`. (The bottom-right `#` is
#' both a position the probe reaches and a position in the target area.)
#'
#' Another initial velocity that causes the probe to be within the target
#' area after any step is `6,3`:
#'
#'     ...............#..#............
#'     ...........#........#..........
#'     ...............................
#'     ......#..............#.........
#'     ...............................
#'     ...............................
#'     S....................#.........
#'     ...............................
#'     ...............................
#'     ...............................
#'     .....................#.........
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................T#TTTTTTTTT
#'     ....................TTTTTTTTTTT
#'
#' Another one is `9,0`:
#'
#'     S........#.....................
#'     .................#.............
#'     ...............................
#'     ........................#......
#'     ...............................
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTT#
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'     ....................TTTTTTTTTTT
#'
#' One initial velocity that *doesn\'t* cause the probe to be within the
#' target area after any step is `17,-4`:
#'
#'     S..............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     .................#.............................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT................................
#'     ....................TTTTTTTTTTT..#.............................
#'     ....................TTTTTTTTTTT................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ................................................#..............
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ...............................................................
#'     ..............................................................#
#'
#' The probe appears to pass through the target area, but is never within
#' it after any step. Instead, it continues down and to the right - only
#' the first few steps are shown.
#'
#' If you\'re going to fire a highly scientific probe out of a super cool
#' probe launcher, you might as well do it with *style*. How high can you
#' make the probe go while still reaching the target area?
#'
#' In the above example, using an initial velocity of `6,9` is the best you
#' can do, causing the probe to reach a maximum `y` position of `45`. (Any
#' higher initial `y` velocity causes the probe to overshoot the target
#' area entirely.)
#'
#' Find the initial velocity that causes the probe to reach the highest `y`
#' position and still eventually be within the target area after any step.
#' *What is the highest `y` position it reaches on this trajectory?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @param part1 set to `TRUE` if solving part 1, `FALSE` otherwise
#' @return For Part One, `f17a(x)` returns .... For Part Two,
#'   `f17b(x)` returns ....
#' @export
#' @examples
#' max(f17a(example_data_17()))
#' length(f17b(example_data_17()))
f17a <- function(x) {
  left <- as.numeric(gsub(".*=([0-9]+)[..].*", "\\1", x))
  right <- as.numeric(gsub(".*[..]([0-9]+),.*", "\\1", x))
  bottom <- as.numeric(gsub(".*y=([-0-9]+)[..].*", "\\1", x))
  top <- as.numeric(gsub(".*[..]([-0-9]+)", "\\1", x))
  opt_fun <- function(velocity_x, velocity_y,
                      top, bottom, left, right){
    x <- y <- m <- 0
    target <- FALSE
    coord <- c(x = 0, y = 0)
    while (y >= bottom & x <= right)   {
      x <- x + velocity_x
      y <- y + velocity_y
      m <- max(m, y)
      if (y >= bottom & y <= top & x <= right & x >= left){
        target <- TRUE
        break
      }
      velocity_x <- velocity_x - 1*(velocity_x > 0) + 1*(velocity_x < 0)
      velocity_y <- velocity_y - 1
      coord <- rbind(coord, c(x = x, y = y))
    }
    ifelse(target, m, -Inf)
  }

  # the maximum y will be achieved for a trajectory that ends in the target area

  # so find valid starting values for vx, that end in the target area
  # vx^2 must be less than 2*right, since x_end = vx*(vx + 1)/2 < right
  # similarly (vx + 1)^2 must be greater than 2*left
  vx <- ceiling(sqrt(2*left) - 1):floor(sqrt(2*right))
  x_end <- vx*(vx + 1)/2
  valid_vx <- vx[x_end >= left & x_end <= right]

  # for negative vy, max y will always be zero, so just consider +ve vy here
  # for positive vy, there will be a step from zero of -(vy+1)
  vy_range <- seq(abs(bottom + 1))
  J <- length(vy_range)
  val <- numeric(length(valid_vx) * J)
  for (i in seq_along(valid_vx)){
    for (j in vy_range){
      val[(i - 1) * J + j] <- opt_fun(valid_vx[i], j, top, bottom, left, right)
    }
  }
  val[is.finite(val)]
}


#' @rdname day17
#' @export
f17b <- function(x, part1 = TRUE) {
  left <- as.numeric(gsub(".*=([0-9]+)[..].*", "\\1", x))
  right <- as.numeric(gsub(".*[..]([0-9]+),.*", "\\1", x))
  bottom <- as.numeric(gsub(".*y=([-0-9]+)[..].*", "\\1", x))
  top <- as.numeric(gsub(".*[..]([-0-9]+)", "\\1", x))
  opt_fun <- function(velocity_x, velocity_y,
                      top, bottom, left, right){
    x <- y <- m <- 0
    target <- FALSE
    coord <- c(x = 0, y = 0)
    while (y >= bottom & x <= right)   {
      x <- x + velocity_x
      y <- y + velocity_y
      m <- max(m, y)
      if (y >= bottom & y <= top & x <= right & x >= left){
        target <- TRUE
        break
      }
      velocity_x <- velocity_x - 1*(velocity_x > 0) + 1*(velocity_x < 0)
      velocity_y <- velocity_y - 1
      coord <- rbind(coord, c(x = x, y = y))
    }
    ifelse(target, m, -Inf)
  }

  # now trajectory must reach target area, but can go beyond
  vx_range <- ceiling(sqrt(2*left) - 1):right
  # vy can now be negative, down to bottom
  vy_range <- bottom:(-bottom - 1)

  val <- matrix(nrow = length(vx_range), ncol = length(vy_range))
  for (i in seq_along(vx_range)){
    for (j in seq_along(vy_range)){
      val[i, j] <- opt_fun(vx_range[i], vy_range[j], top, bottom, left, right)
    }
  }
  c(val[is.finite(val)])
}



f17_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day17
#' @export
example_data_17 <- function(example = 1) {
  l <- list(
    a = "target area: x=20..30, y=-10..-5"
  )
  l[[example]]
}
