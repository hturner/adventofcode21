#' Day 15: Chiton
#'
#' [Chiton](https://adventofcode.com/2021/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' You\'ve almost reached the exit of the cave, but the walls are getting
#' closer together. Your submarine can barely still fit, though; the main
#' problem is that the walls of the cave are covered in
#' [chitons](https://en.wikipedia.org/wiki/Chiton), and it would be best
#' not to bump any of them.
#'
#' The cavern is large, but has a very low ceiling, restricting your motion
#' to two dimensions. The shape of the cavern resembles a square; a quick
#' scan of chiton density produces a map of *risk level* throughout the
#' cave (your puzzle input). For example:
#'
#'     1163751742
#'     1381373672
#'     2136511328
#'     3694931569
#'     7463417111
#'     1319128137
#'     1359912421
#'     3125421639
#'     1293138521
#'     2311944581
#'
#' You start in the top left position, your destination is the bottom right
#' position, and you [cannot move
#' diagonally]{title="Can't go diagonal until we can repair the caterpillar unit. Could be the liquid helium or the superconductors."}.
#' The number at each position is its *risk level*; to determine the total
#' risk of an entire path, add up the risk levels of each position you
#' *enter* (that is, don\'t count the risk level of your starting position
#' unless you enter it; leaving it adds no risk to your total).
#'
#' Your goal is to find a path with the *lowest total risk*. In this
#' example, a path with the lowest total risk is highlighted here:
#'
#'     1163751742
#'     1381373672
#'     2136511328
#'     3694931569
#'     7463417111
#'     1319128137
#'     1359912421
#'     3125421639
#'     1293138521
#'     2311944581
#'
#' The total risk of this path is `40` (the starting position is never
#' entered, so its risk is not counted).
#'
#' *What is the lowest total risk of any path from the top left to the
#' bottom right?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f15a(x)` returns .... For Part Two,
#'   `f15b(x)` returns ....
#' @export
#' @examples
#' f15a(example_data_15())
#' f15b()
f15a <- function(x) {
  # convert weight matrix to graph
  nr <- nrow(x)
  nc <- ncol(x)
  n <- length(x)
  vertices <- array(seq_len(n), dim = dim(x))
  ## different weights in different directions!
  edges <- data.frame(from = c(seq_len(n - nr),
                               seq(nr + 1, n),
                               t(vertices)[seq_len(n - nr)],
                               t(vertices)[seq(nr + 1, n)]),
                      to = c(seq(nr + 1, n),
                             seq_len(n - nr),
                             t(vertices)[seq(nr + 1, n)],
                             t(vertices)[seq_len(n - nr)]),
                      weight = c(x[,-1], x[, -nc],
                                 t(x)[,-1], t(x)[,-nc]))
  g <- igraph::graph_from_data_frame(edges, directed = TRUE)
  p <- igraph::shortest_paths(g, from = 1, to = n)[[1]][[1]]
  sum(x[p[-1]])
}


#' @rdname day15
#' @export
f15b <- function(x) {
  x <- cbind(x, (x + 1) %% 9, (x + 2) %% 9, (x + 3) %% 9, (x + 4) %% 9)
  x <- rbind(x, (x + 1) %% 9, (x + 2) %% 9, (x + 3) %% 9, (x + 4) %% 9)
  x[x == 0] <- 9
  x
}


f15_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day15
#' @export
example_data_15 <- function(example = 1) {
  l <- list(
    a = matrix(c(1, 1, 2, 3, 7, 1, 1, 3, 1, 2, 1, 3, 1,
                 6, 4, 3, 3, 1, 2, 3, 6, 8, 3, 9, 6, 1, 5, 2, 9,
                 1, 3, 1, 6, 4, 3, 9, 9, 5, 3, 1, 7, 3, 5, 9, 4,
                 1, 9, 4, 1, 9, 5, 7, 1, 3, 1, 2, 1, 2, 3, 4, 1,
                 3, 1, 1, 7, 8, 2, 1, 8, 4, 7, 6, 3, 5, 1, 1, 4,
                 6, 5, 5, 4, 7, 2, 6, 1, 3, 2, 3, 2, 8, 2, 2, 8,
                 9, 1, 7, 1, 9, 1, 1), 10, 10)
  )
  l[[example]]
}
