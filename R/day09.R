#' Day 09: Smoke Basin
#'
#' [Smoke Basin](https://adventofcode.com/2021/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' These caves seem to be [lava
#' tubes](https://en.wikipedia.org/wiki/Lava_tube). Parts are even still
#' volcanically active; small hydrothermal vents release smoke into the
#' caves that slowly [settles like
#' rain]{title="This was originally going to be a puzzle about watersheds, but we're already under water."}.
#'
#' If you can model how the smoke flows through the caves, you might be
#' able to avoid it and be that much safer. The submarine generates a
#' heightmap of the floor of the nearby caves for you (your puzzle input).
#'
#' Smoke flows to the lowest point of the area it\'s in. For example,
#' consider the following heightmap:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Each number corresponds to the height of a particular location, where
#' `9` is the highest and `0` is the lowest a location can be.
#'
#' Your first goal is to find the *low points* - the locations that are
#' lower than any of its adjacent locations. Most locations have four
#' adjacent locations (up, down, left, and right); locations on the edge or
#' corner of the map have three or two adjacent locations, respectively.
#' (Diagonal locations do not count as adjacent.)
#'
#' In the above example, there are *four* low points, all highlighted: two
#' are in the first row (a `1` and a `0`), one is in the third row (a `5`),
#' and one is in the bottom row (also a `5`). All other locations on the
#' heightmap have some lower adjacent location, and so are not low points.
#'
#' The *risk level* of a low point is *1 plus its height*. In the above
#' example, the risk levels of the low points are `2`, `1`, `6`, and `6`.
#' The sum of the risk levels of all low points in the heightmap is
#' therefore `15`.
#'
#' Find all of the low points on your heightmap. *What is the sum of the
#' risk levels of all low points on your heightmap?*
#'
#' **Part Two*
#'
#' Next, you need to find the largest basins so you know what areas are
#' most important to avoid.
#'
#' A *basin* is all locations that eventually flow downward to a single low
#' point. Therefore, every low point has a basin, although some basins are
#' very small. Locations of height `9` do not count as being in any basin,
#' and all other locations will always be part of exactly one basin.
#'
#' The *size* of a basin is the number of locations within the basin,
#' including the low point. The example above has four basins.
#'
#' The top-left basin, size `3`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The top-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The middle basin, size `14`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The bottom-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Find the three largest basins and multiply their sizes together. In the
#' above example, this is `9 * 14 * 9 = 1134`.
#'
#' *What do you get if you multiply together the sizes of the three largest
#' basins?*
#'
#' @param x input data
#' @param low matrix of coordinates for lowest point in each basin
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' x <- example_data_09()
#' low <- f09a(x)
#' sum(1 + x[low])
#' f09b(x, low)
f09a <- function(x) {
  nr <- nrow(x)
  nc <- ncol(x)
  up <- rbind(10, x[-nr,]) > x
  down <- rbind(x[-1,], 10) > x
  left <- cbind(10, x[,-nc]) > x
  right <- cbind(x[,-1], 10) > x
  which(up & down & left & right, arr.ind = TRUE)
}


#' @rdname day09
#' @export
f09b <- function(x, low) {
  # flood fill basin with 2s given starting point
  # (most basic algorithm: https://en.wikipedia.org/wiki/Flood_fill)
  floodfill <- function(M, row, col) {
    # return M if starting point not in basin
    if (!row %in% 1:nrow(M) || !col %in% 1:ncol(M)) return(M)
    if (M[row, col] != 0) return(M)
    # replace old value with new value
    M[row, col] <- 2
    # repeat for cells up, down, left & right
    M <- Recall(M, row + 1, col    )
    M <- Recall(M, row - 1, col    )
    M <- Recall(M, row    , col - 1)
    M <- Recall(M, row    , col + 1)
    return(M)
  }

  # find basins (edge = 1, inside basin = 0)
  basins <- x == 9
  mode(basins) <- "numeric"

  # floodfill each basin starting from lowest point
  # and count number of filled values (value = 2)
  n <- nrow(low)
  size <- numeric(n)
  for (i in seq_len(n)){
    size[i] <- sum(floodfill(basins, low[i, "row"], low[i, "col"]) == 2)
  }
  prod(sort(size, decreasing = TRUE)[1:3])
}


f09_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = matrix(c(2,1,9,9,9,4,3,2,1,0,
                 3,9,8,7,8,9,4,9,2,1,
                 9,8,5,6,7,8,9,8,9,2,
                 8,7,6,7,8,9,6,7,8,9,
                 9,8,9,9,9,6,5,6,7,8), byrow = TRUE, nrow = 5)
    )
  l[[example]]
}
