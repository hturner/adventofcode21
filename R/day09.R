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
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b()
f09a <- function(x) {
  nr <- nrow(x)
  nc <- ncol(x)
  up <- rbind(10, x[-nr,]) - x > 0
  down <- rbind(x[-1,], 10) - x > 0
  left <- cbind(10, x[,-nc]) - x > 0
  right <- cbind(x[,-1], 10) - x > 0
  sum(1 + x[up & down & left & right])
}


#' @rdname day09
#' @export
f09b <- function(x) {
  nr <- nrow(x)
  nc <- ncol(x)
  up <- rbind(10, x[-nr,]) - x > 0
  down <- rbind(x[-1,], 10) - x > 0
  left <- cbind(10, x[,-nc]) - x > 0
  right <- cbind(x[,-1], 10) - x > 0
  low <- up & down & right & left
  id <- which(low, arr.ind = TRUE)
  nine <- x == 9
  n <- numeric(nrow(id))
  for (i in seq(nrow(id))){
    count_up_and_down <- function(nine, pos, keep = FALSE){
      count <- 1
      # go up to next invalid value (or end of column)
      for (j in rev(seq_len(pos[1] - 1))){
        if (!identical(nine[j, pos[2]], keep)) break
        count <- count + 1
      }
      # go down to next invalid value (or end of column)
      for (j in pos[1] + seq_len(nr - pos[1])){
        if (!identical(nine[j, pos[2]], keep)) break
        count <- count + 1
      }
      count
    }

    pos <- id[i,]
    # count non nines up and down from low point
    n[i] <- countF <- count_up_and_down(nine, pos, FALSE)
    # go right along columns
    for (j in seq_len(nc - pos[2])){
      # if not a nine, count non nines up and down
      if (!nine[pos[1], pos[2] + j]) {
        countF <- count_up_and_down(nine, c(pos[1], pos[2] + j), FALSE)
        n[i] <- n[i] + countF
      } else {
        # if a nine, check if counting along edge
        countT <- count_up_and_down(nine, c(pos[1], pos[2] + j), TRUE)
        if (countF == countT) break
        # else go down to next value that is not a 9 and count up and down
        for (k in seq_len(nr - pos[1])){
          if (nine[pos[1] + k, pos[2] + j]) break
        }
        countF <- count_up_and_down(nine, c(pos[1] + k, pos[2] + j), FALSE)
        n[i] <- n[i] + countF
      }
    }
    # go left along columns
    for (j in seq_len(pos[2] - 1)){
      if (!nine[pos[1], pos[2] - j]) {
        countF <- count_up_and_down(nine, c(pos[1], pos[2] - j), FALSE)
        n[i] <- n[i] + countF
      } else {
        countT <- count_up_and_down(nine, c(pos[1], pos[2] - j), TRUE)
        if (countF == countT) break
        # go down to next value that is not a 9
        for (k in seq_len(nr - pos[1])){
          if (!nine[pos[1] + k, pos[2] - j]) break
        }
        countF <- count_up_and_down(nine, c(pos[1] + k, pos[2] - j), FALSE)
        n[i] <- n[i] + countF
      }
    }
  }
  #prod(sort(n, decreasing = TRUE)[1:3])
  list(id = id, n = n)
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
