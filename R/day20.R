#' Day 20: Trench Map
#'
#' [Trench Map](https://adventofcode.com/2021/day/20)
#'
#' @name day20
#' @rdname day20
#' @details
#'
#' **Part One**
#'
#' With the scanners fully deployed, you turn their attention to mapping
#' the floor of the ocean trench.
#'
#' When you get back the image from the scanners, it seems to just be
#' random noise. Perhaps you can combine an image enhancement algorithm and
#' the input image (your puzzle input) to clean it up a little.
#'
#' For example:
#'
#'     ..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#'     #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
#'     .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
#'     .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
#'     .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
#'     ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
#'     ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
#'
#'     #..#.
#'     #....
#'     ##..#
#'     ..#..
#'     ..###
#'
#' The first section is the *image enhancement algorithm*. It is normally
#' given on a single line, but it has been wrapped to multiple lines in
#' this example for legibility. The second section is the *input image*, a
#' two-dimensional grid of *light pixels* (`#`) and *dark pixels* (`.`).
#'
#' The image enhancement algorithm describes how to enhance an image by
#' *simultaneously* converting all pixels in the input image into an output
#' image. Each pixel of the output image is determined by looking at a 3x3
#' square of pixels centered on the corresponding input image pixel. So, to
#' determine the value of the pixel at (5,10) in the output image, nine
#' pixels from the input image need to be considered: (4,9), (4,10),
#' (4,11), (5,9), (5,10), (5,11), (6,9), (6,10), and (6,11). These nine
#' input pixels are combined into a single binary number that is used as an
#' index in the *image enhancement algorithm* string.
#'
#' For example, to determine the output pixel that corresponds to the very
#' middle pixel of the input image, the nine pixels marked by `[...]` would
#' need to be considered:
#'
#'     # . . # .
#'     #[. . .].
#'     #[# . .]#
#'     .[. # .].
#'     . . # # #
#'
#' Starting from the top-left and reading across each row, these pixels are
#' `...`, then `#..`, then `.#.`; combining these forms `...#...#.`. By
#' turning dark pixels (`.`) into `0` and light pixels (`#`) into `1`, the
#' binary number `000100010` can be formed, which is `34` in decimal.
#'
#' The image enhancement algorithm string is exactly 512 characters long,
#' enough to match every possible 9-bit binary number. The first few
#' characters of the string (numbered starting from zero) are as follows:
#'
#'     0         10        20        30  34    40        50        60        70
#'     |         |         |         |   |     |         |         |         |
#'     ..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#'
#' In the middle of this first group of characters, the character at index
#' 34 can be found: `#`. So, the output pixel in the center of the output
#' image should be `#`, a *light pixel*.
#'
#' This process can then be repeated to calculate every pixel of the output
#' image.
#'
#' Through advances in imaging technology, the images being operated on
#' here are *infinite* in size. *Every* pixel of the infinite output image
#' needs to be calculated exactly based on the relevant pixels of the input
#' image. The small input image you have is only a small region of the
#' actual infinite input image; the rest of the input image consists of
#' dark pixels (`.`). For the purposes of the example, to save on space,
#' only a portion of the infinite-sized input and output images will be
#' shown.
#'
#' The starting input image, therefore, looks something like this, with
#' more dark pixels (`.`) extending forever in every direction not shown
#' here:
#'
#'     ...............
#'     ...............
#'     ...............
#'     ...............
#'     ...............
#'     .....#..#......
#'     .....#.........
#'     .....##..#.....
#'     .......#.......
#'     .......###.....
#'     ...............
#'     ...............
#'     ...............
#'     ...............
#'     ...............
#'
#' By applying the image enhancement algorithm to every pixel
#' simultaneously, the following output image can be obtained:
#'
#'     ...............
#'     ...............
#'     ...............
#'     ...............
#'     .....##.##.....
#'     ....#..#.#.....
#'     ....##.#..#....
#'     ....####..#....
#'     .....#..##.....
#'     ......##..#....
#'     .......#.#.....
#'     ...............
#'     ...............
#'     ...............
#'     ...............
#'
#' Through further advances in imaging technology, the above output image
#' can also be used as an input image! This allows it to be enhanced *a
#' second time*:
#'
#'     ...............
#'     ...............
#'     ...............
#'     ..........#....
#'     ....#..#.#.....
#'     ...#.#...###...
#'     ...#...##.#....
#'     ...#.....#.#...
#'     ....#.#####....
#'     .....#.#####...
#'     ......##.##....
#'     .......###.....
#'     ...............
#'     ...............
#'     ...............
#'
#' Truly incredible - now the small details are really starting to come
#' through. After enhancing the original input image twice, `35` pixels are
#' lit.
#'
#' Start with the original input image and apply the image enhancement
#' algorithm twice, being careful to account for the infinite size of the
#' images. *How many pixels are lit in the resulting image?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param code image enhancement string
#' @param x logical matrix representing image, `TRUE` for "#", `FALSE` for `.`
#' @param steps number of enhancement steps
#' @return For Part One, `f20a(x)` returns .... For Part Two,
#'   `f20b(x)` returns ....
#' @export
#' @examples
#' f20a(example_data_20(1), example_data_20(2), steps = 2)
f20a <- function(code, x, steps) {
  enhance <- function(x){
    nr <- nrow(x)
    nc <- ncol(x)
    inf_pixel <- attr(x, "inf_pixel")
    # put x inside 2 layers of infinite pixels
    y <- matrix(inf_pixel, nr + 4, nc + 4)
    y[3:(nr + 2),3:(nc + 2)] <- x
    # copy y to create enhanced matrix based on current y values
    z <- y
    for(i in 2:(nr + 3)){
      for(j in 2:(nc + 3)){
        z[i,j] <- code[
          c(t(y[(i-1):(i+1),(j-1):(j+1)])) %*%
            2^c(8:0) + 1
        ]
      }
    }
    # return updated z pixels and update infinite pixel
    structure(z[2:(nr + 3), 2:(nc + 3)],
              inf_pixel = code[rep(inf_pixel, 9) %*% 2^c(8:0) + 1])
  }

  x <- structure(x, inf_pixel = 0)
  for (s in seq(steps)){
    x <- enhance(x)
  }
  sum(x)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day20
#' @export
example_data_20 <- function(example = 1) {
  l <- list(
    a = c(0,0,1,0,1,0,0,1,1,1,1,1,0,1,0,1,0,1,0,1,1,1,0,1,1,0,0,0,0,0,1,1,1,0,1,1,0,1,0,0,1,1,1,0,1,1,1,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,1, 1,0,0,1,1,1,1,1,1,0,1,1,1,0,0,0,1,1,1,1,0,0,1,0,0,1,1,1,1,1,0,0,1,1,0,0,1,0,1,1,1,1,1,0,0,0,1,1,0,1,0,1,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,1,1, 0,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,0,0,0,1,0,1,1,0,1,1,0,0,1,0,0,1,0,0,1,1,1,1,1,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0, 0,1,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,0,1,1,1,1,0,1,1,0,1,0,0,0,0,0, 0,1,0,0,1,0,0,0,1,1,0,1,0,1,1,0,0,1,0,0,0,1,1,0,1,0,1,1,0,0,1,1,1,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,1,1,0,1,1,1,0,1,1,0,0,0,1,0,0, 0,0,0,1,1,1,1,0,1,0,0,1,0,0,1,0,1,1,0,1,0,0,0,0,1,1,0,0,1,0,1,1,1,1,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0, 0,0,1,1,0,0,1,1,1,1,0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,1,0,0,1,0,1,0,0,1,1,1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,0,1,1,1,1,0,0,0,0,0,0,1,0,0,1),
    x = matrix(c(1,0,0,1,0,
                  1,0,0,0,0,
                  1,1,0,0,1,
                  0,0,1,0,0,
                  0,0,1,1,1), byrow = TRUE, ncol=5)
  )
  l[[example]]
}
