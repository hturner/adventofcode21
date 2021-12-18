#' Day 18: Snailfish
#'
#' [Snailfish](https://adventofcode.com/2021/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' You descend into the ocean trench and encounter some
#' [snailfish](https://en.wikipedia.org/wiki/Snailfish). They say they saw
#' the sleigh keys! They\'ll even tell you which direction the keys went if
#' you help one of the smaller snailfish with his
#' *[math]{title="Or 'maths', if you have more than one."} homework*.
#'
#' Snailfish numbers aren\'t like regular numbers. Instead, every snailfish
#' number is a *pair* - an ordered list of two elements. Each element of
#' the pair can be either a regular number or another pair.
#'
#' Pairs are written as `[x,y]`, where `x` and `y` are the elements within
#' the pair. Here are some example snailfish numbers, one snailfish number
#' per line:
#'
#'     [1,2]
#'     [[1,2],3]
#'     [9,[8,7]]
#'     [[1,9],[8,5]]
#'     [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
#'     [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
#'     [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
#'
#' This snailfish homework is about *addition*. To add two snailfish
#' numbers, form a pair from the left and right parameters of the addition
#' operator. For example, `[1,2]` + `[[3,4],5]` becomes
#' `[[1,2],[[3,4],5]]`.
#'
#' There\'s only one problem: *snailfish numbers must always be reduced*,
#' and the process of adding two snailfish numbers can result in snailfish
#' numbers that need to be reduced.
#'
#' To *reduce a snailfish number*, you must repeatedly do the first action
#' in this list that applies to the snailfish number:
#'
#' -   If any pair is *nested inside four pairs*, the leftmost such pair
#'     *explodes*.
#' -   If any regular number is *10 or greater*, the leftmost such regular
#'     number *splits*.
#'
#' Once no action in the above list applies, the snailfish number is
#' reduced.
#'
#' During reduction, at most one action applies, after which the process
#' returns to the top of the list of actions. For example, if *split*
#' produces a pair that meets the *explode* criteria, that pair *explodes*
#' before other *splits* occur.
#'
#' To *explode* a pair, the pair\'s left value is added to the first
#' regular number to the left of the exploding pair (if any), and the
#' pair\'s right value is added to the first regular number to the right of
#' the exploding pair (if any). Exploding pairs will always consist of two
#' regular numbers. Then, the entire exploding pair is replaced with the
#' regular number `0`.
#'
#' Here are some examples of a single explode action:
#'
#' -   `[[[[[9,8],1],2],3],4]` becomes `[[[[0,9],2],3],4]` (the `9` has no
#'     regular number to its left, so it is not added to any regular
#'     number).
#' -   `[7,[6,[5,[4,[3,2]]]]]` becomes `[7,[6,[5,[7,0]]]]` (the `2` has no
#'     regular number to its right, and so it is not added to any regular
#'     number).
#' -   `[[6,[5,[4,[3,2]]]],1]` becomes `[[6,[5,[7,0]]],3]`.
#' -   `[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]` becomes
#'     `[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]` (the pair `[3,2]` is unaffected
#'     because the pair `[7,3]` is further to the left; `[3,2]` would
#'     explode on the next action).
#' -   `[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]` becomes
#'     `[[3,[2,[8,0]]],[9,[5,[7,0]]]]`.
#'
#' To *split* a regular number, replace it with a pair; the left element of
#' the pair should be the regular number divided by two and rounded *down*,
#' while the right element of the pair should be the regular number divided
#' by two and rounded *up*. For example, `10` becomes `[5,5]`, `11` becomes
#' `[5,6]`, `12` becomes `[6,6]`, and so on.
#'
#' Here is the process of finding the reduced result of
#' `[[[[4,3],4],4],[7,[[8,4],9]]]` + `[1,1]`:
#'
#'     after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
#'     after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
#'     after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
#'     after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
#'     after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
#'     after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
#'
#' Once no reduce actions apply, the snailfish number that remains is the
#' actual result of the addition operation:
#' `[[[[0,7],4],[[7,8],[6,0]]],[8,1]]`.
#'
#' The homework assignment involves adding up a *list of snailfish numbers*
#' (your puzzle input). The snailfish numbers are each listed on a separate
#' line. Add the first snailfish number and the second, then add that
#' result and the third, then add that result and the fourth, and so on
#' until all numbers in the list have been used once.
#'
#' For example, the final sum of this list is
#' `[[[[1,1],[2,2]],[3,3]],[4,4]]`:
#'
#'     [1,1]
#'     [2,2]
#'     [3,3]
#'     [4,4]
#'
#' The final sum of this list is `[[[[3,0],[5,3]],[4,4]],[5,5]]`:
#'
#'     [1,1]
#'     [2,2]
#'     [3,3]
#'     [4,4]
#'     [5,5]
#'
#' The final sum of this list is `[[[[5,0],[7,4]],[5,5]],[6,6]]`:
#'
#'     [1,1]
#'     [2,2]
#'     [3,3]
#'     [4,4]
#'     [5,5]
#'     [6,6]
#'
#' Here\'s a slightly larger example:
#'
#'     [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
#'     [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
#'     [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
#'     [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
#'     [7,[5,[[3,8],[1,4]]]]
#'     [[2,[2,2]],[8,[8,1]]]
#'     [2,9]
#'     [1,[[[9,3],9],[[9,0],[0,7]]]]
#'     [[[5,[7,4]],7],1]
#'     [[[[4,2],2],6],[8,7]]
#'
#' The final sum `[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]` is
#' found after adding up the above snailfish numbers:
#'
#'       [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
#'     + [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
#'     = [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
#'
#'       [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
#'     + [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
#'     = [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
#'
#'       [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
#'     + [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
#'     = [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
#'
#'       [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
#'     + [7,[5,[[3,8],[1,4]]]]
#'     = [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
#'
#'       [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
#'     + [[2,[2,2]],[8,[8,1]]]
#'     = [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
#'
#'       [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
#'     + [2,9]
#'     = [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
#'
#'       [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
#'     + [1,[[[9,3],9],[[9,0],[0,7]]]]
#'     = [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
#'
#'       [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
#'     + [[[5,[7,4]],7],1]
#'     = [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
#'
#'       [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
#'     + [[[[4,2],2],6],[8,7]]
#'     = [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
#'
#' To check whether it\'s the right answer, the snailfish teacher only
#' checks the *magnitude* of the final sum. The magnitude of a pair is 3
#' times the magnitude of its left element plus 2 times the magnitude of
#' its right element. The magnitude of a regular number is just that
#' number.
#'
#' For example, the magnitude of `[9,1]` is `3*9 + 2*1 = 29`; the magnitude
#' of `[1,9]` is `3*1 + 2*9 = 21`. Magnitude calculations are recursive:
#' the magnitude of `[[9,1],[1,9]]` is `3*29 + 2*21 = 129`.
#'
#' Here are a few more magnitude examples:
#'
#' -   `[[1,2],[[3,4],5]]` becomes `143`.
#' -   `[[[[0,7],4],[[7,8],[6,0]]],[8,1]]` becomes `1384`.
#' -   `[[[[1,1],[2,2]],[3,3]],[4,4]]` becomes `445`.
#' -   `[[[[3,0],[5,3]],[4,4]],[5,5]]` becomes `791`.
#' -   `[[[[5,0],[7,4]],[5,5]],[6,6]]` becomes `1137`.
#' -   `[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]` becomes
#'     `3488`.
#'
#' So, given this example homework assignment:
#'
#'     [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
#'     [[[5,[2,8]],4],[5,[[9,9],0]]]
#'     [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
#'     [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
#'     [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
#'     [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
#'     [[[[5,4],[7,7]],8],[[8,3],8]]
#'     [[9,3],[[9,9],[6,[4,9]]]]
#'     [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
#'     [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
#'
#' The final sum is:
#'
#'     [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
#'
#' The magnitude of this final sum is `4140`.
#'
#' Add up all of the snailfish numbers from the homework assignment in the
#' order they appear. *What is the magnitude of the final sum?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f18a(x)` returns .... For Part Two,
#'   `f18b(x)` returns ....
#' @export
#' @examples
#' f18a(example_data_18())
#' f18b()
f18a <- function(x) {
  as_list <- function(x){
    x <- gsub("[", "list(a =", x, fixed = TRUE)
    x <- gsub(",", ",b =", x, fixed = TRUE)
    x <- gsub("]", ")", x, fixed = TRUE)
    eval(parse(text = x))
  }

  label <- function(x, pair = TRUE){
    nm <- gsub("[.]", "$", names(x))
    if (pair) nm <- sub("[$ab]{2}$", "", nm)
    nm
  }

  explode <- function(x){
    z <- unlist(x)
    lab <- label(z, pair = TRUE)
    nested4 <- nchar(lab) == 7
    do <- any(nested4)
    if (do) {
      id <- which(nested4)[1:2]
      i <- id[1] - 1
      if (i > 0) z[i] <- z[i] + z[id[1]]
      j <- id[2] + 1
      if (j <= length(z)) z[j] <- z[j] + z[id[2]]
      x <- relist(z, x)
      eval(parse(text = paste0("x$", lab[id[1]], "<- 0")))
    }
    structure(x, explode = do)
  }

  split <- function(x){
    z <- unlist(x)
    lab <- label(z, pair = FALSE)
    over <- z >= 10
    do <- any(over)
    if (do){
      id <- which(over)[1]
      num <- eval(parse(text = paste0("unname(z[", id, "])")))
      pair <- list(a = floor(num/2), b = ceiling(num/2))
      expr <- parse(text = paste0("x$", lab[id], " <- pair"))
      eval(eval(substitute(expr, list(pair = pair))))
    }
    structure(x, split = do)
  }
  add <- function(a, b){
    x <- list(a = a, b = b)
    # reduce
    repeat{
      x <- explode(x)
      done_explode <- attr(x, "explode")
      if (!done_explode){
        x <- split(x)
        done_split <- attr(x, "split")
        if (!done_split) break
      }
    }
    x
  }
  magnitude <- function(a, b){
    if (length(a) == 1 & length(b) == 1){
      return(3*a + 2*b)
    }
    if (length(a) == 1 & length(b) == 2){
      return(3*a + 2*Recall(b[[1]], b[[2]]))
    }
    if (length(a) == 2 & length(b) == 1){
      return(3*Recall(a[[1]], a[[2]]) + 2*b)
    }
    return(3*Recall(a[[1]], a[[2]]) + 2*Recall(b[[1]], b[[2]]))
  }

  y <- lapply(x, as_list)
  n <- length(y)
  res <- y[[1]]
  for (i in 2:n){
    res <- add(res, y[[i]])
  }

  magnitude(res[[1]], res[[2]])
}


#' @rdname day18
#' @export
f18b <- function(x) {

}


f18_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_data_18 <- function(example = 1) {
  l <- list(
    a = c("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
          "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
          "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
          "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
          "[7,[5,[[3,8],[1,4]]]]", "[[2,[2,2]],[8,[8,1]]]", "[2,9]",
          "[1,[[[9,3],9],[[9,0],[0,7]]]]",
          "[[[5,[7,4]],7],1]", "[[[[4,2],2],6],[8,7]]"),
    b = c("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
          "[[[5,[2,8]],4],[5,[[9,9],0]]]",
          "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
          "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
          "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
          "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
          "[[[[5,4],[7,7]],8],[[8,3],8]]",
          "[[9,3],[[9,9],[6,[4,9]]]]",
          "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
          "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")
  )
  l[[example]]
}
