#' Day 14: Extended Polymerization
#'
#' [Extended Polymerization](https://adventofcode.com/2021/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' The incredible pressures at this depth are starting to put a strain on
#' your submarine. The submarine has
#' [polymerization](https://en.wikipedia.org/wiki/Polymerization) equipment
#' that would produce suitable materials to reinforce the submarine, and
#' the nearby volcanically-active caves should even have the necessary
#' input elements in sufficient quantities.
#'
#' The submarine manual contains [instructions]{title="HO
#'
#' HO -> OH"} for finding the optimal polymer formula; specifically, it
#' offers a *polymer template* and a list of *pair insertion* rules (your
#' puzzle input). You just need to work out what polymer would result after
#' repeating the pair insertion process a few times.
#'
#' For example:
#'
#'     NNCB
#'
#'     CH -> B
#'     HH -> N
#'     CB -> H
#'     NH -> C
#'     HB -> C
#'     HC -> B
#'     HN -> C
#'     NN -> C
#'     BH -> H
#'     NC -> B
#'     NB -> B
#'     BN -> B
#'     BB -> N
#'     BC -> B
#'     CC -> N
#'     CN -> C
#'
#' The first line is the *polymer template* - this is the starting point of
#' the process.
#'
#' The following section defines the *pair insertion* rules. A rule like
#' `AB -> C` means that when elements `A` and `B` are immediately adjacent,
#' element `C` should be inserted between them. These insertions all happen
#' simultaneously.
#'
#' So, starting with the polymer template `NNCB`, the first step
#' simultaneously considers all three pairs:
#'
#' -   The first pair (`NN`) matches the rule `NN -> C`, so element `C` is
#'     inserted between the first `N` and the second `N`.
#' -   The second pair (`NC`) matches the rule `NC -> B`, so element `B` is
#'     inserted between the `N` and the `C`.
#' -   The third pair (`CB`) matches the rule `CB -> H`, so element `H` is
#'     inserted between the `C` and the `B`.
#'
#' Note that these pairs overlap: the second element of one pair is the
#' first element of the next pair. Also, because all pairs are considered
#' simultaneously, inserted elements are not considered to be part of a
#' pair until the next step.
#'
#' After the first step of this process, the polymer becomes `NCNBCHB`.
#'
#' Here are the results of a few steps using the above rules:
#'
#'     Template:     NNCB
#'     After step 1: NCNBCHB
#'     After step 2: NBCCNBBBCBHCB
#'     After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
#'     After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#'
#' This polymer grows quickly. After step 5, it has length 97; After step
#' 10, it has length 3073. After step 10, `B` occurs 1749 times, `C` occurs
#' 298 times, `H` occurs 161 times, and `N` occurs 865 times; taking the
#' quantity of the most common element (`B`, 1749) and subtracting the
#' quantity of the least common element (`H`, 161) produces
#' `1749 - 161 = 1588`.
#'
#' Apply 10 steps of pair insertion to the polymer template and find the
#' most and least common elements in the result. *What do you get if you
#' take the quantity of the most common element and subtract the quantity
#' of the least common element?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param template vector of letters in template
#' @param code vector of insertion letters named by corresponding pair of
#' letters
#' @param steps number of steps
#' @return For Part One, `f14a(x)` returns .... For Part Two,
#'   `f14b(x)` returns ....
#' @export
#' @examples
#' f14a(example_data_14(1), example_data_14(2))
#' f14b(example_data_14(1), example_data_14(2))
f14a <- function(template, code, steps = 10) {
  for(i in seq_len(steps)){
    n <- length(template)
    one <- template[1:(n-1)]
    two <- template[-1]
    pair <- paste0(one, two)
    template <- c(rbind(one,code[pair]),template[n])
  }
  diff(range(table(template)))
}


#' @rdname day14
#' @export
f14b <- function(template, code, steps = 10) {
  # matrix matching old pairs to new pairs
  nr <- length(code)
  pairs <-  names(code)
  key <- matrix(0, nr, nr, dimnames = list(pairs, pairs))
  one <- substr(names(code), 1, 1)
  two <- substr(names(code), 2, 2)
  key[cbind(pairs, paste0(one, code))] <- 1
  key[cbind(pairs, paste0(code, two))] <- 1
  # initial count of pairs
  count <- structure(numeric(nr), names = pairs)
  n <- length(template)
  one <- template[1:(n-1)]
  two <- template[-1]
  tab <- table(paste0(one, two))
  count[names(tab)] <- tab
  # update count of pairs in each step
  for (i in seq_len(steps)){
    count <- colSums(key * count)
  }
  count_values <- c(structure(count, names = substr(pairs, 1, 1)),
                    structure(1, names = template[n]))
  tab <- rowsum(count_values, names(count_values))
  diff(range(tab))
}


f14_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = c("N", "N", "C", "B"),
    b = c("CH" = "B", "HH" = "N", "CB" = "H", "NH" = "C", "HB" = "C",
          "HC" = "B", "HN" = "C", "NN" = "C", "BH" = "H", "NC" = "B",
          "NB" = "B", "BN" = "B", "BB" = "N", "BC" = "B", "CC" = "N",
          "CN" = "C")
  )
  l[[example]]
}
