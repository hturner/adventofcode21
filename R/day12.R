#' Day 12: Passage Pathing
#'
#' [Passage Pathing](https://adventofcode.com/2021/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' With your [submarine\'s subterranean subsystems subsisting
#' suboptimally]{title="Sublime."}, the only way you\'re getting out of
#' this cave anytime soon is by finding a path yourself. Not just *a* path
#' - the only way to know if you\'ve found the *best* path is to find *all*
#' of them.
#'
#' Fortunately, the sensors are still mostly working, and so you build a
#' rough map of the remaining caves (your puzzle input). For example:
#'
#'     start-A
#'     start-b
#'     A-c
#'     A-b
#'     b-d
#'     A-end
#'     b-end
#'
#' This is a list of how all of the caves are connected. You start in the
#' cave named `start`, and your destination is the cave named `end`. An
#' entry like `b-d` means that cave `b` is connected to cave `d` - that is,
#' you can move between them.
#'
#' So, the above cave system looks roughly like this:
#'
#'         start
#'         /   \
#'     c--A-----b--d
#'         \   /
#'          end
#'
#' Your goal is to find the number of distinct *paths* that start at
#' `start`, end at `end`, and don\'t visit small caves more than once.
#' There are two types of caves: *big* caves (written in uppercase, like
#' `A`) and *small* caves (written in lowercase, like `b`). It would be a
#' waste of time to visit any small cave more than once, but big caves are
#' large enough that it might be worth visiting them multiple times. So,
#' all paths you find should *visit small caves at most once*, and can
#' *visit big caves any number of times*.
#'
#' Given these rules, there are `10` paths through this example cave
#' system:
#'
#'     start,A,b,A,c,A,end
#'     start,A,b,A,end
#'     start,A,b,end
#'     start,A,c,A,b,A,end
#'     start,A,c,A,b,end
#'     start,A,c,A,end
#'     start,A,end
#'     start,b,A,c,A,end
#'     start,b,A,end
#'     start,b,end
#'
#' (Each line in the above list corresponds to a single path; the caves
#' visited by that path are listed in the order they are visited and
#' separated by commas.)
#'
#' Note that in this cave system, cave `d` is never visited by any path: to
#' do so, cave `b` would need to be visited twice (once on the way to cave
#' `d` and a second time when returning from cave `d`), and since cave `b`
#' is small, this is not allowed.
#'
#' Here is a slightly larger example:
#'
#'     dc-end
#'     HN-start
#'     start-kj
#'     dc-start
#'     dc-HN
#'     LN-dc
#'     HN-end
#'     kj-sa
#'     kj-HN
#'     kj-dc
#'
#' The `19` paths through it are as follows:
#'
#'     start,HN,dc,HN,end
#'     start,HN,dc,HN,kj,HN,end
#'     start,HN,dc,end
#'     start,HN,dc,kj,HN,end
#'     start,HN,end
#'     start,HN,kj,HN,dc,HN,end
#'     start,HN,kj,HN,dc,end
#'     start,HN,kj,HN,end
#'     start,HN,kj,dc,HN,end
#'     start,HN,kj,dc,end
#'     start,dc,HN,end
#'     start,dc,HN,kj,HN,end
#'     start,dc,end
#'     start,dc,kj,HN,end
#'     start,kj,HN,dc,HN,end
#'     start,kj,HN,dc,end
#'     start,kj,HN,end
#'     start,kj,dc,HN,end
#'     start,kj,dc,end
#'
#' Finally, this even larger example has `226` paths through it:
#'
#'     fs-end
#'     he-DX
#'     fs-he
#'     start-DX
#'     pj-DX
#'     end-zg
#'     zg-sl
#'     zg-pj
#'     pj-he
#'     RW-he
#'     fs-DX
#'     pj-RW
#'     zg-RW
#'     start-pj
#'     he-WI
#'     zg-he
#'     pj-fs
#'     start-RW
#'
#' *How many paths through this cave system are there that visit small
#' caves at most once?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a(example_data_12())
#' f12b()
#' @importFrom dplyr left_join
f12a <- function(x) {
 middle <- x$from != "start" & x$to !="end"
 x <- rbind(x, data.frame(from = x$to[middle], to = x$from[middle]))
 n <- 0
 i <- 1
 paths <- data.frame(x1 = "start")
 join_by <- "from"
 while (nrow(paths)){
   i <- i + 1
   names(join_by) <- paste0("x", i - 1)
   paths <- dplyr::left_join(paths, x, by = join_by)
   names(paths)[i] <- paste0("x", i)
   lower <- grepl("^[a-z]", paths[,i])
   revisited <- rowSums(paths[,-i, drop = FALSE] == paths[,i]) > 0
   ended <- paths[, i] == "end"
   paths <- paths[!(ended | (lower & revisited)),]
   n <- n + sum(ended)
 }
 n
}


#' @rdname day12
#' @export
f12b <- function(x) {
  middle <- x$from != "start" & x$to !="end"
  x <- rbind(x, data.frame(from = x$to[middle], to = x$from[middle]))
  n <- 0
  i <- 1
  paths <- data.frame(max = 0, x1 = "start")
  join_by <- "from"
  while (nrow(paths)){
    i <- i + 1
    names(join_by) <- paste0("x", i - 1)
    paths <- left_join(paths, x, by = join_by)
    names(paths)[i + 1] <- paste0("x", i)
    lower <- grepl("^[a-z]", paths[,i + 1])
    revisits <- rowSums(paths[,-(i + 1), drop = FALSE] == paths[,i + 1])
    maxed <- revisits > 0 & paths[,"max"] == 1
    paths[, "max"] <- pmax(paths[, "max"], lower * revisits)
    ended <- paths[, i + 1] == "end"
    paths <- paths[!(ended | (lower & maxed)),]
    n <- n + sum(ended)
  }
  n
}


f12_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day12
#' @export
example_data_12 <- function(example = 1) {
  l <- list(
    a = data.frame(from = c("start", "start", "A", "A", "b", "A", "b"),
                   to = c("A", "b", "c", "b", "d", "end", "end")),
    b = data.frame(from = c("dc", "HN", "start", "dc", "dc", "LN",
                    "HN", "kj", "kj", "kj"),
                   to = c("end", "start", "kj", "start",
                          "HN", "dc", "end", "sa", "HN", "dc"))
  )
  l[[example]]
}
