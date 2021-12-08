#install.packages("remotes")
#install.packages("rmarkdown")
#install.packages("devtools")
#remotes::install_github("tjmahr/aoc")

library(aoc)
day <- 8
use_day(day)
devtools::load_all()
download_part2_to_roxygen_md(day, clip = FALSE)

x <- example_data_08()
x <- c("acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd",
       "cdfgeb", "eafb", "cagedb", "ab", "cdfeb", "fcadb", "cdfeb", "cdbaf")

be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
  fdgacbe cefdb cefbgd gcbe
