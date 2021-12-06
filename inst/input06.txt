#install.packages("remotes")
#install.packages("rmarkdown")
#install.packages("devtools")
#remotes::install_github("tjmahr/aoc")

library(aoc)
day <- 6
use_day(day)
devtools::load_all()
download_part2_to_roxygen_md(day, clip = FALSE)
