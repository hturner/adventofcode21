Advent of Code 2021: First days with R
================

The Advent of Code (<https://adventofcode.com>) is a series of daily
programming puzzles running up to Christmas. On 3 December, the Warwick
R User Group (<https://www.meetup.com/Warwick-useRs>) met jointly with
the Manchester R-thritis Statistical Computing Group
(<https://personalpages.manchester.ac.uk/staff/david.selby/rthritis.html>)
to informally discuss our solutions to the puzzles from the first two
days. Some of the participants shared their solutions in advance as
shared in this slide deck:
<https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-12-03-advent2021/>.

In this post, Heather Turner (RSE Fellow, Statistics) shares her
solutions and how they can be improved based on the ideas put forward at
the meetup, while James Tripp (Senior Research Software Engineer,
Information and Digital Group) reflects on some issues raised in the
meetup discussion.

## Day 1: Sonar Sweep

For a full description of the problem for Day 1, see
<https://adventofcode.com/2021/day/1>.

## Day 1 - Part 1

How many measurements are larger than the previous measurement?

    199  (N/A - no previous measurement)
    200  (increased)
    208  (increased)
    210  (increased)
    200  (decreased)
    207  (increased)
    240  (increased)
    269  (increased)
    260  (decreased)
    263  (increased)

First we’ll create a vector with the example data:

``` r
x <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
```

Then the puzzle can be solved with the following R function, that takes
the vector `x` as input, uses `diff()` to compute differences between
consecutive values of `x`, then sums the differences that are positive:

``` r
f01a <- function(x) {
  dx <- diff(x)
  sum(sign(dx) == 1)
}
f01a(x)
```

    ## [1] 7

Inspired by David Selby’s solution
(<https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-12-03-advent2021/resources/adventofcode.html#7>),
this could be made slightly simpler by finding the positive differences
with `dx > 0`, rather than using the `sign()` function.

## Day 1 - Part 2

How many **sliding three-measurement sums** are larger than the previous
sum?

    199  A           607  (N/A - no previous sum)
    200  A B         618  (increased)
    208  A B C       618  (no change)
    210    B C D     617  (decreased)
    200  E   C D     647  (increased)
    207  E F   D     716  (increased)
    240  E F G       769  (increased)
    269    F G H     792  (increased)
    260      G H
    263        H

This can be solved by the following function of `x`. First, the rolling
sums of three consecutive values are computed in a vectorized
computation, i.e. creating three vectors containing the first, second
and third value in the sum, then adding the vectors together. Then, the
function from Part 1 is used to sum the positive differences between
these values.

``` r
f01b <- function(x) {
  n <- length(x)
  sum3 <- x[1:(n - 2)] + x[2:(n - 1)] + x[3:n]
  f01a(sum3)
}
f01b(x)
```

    ## [1] 5

David Schoch put forward a solution
(<https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-12-03-advent2021/resources/adventofcode.html#8>)
that takes advantage of the fact that the difference between consecutive
rolling sums of three values is just the difference between values three
places apart (the second and third values in the first sum cancel out
the first and second values in the second sum). Putting what we’ve
learnt together gives this much neater solution for Day 1 Part 2:

``` r
f01b_revised <- function(x) {
  dx3 <- diff(x, lag = 3)
  sum(dx3 > 0)
}
f01b_revised(x)
```

    ## [1] 5

# Day 2: Dive!

For a full description of the problem see
<https://adventofcode.com/2021/day/2>.

## Day 2 - Part 1

-   `forward X` increases the horizontal position by `X` units.
-   `down X` increases the depth by `X` units.
-   `up X` decreases the depth by `X` units.

<!-- -->

                      horizontal  depth
    forward 5   -->            5      -
    down 5      -->                   5
    forward 8   -->           13
    up 3        -->                   2
    down 8      -->                  10
    forward 2   -->           15

    ==> horizontal = 15, depth = 10

First we’ll create a data frame with the example data

``` r
x <-data.frame(direction = c("forward", "down", "forward",
                             "up", "down", "forward"),
               amount = c(5, 5, 8, 3, 8, 2))
```

Then the puzzle can be solved with the following function, which takes
the variables `direction` and `amount` as input. The horizontal position
is the sum of the amounts where the direction is “forward”. The depth is
the sum of the amounts where direction is “down” minus the sum of the
amounts where direction is “up”.

``` r
f02a <- function(direction, amount) {
  horizontal <- sum(amount[direction == "forward"])
  depth <- sum(amount[direction == "down"]) - sum(amount[direction == "up"])
  c(horizontal = horizontal, depth = depth)
}
f02a(x$direction, x$amount)
```

    ## horizontal      depth 
    ##         15         10

The code above uses logical indexing to select the amounts that
contribute to each sum. An alternative presented by David Selby
(<https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-12-03-advent2021/resources/adventofcode.html#16>)
is to coerce the logical indices to numeric (coercing `TRUE` to 1 and
`FALSE` to 0) and multiply amount by the resulting vectors:

``` r
f02a_selby <- function(direction, amount) {
  horizontal_move <- amount * (direction == 'forward')
  depth_move <- amount * ((direction == 'down') - (direction == 'up'))
  c(horizontal = sum(horizontal_move), depth = sum(depth_move))
}
```

Benchmarking on 1000 datasets of 1000 rows this solution is only
marginally faster (31 μs vs 37 μs), but it has an advantage in Part 2!

------------------------------------------------------------------------

## Day 2 - Part 2

-   `down X` increases your aim by `X` units.
-   `up X` decreases your aim by `X` units.
-   `forward X` does two things:
    -   It increases your horizontal position by `X` units.
    -   It increases your depth by your aim **multiplied by** `X`.

<!-- -->

                      horizontal  aim  depth
    forward 5   -->            5    -      - 
    down 5      -->                 5      
    forward 8   -->           13          40
    up 3        -->                 2
    down 8      -->                10
    forward 2   -->           15          60

    ==> horizontal = 15, depth = 60

The following function solves this problem by first computing the sign
of the change to aim, which is negative if the direction is “up” and
positive otherwise. Then for each change in position, if the direction
is “forward” the function adds the amount to the horizontal position and
the amount multiplied by aim to the depth, otherwise it adds the sign
multiplied by the amount to the aim.

``` r
f02b <- function(direction, amount) {
  horizontal <- depth <- aim <- 0
  sign <- ifelse(direction == "up", -1, 1)
  for (i in seq_along(direction)){
    if (direction[i] == "forward"){
      horizontal <- horizontal + amount[i]
      depth <- depth + aim * amount[i]
      next
    }
    aim <- aim + sign[i]*amount[i]
  }
  c(horizontal = horizontal, depth = depth)
}
f02b(x$direction, x$amount)
```

    ## horizontal      depth 
    ##         15         60

As an intepreted language, for loops can be slow in R and vectorized
solutions are often preferable if memory is not an issue. David Selby
showed that his solution from Part 1 can be extended to solve the
problem in Part 2, by using cumulative sums of the value that
represented `depth` in Part 1 to compute the `aim` value in Part 2.

``` r
f02b_revised <- function(direction, amount) {
  horizontal_move <- amount * (direction == "forward")
  aim <- cumsum(amount * (direction == "down") - amount * (direction == "up"))
  depth_move <- aim * horizontal_move
  c(horizontal = sum(horizontal_move), depth = sum(depth_move))
}
f02b_revised(x$direction, x$amount)
```

    ## horizontal      depth 
    ##         15         60

Benchmarking these two solutions on 1000 data sets of 1000 rows, the
vectorized solution is ten times faster (58 μs vs 514 μs).

# Reflections
