library(adventofcode21)
x <- read.fwf("./inst/input03.txt", widths = rep(1, 12))

p1 <- f03a(x)
p2 <- f03b(x)

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)
