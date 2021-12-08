library(adventofcode21)
x <- readLines("./inst/input08.txt")
x <- strsplit(x, split = "\\W+")
x <- lapply(x, function(e) list(e[1:10], e[11:14]))

p1 <- f08a(x)
p2 <- f08b(x)

stopifnot(p1 == aoc_solutions$day08a)
stopifnot(p2 == aoc_solutions$day08b)
