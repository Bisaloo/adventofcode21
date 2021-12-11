library(adventofcode21)
x <- readLines("./inst/input11.txt")
x <- strsplit(x, "")
x <- do.call(rbind, x)
storage.mode(x) <- "numeric"

p1 <- f11a(x)
p2 <- f11b(x)

stopifnot(p1 == aoc_solutions$day11a)
stopifnot(p2 == aoc_solutions$day11b)
