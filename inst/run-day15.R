library(adventofcode21)
x <- readLines("./inst/input15.txt")
x <- do.call(rbind, strsplit(x, ""))
storage.mode(x) <- "numeric"

p1 <- f15a(x)
p2 <- f15b(x)

stopifnot(p1 == aoc_solutions$day15a)
stopifnot(p2 == aoc_solutions$day15b)
