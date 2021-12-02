library(adventofcode21)
x <- read.table("./inst/input02.txt", sep = " ")
names(x) <- c("direction", "distance")

p1 <- f02a(x)
p2 <- f02b(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
