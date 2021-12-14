library(adventofcode21)
x <- readLines("./inst/input14.txt")

x <- list(
  template = x[1],
  rules = setNames(as.data.frame(do.call(rbind, strsplit(x[3:102], " -> "))),
                   c("pair", "insert"))
)

p1 <- f14a(x)
p2 <- f14b(x)

stopifnot(p1 == aoc_solutions$day14a)
stopifnot(p2 == aoc_solutions$day14b)
