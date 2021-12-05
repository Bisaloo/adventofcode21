library(adventofcode21)
x <- readLines("./inst/input05.txt") %>%
  strsplit("\\D+") %>%
  lapply(as.numeric)
x <- do.call(rbind.data.frame, x)
names(x) <- c("x1", "y1", "x2", "y2")

p1 <- f05a(x)
p2 <- f05b(x)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)

readRDS(
