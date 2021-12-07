library(adventofcode21)
x <- scan("./inst/input07.txt", sep = ",")

p1 <- f07a(x)
p2 <- f07b(x)

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)
